#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk)

(eval-when (:compile-toplevel)
  (defun process-args (args optional-p &optional (extension-p nil))
    "Splits ARGS into a list of argument names and a list of types which can be used for type declarations.
If OPTIONAL-P is truthy NULL is appended to each type declaration."
    (multiple-value-bind (arg-names
                          arg-types
                          unused-args)
        (loop for arg in args
              collect (first arg) into arg-names
              collect (list 'declare
                            (if optional-p
                                (list (list 'or (second arg) 'null) (first (first arg)))
                                (list (second arg) (first arg))))
              into arg-types
              when (third arg) collect (list 'declare (list 'ignore (first (first arg)))) into unused-args
              finally (return (cl:values arg-names arg-types unused-args)))
      (when (and optional-p
                 extension-p)
        (setf arg-names (reverse arg-names))
        (setf arg-types (reverse arg-types))
        (push (list 'extension-loader '*default-extension-loader*)
              arg-names)
        (push (list 'declare (list '%vk:extension-loader 'extension-loader))
              arg-types)
        (setf arg-names (reverse arg-names))
        (setf arg-types (reverse arg-types)))
      (cl:values arg-names arg-types unused-args)))
  
  (defun process-variables (variables &optional (extension-p nil))
    "Elements of VARIABLES should look like this:
(arg-name arg-type contents ...options)
options are :handle, :in/:out, :optional

arg-name is the name of the symbol used when calling the %vk function
contents is the source of the data which should be translated to the args memory location (i.e. the argument of the vk-function or something like the length of an argument of the vk-function)
"
    (let ((translated-args nil)
          (vk-input-args nil)
          (output-args nil)
          (let-args nil))
      (loop for var in variables
            do (let ((var-sym (gensym (string (first var)))))
                 (push
                  ;; single handles and single raw values are used directly
                  (if (or (and (member :handle var)
                               (member :in var)
                               (not (member :list var)))
                          (and (member :raw var)
                               (member :in var)
                               (eq (first var) (third var))))
                      (first var)
                      var-sym)
                  vk-input-args)
                 ;; all input variables which are not single handles or single raw values have to be translated
                 (when (or (and (member :in var)
                                (member :out var)
                                (not (member :ignore-in var)))
                           (and (not (member :out var))
                                (not (and (not (member :list var))
                                          (or (member :handle var)
                                              (member :raw var))))))
                     (push
                      (list var-sym
                            (second var) ;; type
                            (third var)) ;; contents
                      translated-args))
                 (when (member :out var)
                   (push
                     (list (first var)
                           (list var-sym
                                 (second var))) ;; type
                     output-args))
                 ;; single raw  values which depend on another variable (e.g. (length <other-var>) should be in a let-form
                 (when (and (member :raw var)
                                (member :in var)
                                (not (eq (first var) (third var))))
                     (push
                      (list var-sym (third var))
                      let-args))
                 ;; wrapped handles should also be in a let-form
                 (when (and (member :handle var)
                            (member :in var)
                            (or (member :dispatchable var)
                                (member :non-dispatchable var)))
                   (let ((init-form (if (member :list var)
                                        (list 'cl:map
                                              ''list
                                              (if (member :dispatchable var)
                                                  ''%dispatchable-handle
                                                  ''%non-dispatchable-handle)
                                              (first var))
                                        (list (if (member :dispatchable var)
                                                  '%dispatchable-handle
                                                  '%non-dispatchable-handle)
                                              (first var)))))
                     (push (list (first var) init-form) let-args)))))
      (when extension-p (push 'extension-loader vk-input-args))
      (cl:values (reverse translated-args) (reverse let-args) (reverse vk-input-args) (reverse output-args)))))

(defmacro defvkfun ((name
                     vulkan-fun
                     required-args
                     optional-args
                     &key
                       (no-vk-result-p nil)
                       (trivial-return-type nil)
                       (extension-p nil)
                       (len-provider 1)
                       (enumerate-p nil)
                       (first-array-arg-name nil)
                       (second-array-arg-name nil)
                       (count-arg-name nil)
                       (vk-constructor nil)
                       (handle-constructor nil)
                       (returns-struct-chain-p nil))
                    &body body)
  "Defines a function wrapping a function in the VULKAN package.
All functions in VK bind VK-ALLOC:*ALLOCATED-FOREIGN-OBJECTS* to a hash table local in their scope.

Arguments:
NAME          - the name of the function in VK
VULKAN-FUN    - the %VK function to wrap
REQUIRED-ARGS - required argument definitions for the VK function
OPTIONAL-ARGS - optional argument definitions for the VK function

Keyword Arguments:
NO-VK-RESULT-P         - T if the function does not return a VkResult
TRIVIAL-RETURN-TYPE    - the trivially translatable return type of the %VK function, can be :TRIVIAL or a pointer type, defaults to NIL
EXTENSION-P            - T if the function needs to be loaded explicitly (all extension functions except vk*KHR), EXTENSION-LOADER is added as optional argument to the VK function
LEN-PROVIDER           - a function call giving the size of a %VK array argument (:CREATE-HANDLES :ALLOCATE-HANDLES :GET-VALUE-ARRAY-AND-VALUE)
ENUMERATE-P            - set by functions enumerating values, they must query the result at least 2 times
FIRST-ARRAY-ARG-NAME   - the first array %VK output argument (:GET-STRUCT(-CHAINS) :ENUMERATE-* :GET-VALUE-ARRAY-AND-VALUE)
SECOND-ARRAY-ARG-NAME  - the second array %VK output argument, only in one case (:ENUMERATE-TWO-STRUCT-CHAINS) 
COUNT-ARG-NAME         - the %VK output argument acting as a counter for FIRST-ARRAY-ARG-NAME and SECOND-ARRAY-ARG-NAME
VK-CONSTRUCTOR         - some functions (:GET-STRUCT-CHAIN) must pass valid instances of the struct which they create using VK-CONSTRUCTOR
HANDLE-CONSTRUCTOR     - constructor of the handle wrapper used by functions returning handles (:GET-OR-CREATE-HANDLE :CREATE-HANDLES :ALLOCATE-HANDLES :ENUMERATE-HANDLES)
RETURNS-STRUCT-CHAIN-P - T if the function returns a pNext-extensible struct, NIL otherwise (default)

Body:
DOCSTRING - the first element of the body is the docstring.
VARIABLES - the rest of the body is the definition of arguments for the %VK function to wrap.
"
  (let* ((docstring (car body))
         (variables (cdr body))
         (single-array-result-p (and (and first-array-arg-name
                                          count-arg-name)
                                     (not second-array-arg-name)))
         (two-array-results-p (and first-array-arg-name
                                   second-array-arg-name
                                   count-arg-name))
         (single-array-single-value-p (and (not single-array-result-p)
                                           first-array-arg-name
                                           (not (eq len-provider 1)))))
    (assert (not (and enumerate-p
                      (not (or single-array-result-p
                               two-array-results-p))))
            () "Enumerate functions must have at least one array result.")
    (assert (not (and two-array-results-p
                      (not enumerate-p)))
            () "Function returning two lists must be an enumerate function.")
    (multiple-value-bind (required-arg-names
                          required-arg-declares)
        (process-args required-args nil)
      (multiple-value-bind (optional-arg-names
                            optional-arg-declares
                            ignore-arg-declares)
          (process-args optional-args t extension-p)
        (multiple-value-bind (translated-args
                              let-args
                              vk-input-args
                              output-args)
            (process-variables variables extension-p)
          (let ((handle-or-struct-def (second (first output-args)))
                (first-array-arg (when first-array-arg-name
                                   (find-if (lambda (a)
                                              (eq (first a) first-array-arg-name))
                                            output-args)))
                (second-array-arg (when second-array-arg-name
                                    (find-if (lambda (a)
                                               (eq (first a) second-array-arg-name))
                                             output-args)))
                (count-arg (when count-arg-name
                             (find-if (lambda (a)
                                        (eq (first a) count-arg-name))
                                      output-args)))
                (other-return-arg (when single-array-single-value-p
                                    (find-if-not (lambda (a)
                                                   (eq (first a) first-array-arg-name))
                                                 output-args)))
                (result (gensym "RESULT"))
                (i (gensym "INDEX"))
                (translated-count (gensym "TRANSLATED-COUNT"))
                (first-result-array (gensym "FIRST-RESULT-ARRAY"))
                (second-result-array (gensym "SECOND-RESULT-ARRAY")))
            `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
               ,docstring
               ;;(declare (optimize (speed 3)))
               ,@required-arg-declares
               ,@optional-arg-declares
               ,@ignore-arg-declares
               ;; bind *allocated-foreign-objects* to a local hash table in the function scope
               (let ((vk-alloc:*allocated-foreign-objects* (make-hash-table)))
                 (let (,@let-args)
                   (vk-alloc:with-foreign-allocated-objects (,@(remove-if (lambda (arg)
                                                                            ;; if lists of struct chains are returned
                                                                            ;; we don't want to translate them just yet
                                                                            (or (when first-array-arg-name
                                                                                  (eq (third arg)
                                                                                      first-array-arg-name))
                                                                                (when second-array-arg-name
                                                                                  (eq (third arg)
                                                                                      second-array-arg-name))))
                                                                          translated-args))
                     ,(cond
                        ;; trivial & fill-arbitrary-buffer
                        (trivial-return-type
                         (if (eq trivial-return-type :trivial)
                             `(,vulkan-fun ,@vk-input-args)
                             `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                (unless (cffi:null-pointer-p ,result)
                                  ,result))))
                        ;; single-array-single-result
                        (single-array-single-value-p
                         `(cffi:with-foreign-objects ((,@(second first-array-arg) ,len-provider)
                                                      (,@(second other-return-arg)))
                            (let ((,result (,vulkan-fun ,@vk-input-args)))
                              (cl:values (loop for ,i from 0 below ,len-provider
                                               collect (cffi:mem-aref ,@(second first-array-arg) ,i))
                                         (cffi:mem-aref ,@(second other-return-arg))
                                         ,result))))
                        ;; get-struct(-chain)s & enumerate(-struct-chains)
                        (single-array-result-p
                         (flet ((return-enumerated-list ()
                                  `(let ((,result :incomplete))
                                     (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                                           while (eq ,result :incomplete)
                                           finally (return (when (eq ,result :success)
                                                             (let ((,translated-count (cffi:mem-aref ,@(second count-arg))))
                                                               (cl:values
                                                                (when (> ,translated-count 0)
                                                                  (cffi:with-foreign-object (,@(second first-array-arg) ,translated-count)
                                                                    (setf ,result (,vulkan-fun ,@vk-input-args))
                                                                    (loop for ,i from 0 below ,translated-count
                                                                          collect ,(if handle-constructor
                                                                                       `(,handle-constructor (cffi:mem-aref ,@(second first-array-arg) ,i))
                                                                                       `(cffi:mem-aref ,@(second first-array-arg) ,i)))))
                                                                ,result)))))))
                                (return-list (use-translated-count-p)
                                  (if no-vk-result-p
                                      `(progn
                                         (,vulkan-fun ,@vk-input-args)
                                         (loop for ,i from 0 below ,(if use-translated-count-p
                                                                        translated-count
                                                                        (list 'length
                                                                              first-array-arg-name))
                                               collect (cffi:mem-aref ,@(second first-array-arg) ,i)))
                                      `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                         (cl:values
                                          (loop for ,i from 0 below ,(if use-translated-count-p
                                                                         translated-count
                                                                         (list 'length
                                                                               first-array-arg-name))
                                                collect (cffi:mem-aref ,@(second first-array-arg) ,i))
                                          ,result)))))
                           (if enumerate-p
                               (if returns-struct-chain-p
                                   ;; enumerate-struct-chains
                                   `(if ,first-array-arg-name
                                        (vk-alloc:with-foreign-allocated-objects (,@(remove-if-not (lambda (arg)
                                                                                                     (when first-array-arg-name
                                                                                                       (eq (third arg)
                                                                                                           first-array-arg-name)))
                                                                                                   translated-args)
                                                                                  (,@(second count-arg)
                                                                                   (cl:length ,first-array-arg-name)
                                                                                   nil))
                                          ,(return-enumerated-list))
                                        (cffi:with-foreign-object (,@(second count-arg))
                                          (let ((,(first (second first-array-arg)) (cffi:null-pointer)))
                                            ,(return-enumerated-list))))
                                   ;; enumerate
                                   `(cffi:with-foreign-object (,@(second count-arg))
                                      (let ((,(first (second first-array-arg)) (cffi:null-pointer)))
                                        ,(return-enumerated-list))))
                               (if returns-struct-chain-p
                                   ;; get-struct-chains
                                   (progn
                                     (assert vk-constructor
                                             () "FIRST-VK-CONSTRUCTOR must be provided for get-struct-chains functions")
                                     `(if ,first-array-arg-name
                                          (vk-alloc:with-foreign-allocated-objects (,@(remove-if-not (lambda (arg)
                                                                                                       (when first-array-arg-name
                                                                                                         (eq (third arg)
                                                                                                             first-array-arg-name)))
                                                                                                     translated-args)
                                                                                    (,@(second count-arg)
                                                                                     (cl:length ,first-array-arg-name)
                                                                                     nil))
                                            ,(return-list nil))
                                          (cffi:with-foreign-object (,@(second count-arg))
                                            (let ((,(first (second first-array-arg)) (cffi:null-pointer)))
                                              (,vulkan-fun ,@vk-input-args)
                                              (let ((,translated-count (cffi:mem-aref ,@(second count-arg))))
                                                (vk-alloc:with-foreign-allocated-object (,@(second first-array-arg)
                                                                                         (make-list ,translated-count
                                                                                                    :initial-element (,vk-constructor)))
                                                  ,(return-list t)))))))
                                   ;; get-structs
                                   `(cffi:with-foreign-object (,@(second count-arg))
                                      (let ((,(first (second first-array-arg)) (cffi:null-pointer)))
                                        (,vulkan-fun ,@vk-input-args)
                                        (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                                          (cffi:with-foreign-object (,@(second first-array-arg) ,translated-count)
                                            ,(return-list t)))))))))
                        ;; enumerate two struct chains
                        (two-array-results-p
                         `(cffi:with-foreign-object (,@(second count-arg))
                            (let ((,(first (second first-array-arg)) (cffi:null-pointer))
                                  (,(first (second second-array-arg)) (cffi:null-pointer))
                                  (,result :incomplete))
                              (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                                    while (eq ,result :incomplete)
                                    finally (return (when (eq ,result :success)
                                                      (let ((,translated-count (cffi:mem-aref ,@(second count-arg))))
                                                        (if (> ,translated-count 0)
                                                            (cffi:with-foreign-objects ((,@(second first-array-arg) ,translated-count)
                                                                                        (,@(second second-array-arg) ,translated-count))
                                                              (setf ,result (,vulkan-fun ,@vk-input-args))
                                                              (loop for ,i from 0 below ,translated-count
                                                                    collect (cffi:mem-aref ,@(second first-array-arg) ,i) into ,first-result-array
                                                                    collect (cffi:mem-aref ,@(second second-array-arg) ,i) into ,second-result-array
                                                                    finally (return (cl:values ,first-result-array ,second-result-array ,result))))
                                                            (cl:values nil nil ,result)))))))))
                        (t
                         (flet ((return-no-vk-result ()
                                  (progn
                                    (assert (eq len-provider 1)
                                            () "LEN-PROVIDER must be 1 for get-structs function which don't return a VkResult.")
                                    `(progn
                                       (,vulkan-fun ,@vk-input-args)
                                       ,(if handle-constructor
                                            `(,handle-constructor (cffi:mem-aref ,@handle-or-struct-def))
                                            `(cffi:mem-aref ,@handle-or-struct-def))))))
                           (if returns-struct-chain-p
                               ;; get-struct-chain
                               (progn
                                 (assert no-vk-result-p
                                         () "Functions returning struct chains must not return a VkResult.")
                                 (return-no-vk-result))
                               ;; create-handle & create-handles & get-struct
                               `(cffi:with-foreign-object (,@handle-or-struct-def ,len-provider)
                                  ,(if no-vk-result-p
                                       (return-no-vk-result)
                                       `(let ((,result (,vulkan-fun ,@vk-input-args)))
                                          (cl:values
                                           ,(cond
                                              ((not (eq len-provider 1))
                                               `(loop for ,i from 0 below ,len-provider
                                                      collect ,(if handle-constructor
                                                                   `(,handle-constructor (cffi:mem-aref ,@handle-or-struct-def ,i))
                                                                   `(cffi:mem-aref ,@handle-or-struct-def ,i))))
                                              (t
                                               (if handle-constructor
                                                   `(,handle-constructor (cffi:mem-aref ,@handle-or-struct-def))
                                                   `(cffi:mem-aref ,@handle-or-struct-def))))
                                           ,result))))))))))))))))))
