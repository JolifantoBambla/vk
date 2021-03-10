#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk)

(eval-when (:compile-toplevel ;; todo: remove other stages when this is done
            :load-toplevel
            :execute)
  (defun process-args (args optional-p &optional (extension-p nil))
    "Splits ARGS into a list of argument names and a list of types which can be used for type declarations.
If OPTIONAL-P is truthy NULL is appended to each type declaration."
    (loop for arg in args
          for i from 1
          collect (first arg) into arg-names
          collect (list 'declare (if optional-p (list (list 'or (second arg) 'null) (first (first arg))) (list (second arg) (first arg)))) into arg-types
          when (and optional-p
                    extension-p
                    (= i (length args)))
          collect '(extension-loader *default-extension-loader) into arg-names
          when (and optional-p
                    extension-p
                    (= i (length args)))
          collect (list 'declare (list (list or %vk:extension-loader null) extension-loader)) into arg-types
          finally (return (cl:values arg-names arg-types))))
  
  (defun process-variables (variables &optional (extension-p nil))
    "Elements of VARIABLES should look like this:
(arg-name arg-type contents ...options)
options are :handle, :in/:out, :optional
"
    (let ((translated-args nil)
          (vk-input-args nil)
          (output-args nil))
      (loop for var in variables
            do (let ((var-sym (gensym)))
                 (push
                  (if (and (find :handle var)
                           (find :in var))
                      (first var)
                      var-sym)
                  vk-input-args)
                 (if (and (not (find :handle var))
                          (not (find :out var)))
                     (push
                      (list var-sym
                            (second var)             ;; type
                            (third var))             ;; contents
                      translated-args))
                 (if (find :out var)
                     (push
                      (list (first var)
                            (list var-sym
                                  (second var))) ;; type, TODO: how to handle count?
                      output-args))))
      (when extension-p (push extension-loader vk-input-args))
      (cl:values (reverse translated-args) (reverse vk-input-args) (reverse output-args)))))

;;; ---------------------------------------------- 0 output parameters -----------------------------------------------------------------

;; case 0a: no or implicit return value - e.g. vkDestroyInstance
;; case 0b: non-trivial return value - e.g. vkGetInstanceProcAddr
(defmacro defvk-simple-fun ((name vulkan-fun docstring required-args optional-args &optional (return-type nil) (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is a function that has no output parameters, but might return a value.
E.g. no value returned: vkDestroyInstance
E.g. value returned: vkGetInstanceProcAddr

Note: VkBool32 and VkResult are treated as no return values, since they are implicitly converted to lisp values and don't have to be translated explicitely."
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args) (process-variables variables extension-p)
        (let ((result (gensym)))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
                  ,docstring
                  ,@required-arg-declares
                  ,@optional-arg-declares
                  (vk-alloc:with-foreign-allocated-objects (,@translated-args)
                    ,(if return-type
                         `(let ((,result (,vulkan-fun ,@vk-input-args)))
                            (unless (cffi:null-pointer-p ,result)
                              (cffi:mem-aref ,result ,return-type)))
                         `(,vulkan-fun ,@vk-input-args)))))))))

;;; --------------------------------------------- 1 output parameter -------------------------------------------------------------------

;; case 1a-1: create a handle - e.g. vkCreateInstance
;; case 1a-2: get an existing handle - e.g. vkGetDeviceQueue
(defmacro defvk-create-handle-fun ((name vulkan-fun docstring required-args optional-args &optional (no-result-p nil) (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that creates or gets some sort of handle and might return a RESULT.
E.g. returning a result: vkCreateInstance
E.g. returning no result: vkGetDeviceQueue"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((handle-def (second (first output-args)))
              (result (gensym)))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (vk-alloc:with-foreign-allocated-objects (,@translated-args)
               (cffi:with-foreign-object (,@handle-def)
                 ,(if no-result-p
                      `(progn
                         (,vulkan-fun ,@vk-input-args)
                         (cffi:mem-aref ,@handle-def))
                      `(let ((,result (,vulkan-fun ,@vk-input-args)))
                         (cl:values (cffi:mem-aref ,@handle-def)
                                    ,result)))))))))))

;; case 1b-1: create a list of handles (len by arg) - e.g. vkCreateGraphicsPipelines
;; case 1b-2: create a list of handles (len by struct member) - e.g. vkAllocateCommandBuffers
(defmacro defvk-create-handles-fun ((name vulkan-fun docstring required-args optional-args len-provider &optional (extension-p nil)) &body variables)
    "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that creates multiple handles and returns a RESULT.
The number of created handles is given by LEN-PROVIDER.
E.g. LEN-PROVIDER is the length of an input parameter: vkCreateGraphicsPipelines
E.g. LEN-PROVIDER is a slot value of an input parameter: vkAllocateCommandBuffers"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((handle-def (second (first output-args)))
              (result (gensym))
              (i (gensym)))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (vk-alloc:with-foreign-allocated-objects (,@translated-args)
               (cffi:with-foreign-object (,@handle-def ,len-provider)
                 (let ((,result (,vulkan-fun ,@vk-input-args)))
                   (cl:values (loop for ,i from 0 below ,len-provider
                                    collect (cffi:mem-aref ,@handle-def ,i))
                              ,result))))))))))

;; case 1c-1: get a struct extended by its NEXT slot - e.g. vkGetPhysicalDeviceFeatures2
;; case 1c-2: get a struct without NEXT slot - e.g. vkGetPhysicalDeviceProperties
(defmacro defvk-get-struct-fun ((name vulkan-fun docstring required-args optional-args &optional (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets a struct and returns a RESULT.
E.g. vkGetPhysicalDeviceProperties"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((struct-def (second (first output-args))))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (vk-alloc:with-foreign-allocated-objects (,@translated-args)
               (cffi:with-foreign-object (,@struct-def)
                 (,vulkan-fun ,@vk-input-args)
                 (cffi:mem-aref ,@struct-def))))))))) ;; TODO: translate-from-foreign including NEXT translation!!

;; TODO: maybe take a type and a size instead?
;; case 1d: arbitrary data as output param - e.g. vkGetQueryPoolResults
(defmacro defvk-fill-arbitrary-buffer-fun ((name vulkan-fun docstring required-args optional-arg &optionals (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that fills a buffer of arbitrary size.
The allocated buffer as well as its size are provided by the user.
E.g. vkGetQueryPoolResults"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args) (process-variables variables extension-p)
        `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
           ,docstring
           ,@required-arg-declares
           ,@optional-arg-declares
           (vk-alloc:with-foreign-allocated-objects (,@translated-args)
             (,vulkan-fun ,@vk-input-args)))))))


;;; --------------------------------------------- 2 output parameters ------------------------------------------------------------------

;; case 2a: e.g. vkGetPhysicalDeviceQueueFamilyProperties2
(defmacro defvk-get-structs-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-name &optional (no-result-p nil) (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets multiple structs and might return a RESULT.
E.g. vkGetPhysicalDeviceQueueFamilyProperties2"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((count-arg (find-if (lambda (a)
                                    (eq (first a) count-arg-name))
                                  output-args))
              (array-arg (find-if (lambda (a)
                                    (eq (first a) array-arg-name))
                                  output-args))
              (translated-count (gensym))
              (result (gensym))
              (i (gensym)))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (vk-alloc:with-foreign-allocated-objects (,@translated-args)
               (cffi:with-foreign-object (,@ (second count-arg))
                 (let ((,(first (second array-arg)) (cffi:null-pointer)))
                   (,vulkan-fun ,@vk-input-args)
                   (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                     (cffi:with-foreign-object (,@ (second array-arg) ,translated-count)
                       ,(if no-result-p
                            `(progn
                               (,vulkan-fun ,@vk-input-args)
                               (loop for ,i from 0 below ,translated-count
                                     collect (cffi:mem-aref ,@ (second array-arg) ,i)))
                            `(let ((,result (,vulkan-fun ,@vk-input-args)))
                               (cl:values (loop for ,i from 0 beloq ,translated-count
                                                collect (cffi:mem-aref ,@ (second array-arg) ,i))))))))))))))))

;; case 2b: ???
(defmacro defvk-multiple-singular-returns-fun ((name vulkan-fun docstring required-args optional-args &optional (extension-p nil)) &body body)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets two separate non-array values and returns a RESULT.
E.g. ???"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((result (gensym)))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (vk-alloc:with-foreign-allocated-objects (,@translated-args)
               (cffi:with-foreign-objects (,@ (mapcar #'second output-args))
                 (let ((,result (,vulkan-fun ,@vk-input-args)))
                   (cl:values ,@ (mapcar #'first (mapcar #'second output-args))
                                 ,result))))))))))

;; case 2c: enumerate - e.g. vkEnumeratePhysicalDevices
(defmacro defvk-enumerate-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-name &optional (no-result-p nil) (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that enumerates values and returns a RESULT.
E.g. vkEnumeratePhysicalDevices"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((count-arg (find-if (lambda (a)
                                    (eq (first a) count-arg-name))
                                  output-args))
              (array-arg (find-if (lambda (a)
                                    (eq (first a) array-arg-name))
                                  output-args))
              (translated-count (gensym))
              (result (gensym))
              (i (gensym)))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (vk-alloc:with-foreign-allocated-objects (,@translated-args)
               (cffi:with-foreign-object (,@ (second count-arg))
                 (let ((,(first (second array-arg)) (cffi:null-pointer))
                       (,result :incomplete))
                   (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                         while (eq ,result :incomplete)
                         finally (return (when (eq ,result :success)
                                           (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                                             (cl:values
                                              (when (> ,translated-count 0)
                                                (cffi:with-foreign-object (,@ (second array-arg) ,translated-count)
                                                  (setf ,result (,vulkan-fun ,@vk-input-args))
                                                  (loop for ,i from 0 below ,translated-count
                                                        collect (cffi:mem-aref ,@ (second array-arg) ,i))))
                                              ,result))))))))))))))

;; case 2d: return multiple values. one array of the same size as an input array and one additional non-array value - e.g. vkGetCalibratedTimestampsEXT
(defmacro defvk-get-array-and-singular-fun ((name vulkan-fun docstring required-args optional-args len-provider array-arg-name &optional (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that gets an array of values and a single value and returns a RESULT.
E.g. vkGetCalibratedTimestampsEXT"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t extension-p)
      (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((array-arg (find-if (lambda (a)
                                    (eq (first a) array-arg-name))
                                  output-args))
              (other-output (find-if-not (lambda (a)
                                           (eq (first a) array-arg-name))
                                         output-args))
              (result (gensym))
              (i (gensym)))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (vk-alloc:with-foreign-allocated-objects (,@translated-args)
               (cffi:with-foreign-objects ((,@ (second array-arg) ,len-provider)
                                           (,@ (second other-output)))
                 (let ((,result (,vulkan-fun ,@vk-input-args)))
                   (cl:values (loop for ,i from 0 below ,array-arg-name
                                    collect (cffi:mem-aref ,@ (second array-arg) ,i))
                              (cffi:mem-aref ,@ (second other-output))
                              ,result))))))))))

;;; --------------------------------------------- 3 output parameters ------------------------------------------------------------------

;; case 3: return two arrays using the same counter which is also an output argument - e.g vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
(defmacro defvk-enumerate-two-arrays-fun ((name vulkan-fun docstring required-args optional-args count-arg-name array-arg-names &optional (no-result-p nil) (extension-p nil)) &body variables)
  "Defines a function named NAME which wraps VULKAN-FUN.
VULKAN-FUN is function that enumerates two kinds of values and returns a RESULT.
E.g. vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR"
  (multiple-value-bind (required-arg-names required-arg-declares) (process-args required-args nil)
    (multiple-value-bind (optional-arg-names optional-arg-declares) (process-args optional-args t) extension-p
      (multiple-value-bind (translated-args vk-input-args output-args) (process-variables variables extension-p)
        (let ((count-arg (find-if (lambda (a)
                                    (eq (first a) count-arg-name))
                                  output-args))
              (array-args (remove-if-not (lambda (a)
                                           (find (first a) array-arg-names))
                                   output-args))
              (translated-count (gensym))
              (result (gensym))
              (i (gensym))
              (first-array (gensym))
              (second-array (gensym)))
          `(defun ,name (,@required-arg-names &optional ,@optional-arg-names)
             ,docstring
             ,@required-arg-declares
             ,@optional-arg-declares
             (vk-alloc:with-foreign-allocated-objects (,@translated-args)
               (cffi:with-foreign-object (,@ (second count-arg))
                 (let ((,(first (second (first array-args))) (cffi:null-pointer))
                       (,(first (second (second array-args))) (cffi:null-pointer))
                       (,result :incomplete))
                   (loop do (setf ,result (,vulkan-fun ,@vk-input-args))
                         while (eq ,result :incomplete)
                         finally (return (when (eq ,result :success)
                                           (let ((,translated-count (cffi:mem-aref ,@ (second count-arg))))
                                             (if (> ,translated-count 0)
                                                 (multiple-value-bind (,first-array ,second-array)
                                                     (cffi:with-foreign-objects ((,@ (second (first array-args)) ,translated-count)
                                                                                 (,@ (second (second array-args)) ,translated-count))
                                                       (setf ,result (,vulkan-fun ,@vk-input-args))
                                                       (loop for ,i from 0 below ,translated-count
                                                             collect (cffi:mem-aref ,@ (second (first array-args)) ,i) into ,first-array
                                                             collect (cffi:mem-aref ,@ (second (second array-args)) ,i) into ,second-array
                                                             finally (return (values ,first-array ,second-array ,result)))))
                                                 (cl:values nil nil ,result)))))))))))))))

