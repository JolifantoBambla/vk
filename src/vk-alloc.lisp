#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk-alloc)

(defclass translatable-object ()
  ((data
    :initarg :data
    :initform nil
    :accessor data)
   (foreign-type-specifier
    :initarg :foreign-type-specifier
    :initform nil
    :accessor foreign-type-specifier))
  (:documentation "A helper class storing DATA along with a FOREIGN-TYPE-SPECIFIER which can be used to translate the object to/from foreign memory.

E.g. (using CFFI):
(defcstruct (foo :class c-foo)
  (a :double))

(defstruct foo
  (a 0.0 :type double-float))

(defmethod translate-into-foreign-memory (value (type c-foo) ptr) ...)

(defparameter *my-foo* (make-instance 'translatable-object :data (make-foo) :foreign-type-specifier '(:struct foo))) 

(with-foreign-object (ptr (foreign-type-specifier *my-foo*))
  (setf (mem-aref ptr (foreign-type-specifier *my-foo*)) (data *my-foo*)))
"))

(defparameter *allocated-foreign-objects* (make-hash-table)
  "A hash table storing allocated foreign objects and dependencies between them.
Each foreign object (key) is associated with a list of other foreign objects that were allocated during allocation of the key and should be freed when the key is freed.

Each foreign object must appear once at most within a value of this hash table.
E.g.
((<foreign1> '(<foreign2> <foreign3>))
 (<foreign2> nil)
 (<foreign3> '(<foreign4>))
 (<foreign4> nil))

A note on multithreading: Hash tables are by default not thread safe. Since entries in this hash table are not meant to be shared between threads, you can bind *ALLOCATED-FOREIGN-OBJECTS* to a thread-local variable and it should be fine.")

(defparameter *allocate-foreign-object-func* #'cffi:foreign-alloc
  "Configures how foreign resources are allocated. You might want to use this for implementing a memory pool to reuse allocated resources, etc.")

(defparameter *free-foreign-object-func* #'cffi:foreign-free
  "Configures how foreign resources are freed. You might want to use this for implementing a memory pool to reuse allocated resources, etc.")

(defun free-allocated-foreign-chain (foreign-obj)
  "Frees all foreign objects associated with FOREIGN-OBJ.

See *ALLOCATED-FOREIGN-OBJECTS*
See *FREE-FOREIGN-OBJECT-FUNC*"
  (funcall *free-foreign-object-func* foreign-obj)
  (dolist (child (gethash foreign-obj *allocated-foreign-objects*)) (free-allocated-foreign-chain child))
  (remhash foreign-obj *allocated-foreign-objects*))

(defun free-allocated-children (foreign-obj)
  "Frees all children that were allocated during allocation of FOREIGN-OBJ which is assumed to be a stack-allocated resource, whereas its children were heap-allocated.

See FREE-ALLOCATED-FOREIGN-CHAIN"
  (dolist (child (gethash foreign-obj *allocated-foreign-objects*)) (free-allocated-foreign-chain child))
  (remhash foreign-obj *allocated-foreign-objects*))

(defun foreign-allocate-and-fill (type content parent-ptr)
  "Allocates a foreign resource of TYPE and fill it with CONTENT.

The allocated foreign resource is stored in *ALLOCATED-FOREIGN-OBJECTS*, where it is associated with PARENT-PTR.

If CONTENT is NIL, no resource is allocated and a CFFI:NULL-POINTER is returned.

See *ALLOCATED-FOREIGN-OBJECTS*
See *ALLOCATE-FOREIGN-OBJECT-FUNC*"
  (if content
      (let ((p-resource nil))
        (if (arrayp content)
            (let ((array-type (alexandria:flatten (list :array type (array-dimensions content)))))
              (setf p-resource (funcall *allocate-foreign-object-func* array-type :count (array-total-size content)))
              (cffi:lisp-array-to-foreign content p-resource array-type))
            (if (listp content)
                (setf p-resource (funcall *allocate-foreign-object-func* type :initial-contents content))
                (setf p-resource (funcall *allocate-foreign-object-func* type :initial-element content))))
        (unless (cffi:null-pointer-p parent-ptr)
          (push p-resource (gethash parent-ptr *allocated-foreign-objects*)))
        p-resource)
      (cffi:null-pointer)))

(defmacro with-foreign-allocated-object ((var type content) &body body)
  "Bind VAR and translate CONTENT to a foreign pointer of TYPE during BODY.
The pointer in VAR is invalid beyond the dynamic extent of BODY.

If the supplied CONTENT is NIL a CFFI:NULL-POINTER is bound to VAR and no resources are allocated.
If the supplied CONTENT satisfies CFFI:POINTER-P the CONTENT is bound to VAR as is and no resources are allocated. 

See CFFI:WITH-FOREIGN-OBJECT
See CFFI:NULL-POINTER-P"
  (let ((contents (gensym "CONTENT-LIST"))
        (iterator (gensym "LOOP-INDEX"))
        (element (gensym "ELEMENT")))
    `(if (or (cffi:pointerp ,content)
             (not ,content))
        (let ((,var (if (not ,content)
                        (cffi:null-pointer)
                        ,content)))
          ,@body)
        ,(cond
           ((eq type :string)
            ;; cffi:with-foreign-object seems to do something different with strings than cffi:with-foreign-string does
            ;; (kinda obvious given there are two distinct macros?)
            `(if (listp ,content)
                 ;; not exactly sure why this works for lists of strings but not single strings - encoding?
                 (cffi:with-foreign-object (,var ,type (length ,content))
                   (loop for ,iterator from 0 below (length ,content)
                         for ,element in ,content
                         do (setf (cffi:mem-aref ,var ,type ,iterator) ,element))
                   ,@body)
                 (cffi:with-foreign-string (,var ,content)
                   ,@body)))
           ;; unions need special care: https://github.com/JolifantoBambla/vk/issues/5
           ((and (listp type)
                 (listp (second type))
                 (member :union (second type)))
            `(let ((,var (foreign-allocate-and-fill ,type ,content (cffi:null-pointer))))
               (unless (cffi:null-pointer-p ,var)
                 (unwind-protect
                      (progn ,@body)
                   (free-allocated-foreign-chain ,var)))))
           (t
            `(let ((,contents (if (or (vectorp ,content)
                                      (and (listp ,content)
                                           (not (keywordp (first ,content)))))
                                  ,content
                                  (list ,content))))
               (cffi:with-foreign-object (,var
                                          ,type
                                          (length ,contents))
                 (unwind-protect
                      (progn
                        (loop for ,iterator from 0 below (length ,contents)
                              for ,element in ,contents
                              do (setf (cffi:mem-aref ,var ,type ,iterator) ,element))
                        ,@body)
                   (free-allocated-children ,var)))))))))

(defmacro with-foreign-allocated-objects (bindings &rest body)
  "Behaves like WITH-FOREIGN-ALLOCATED-OBJECT but for multiple BINDINGS instead of just one.
 
See WITH-FOREIGN-ALLOCATED-OBJECT"
  (if bindings
      `(with-foreign-allocated-object ,(car bindings)
         (with-foreign-allocated-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))
