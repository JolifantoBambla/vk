;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyright (c) 2021, Lukas Herzberger <herberger.lukas@gmail.com>
;;; Copyright (c) 2016, Bart Botta  <00003b@gmail.com>
;;;   All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:vulkan)

(define-foreign-library vulkan
  (:darwin (:or "libvulkan.1.dylib"))
  (:unix (:or "libvulkan.so"))
  (:windows "vulkan-1.dll"))

(use-foreign-library vulkan)

(defstruct extension-loader
  "An EXTENSION-LOADER is used to define extension functions.
All extension functions take an optional EXTENSION-LOADER argument as last parameter that defaults to *DEFAULT-EXTENSION-LOADER*.

For each defined extension function a function pointer (fetched from its INSTANCE or DEVICE) is stored in INSTANCE-FUNC-POINTERS or DEVICE-FUNC-POINTERS respectively.
In order to define instance-level extension functions you need to provide it with an INSTANCE as generated by %VK:CREATE-INSTANCE or returned by VK:CREATE-INSTANCE.

Function pointers returned by %VK:GET-INSTANCE-PROC-ADDR (i.e. vkGetInstanceProcAddr) may point to dispatch code that calls a different implementation for different %VK:DEVICE handles (i.e. VkDevice) or their child objects.
To avoid the overhead of this dispatch mechanism function pointers (for functions using a %VK:DEVICE or one of its children as their dispatchable object) can be fetched on a %VK:DEVICE level.
In order to define device-level extension functions you need to provide it with a DEVICE as generated by %VK:CREATE-DEVICE or returned by VK:CREATE-DEVICE.

See CREATE-INSTANCE
See CREATE-DEVICE"
  (instance nil :type (or cffi:foreign-pointer null))
  (device nil :type (or cffi:foreign-pointer null))
  (instance-func-pointers (make-hash-table) :type hash-table)
  (device-func-pointers (make-hash-table) :type hash-table))

(defparameter *default-extension-loader* (make-extension-loader)
  "The default extension loader that is passed to all extension functions as the default value for their optional EXTENSION-LOADER argument.
It defaults to NIL, so if you need extension functions you should set *DEFAULT-EXTENSION-LOADER* to an instance of EXTENSION-LOADER.
In order to actually load extensions the INSTANCE and/or DEVICE slots of the EXTENSION-LOADER instance must be set.

For each defined extension function a function pointer (fetched from its INSTANCE or DEVICE) is stored in INSTANCE-FUNC-POINTERS or DEVICE-FUNC-POINTERS respectively.
In order to define instance-level extension functions you need to provide it with an INSTANCE as generated by %VK:CREATE-INSTANCE or returned by VK:CREATE-INSTANCE.

Function pointers returned by %VK:GET-INSTANCE-PROC-ADDR (i.e. vkGetInstanceProcAddr) may point to dispatch code that calls a different implementation for different %VK:DEVICE handles (i.e. VkDevice) or their child objects.
To avoid the overhead of this dispatch mechanism function pointers (for functions using a %VK:DEVICE or one of its children as their dispatchable object) can be fetched on a %VK:DEVICE level.
In order to define device-level extension functions you need to provide it with a DEVICE as generated by %VK:CREATE-DEVICE or returned by VK:CREATE-DEVICE.

See EXTENSION-LOADER
See CREATE-INSTANCE
See CREATE-DEVICE")

(defmacro defvkfun ((cname lname) result-type &body body)
  `(defcfun (,cname ,lname :library vulkan) ,result-type ,@body))

(defmacro defvkextfun ((cname lname) result-type &body args)
  (let ((extension-loader (gensym))
        (func-pointer (gensym)))
    `(defun ,lname (,@ (mapcar 'car args) &optional (,extension-loader *default-extension-loader*))
       (assert (and ,extension-loader
                    (or (extension-loader-device ,extension-loader)
                        (extension-loader-instance ,extension-loader)))
               (,extension-loader)
               "No extension loader / Provided extension loader has neither an instance nor a device!")
       ;; always try fetching device-level function pointers first to avoid dispatch overhead
       (let ((,func-pointer (or (gethash ',lname (extension-loader-device-func-pointers ,extension-loader))
                                (and (extension-loader-device ,extension-loader)
                                     ;; todo: this might return a null pointer!
                                     (setf (gethash ',lname (extension-loader-device-func-pointers ,extension-loader))
                                           (get-device-proc-addr (extension-loader-device ,extension-loader) ,cname)))
                                (gethash ',lname (extension-loader-instance-func-pointers ,extension-loader))
                                (and (extension-loader-instance ,extension-loader)
                                     ;; todo: this might return a null pointer!
                                     (setf (gethash ',lname (extension-loader-instance-func-pointers ,extension-loader))
                                           (get-instance-proc-addr (extension-loader-instance ,extension-loader) ,cname))))))
         (foreign-funcall-pointer
          ,func-pointer
          nil
          ,@(loop for arg in args
                  collect (second arg) collect (first arg))
          ,result-type)))))

(if (= 8 (foreign-type-size :pointer))
    (defctype size-t :uint64)
    (defctype size-t :uint32))
