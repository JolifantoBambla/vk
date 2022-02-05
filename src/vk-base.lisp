#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk)

(defparameter *default-allocator* (cffi:null-pointer)
  "The default allocator that is used for the optional ALLOCATOR parameter of all functions taking an instance of ALLOCATION-CALLBACKS.
It defaults to a CFFI:NULL-POINTER.

You can either set this to an instance of ALLOCATION-CALLBACKS or to a foreign pointer to an already translated instance of ALLOCATION-CALLBACKS.
In general you'll want the latter since it saves you the cost of translating the instance in every call that uses it.")

(defun make-extension-loader (&key instance device)
  "Creates an EXTENSION-LOADER instance.

See EXTENSION-LOADER"
  (%vk:make-extension-loader
   :instance (when instance
               (%dispatchable-handle instance))
   :device (when device
             (%dispatchable-handle device))))

(defun extension-loader-instance (extension-loader)
  "Gets the value bound to the INSTANCE slot of a given EXTENSION-LOADER.

See EXTENSION-LOADER"
  (make-instance-wrapper (%vk:extension-loader-instance extension-loader)))

(defun (setf extension-loader-instance) (instance extension-loader)
  "Binds a given VALUE to the INSTANCE slot of a given EXTENSION-LOADER.

See EXTENSION-LOADER"
  (setf (%vk:extension-loader-instance extension-loader) (%dispatchable-handle instance)))

(defun extension-loader-device (extension-loader)
  "Gets the value bound to the DEVICE slot of a given EXTENSION-LOADER.

See EXTENSION-LOADER"
  (make-device-wrapper (%vk:extension-loader-device extension-loader)))

(defun (setf extension-loader-device) (device extension-loader)
    "Binds a given VALUE to the DEVICE slot of a given EXTENSION-LOADER.

See EXTENSION-LOADER"
  (setf (%vk:extension-loader-device extension-loader) (%dispatchable-handle device)))

(defun raw-handle (handle-wrapper)
  "Gets the raw CFFI:FOREIGN-POINTER from a wrapped handle.

See CFFI:FOREIGN-POINTER"
  (etypecase handle-wrapper
    (%dispatchable (%dispatchable-handle handle-wrapper))
    (%non-dispatchable (%non-dispatchable-handle handle-wrapper))))

