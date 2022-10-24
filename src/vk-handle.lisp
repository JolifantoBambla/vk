#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk)

(deftype non-dispatchable-handle ()
  "Represents the type [VK_DEFINE_NON_DISPATCHABLE_HANDLE](https://registry.khronos.org/vulkan/specs/1.3-extensions/man/html/VK_DEFINE_NON_DISPATCHABLE_HANDLE.html)."
  (if (= 8 (cffi:foreign-type-size :pointer))
      'cffi:foreign-pointer
      '(unsigned-byte 64)))

(defstruct (%dispatchable
            (:constructor %make-dispatchable (handle)))
  "Base type for dispatchable handles."
  (handle (error "handle must be supplied") :type cffi:foreign-pointer :read-only t))

(defstruct (%non-dispatchable
            (:constructor %make-non-dispatchable (handle)))
  "Base type for non-dispatchable handles."
  (handle (error "handle must be supplied")
   :type #.(if (= 8 (cffi:foreign-type-size :pointer))
               'cffi:foreign-pointer
               '(unsigned-byte 64))
   :read-only t))

