#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk)

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

