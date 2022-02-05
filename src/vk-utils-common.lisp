#|
 Copyright(c) 2021 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT
|#

(in-package :vk-utils)

(cffi:defcfun ("memcpy" memcpy) :pointer
  "Copies COUNT bytes from the memory location pointed to by SRC to the memory location pointed to by DEST.

Args:
 - DEST: a CFFI:FOREIGN-POINTER
 - SRC: a CFFI:FOREIGN-POINTER
 - COUNT: a UNSIGNED-BYTE

Returns DEST (i.e. a CFFI:FOREIGN-POINTER)."
  (dest :pointer)
  (src :pointer)
  (count :size))

(defun split-api-version (version)
  "Splits a packed version number as returned by VK:MAKE-API-VERSION into a list of integers in the form of (major minor patch).

See VK:MAKE-API-VERSION"
  (list (vk:api-version-variant version)
        (vk:api-version-major version)
        (vk:api-version-minor version)
        (vk:api-version-patch version)))

(defun format-api-version (version)
  "Formats a packed version number as returned by MAKE-API-VERSION into a human readable string.

See MAKE-API-VERSION"
  (cl:format nil "~{~a~^.~}" (split-api-version version)))

(defun read-shader-source (shader-path)
  "Reads a compiled SPIR-V shader file located at SHADER-PATH into a vector of 32-bit integers.
The result of this function is ready to use as :CODE in a VK:SHADER-MODULE-CREATE-INFO.

See VK:SHADER-MODULE-CREATE-INFO
"
  (with-open-file (stream shader-path :element-type '(unsigned-byte 32))
                 (let ((shader-code (make-array 1024
                                                :element-type '(unsigned-byte 32)
                                                :adjustable t
                                                :fill-pointer 0)))
                   (loop for b = (read-byte stream nil nil)
                         while b
                         do (vector-push-extend b shader-code)
                         finally (return (adjust-array shader-code (length shader-code)))))))
