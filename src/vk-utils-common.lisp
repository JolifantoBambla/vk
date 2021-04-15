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
  (count %vk:size-t))

(defun split-api-version (version)
  "Splits a packed version number as returned by VK:MAKE-API-VERSION into a list of integers in the form of (major minor patch).

See VK:MAKE-API-VERSION"
  (list (ldb (byte 12 22) version)
        (ldb (byte 10 12) version)
        (ldb (byte 12 0) version)))

(defun format-api-version (version)
  "Formats a packed version number as returned by MAKE-API-VERSION into a human readable string.

See MAKE-API-VERSION"
  (let ((split-version (split-api-version version)))
    (format nil "~a.~a.~a"         
                 (first split-version)
                 (second split-version)
                 (third split-version))))
