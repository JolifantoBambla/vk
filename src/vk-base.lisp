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

(defun make-api-version (major minor patch)
  "Packs a version number defined by its MAJOR, MINOR and PATCH version numbers into an integer.
This can be used to set the API-VERSION slot of a INSTANCE-CREATE-INFO.

See INSTANCE-CREATE-INFO"
  (logior (ash major 22)
          (ash minor 12)
          patch))
