;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Copyrigth (c) 2021, Lukas Herzberger <herzberger.lukas@gmail.com>
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

(defun %print-vk-cond (condition stream type)
  (format stream "vulkan ~a: ~a~%  (~s = ~s)"
          type
          (message condition)
          (raw-value condition)
          (or (enum condition) "??")))
(defun print-vk-condition (c s) (%print-vk-cond c s "warning"))
(defun print-vk-error (c s) (%print-vk-cond c s "error"))

;;; used for positive non-zero returns
(define-condition vk-condition (condition)
  ((enum :reader enum :initarg :enum)
   (value :reader raw-value :initarg :value)
   (message :reader message :initarg :message))
  (:report print-vk-condition))

;;; used for negative non-zero returns
(define-condition vk-error (vk-condition error)
  ()
  (:report print-vk-error))

(defmacro %define-conditions (parent &body r)
  `(progn
     ,@ (loop for c in r
              for k = (find-symbol (symbol-name c) (find-package :keyword))
              for v = (foreign-enum-value 'result k)
              for message = (gethash k *result-comments*)
              collect `(define-condition ,c (,parent)
                         ()
                         (:default-initargs :enum ,k :value ,v :message ,message)))))

