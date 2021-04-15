;;;; staple.ext.lisp

(in-package :staple)

(defclass vk-page (staple:simple-page) ())

(defmethod staple:template ((system (eql (asdf:find-system :vk))))
  (asdf:system-relative-pathname system #P".docs/docs-template.ctml"))

(defmethod staple:page-type ((system (eql (asdf:find-system :vk))))
  'vk-page)

(defmethod staple:subsystems ((system (eql (asdf:find-system :vk))))
  (list (asdf:find-system :vk)))

;;; disabled definitions
(defmethod staple:definition-wanted-p ((definition definitions:generic-function) page))
(defmethod staple:definition-wanted-p ((definition definitions:transform) page))
(defmethod staple:definition-wanted-p ((definition definitions:source-transform) page))
(defmethod staple:definition-wanted-p ((definition definitions:optimizer) page))
(defmethod staple:definition-wanted-p ((definition definitions:vop) page))

;; order changes
;; special-variable before constant
(defmethod staple:definition-order ((definition definitions:special-variable)) 195)

(defmethod staple:format-documentation ((docstring string) (page vk-page))
  (let ((formatted (call-next-method docstring page)))
    ;; hacky replacement of markdown-links in docstrings formatted by staple
    (loop while (search "](" formatted)
          for split-pos = (search "](" formatted)
          for c-name-start = (search "[" formatted :from-end t :end2 split-pos)
          for link-end = (search ")" formatted :start2 split-pos)
          do (setf formatted (format nil "~a<a href=\"~a\" target=\"_blank\">~a</a>~a"
                                     (subseq formatted 0 c-name-start)
                                     (subseq formatted (+ split-pos 2) link-end)
                                     (subseq formatted (+ c-name-start 1) split-pos)
                                     (subseq formatted (+ link-end 1)))))
    formatted))

;; even hackier way to distinguish between enums and handles (it doesn't even work when vk-page is not defined in staple)
(defmethod clip:clip ((definition definitions:type-definition) field)
  (cond
    ((string= (string-downcase field) "enum")
     (search "represents the enum" (string-downcase (definitions:documentation definition))))
    ((string= (string-downcase field) "handle")
     (or (search "represents the (non-dispatchable) handle" (string-downcase (definitions:documentation definition)))
         (search "represents the handle" (string-downcase (definitions:documentation definition)))))
    (t (call-next-method))))

