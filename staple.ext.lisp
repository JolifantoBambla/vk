;;;; staple.ext.lisp

(defclass vk-page (staple:simple-page) ())

(defmethod staple:page-type ((system (eql (asdf:find-system :vk))))
  'vk-page)

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

