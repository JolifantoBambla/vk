;;;; translators.lisp

(parachute:define-test translators)

(parachute:define-test simple-struct
  :parent translators
  (parachute:true
    (let ((application-info (make-instance 'vk:application-info
                                           :application-name "vk/test"
                                           :application-version 1
                                           :engine-name "vk"
                                           :engine-version 1
                                           :api-version 1)))
      (cffi:with-foreign-object (p '(:struct %vk:application-info))
        (setf (cffi:mem-aref p '(:struct %vk:application-info)) application-info)
        (string= (vk:engine-name (cffi:mem-aref p '(:struct %vk:application-info))) "vk")))))
