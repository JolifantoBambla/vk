;;;; translators.lisp

(defpackage #:vk/tests/translators
  (:use #:cl
        #:rove))
(in-package #:vk/tests/translators)

(deftest translate-simple-struct
  (ok (let ((application-info (make-instance 'vk:application-info
                                             :application-name "vk/test"
                                             :application-version 1
                                             :engine-name "vk"
                                             :engine-version 1
                                             :api-version 1)))
        (cffi:with-foreign-object (p '(:struct %vk:application-info))
          (setf (cffi:mem-aref p '(:struct %vk:application-info)) application-info)
          (string= (vk:engine-name (cffi:mem-aref p '(:struct %vk:application-info))) "vk")))))

(deftest translate-image-blit
  (ok (let* ((src-offset0 (vk:make-offset-3d :x 100 :y 100 :z 0))
             (src-offset1 (vk:make-offset-3d :x 200 :y 200 :z 1))
             (dst-offset0 (vk:make-offset-3d :x 300 :y 300 :z 0))
             (dst-offset1 (vk:make-offset-3d :x 400 :y 400 :z 1))
             (image-blit (vk:make-image-blit
                          :src-offsets (vector src-offset0 src-offset1)
                          :src-subresource (vk:make-image-subresource-layers
                                            :aspect-mask :color
                                            :mip-level 0
                                            :base-array-layer 0
                                            :layer-count 1)
                          :dst-offsets (vector dst-offset0 dst-offset1)
                          :dst-subresource (vk:make-image-subresource-layers
                                            :aspect-mask :color
                                            :mip-level 1
                                            :base-array-layer 0
                                            :layer-count 1))))
        (flet ((offset-3d-equal-p (a b)
                 (and (eq (vk:x a) (vk:x b))
                      (eq (vk:y a) (vk:y b))
                      (eq (vk:z a) (vk:z b)))))
          (vk-alloc:with-foreign-allocated-object (c-image-blit '(:struct %vk:image-blit) image-blit)
            (let ((translated (cffi:mem-aref c-image-blit '(:struct %vk:image-blit))))
              (and (offset-3d-equal-p src-offset0 (first (vk:src-offsets translated)))
                   (offset-3d-equal-p src-offset1 (second (vk:src-offsets translated)))
                   (offset-3d-equal-p dst-offset0 (first (vk:dst-offsets translated)))
                   (offset-3d-equal-p dst-offset1 (second (vk:dst-offsets translated))))))))))

