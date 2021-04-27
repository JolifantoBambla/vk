(defsystem vk
  :version "1.0.1"
  :license "MIT"
  :description "Common Lisp bindings for the Vulkan API."
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :maintainer "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :homepage "https://jolifantobambla.github.io/vk/"
  :bug-tracker "https://github.com/JolifantoBambla/vk/issues"
  :source-control (:git "https://github.com/JolifantoBambla/vk.git")
  :depends-on (cffi alexandria)
  :components
  ((:module "src"
    :serial t
    :components ((:file "package")
                 (:file "vk-alloc")
                 (:file "vulkan-bindings")
                 (:file "vulkan-api-constants")
                 (:file "vulkan-types")
                 (:file "vk-types")
                 (:file "vulkan-define-conditions")
                 (:file "vulkan-errors")
                 (:file "vulkan-extra-types")
                 (:file "vk-translate-to-foreign")
                 (:file "vk-expand-to-foreign")
                 (:file "vk-translate-from-foreign")
                 (:file "vk-expand-from-foreign")
                 (:file "vulkan-commands")
                 (:file "vk-base")
                 (:file "vk-bindings")
                 (:file "vk-commands")
                 (:file "vk-utils-common"))))
  :in-order-to ((test-op (test-op vk/tests))))

(defsystem vk/tests
  :depends-on (:vk
               :rove)
  :defsystem-depends-on (:rove)
  :components ((:module "test"
                :components ((:file "translators"))))
  :perform (test-op :after (op c) (uiop:symbol-call :rove :run c)))
