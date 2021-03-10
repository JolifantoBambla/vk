(defsystem vk
  :description "Common Lisp bindings for the Vulkan API."
  :depends-on (cffi alexandria)
  :license "MIT"
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
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
                 (:file "vk-translate-from-foreign")
                 (:file "vulkan-commands")
                 (:file "vk-base")
                 (:file "vk-bindings")
                 (:file "vk-commands")))))
