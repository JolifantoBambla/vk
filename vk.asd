(defsystem vk
  :version "0.0.0"
  :license "MIT"
  :description "Common Lisp bindings for the Vulkan API."
  :author "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :maintainer "Lukas Herzberger <herzberger.lukas at gmail.com>"
  :homepage "https://jolifantobambla.github.io/vk/"
  :bug-tracker "https://github.com/JolifantoBambla/vk-generator/issues"
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
                 (:file "vk-translate-from-foreign")
                 (:file "vulkan-commands")
                 (:file "vk-base")
                 (:file "vk-bindings")
                 (:file "vk-commands")))))
