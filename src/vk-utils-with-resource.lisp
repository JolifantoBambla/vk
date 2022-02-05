;;; this file is automatically generated, do not edit

(in-package :vk-utils)

(defmacro with-instance ((resource create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-INSTANCE call.
See VK:CREATE-INSTANCE
See VK:DESTROY-INSTANCE"
  `(let ((,resource (vk:create-instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-device ((resource physical-device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DEVICE call.
See VK:CREATE-DEVICE
See VK:DESTROY-DEVICE"
  `(let ((,resource (vk:create-device ,physical-device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-command-buffers ((resources device allocate-info &key) &body body)
  "Binds RESOURCES to the result of a VK:ALLOCATE-COMMAND-BUFFERS call.
See VK:ALLOCATE-COMMAND-BUFFERS
See VK:FREE-COMMAND-BUFFERS"
  `(let ((,resources (vk:allocate-command-buffers ,device ,allocate-info)))
     (unwind-protect
         (progn ,@body)
       (vk:free-command-buffers ,device (vk:command-pool ,allocate-info) ,resources))))

(defmacro with-memory ((resource device allocate-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:ALLOCATE-MEMORY call.
See VK:ALLOCATE-MEMORY
See VK:FREE-MEMORY"
  `(let ((,resource (vk:allocate-memory ,device ,allocate-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:free-memory ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-command-pool ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-COMMAND-POOL call.
See VK:CREATE-COMMAND-POOL
See VK:DESTROY-COMMAND-POOL"
  `(let ((,resource (vk:create-command-pool ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-command-pool ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-buffer ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-BUFFER call.
See VK:CREATE-BUFFER
See VK:DESTROY-BUFFER"
  `(let ((,resource (vk:create-buffer ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-buffer ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-buffer-view ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-BUFFER-VIEW call.
See VK:CREATE-BUFFER-VIEW
See VK:DESTROY-BUFFER-VIEW"
  `(let ((,resource (vk:create-buffer-view ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-buffer-view ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-image ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-IMAGE call.
See VK:CREATE-IMAGE
See VK:DESTROY-IMAGE"
  `(let ((,resource (vk:create-image ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-image ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-image-view ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-IMAGE-VIEW call.
See VK:CREATE-IMAGE-VIEW
See VK:DESTROY-IMAGE-VIEW"
  `(let ((,resource (vk:create-image-view ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-image-view ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-shader-module ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-SHADER-MODULE call.
See VK:CREATE-SHADER-MODULE
See VK:DESTROY-SHADER-MODULE"
  `(let ((,resource (vk:create-shader-module ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-shader-module ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-ray-tracing-pipelines-khr ((resources device create-infos &key deferred-operation pipeline-cache allocator extension-loader) &body body)
  "Binds RESOURCES to the result of a VK:CREATE-RAY-TRACING-PIPELINES-KHR call.
See VK:CREATE-RAY-TRACING-PIPELINES-KHR
See VK:DESTROY-PIPELINE"
  (let ((resource (gensym "RESOURCE")))
    `(let ((,resources (vk:create-ray-tracing-pipelines-khr ,device ,create-infos (or ,deferred-operation (vk:make-deferred-operation-khr-wrapper (cffi:null-pointer))) (or ,pipeline-cache (vk:make-pipeline-cache-wrapper (cffi:null-pointer))) (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
       (unwind-protect
           (progn ,@body)
         (loop for ,resource in ,resources do
               (vk:destroy-pipeline ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))))

(defmacro with-ray-tracing-pipelines-nv ((resources device create-infos &key pipeline-cache allocator extension-loader) &body body)
  "Binds RESOURCES to the result of a VK:CREATE-RAY-TRACING-PIPELINES-NV call.
See VK:CREATE-RAY-TRACING-PIPELINES-NV
See VK:DESTROY-PIPELINE"
  (let ((resource (gensym "RESOURCE")))
    `(let ((,resources (vk:create-ray-tracing-pipelines-nv ,device ,create-infos (or ,pipeline-cache (vk:make-pipeline-cache-wrapper (cffi:null-pointer))) (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
       (unwind-protect
           (progn ,@body)
         (loop for ,resource in ,resources do
               (vk:destroy-pipeline ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))))

(defmacro with-compute-pipelines ((resources device create-infos &key pipeline-cache allocator) &body body)
  "Binds RESOURCES to the result of a VK:CREATE-COMPUTE-PIPELINES call.
See VK:CREATE-COMPUTE-PIPELINES
See VK:DESTROY-PIPELINE"
  (let ((resource (gensym "RESOURCE")))
    `(let ((,resources (vk:create-compute-pipelines ,device ,create-infos (or ,pipeline-cache (vk:make-pipeline-cache-wrapper (cffi:null-pointer))) (or ,allocator vk:*default-allocator*))))
       (unwind-protect
           (progn ,@body)
         (loop for ,resource in ,resources do
               (vk:destroy-pipeline ,device ,resource (or ,allocator vk:*default-allocator*)))))))

(defmacro with-graphics-pipelines ((resources device create-infos &key pipeline-cache allocator) &body body)
  "Binds RESOURCES to the result of a VK:CREATE-GRAPHICS-PIPELINES call.
See VK:CREATE-GRAPHICS-PIPELINES
See VK:DESTROY-PIPELINE"
  (let ((resource (gensym "RESOURCE")))
    `(let ((,resources (vk:create-graphics-pipelines ,device ,create-infos (or ,pipeline-cache (vk:make-pipeline-cache-wrapper (cffi:null-pointer))) (or ,allocator vk:*default-allocator*))))
       (unwind-protect
           (progn ,@body)
         (loop for ,resource in ,resources do
               (vk:destroy-pipeline ,device ,resource (or ,allocator vk:*default-allocator*)))))))

(defmacro with-pipeline-layout ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-PIPELINE-LAYOUT call.
See VK:CREATE-PIPELINE-LAYOUT
See VK:DESTROY-PIPELINE-LAYOUT"
  `(let ((,resource (vk:create-pipeline-layout ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-pipeline-layout ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-sampler ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-SAMPLER call.
See VK:CREATE-SAMPLER
See VK:DESTROY-SAMPLER"
  `(let ((,resource (vk:create-sampler ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-sampler ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-descriptor-sets ((resources device allocate-info &key) &body body)
  "Binds RESOURCES to the result of a VK:ALLOCATE-DESCRIPTOR-SETS call.
See VK:ALLOCATE-DESCRIPTOR-SETS
See VK:FREE-DESCRIPTOR-SETS"
  `(let ((,resources (vk:allocate-descriptor-sets ,device ,allocate-info)))
     (unwind-protect
         (progn ,@body)
       (vk:free-descriptor-sets ,device (vk:descriptor-pool ,allocate-info) ,resources))))

(defmacro with-descriptor-set-layout ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DESCRIPTOR-SET-LAYOUT call.
See VK:CREATE-DESCRIPTOR-SET-LAYOUT
See VK:DESTROY-DESCRIPTOR-SET-LAYOUT"
  `(let ((,resource (vk:create-descriptor-set-layout ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-descriptor-set-layout ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-descriptor-pool ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DESCRIPTOR-POOL call.
See VK:CREATE-DESCRIPTOR-POOL
See VK:DESTROY-DESCRIPTOR-POOL"
  `(let ((,resource (vk:create-descriptor-pool ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-descriptor-pool ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-fence ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-FENCE call.
See VK:CREATE-FENCE
See VK:DESTROY-FENCE"
  `(let ((,resource (vk:create-fence ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-fence ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-semaphore ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-SEMAPHORE call.
See VK:CREATE-SEMAPHORE
See VK:DESTROY-SEMAPHORE"
  `(let ((,resource (vk:create-semaphore ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-semaphore ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-event ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-EVENT call.
See VK:CREATE-EVENT
See VK:DESTROY-EVENT"
  `(let ((,resource (vk:create-event ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-event ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-query-pool ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-QUERY-POOL call.
See VK:CREATE-QUERY-POOL
See VK:DESTROY-QUERY-POOL"
  `(let ((,resource (vk:create-query-pool ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-query-pool ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-framebuffer ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-FRAMEBUFFER call.
See VK:CREATE-FRAMEBUFFER
See VK:DESTROY-FRAMEBUFFER"
  `(let ((,resource (vk:create-framebuffer ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-framebuffer ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-render-pass-2 ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-RENDER-PASS-2 call.
See VK:CREATE-RENDER-PASS-2
See VK:DESTROY-RENDER-PASS"
  `(let ((,resource (vk:create-render-pass-2 ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-render-pass ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-render-pass ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-RENDER-PASS call.
See VK:CREATE-RENDER-PASS
See VK:DESTROY-RENDER-PASS"
  `(let ((,resource (vk:create-render-pass ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-render-pass ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-pipeline-cache ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-PIPELINE-CACHE call.
See VK:CREATE-PIPELINE-CACHE
See VK:DESTROY-PIPELINE-CACHE"
  `(let ((,resource (vk:create-pipeline-cache ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-pipeline-cache ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-indirect-commands-layout-nv ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-INDIRECT-COMMANDS-LAYOUT-NV call.
See VK:CREATE-INDIRECT-COMMANDS-LAYOUT-NV
See VK:DESTROY-INDIRECT-COMMANDS-LAYOUT-NV"
  `(let ((,resource (vk:create-indirect-commands-layout-nv ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-indirect-commands-layout-nv ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-descriptor-update-template ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DESCRIPTOR-UPDATE-TEMPLATE call.
See VK:CREATE-DESCRIPTOR-UPDATE-TEMPLATE
See VK:DESTROY-DESCRIPTOR-UPDATE-TEMPLATE"
  `(let ((,resource (vk:create-descriptor-update-template ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-descriptor-update-template ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-sampler-ycbcr-conversion ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-SAMPLER-YCBCR-CONVERSION call.
See VK:CREATE-SAMPLER-YCBCR-CONVERSION
See VK:DESTROY-SAMPLER-YCBCR-CONVERSION"
  `(let ((,resource (vk:create-sampler-ycbcr-conversion ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-sampler-ycbcr-conversion ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-validation-cache-ext ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-VALIDATION-CACHE-EXT call.
See VK:CREATE-VALIDATION-CACHE-EXT
See VK:DESTROY-VALIDATION-CACHE-EXT"
  `(let ((,resource (vk:create-validation-cache-ext ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-validation-cache-ext ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-acceleration-structure-khr ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-ACCELERATION-STRUCTURE-KHR call.
See VK:CREATE-ACCELERATION-STRUCTURE-KHR
See VK:DESTROY-ACCELERATION-STRUCTURE-KHR"
  `(let ((,resource (vk:create-acceleration-structure-khr ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-acceleration-structure-khr ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-acceleration-structure-nv ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-ACCELERATION-STRUCTURE-NV call.
See VK:CREATE-ACCELERATION-STRUCTURE-NV
See VK:DESTROY-ACCELERATION-STRUCTURE-NV"
  `(let ((,resource (vk:create-acceleration-structure-nv ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-acceleration-structure-nv ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-buffer-collection-fuchsia ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-BUFFER-COLLECTION-FUCHSIA call.
See VK:CREATE-BUFFER-COLLECTION-FUCHSIA
See VK:DESTROY-BUFFER-COLLECTION-FUCHSIA"
  `(let ((,resource (vk:create-buffer-collection-fuchsia ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-buffer-collection-fuchsia ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-deferred-operation-khr ((resource device &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DEFERRED-OPERATION-KHR call.
See VK:CREATE-DEFERRED-OPERATION-KHR
See VK:DESTROY-DEFERRED-OPERATION-KHR"
  `(let ((,resource (vk:create-deferred-operation-khr ,device (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-deferred-operation-khr ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-private-data-slot-ext ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-PRIVATE-DATA-SLOT-EXT call.
See VK:CREATE-PRIVATE-DATA-SLOT-EXT
See VK:DESTROY-PRIVATE-DATA-SLOT-EXT"
  `(let ((,resource (vk:create-private-data-slot-ext ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-private-data-slot-ext ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-cu-module-nvx ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-CU-MODULE-NVX call.
See VK:CREATE-CU-MODULE-NVX
See VK:DESTROY-CU-MODULE-NVX"
  `(let ((,resource (vk:create-cu-module-nvx ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-cu-module-nvx ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-cu-function-nvx ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-CU-FUNCTION-NVX call.
See VK:CREATE-CU-FUNCTION-NVX
See VK:DESTROY-CU-FUNCTION-NVX"
  `(let ((,resource (vk:create-cu-function-nvx ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-cu-function-nvx ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-headless-surface-ext ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-HEADLESS-SURFACE-EXT call.
See VK:CREATE-HEADLESS-SURFACE-EXT
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-headless-surface-ext ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-metal-surface-ext ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-METAL-SURFACE-EXT call.
See VK:CREATE-METAL-SURFACE-EXT
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-metal-surface-ext ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-mac-os-surface-mvk ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-MAC-OS-SURFACE-MVK call.
See VK:CREATE-MAC-OS-SURFACE-MVK
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-mac-os-surface-mvk ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-ios-surface-mvk ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-IOS-SURFACE-MVK call.
See VK:CREATE-IOS-SURFACE-MVK
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-ios-surface-mvk ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-screen-surface-qnx ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-SCREEN-SURFACE-QNX call.
See VK:CREATE-SCREEN-SURFACE-QNX
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-screen-surface-qnx ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-stream-descriptor-surface-ggp ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-STREAM-DESCRIPTOR-SURFACE-GGP call.
See VK:CREATE-STREAM-DESCRIPTOR-SURFACE-GGP
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-stream-descriptor-surface-ggp ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-image-pipe-surface-fuchsia ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-IMAGE-PIPE-SURFACE-FUCHSIA call.
See VK:CREATE-IMAGE-PIPE-SURFACE-FUCHSIA
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-image-pipe-surface-fuchsia ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-direct-fb-surface-ext ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DIRECT-FB-SURFACE-EXT call.
See VK:CREATE-DIRECT-FB-SURFACE-EXT
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-direct-fb-surface-ext ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-xcb-surface-khr ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-XCB-SURFACE-KHR call.
See VK:CREATE-XCB-SURFACE-KHR
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-xcb-surface-khr ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-xlib-surface-khr ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-XLIB-SURFACE-KHR call.
See VK:CREATE-XLIB-SURFACE-KHR
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-xlib-surface-khr ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-win32-surface-khr ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-WIN32-SURFACE-KHR call.
See VK:CREATE-WIN32-SURFACE-KHR
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-win32-surface-khr ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-wayland-surface-khr ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-WAYLAND-SURFACE-KHR call.
See VK:CREATE-WAYLAND-SURFACE-KHR
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-wayland-surface-khr ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-vi-surface-nn ((resource instance create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-VI-SURFACE-NN call.
See VK:CREATE-VI-SURFACE-NN
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-vi-surface-nn ,instance ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-display-plane-surface-khr ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DISPLAY-PLANE-SURFACE-KHR call.
See VK:CREATE-DISPLAY-PLANE-SURFACE-KHR
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-display-plane-surface-khr ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-android-surface-khr ((resource instance create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-ANDROID-SURFACE-KHR call.
See VK:CREATE-ANDROID-SURFACE-KHR
See VK:DESTROY-SURFACE-KHR"
  `(let ((,resource (vk:create-android-surface-khr ,instance ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-surface-khr ,instance ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-swapchain-khr ((resource device create-info &key allocator) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-SWAPCHAIN-KHR call.
See VK:CREATE-SWAPCHAIN-KHR
See VK:DESTROY-SWAPCHAIN-KHR"
  `(let ((,resource (vk:create-swapchain-khr ,device ,create-info (or ,allocator vk:*default-allocator*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-swapchain-khr ,device ,resource (or ,allocator vk:*default-allocator*)))))

(defmacro with-shared-swapchains-khr ((resources device create-infos &key allocator) &body body)
  "Binds RESOURCES to the result of a VK:CREATE-SHARED-SWAPCHAINS-KHR call.
See VK:CREATE-SHARED-SWAPCHAINS-KHR
See VK:DESTROY-SWAPCHAIN-KHR"
  (let ((resource (gensym "RESOURCE")))
    `(let ((,resources (vk:create-shared-swapchains-khr ,device ,create-infos (or ,allocator vk:*default-allocator*))))
       (unwind-protect
           (progn ,@body)
         (loop for ,resource in ,resources do
               (vk:destroy-swapchain-khr ,device ,resource (or ,allocator vk:*default-allocator*)))))))

(defmacro with-debug-report-callback-ext ((resource instance create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DEBUG-REPORT-CALLBACK-EXT call.
See VK:CREATE-DEBUG-REPORT-CALLBACK-EXT
See VK:DESTROY-DEBUG-REPORT-CALLBACK-EXT"
  `(let ((,resource (vk:create-debug-report-callback-ext ,instance ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-debug-report-callback-ext ,instance ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-debug-utils-messenger-ext ((resource instance create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-DEBUG-UTILS-MESSENGER-EXT call.
See VK:CREATE-DEBUG-UTILS-MESSENGER-EXT
See VK:DESTROY-DEBUG-UTILS-MESSENGER-EXT"
  `(let ((,resource (vk:create-debug-utils-messenger-ext ,instance ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-debug-utils-messenger-ext ,instance ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-video-session-khr ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-VIDEO-SESSION-KHR call.
See VK:CREATE-VIDEO-SESSION-KHR
See VK:DESTROY-VIDEO-SESSION-KHR"
  `(let ((,resource (vk:create-video-session-khr ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-video-session-khr ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

(defmacro with-video-session-parameters-khr ((resource device create-info &key allocator extension-loader) &body body)
  "Binds RESOURCE to the result of a VK:CREATE-VIDEO-SESSION-PARAMETERS-KHR call.
See VK:CREATE-VIDEO-SESSION-PARAMETERS-KHR
See VK:DESTROY-VIDEO-SESSION-PARAMETERS-KHR"
  `(let ((,resource (vk:create-video-session-parameters-khr ,device ,create-info (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*))))
     (unwind-protect
         (progn ,@body)
       (vk:destroy-video-session-parameters-khr ,device ,resource (or ,allocator vk:*default-allocator*) (or ,extension-loader vk:*default-extension-loader*)))))

