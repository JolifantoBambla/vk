# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.1.0-v1.2.198] - 2022-10-24 [vk](https://github.com/JolifantoBambla/vk/releases/tag/3.1.0-v1.2.198)

### Added
 - `vk` now exposes a platform dependent type `non-dispatchable-handle` (see #10).

### Fixed
 - Translation of `immutable-samplers` member of `descriptor-set-layout-binding` is now fixed (see #11).

## [3.0.1-v1.2.198] - 2022-05-07 [vk](https://github.com/JolifantoBambla/vk/releases/tag/3.0.1-v1.2.198)

### Fixed
- Translation of all members representing fixed sized arrays now uses `cffi:lisp-array-to-foreign` on the slot pointer directly.

## [3.0.0-v1.2.198] - 2022-02-06 [vk](https://github.com/JolifantoBambla/vk/releases/tag/3.0.0-v1.2.198)

### Added
- Structure types now have pretty printers.
- Errors and conditions are exposed by the `vk-errors` package, which doesn't shadow any symbols in `CL-USER`.
- All version constants and functions packing or unpacking version numbers in the C API are now represented in `vulkan` and `vk`.

### Changed
- Functions in `vk` which return structure types with a `pNext` member that can be used to query extension-specific information now take an instance of such a structure as optional input argument.
- Handle types are now wrapped in `vk`. This affects all function taking handles as arguments or returning handles.
- `vk:make-api-version` takes an additional argument `variant`.
- All types and keywords with an `-i-os` suffix now have an `-ios` suffix instead.
- All wrapper functions in `vk` now use a local hash map to store intermediate pointers during translation instead of the global one provided by `vk-alloc`.

### Fixed
- Functions in `vk` now accept arrays as arguments (they are `coerce`d to lists, though).
- Returned-only structure type slots in `vk` are not filled by default by `make`-style constructors, and not translated if they are `nil`.
- `KHR` extension functions that are not part of the WSI extensions are now treated as extension functions.
- `uint` struct members that are split up into multiple smaller members (e.g. the `uint32_t` composed of `uint32_t instanceCustomIndex:24` and `uint32_t mask:8` in `VkAccelerationStructureInstanceKHR`) are now correctly translated. 

## [2.2.0-v1.2.189] - 2021-09-15 [vk](https://github.com/JolifantoBambla/vk/releases/tag/2.2.0-v1.2.189)

### Added
- `vk` now has `make`-style constructors for all its classes.
- `%vk` and `vk` now expose `extension-loader-instance` and `extension-loader-device`.

### Fixed
- Fixed `with`-style wrappers which call `vk` functions (`with-descriptor-sets` & `with-command-buffers`).

## [2.1.1-v1.2.182] - 2021-08-27 [vk](https://github.com/JolifantoBambla/vk/releases/tag/2.1.1-v1.2.182)

### Fixed
- Fixed default arguments in `with`-style wrapper in `vk-utils` such that they can be dumped into `.fasl` files.

## [2.1.0-v1.2.182] - 2021-08-27 [vk](https://github.com/JolifantoBambla/vk/releases/tag/2.1.0-v1.2.182)

### Added
- `vk-utils` now exports `with`-style wrappers for most handles in the Vulkan API.

### Fixed
- Fixed translation of VkPipelineMultisampleCreateInfo.pSampleMask.

## [2.0.1-v1.2.179] - 2021-06-05 [vk](https://github.com/JolifantoBambla/vk/releases/tag/2.0.1-v1.2.179)

### Fixed
- Fixed translation of unions when used as function arguments directly.

## [2.0.0-v1.2.178] - 2021-05-12 [vk](https://github.com/JolifantoBambla/vk/releases/tag/2.0.0-v1.2.178)

### Changed
- Experimental NVIDIA extensions now have their proper suffix `-NVX` instead of `-NV-X`.

## [1.0.1-v1.2.177] - 2021-05-01 [vk](https://github.com/JolifantoBambla/vk/releases/tag/1.0.1-v1.2.177)

### Fixed
- Fixed typos in README.
- Fixed `extension-loader` argument for extension functions without other optional arguments.
- Fixed optional arguments for functions filling buffers (e.g. `get-query-pool-results`).
- Fixed functions returning multiple non-array values.
- Fixed functions returning an array and a non-array value.
- Fixed functions enumerating two arrays.
- Fixed style warning in `vk-alloc:foreign-allocate-and-fill`.
- Refactored `vk-alloc:with-foreign-allocated-object` such that SBCL can infer the correct type of the `content` argument.
- Give symbols in macros more meaningful names.
