# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
