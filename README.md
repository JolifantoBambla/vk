# vk
[Autogenerated](https://github.com/JolifantoBambla/vk-generator) Common Lisp/CFFI bindings for the Vulkan API.


## Motivation
The goal of this project is to make Vulkan feel lispy without bringing in too much abstraction.
It provides a layer of CLOS wrappers and functions atop CFFI-bindings to the Vulkan shared library that gets rid
of redundant struct members and output parameters of functions as much as possible.

E.g. where you would have to write the following in C++ (without VulkanHpp) to get all GPUs on a computer:

```cpp
std::vector<VkPhysicalDevice> devices;
uint32_t count = 0;

// first get the number of available devices
VkResult res = vkEnumeratePhysicalDevices(instance, &count, nullptr);

// then get the devices
devices.resize(count);
res = vkEnumeratePhysicalDevices(instance, &count, devices.data());
```

You can just write the following with `vk`:

```cl
(let ((devices (vk:enumerate-physical-devices instance)))
  ;; do something with your devices
  )
```


## Requirements

### Supported CL implementations
`vk` has been mostly tested on SBCL.

Minimal tests (loading the system and running a dummy test) suggest that `vk` works on:

* ABCL
* Clozure CL
* ECL
* SBCL

Check out the results of the latest [test actions](https://github.com/JolifantoBambla/vk/actions).

Unfortunately CLISP fails to install using Roswell (at least via GitHub Actions), so it remains untested.

Allegro is installed in a 32 bit version by Roswell (at least via GitHub Actions) which does not support `:long-long`. 64 bit versions are untested.

CMUCL fails to find `libvulkan.so` in the test action.

### Supported operating systems
`vk` has currently only been tested on linux (Ubuntu 20.04) and Windows (SBCL).

MacOS might also work if [MoltenVK](https://github.com/KhronosGroup/MoltenVK) is set up correctly.

### Supported Vulkan API versions
**The current version of `vk` is based on version `v1.2.198`.**

`vk` targets Vulkan 1.2, so all versions support the core API of version 1.2.x.
The main branch is always generated from the most recent version of the [Vulkan API XML registry](https://github.com/KhronosGroup/Vulkan-Docs)
supported by [vk-generator](https://github.com/JolifantoBambla/vk-generator) which also made it to a Vulkan SDK release.
Other versions are available on other branches named after the version (e.g. `v1.2.153`).
For more information about supported versions see the documentation of [vk-generator](https://github.com/JolifantoBambla/vk-generator).

#### Versioning of `vk`
`vk` uses the following versioning scheme: `major.minor.patch-<Vulkan API verion>`.

Since there are a lot of different Vulkan API versions, when there's a bug fix for the current version older versions might not
receive bug fixes right away.
If you absolutely have to work with a specific version and it seems like a bug fix just won't come for that version, feel free to
open an issue in the GitHub repository.


#### Migrating from 2.x.x-v1.2.x to 3.x.x-vx.x.x
Handle types are wrapped in version 3.0.0-v1.2.198 and above.
Should you have used raw handles returned by `vk` functions with functions from other packages (e.g. for window surface creation), you have to unwrap them by calling `vk:raw-handle`.

Functions that query a device and return a struct (class) instance that has a `pNext` member (`next` slot) have an additional optional argument of that struct (class) type to allow querying extension-specific information via this `pNext` member.
In some cases these new optional arguments might not have been added to the end of a function's lambda list from previous versions.

### CL dependencies
* `alexandria`
* `cffi`

#### Test dependencies
* `rove`

### Other dependencies
* The `vulkan` shared library. The easiest way of getting it is by installing the [Vulkan SDK](https://vulkan.lunarg.com/sdk/home).

#### MacOS only
* [MoltenVK](https://github.com/KhronosGroup/MoltenVK)


## Installation
As of the may 2021 dist update `vk` is available on Quicklisp.
Just make sure to have Vulkan installed (see [Vulkan SDK](https://vulkan.lunarg.com/sdk/home)), and then run 

```cl
(ql:quickload :vk)
```


## Packages

### vk
The main package of this system is `vk`.
It provides class and function wrappers over the lower level bindings in the `vulkan` package.

Note that there is no validation done by `vk` whatsoever, so you still need to enable validation layers in the driver yourself when creating a `vk:instance` (i.e. a `VkInstance`) just as you would have to in every other language.

#### Shadowed Symbols
It is **not meant do be `:use`d** by packages directly, since **it shadows symbols from `cl`** that clash with function and/or slot names from the Vulkan API:

* `format`
* `set`
* `stream`
* `type`
* `values`

#### Naming conventions
In `vk` all names in the Vulkan API have been stripped of their prefixes (i.e. `Vk` for types and `vk` for commands) and lispified (e.g. `VkInstanceCreateInfo` is `vk:instance-create-info`).

Struct and union member names as well as command arguments designating pointers in the C API by being prefixes with either `p` or `pp` have also been stripped of those (e.g. `pNext` is just `next`). 

Enum and bitmask value names have been stripped of any redundant prefixes denoting the enum or bitmask they belong to and are represented as keywords in `vk` (e.g. `VK_FORMAT_R4G4_UNORM_PACK8` is just `:r4g4-unorm-pack8`).

##### Exceptions
There are a few name clashes in the C API which break the naming conventions.
Currently, they all are between functions and slot accessors of the same name.
As a general rule, function names take precedence over slot accessors.
Slots and their `:initarg`s still have the same name, but the accessors use the lispified names of their corresponding struct members in the C API.

* The accessors to all slots named `wait-semaphores` are named `p-wait-semaphores` because they clash with the name of the function `vk:wait-semaphores`.
* Types from external headers (e.g. OS specific types) are not modified.
* Slots and accessors with the name `function` or `pFunction` in the C API are called `function-handle`. 

### vulkan (%vk)
`vulkan` (or `%vk`) contains the lower level `cffi` bindings to the C API.

The naming conventions are the same as in `vk` except for struct/union member names and command arguments.

### vk-error
`VkResult` values are automatically translated in both `vk` and `vulkan`.
Each negative result value is represented as an error type.
All error types are exposed via `vk-error` (as well as `vk` and `vulkan`).

### vk-alloc
Contains utilities for allocating resources and translating classes/structs to/from foreign memory.

#### Multithreading
When translating class instances the pointers to all translated struct members which are non-primitive types (e.g of `vk:instance-create-info` if it is bound to an instance of `vk:debug-utils-messenger-create-info-ext`) are stored in the hash table `vk-alloc:*allocated-foreign-objects*` and are freed before the pointer to the translated class instance is freed.
Since hash tables are not thread-safe and there should be no case where type translation needs to span multiple threads, each thread can and should have its own `vk-alloc:*allocated-foreign-objects*` that is independent of those of other threads.

Note that the wrapper functions in `vk` overwrite `vk-alloc:*allocated-foreign-objects*` in their own scope, so if a thread doesn't explicitly translate class instances, you should be fine.

### vk-utils
Contains utils for `vk`.

`vk-utils` contains `with`-style wrapper macros for most handles in the Vulkan API, e.g. `with-instance`:

```cl
;; the instance is created using the given create-info and is destroyed
;; at the end of the implicit progn of this macro:
(vk-utils:with-instance (instance instance-create-info)
  (format t "Found ~a devices!~%"
            (length (vk:enumerate-physical-devices instance))))
```

All available wrappers are listed in the [API reference](https://jolifantobambla.github.io/vk).

Aside from utilities for `vk`, this package also contains the function `memcpy`.


## Samples and Usage
Check out the [API reference](https://jolifantobambla.github.io/vk).

Check out the project [vk-samples](https://github.com/JolifantoBambla/vk-samples) for sample usage of `vk` as well as `vk-utils`.

### Structs
The Vulkan C API contains loads of structs and unions which each have a corresponding CLOS class in `vk`.
All these classes come with `cffi` translators, which automatically translate instances to and from foreign memory whenever they are needed.
This also goes for nested structs, so whenever a struct has a pointer to another struct as a member, you can just bind the slot to an instance of the corresponding class.

E.g. the `pInputAssemblyState` member of a `VkGraphicsPipelineCreateInfo` could be set like this in `vk`:

```cl
(vk:make-graphics-pipeline-create-info
 ...
 :input-assembly-state (vk:make-pipeline-input-assembly-state-create-info
                        :topology :triangle-list
                        :primitive-restart-enable nil)
 ...
 )
```

Note that in some places where a class instance is used (as a slot value or a function argument), you can also use a `cffi:foreign-pointer` as well, which might save you computation time, if you store translated objects yourself somewhere (e.g. `vk:allocation-callbacks`).

`vk` exposes each class directly as well as a `make`-style constructor for every class.

In the C API structs and unions often contain members which specify the length of another member (e.g. of a `const char*`).
Since those are redundant they are not included in the class wrappers and are set automatically during translation.

E.g. during translation, the `queueCreateInfoCount` member of a `VkDeviceCreateInfo` is automatically set to the length of the `queue-create-info` slot of the corresponding `vk:device-create-info` instance:

```cl
(vk:make-device-create-info
 :queue-create-infos (list (vk:make-device-queue-create-info
                            ...
                            )))
```

The exception to this are `void` pointers to arbitrary data, for which the size can not be determined without any knowledge about the type and number of elements in the array/buffer the pointer points to (e.g. the slot `initial-data` of the class `vk:pipeline-cache-create-info` which wraps `VkPipelineCacheCreateInfo`).

Another exception are cases where a slot specifies the length of an optional array (which can be null) but is not optional itself (e.g. `descriptor-count` in `vk:descriptor-set-layout-binding` and `swapchain-count` in `vk:present-regions-khr` or `vk:present-times-info-google`).

#### Unions
A class representing a union in the C API has one slot for each possible member of the union.
In order for translation to work properly only one slot should be bound for an instance of a class representing a union.
If more than one slot is bound, the first one (w.r.t. the order in which slots were specified in the class definition) will be used during translation.
`make`-style constructors for such classes allow only exactly one slot to be set.

#### Extending Structs: pNext
Many structs in the Vulkan API can be extended by one or more other structs using their `pNext` member (a `void` pointer).
In `vk` you can bind the `next` slot of such an instance to an instance of the class you'd like to extend it with.
Like all other slots the data bound to a `next` slot will be automatically translated to foreign memory along with the class instance when it is used as an argument for a function.
Note however, that there is no validation for bound `next` slots on the `vk` side.
E.g. to register a debug messenger to a `vk:instance` during creation, you can write:

```cl
(vk:make-instance-create-info
 :next (vk:make-debug-utils-messenger-create-info-ext
        :message-type '(:validation)
        :message-severity '(:info :warning :error)
        :pfn-user-callback ... ;; some CFFI callback
        :user-data ...) ;; whatever user data you want to pass
 :application-info ...) ;; whatever you want to enable for your Vulkan instance
```

### Handles
Like structs, handle types are wrapped in `vk` with in order to allow type checks when calling `vk` functions.
The rationale here is that not having to restart an interactive programming session because a segmentation fault that could have been prevented by a simple type check crashed the Lisp image is more important than the cost of wrapping and unwrapping handles.
Handle wrappers are structs with a single member `handle` and have the lispified name of their C counterparts (e.g. `instance` instead of `VkInstance`).
To access the raw foreign pointer within a handle wrapper, call `vk:raw-handle`.
To make a handle wrapper from a raw foreign pointer call `vk:make-<handle-name>-wrapper`.

### Enums & Bitmasks
Enums and bitmasks are represented by keywords, just as with all other `cffi` bindings.

### Functions
Almost all functions in the C API return a `VkResult` which indicates if its execution was successful or not.
So when a function should initialize a handle, it will take a pointer to the handle as an argument and by checking the `VkResult` you would be sure if the handle is valid or not.
Since this doesn't feel very lispy, `vk` wraps all functions omitting these output parameters from the lamda lists of the wrapper functions and instead returns them as `cl:values` together with the `VkResult`.
In the `cl:values`, the output parameters are in order of their appearence in the lambda list of the wrapped function followed by its return value (e.g. a wrapped/translated `VkResult`) if it returns a value (e.g. `vk:create-instance` returns `(cl:values <the created instance> <a translated VkResult value>)`).

As with classes, `vk` also omits arguments which specify the length of another argument from the wrapper functions lambda list.
The same goes for return values.
E.g. where `vkEnumeratePhysicalDevices` has two output parameters (`pPhysicalDeviceCount` and `pPhysicalDevices`), `vk:enumerate-physical-devices` only returns the found `physical-device` handles (and the result code, i.e. `:success`):

```cl
(let ((devices (vk:enumerate-physical-devices instance)))
  ;; do something with your devices
  )
```

The exception to this are again `void` pointers (e.g. the parameter `data` in `vk:get-query-pool-results` which wraps `vkGetQueryPoolResults` in the C API) which would require some knowledge about the exact type, etc.

#### VkResult
`VkResult` is an enum with positive and negative values, where negative values encode errors, zero encodes the success of a function and positive values encode the (partial) success of a function.
If a function returns a negative `VkResult` `vk` signals a `vk-error` with the error code as a keyword.

#### VkAllocationCallbacks
All functions that allocate resources and initialize handles take an intance of `vk:allocation-callbacks` (i.e. `VkAllocationCallbacks`) as an optional argument.
Since most of the time, this will be the same instance, `vk` provides the parameter `vk:*default-allocator*` which is used as the default value wherever an instance of `vk:allocation-callbacks` is used.
It defaults to a `cffi:null-pointer`.

### Using Extensions
The Vulkan API offers loads of extensions.
To use them, you need to enable them when creating your `vk:instance` or `vk:device` (depending on the type of the extension) by passing their names via the `enabled-extension-names` slot of their respective `*-create-info`s.
For this purpose `vk` provides the names of all extensions as constants with the following naming scheme `+-*-extension-name+`.

E.g. the name of the `VK_EXT_debug_utils` extension is `vk:+ext-debug-utils-extension-name+`:

```cl
(vk:make-instance-create-info
 :application-info ...
 ;; we need to enable the debug utils extension during instance creation
 :enabled-extension-names (list vk:+ext-debug-utils-extension-name+))
```

Apart from being enabled, functions belonging to an extension also need to be loaded for the `vk:instance` or `vk:device` which used them.
For this purpose every extension function has an additional optional argument at the very end of its lambda list: `extension-loader`.
This always defaults to the parameter `vk:*default-extension-loader*` and must be an instance of the struct `extension-loader`.

In order for the an `extension-loader` to work, it needs to be supplied with a `vk:instance` and/or a `vk:device`.
E.g. via creation:

```cl
(setf vk:*default-extension-loader* (vk:make-extension-loader
                                     :instance instance
                                     :device device))
```

Or by setting them using the readers `(extension-loader-instance vk:*default-extension-loader*)` and `(extension-loader-device vk:*default-extension-loader*)`.

When calling an extension function the passed `extension-loader` is searched for the function pointer of the extension function.
If it is the first call of the function using this `extension-loader` instance, the function pointer is fetched using `vk:get-instance-proc-addr` or `vk:get-device-proc-addr` and stored in an internal hash map of the `extension-loader` instance.
Then and in every subsequent call of the extension function this function pointer is used to call the function.

Note that setting the `device` or `instance` of an `extension-loader` via the exposed accessors in `vk` require the given handles to be wrapped, whereas the `vulkan` counterparts require raw foreign pointers.

So after having initialized the `vk:*default-extension-loader*` you can call extension functions like every other function in `vk`:

```cl
(vk:create-debug-utils-messenger-ext instance
                                     create-info)
```

Note that function pointers fetched for a `vk:device` are favored over `vk:instance` function pointers, so if an `extension-loader` has a `vk:device` and a `vk:instance` and has already loaded the function pointer on a `vk:instance` level, it will fetch and use the `vk:device` level function pointers instead.
The reason for this is that `vk:device` level function pointers avoid the overhead of possible dispatch mechanisms in the driver because the exact `vk:device` is already known.
(This is also true for all other functions in the Vulkan API, so if you really care for performance then you might want to fetch function pointers for all functions via `vk:get-device-proc-addr` and use only those.)


## Caveats
### Validation Errors & Slime
Validation errors produced by `VK_LAYER_KHRONOS_validation` are not logged via a debug utils messengers, but directly to `stdout`.
This means that for slime users validation errors will be logged to `*inferior-lisp*` by default.
See [this stackoverflow answer](https://stackoverflow.com/a/40180199) for more information.

### pNext-member of VkBaseOutStructure
Due to how foreign structs with `pNext` members are translated from foreign memory, a translated `vk:base-out-structure` will always have a foreign pointer or `nil` bound to its `next`-slot.
This should not be a problem however, since there is almost no use for instances of `vk:base-out-structure` aside from determining the actual type of the instance by reading its `s-type`-slot.

### VkShaderModuleCreateInfo
The `VkShaderModuleCreateInfo` struct has a member called `codeSize` which is the number of bytes in its `code` member.
You might be tempted to read your shaders byte by byte, but `VkShaderModuleCreateInfo` actually expects an array of `uint32_t`.
As with other `*Count`-members in the Vulkan API, `vk` determines the value to set for `codeSize` automatically.
For this to work properly, the `code` slot of a `vk:shader-module` also needs to be a sequence of 32-bit integers.

`vk-utils:read-shader-source` exists exactly for this purpose: it reads a SPIR-V binary and spits out a vector of 32-bit integers.

Another option is to use the package [shaderc](https://github.com/JolifantoBambla/shadercl) to compile shaders into a vector of 32-bit integers directly from your REPL.

### Lambda list order
Many `vk` wrapper functions make use of optional arguments if their C counterparts don't require an argument to be set.
In some cases, the order of the wrapper function's lambda list therefore differs from the order of the corresponding C function's argument list.
E.g. `vk:cmd-pipeline-barrier`, where `src-stage-mask` and `dst-stage-mask` are the last two arguments instead of the second and third one.

### Default values of slots
Some slots of `vk`'s wrapper classes have default values (mostly number - `0.0` or `0` - and string slots - `""`).
The choice for these default values are determined automatically by the generator from the member's type in the C API.
In some cases, these defaults don't make much sense (e.g. `vk:viewport`'s `max-depth` slot is `0.0` by default).


## Contributing
Found a bug? Please open a bug report in the GitHub repository.


## Acknowledgements
The whole project is autogenerated by [vk-generator](https://github.com/JolifantoBambla/vk-generator) which has been forked from [cl-vulkan](https://github.com/3b/cl-vulkan) and is partially ported from [Vulkan-Hpp](https://github.com/KhronosGroup/Vulkan-Hpp).

The documentation is autogenerated using [staple](https://github.com/Shinmera/staple).
