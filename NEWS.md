# ggtrace (development version)

## ggtrace 0.6.2

Upkeep release for the [JSM 2023 talk](https://ww2.aievolution.com/JSMAnnual/index.cfm?do=ev.viewEv&ev=2860) "Sub-layer modularity in the Grammar of Graphics".

### New Features

- `layer_*()` snapshot functions gain a `verbose` argument.

- `getOption("ggtrace.rethrow_error")` To control re-printing of the error by workflow functions when `error = TRUE`. Set to `FALSE` on load.

## ggtrace 0.6.1

### Bug fixes

- `get_method_inheritance()` now works correctly for top-level ggprotos - ex: `get_method_inheritance(Geom)` (#99; thanks @yjing14 for bug report).

## ggtrace 0.6.0

Significant usability improvements, including `{cli}` integration.

### New Features

- Convenience functions `layer_before_stat()`, `layer_after_stat()`, `layer_before_geom()`, and `layer_after_scale()` that returns a snapshot of layer data in the internals. Inspired by `ggplot2::layer_data()` with a similar interface. (#97; thanks @JoFrhwld for suggestion)

- Interactive debugging functions `last_layer_errorcontext()` and `last_sublayer_errorcontext()` which return the internal context of layer errors at the level of the `Layer` and sub-`Layer` (e.g., `Stat` or `Geom`) ggproto methods, respectively. `last_sublayer_errorcontext()` is still in experimental phase (error-prone, may be removed in future).
 
- New catch-all Inspect workflow function `ggtrace_inspect_on_error()` which dumps information about the `method` that errors while rendering `x`.

# ggtrace 0.5.x

## ggtrace 0.5.3

### New Features

- Inspect workflow functions gain a `error = TRUE` argument, which allows inspection of an earlier intermediate step even when the ggplot rendering process fails down the line (#89)

### Bug fixes

- Internal `is.ggtrace_placeholder()` returns scalar logical (#94)

## ggtrace 0.5.2

### Breaking Changes

- `ggtrace_inspect_vars()` simplifies the output when `at` is length 1.

### Bug fixes

- `ggtrace_capture_env()` removes ggtrace-internal variables before snapshotting the environment (#88)

## ggtrace 0.5.1

### New Features

- `cond` argument of workflow functions now support integer shorthand for conditioning on the counter. E.g., `cond = 1L` is converted to `cond = quote(._counter_ == 1L)`. Multi-length integer vector is supported for highjack workflows. (#84)

## ggtrace 0.5.0

### New Features

- Added complements to the `base::debug()` family of functions that are compatible with ggproto methods - `ggdebug()`, `ggdebugonce()`, `ggundebug()`
- `get_method_inheritance()` to get the list of methods from self and parent ggprotos
- `ggtrace_inspect_n()` to get the number of times a method was called in the evaluation of a ggplot
- `ggtrace_inspect_which()` to get the indices when a condition `cond` evaluated to true inside a method
- `ggtrace_inspect_vars()` to get the value of variables at specified steps of a method's execution
- `ggtrace_inspect_args()` to get the value of arguments passed to a method
- `ggtrace_highjack_args()` to modify formals of a method at its execution
- `with_ggtrace()` gets a `out` argument which can take 1 of three options:

    - "t" or "tracedump" (default): returns the local tracedump from triggering traces on the `method` as the ggplot `x` is evaluated
    - "g" or "gtable": Invisibly returns the `<gtable>` grob after evaluating `x` with injected expressions in `method`.
    - "b" or "both": returns the tracedump while rendering the gtable (with `grid::grid.draw()`) as a side effect.

- Low-level functions `ggtrace()`/`gguntrace()` and wrapper `with_ggtrace()` can now take quosures in the `method` argument, which allows them to be used more programmatically.

### Breaking Changes

- `ggtrace_capture_env()` default value of `at` is changed to `-1L`, which captures a snapshot of the runtime environment right before the method returns. Only the first element is used if `at` is length > 1
- "modify" workflows are renamed to "highjack" (starting with `ggtrace_highjack_return()`) to reflect the fact that they always return the graphical output (gtable grob) (#78)

### Bug Fixes

- Fixed an issue where a trace would fail to remove itself with `ggtrace(once = TRUE)` if it was triggered by a copy of the traced function (#59)

# ggtrace 0.4.x

## ggtrace 0.4.8

### New Features

- Added `with_ggtrace()` for a functional interface to `ggtrace()`
- Added `ggtrace_capture_fn()` and `ggtrace_capture_env()`, which return a snapshot of the function/environment of the ggproto method at execution time
- Added `ggtrace_inspect_return()` and `ggtrace_modify_return()` to grab and swap return values at method's execution

### Miscellaneous

- Added workflows section to docs/references

## ggtrace 0.4.7

### New Features

- Added `ggformals()` which returns the `formals()` of functions and ggproto methods
- `ggbody()` gains a `as.list` argument to control whether output of `body()` is turned into a list
- Exported `set_last_ggtrace()` and `set_global_ggtrace()` for the tracedumps

### Bug Fix

- Fixed compatibility issues with {rlang} v1.0.0 new changes to the _call_ and _expression_ API

## ggtrace 0.4.6

### New Features

- Added `get_method()` which returns ggproto methods as functions
- Added wrappers for one-off workflows `ggdebugonce()` and `ggtraceback()` 

### Improvements

- The default value for the `verbose` argument of `ggtrace()` is changed to `FALSE`.

## ggtrace 0.4.5

### Improvements

- Deparsed expressions printed as messages are now wrapped in backticks (#57)
- `ggedit()` now only works when `isTRUE(interactive())` (#62)

### New Features

- Added `global_ggtrace_on/off()` which are aliases for `global_ggtrace_state(TRUE/FALSE)` (#63)

### Miscellaneous

- Added [exploratory debugging case study vignette](https://yjunechoe.github.io/ggtrace/articles/casestudy-ggxmean.html)

## ggtrace 0.4.4

### Improvements

- `clear_(last|global)_ggtrace()` functions now print a message saying that the trace dump has been cleared.
- Better error handling for closures that aren't searchable from environments

### Bug Fix

- Fixed but where methods with class <function> were being treated as bare functions. Now closures and methods outside of ggproto objects (e.g., R6 classes) can be traced too.

### Miscellaneous

- Added [FAQ vignette](https://yjunechoe.github.io/ggtrace/articles/FAQ.html)

## ggtrace 0.4.3

### Breaking changes

- Global tracedump is turned off by default (`global_ggtrace_state()` is `FALSE` on load) and must be explicitly activated with `global_ggtrace_state(TRUE)`.

### Improvements

- Improved messages for `global_ggtrace_state()`

## ggtrace 0.4.2

### Bug Fixes

- Fixed bug where `print_output = TRUE` would evaluate the expression twice (problematic for Inject workflows and causing general slowdowns)

## ggtrace 0.4.1

### New Features

- Global collection of tracedumps can be turned on/off with `global_ggtrace_state()`. It is still active by default but should memory become a concern, it can be turned off with `global_ggtrace_state(state = FALSE)`.

## ggtrace 0.4.0

### New Features

- All functions in the package now support the tracing/untracing of **any arbitrary functions** (exported, unexported, user-defined, etc.). This also includes S3/S4 methods like `ggplo2:::ggplot_build.ggplot` and `ggplot2:::ggplot_add.Layer`. Some other examples of what's now possible.

### Improvements

- `ggtrace()` gains a `...` for extensibility in future updates.
- `ggedit()` gains a `remove_trace` argument. When `TRUE`, it untraces first before editing.
- Improved error messages for invalid expressions passed to `method` argument of `ggbody()`

### Bug Fixes

- Fix bug where `ggtrace()` fails to evaluate the first line when it's the only reachable line (#44)

# ggtrace 0.3.x

## ggtrace 0.3.7

### Bug Fixes

- Fixed bug where incomplete traces due to early returns would not be logged without any special warning. `ggtrace()` now throws a warning when the actual number of traced steps does not match the expected number of traced steps (i.e., length of `trace_steps`) and logs incomplete tracedumps (#44)

## ggtrace 0.3.6

### Improvements

- `ggtrace(..., once = TRUE)` is less noisy about there being a persistent trace. It now only sends a line of message saying so when the persistent trace is created. When it's triggered, it'll tell you that there is a persistent trace on the methodbut will not remind you to untrace it later nor print the corresponding `gguntrace()` code to do so.

### Bug Fixes

- Fixed bug where `trace_exprs` evaluating to `NULL` would be removed from the tracedump (#38)

## ggtrace 0.3.5

### New features

- New function `is_traced()` checks whether a method is currently being traced

### Improvements

- `ggtrace()` now breaks early with a more informative message if the method is not a function (ex: `Stat$extra_params`, which is a 1-length character vector `"na.rm"`)

- `ggbody()` now warns if it's returning the body of a method that's currently being traced (#35)

- `ggedit()` now informs you if you're editing on top of an existing trace/edit. 

### Bug Fixes

- Fixed bug where `trace_exprs` would fail to be recycled if it is a list of length-1. Now a 1-length list of an expression as well as an expression by itself will get recycled to match the number of steps passed to `trace_steps`.

- Fixed bug where `ggtrace()` wouldn't loop over `trace_exprs` after the first time it's triggered with persistent tracing on (`once = TRUE`). Issue was due to failing to reset the internal counter after each time the trace is triggered.

## ggtrace 0.3.4

### Breaking changes

- The `.print` argument to `ggtrace()` is renamed to `print_output` to make its functionality more transparent.

### New features

Several options for finer control over printing and formatting of output from `ggtrace()`, in addition to the existing `.print` argument:

- `ggtrace()` gets a `use_names` argument. When `TRUE`, it uses the names of the list of expressions passed to `trace_exprs` as the names for the tracedump set to `last_ggtrace()` and added to `global_ggtrace()`.

- `ggtrace()` gets a `verbose` argument. When `FALSE`, it suppresses the display of all non-`message()` information, including information about which expression is evaluated where, as well as the output of those expressions when evaluated (which can be selectively suppressed with `.print` for finer control). `verbose` is `TRUE` by default.

- Setting `options(ggtrace.suppressMessages = TRUE)` will also suppress `messages()`s about what method is being traced, whether a trace has been triggered on a method, whether there exists a persistent trace, etc. This information is very important so using this option is not recommended, but it has been made available. This option is set to `FALSE` on package load.

- Setting `options(ggtrace.as_tibble = TRUE)` will return evaluated expressions as tibbles if the output is a data frame. Using this option may be convenient for interactive inspections but it is not recommended for testing or debugging (see [related {ggplot2} Github issue](https://github.com/tidyverse/ggplot2/issues/3018)). This option is set to `FALSE` on package load.

### Improvements

- Tracedumps accumulated in `global_ggtrace()` are named after the method (+ hexadecimal ID) for ease of searching. (#31)

- Triggering of a trace is now informed via `message()` instead of `cat()` (#29)

### Bug Fixes

- `ggtrace()` correctly throws an error when `trace_steps` is not ordered. This is checked after the negative index conversion, so something like `trace_steps = c(1, -1)` still works fine).

## ggtrace 0.3.3

### New features

- New function `global_ggtrace()` which returns accumulated tracedumps from `ggtrace()`. This is useful in conjunction with `ggtrace(once = FALSE)` for tracing a method that you expect will be called multiple times (ex: `Stat$compute_group` gets triggered the same number of times as the number of groups in a panel).

- New function `clear_global_ggtrace()` which clears `global_ggtrace()` by setting it to `NULL`.

### Improvements

- Functions now pass around quosures instead of expressions (#29)

- Testing setup (#28)

### Bug Fixes

- Fixed a bug where the number of expressions passed to `trace_exprs` were allowed to be different from the number of `trace_steps`, causing `ggtrace()` to silently fail. This now throws an error.

## ggtrace 0.3.2

### Improvements

- `trace_exprs` argument can now take a named list of expressions (#21)

- Improved documentation for `ggbody()`

- More informative messages for `ggtrace()` and `gguntrace()`

### Bug fixes

- No longer errors on exit when creating a persistence trace with `once = FALSE`

## ggtrace 0.3.1

### Improvements

- The unary functions `ggedit()` and `gguntrace()` gain dynamic dots `...` as the second argument, which gets ignored. This now makes it easy to call these function by modifying the call to an earlier`{ggtrace}` function from the console or in other interactive contexts.


- Significant re-write of `ggbody()` for better error handling (#23)

    - Aborts if method is not a call or is not of the accepted form, with specific error messages for each.

    - If a method doesn't exist in a parent, directs users to call `ggbody(, inherit = TRUE)`
    
    - If the recursive search with `inherit = TRUE` fails, directs users to load all relevant packages
    
    - Notifies if `inherit = TRUE` but method is defined for the object, not inherited

- Better error handling for `gguntrace()` when the method is no longer being traced (#24)

    - Uses the re-written `ggbody()` to validate the method
    
    - Unlike `base::untrace()`, no longer errors if given a method not currently being traced. It now prints a message saying so instead

- Standardization of messages printed by all `{ggtrace}` functions. Messages are now more informative and refer to the ggproto method in the callable format `ggproto$method`.

## ggtrace 0.3.0

### Breaking changes

- The `obj` argument is completely removed from all functions in the package. The constraint on supplying methods as expressions forces users to be intentional about tracing ggproto methods by having to provide them as code. This also allows the functions to return more informative messages, which was the main motivation for this breaking change.

    - <p>Because the `obj` argument was only designed for compatibility with the `get("method", ggproto)` syntax for retrieving the function body of ggproto methods, it should not affect interactive workflows. In fact, the shortform `method = ggproto$method` is more convenient and has always been recommended for passing a ggproto method to `{ggtrace}` functions.</p>

- As a reminder, all functions that take the ggproto method in the `method` argument expects an expression in the following forms (this part hasn't changed):

    - `ggproto$method`
    - `namespace::ggproto$method`
    - `namespace:::ggproto$method`
  
### Improvements

- Accurate string conversion for ggproto objects (#9), made possible by the breaking change.

- `trace_steps` argument can now take negative indices (#22) and has better error handling for out of range indices.
  
- Better deparsing for the `split_ggproto_method()` internal.

- Added a Tips & Tricks section to the documentation for `ggtrace()`.

### Bug fixes

- Internal variable `.store` renamed to `.ggtrace_storage` to prevent overriding `ggplot2::.store` (#18)

# ggtrace 0.2.x

## ggtrace 0.2.0

### Breaking changes

- The `~line` keyword for `ggtrace()` is renamed to `~step` for consistency with the argument name `trace_steps` (#14)

- For safety reasons, the `~step` keyword will now only be substituted for the expression at the current step only if `~step` is by itself (i.e., is an exact match) (#16, #11).

    - <p>For example, `~step` will not be substituted if `quote(head(~step))` is passed to `trace_exprs`. Users are encouraged to return the method's environment with `quote(environment())` or interactively debug with `ggedit()` if they want to manipulate the expression.</p>

- The position of the `obj` argument of `ggtrace()` has been moved from second to fourth, to allow for shortcuts like `ggtrace(method = ..., 2:3, quote(data))`, which will evaluate and store the output of the `data` variable at the second and third steps of the method body. (#15)

### New features

- `trace_exprs` argument of `ggtrace()` is now optional. If not provided, defaults to `~step` (#13)

- You can now tell `ggbody()` to (recursively) search for the method from its parents with `inherit = TRUE` (#12) 

### Improvements

- Documentation for some of the functions now contain a **Gotchas** section for explanations of / solutions to common problems (#10)

# ggtrace 0.1.x

## ggtrace 0.1.2

### New features

- New function `gguntrace()` with the same syntax for specifying the ggproto method.

- `ggtrace()` gains a `once = TRUE` argument, which can be set to `FALSE` for persistent tracing

### Bug fixes

- Fix bug in `ggtrace()` where `step_deparsed` was being returned as a multi-length vector

## ggtrace 0.1.1

### New features
- New function [`ggedit()`](https://yjunechoe.github.io/ggtrace/reference/ggedit.html) for interactive debugging via directly editing the source code.

### Improvements

- Refactored `ggtrace()`. The package now only depends on `{rlang}`.

- Significant re-write to the readme / documentation

## ggtrace 0.1.0

### Initial release

pkgdown site: [https://yjunechoe.github.io/ggtrace](https://yjunechoe.github.io/ggtrace)

Functions:

- [ggtrace()](https://yjunechoe.github.io/ggtrace/reference/ggtrace.html)
- [ggbody()](https://yjunechoe.github.io/ggtrace/reference/ggbody.html)
- [last_ggtrace()](https://yjunechoe.github.io/ggtrace/reference/last_ggtrace.html)
