# ggtrace (development version)

# ggtrace 0.3.5

### **New features**

- New function `is_traced()` checks whether a method is currently being traced

### **Improvements**

- `ggtrace()` now breaks early with a more informative message if the method is not a function (ex: `Stat$extra_params`, which is a 1-length character vector `"na.rm"`)

- `ggbody()` now warns if it's returning the body of a method that's currently being traced (#35)

- `ggedit()` now informs you if you're editing on top of an existing trace/edit. 

### **Bug Fixes**

- Fixed bug where `trace_exprs` would fail to be recycled if it is a list of length-1. Now a 1-length list of an expression as well as an expression by itself will get recycled to match the number of steps passed to `trace_steps`.

- Fixed bug where `ggtrace()` wouldn't loop over `trace_exprs` after the first time it's triggered with persistent tracing on (`once = TRUE`). Issue was due to failing to reset the internal counter after each time the trace is triggered.

# ggtrace 0.3.4

### **Breaking changes**

- The `.print` argument to `ggtrace()` is renamed to `print_output` to make its functionality more transparent.

### **New features**

Several options for finer control over printing and formatting of output from `ggtrace()`, in addition to the existing `.print` argument:

- `ggtrace()` gets a `use_names` argument. When `TRUE`, it uses the names of the list of expressions passed to `trace_exprs` as the names for the tracedump set to `last_ggtrace()` and added to `global_ggtrace()`.

- `ggtrace()` gets a `verbose` argument. When `FALSE`, it suppresses the display of all non-`message()` information, including information about which expression is evaluated where, as well as the output of those expressions when evaluated (which can be selectively suppressed with `.print` for finer control). `verbose` is `TRUE` by default.

- Setting `options(ggtrace.suppressMessages = TRUE)` will also suppress `messages()`s about what method is being traced, whether a trace has been triggered on a method, whether there exists a persistent trace, etc. This information is very important so using this option is not recommended, but it has been made available. This option is set to `FALSE` on package load.

- Setting `options(ggtrace.as_tibble = TRUE)` will return evaluated expressions as tibbles if the output is a data frame. Using this option may be convenient for interactive inspections but it is not recommended for testing or debugging (see [related {ggplot2} Github issue](https://github.com/tidyverse/ggplot2/issues/3018)). This option is set to `FALSE` on package load.

### **Improvements**

- Tracedumps accumulated in `global_ggtrace()` are named after the method (+ hexadecimal ID) for ease of searching. (#31)

- Triggering of a trace is now informed via `message()` instead of `cat()` (#29)

### **Bug Fixes**

- `ggtrace()` correctly throws an error when `trace_steps` is not ordered. This is checked after the negative index conversion, so something like `trace_steps = c(1, -1)` still works fine).

# ggtrace 0.3.3

### **New features**

- New function `global_ggtrace()` which returns accumulated tracedumps from `ggtrace()`. This is useful in conjunction with `ggtrace(once = FALSE)` for tracing a method that you expect will be called multiple times (ex: `Stat$compute_group` gets triggered the same number of times as the number of groups in a panel).

- New function `clear_global_ggtrace()` which clears `global_ggtrace()` by setting it to `NULL`.

### **Improvements**

- Functions now pass around quosures instead of expressions (#29)

- Testing setup (#28)

### **Bug Fixes**

- Fixed a bug where the number of expressions passed to `trace_exprs` were allowed to be different from the number of `trace_steps`, causing `ggtrace()` to silently fail. This now throws an error.

# ggtrace 0.3.2

### **Improvements**

- `trace_exprs` argument can now take a named list of expressions (#21)

- Improved documentation for `ggbody()`

- More informative messages for `ggtrace()` and `gguntrace()`

### **Bug fixes**

- No longer errors on exit when creating a persistence trace with `once = FALSE`

# ggtrace 0.3.1

### **Improvements**

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

# ggtrace 0.3.0

### **Breaking changes**

- The `obj` argument is completely removed from all functions in the package. The constraint on supplying methods as expressions forces users to be intentional about tracing ggproto methods by having to provide them as code. This also allows the functions to return more informative messages, which was the main motivation for this breaking change.

    - <p>Because the `obj` argument was only designed for compatibility with the `get("method", ggproto)` syntax for retrieving the function body of ggproto methods, it should not affect interactive workflows. In fact, the shortform `method = ggproto$method` is more convenient and has always been recommended for passing a ggproto method to `{ggtrace}` functions.</p>

- As a reminder, all functions that take the ggproto method in the `method` argument expects an expression in the following forms (this part hasn't changed):

    - `ggproto$method`
    - `namespace::ggproto$method`
    - `namespace:::ggproto$method`
  
### **Improvements**

- Accurate string conversion for ggproto objects (#9), made possible by the breaking change.

- `trace_steps` argument can now take negative indices (#22) and has better error handling for out of range indices.
  
- Better deparsing for the `split_ggproto_method()` internal.

- Added a Tips & Tricks section to the documentation for `ggtrace()`.

### **Bug fixes**

- Internal variable `.store` renamed to `.ggtrace_storage` to prevent overriding `ggplot2::.store` (#18)

# ggtrace 0.2.0

### **Breaking changes**

- The `~line` keyword for `ggtrace()` is renamed to `~step` for consistency with the argument name `trace_steps` (#14)

- For safety reasons, the `~step` keyword will now only be substituted for the expression at the current step only if `~step` is by itself (i.e., is an exact match) (#16, #11).

    - <p>For example, `~step` will not be substituted if `quote(head(~step))` is passed to `trace_exprs`. Users are encouraged to return the method's environment with `quote(environment())` or interactively debug with `ggedit()` if they want to manipulate the expression.</p>

- The position of the `obj` argument of `ggtrace()` has been moved from second to fourth, to allow for shortcuts like `ggtrace(method = ..., 2:3, quote(data))`, which will evaluate and store the output of the `data` variable at the second and third steps of the method body. (#15)

### **New features**

- `trace_exprs` argument of `ggtrace()` is now optional. If not provided, defaults to `~step` (#13)

- You can now tell `ggbody()` to (recursively) search for the method from its parents with `inherit = TRUE` (#12) 

### **Improvements**

- Documentation for some of the functions now contain a **Gotchas** section for explanations of / solutions to common problems (#10)

# ggtrace 0.1.2

### **New features**

- New function `gguntrace()` with the same syntax for specifying the ggproto method.

- `ggtrace()` gains a `once = TRUE` argument, which can be set to `FALSE` for persistent tracing

### **Bug fixes**

- Fix bug in `ggtrace()` where `step_deparsed` was being returned as a multi-length vector

# ggtrace 0.1.1

### **New features**
- New function [`ggedit()`](https://yjunechoe.github.io/ggtrace/reference/ggedit.html) for interactive debugging via directly editing the source code.

### **Improvements**

- Refactored `ggtrace()`. The package now only depends on `{rlang}`.

- Significant re-write to the readme / documentation

# ggtrace 0.1.0

Initial release

pkgdown site: [https://yjunechoe.github.io/ggtrace](https://yjunechoe.github.io/ggtrace)

Functions:

- [ggtrace()](https://yjunechoe.github.io/ggtrace/reference/ggtrace.html)
- [ggbody()](https://yjunechoe.github.io/ggtrace/reference/ggbody.html)
- [last_ggtrace()](https://yjunechoe.github.io/ggtrace/reference/last_ggtrace.html)
