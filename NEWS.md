# ggtrace (development version)

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
