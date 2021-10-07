# ggtrace (development version)

# ggtrace 0.3.1

### **Improvements**

- Better error handling for inherited methods in `ggbody()` (#23)

    - If a method doesn't exist in a parent, directs users to call `ggbody(, inherit = TRUE)`
    
    - If the recursive search with `inherit = TRUE` fails, directs users to load all relevant packages
  

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
