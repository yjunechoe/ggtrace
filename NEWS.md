# ggtrace (development version)

### **Breaking changes**

- The `~line` keyword for `ggtrace()` is renamed to `~step` for consistency with the argument name `trace_steps`

- For safety reasons, the `~step` keyword will now only be substituted for the expression at the current step only if `~step` is by itself (i.e., an exact match). For example, `~step` will not be substituted if `quote(head(~step))` is passed to `trace_exprs`. Users are encouraged to return the method's environment with `quote(environment())` or interactively debug with `ggedit()` if they want to manipulate the expression.

- The position of the `obj` argument of `ggtrace()` has been moved from second to fourth, to allow for shortcuts like `ggtrace(method = ..., 2:3, quote(data))`, which will evaluate and store the output of the `data` variable at the second and third steps of the method body.

### **New features**

- `trace_exprs` argument of `ggtrace()` is now optional. If not provided, defaults to `~step`.

### **Improvements**

- Documentation for some of the functions now contain a **Gotchas** section for explanations of / solutions to common problems

### **Bug fixes**

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
