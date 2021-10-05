# ggtrace (development version)

# ggtrace 0.1.2

- New function `gguntrace()` with the same syntax for specifying the ggproto method.

- `ggtrace()` gains a `once = TRUE` argument, which can be set to `FALSE` for persistent tracing

- Fix bug in `ggtrace()` where `step_deparsed` was being returned as a multi-length vector

# ggtrace 0.1.1

- Added [`ggedit()`](https://yjunechoe.github.io/ggtrace/reference/ggedit.html) for interactive debugging via directly editing the source code.

- Refactored `ggtrace()`. The package now only depends on `{rlang}`.

- Significant improvements to the readme / documentation

# ggtrace 0.1.0

Initial release.
