# New in 2.0

- Circe has had its IRC backend completely rewritten. It is now a
  separate library, `irc.el`, and much more powerful. Alas, this means
  a lot of existing configuration code will break.
- Because of this, Circe now fully supports SASL authentication,
  extended joins, and a few other modern IRC capabilities.
- XKCD references, CVE numbers and github issues are now buttonized.
- All IRC buffers change to the home directory by default.
- Circe now uses [buttercup][] for tests and Travis-CI for continuous
  integration tests.
- A number of options were removed to focus on sensible defaults.
  Re-check your configuration.
- Nick colors are now pre-computed to make them more appropriate for
  the current display and more distinct from each other.
- A lot of format strings have been added. Check the `circe-format`
  customization group.

[buttercup]: https://github.com/jorgenschaefer/emacs-buttercup
