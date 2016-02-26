# New in 2.2

- Server configuration now accepts the `:reduce-lurker-spam` keyword
  to set that variable.
- Lui now supports inline markup with `*bold*` and similar. Customize
  `lui-formatting-list` for this.
- `lui-add-input` is a new function to tell lui about new input that
  did not originate from lui itself. It is added to the history.
- Circe now adds the argument to `/query` to the chat history of a
  query buffer.
- The new variables `lui-time-stamp-time` and `lui-time-stamp-zone`
  allow programmers to customize the time zone for time stamps in lui.
- And lots of bug fixes.

# New in 2.1

- New option: `circe-inhibit-nick-highlight-function` – this allows
  you to disable nick highlighting in some messages.
- New extension: `circe-new-day-notifier.el` – show date changes in
  chat buffers. (Thanks to Pásztor János!)
- Improve Bitlbee support by adding a default port (6667) and
  disabling lagmon if it is used.
- Improved buttonizing of various references, like PEP links or Emacs
  debbugs references.
- Fix a bug that would confuse Emacs with lots of `nil` faces
- Lots of other bug fixes.

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
