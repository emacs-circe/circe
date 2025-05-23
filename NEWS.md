# Upcoming

# New in 2.14

- The `lui-irc-colors` module now supports spoiler text. When
  selecting such text, the contents are displayed. In GUI Emacs,
  hovering over text will show them, too.
- Backslashes are now considered part of nickname syntax. This is most
  noticeable when using the `circe-color-nicks` module with
  `circe-color-nicks-everywhere` enabled.
- Text obtained with `filter-buffer-substring` from Lui buffers is now
  no longer read-only. This paves the way towards supporting `M-x
  zone` in chat buffers.
- The `lui-autopaste` module removed support for the discontinued
  sprunge.us paste service. Due to the other supported ix.io service
  being on a break as well, support for the paste services 0x0.st and
  paste.rs has been added.
- Several byte-compiler warnings have been fixed
- The `circe-display-images` module was changed to no longer require
  capturing groups in `circe-display-images-image-regex`. This is a
  backwards-incompatible change as the entire matched string is used
  instead, however this only poses a problem if its value has been
  changed to match more than just the URL.

# New in 2.13

- Verification with NSM now ensures that `gnutls-verify-error` is
  disabled to detect expired certificates.
- The `lui-irc-colors` module now supports strikethrough/monospace
  text.
- The `circe-color-nicks` module now highlights nicks in query buffers
  as well.
- The minimum supported Emacs version has been bumped to 25.1.
- Emacs Lisp identifiers in Lui buttons now allow colons
- Password values in the network options are validated for the
  expected type to prevent a common configuration mistake
- Several byte-compiler warnings have been fixed

# New in 2.12

- `circe-color-nicks` is now significantly faster. This is noticable
  when performing ZNC playbacks.
- `lui-track-bar` supports `lui-track-bar-use-fringe-p` to use a
  fringe indicator instead of a full line to mark the last read
  position in a buffer.
- The `lui-track-bar` functionality has been moved to `lui-track` to
  allow selecting different types of last read position marks. The old
  variables have been marked as obsolete to guide users to the new
  ones.
- Old XEmacs-compatibility code has been removed.
- TLS handling is now significantly more robust and fast if the
  `gnutls-boot-parameters` function is available (provided by Emacs
  26.1 and newer).
- The ptpb.pw service in `lui-autopaste` has been replaced with
  sprunge.us as it's no longer available.
- The behavior of `tracking-max-mode-line-entries` has been adjusted
  to not display any buffers when set to zero.
- `/gquit` now unconditionally sends `/quit` and inhibits
  auto-reconnect.
- Several byte-compiler deprecation warnings have been fixed.
- Tracking can be sped up by customizing `tracking-shorten-modes`.
- `circe-display-images` no longer requires ImageMagick support.
- Certificate errors are now handled by NSM if available.
- SASL EXTERNAL and TLS client certificates are now supported.
- The minimum supported Emacs version has been bumped to 24.5.
- All mentions of Freenode have been changed to Libera.Chat.
- The display of the buffer name can now be customized, tracking has
  been updated to shorten superfluous tails as a result of that.
- Channel and/or network name are now mentioned when killing a
  chat/server buffer.
- The Hackint network has been added to the default network list.
- The repository has changed to an organization due to a maintainer
  change.
- CI has been migrated from Travis to GitHub CI.
- All failing CI tests have been fixed.
- Networks now support an option to enable logging per network.
- The list of valid characters for log paths has been adjusted to
  avoid Windows-specific illegal file name characters.

# New in 2.11

- Several warnings about missing defcustom types have been fixed.

# New in 2.10

- The `/stats` command has been added.

# New in 2.9

- Minor nick coloring corrections

# New in 2.7

- `circe-highlight-all-nicks` is obsolete. Use
  `circe-color-nicks`instead.
- Emacs 24.3 is not supported anymore.
- Circe can now optionally display images inline. Use
  `enable-circe-display-images` for this feature.
- You can now limit the number of elements in the mode line. See
  `enable-circe-display-images`. You can also sort buffers with faces
  first, to see highlights first, by setting
  `enable-circe-display-images`.
- And lots of bugfixes.

# New in 2.6

- No new features, but some bug fixes.

# New in 2.5

- Update the openssl invocation to current versions of openssl. For
  some reason, they just remove a command line argument.
- Some other bug fixes.

# New in 2.4

- `circe-server-killed-confirmation` now can kill every buffer even
  without asking (thanks to Rudi Grinberg)
- lui has been improved to know about past messages to facilitate
  editing and deletion of old messages, primarily for protocols like
  Slack (thanks to Tom Willemse)
- Lots of bug fixes

# New in 2.3

- Circe (Lui) now has a track bar. Use `(enable-lui-track-bar)` to get
  a bar where you stopped reading when you did hide a buffer.
- Buffers are now by default limited to 100k, as large buffers cause
  unreasonable slowdown in Emacs.
- Autopaste now defaults to ix.io and also knows about ptpb.pw.
- A number of faces have been updated to be nicer to the eye.
- Improve compatibility with the Slack IRC gateway.
- Lots of bug fixes.

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
