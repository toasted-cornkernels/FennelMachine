---
name: fennelmachine-tour
description: Give the user a live, interactive tour of FennelMachine (this Hammerspoon config) by driving the real leader keys on their screen, chapter by chapter, with a caption overlay. Use when the user asks for an interactive demo, tour or walkthrough of FennelMachine features, or wants to see new commands in action.
---

# FennelMachine interactive tour

`tour.lua` (next to this file) runs in the user's live Hammerspoon. It presses the
real keys (`Cmd-SPC f R`, …) so which-key, the minibuffer and the labels show up
exactly as when the user types them, while a caption at the top of the screen says
what each chapter shows.

## Running it

```sh
hs -c 'dofile("<this folder>/tour.lua")'
```

- **Ctrl-Alt-N** starts the next chapter; **Ctrl-Alt-Q** quits at any time.
- Chapter 1 starts right away and opens `/tmp/fm-demo/notes.txt` in TextEdit.
- Tell the user to wait until a chapter's keys are done (up to ~25 s) before
  pressing Ctrl-Alt-N, and not to type or click while a chapter plays.
- Quitting (or Ctrl-Alt-N after the last chapter) cleans up: it restores the
  `bookmarks`, `shell.history` and `kmacro.last` settings saved at start, kills
  TextEdit if the tour launched it, and deletes `/tmp/fm-demo`.
- To close a running tour for the user: `hs -c 'if DEMO and DEMO.quit then DEMO.quit() end'`.

## Chapters

Welcome · files (`f j`, `SPC TAB`) · rename (`f R`) · bookmarks (`f b m`, `f b b`) ·
app switcher and ibuffer (`b b`, `b B`) · new window and ace-window (`b c`, `w a`) ·
ace swap/delete (`w S`, `w D`) · menu search (`s j`) · keyboard macros
(`K (`, `K )`, `K e`) · shell command (`!`) · resume (`r l`) · link hints in
Safari (`j l`) · window numbers and the launcher (`1`, `SPC SPC`) · the end.

## Rehearse before showing it

Run it once yourself and check the state after each chapter, since screenshots
aren't available (`screencapture` and `hs.screen:snapshot()` lack permission):

```sh
hs -c 'DEMO.next()'; sleep 25
hs -c 'print(DEMO.chapter, hs.window.focusedWindow():title(), require("lib.minibuffer")["active?"]())'
```

Also grep the console for `message:` lines and errors
(`hs -c 'print(hs.console.getConsole():sub(-60000))' | grep "message:\|rror"`).
Then `DEMO.quit()` and start it fresh for the user.

## Adding or changing chapters

A chapter is `{title = …, body = "two lines\nof caption", actions = {…}}`. Each
action returns the seconds to wait before the next one:

| Helper                         | Does                                                       |
|--------------------------------|------------------------------------------------------------|
| `leader("f", {{"shift"}, "r"})` | `Cmd-SPC`, then each key 1.1 s apart, so which-key is seen |
| `key(mods, k, d)`              | one keystroke, e.g. `key({}, "return", 2)`                 |
| `text(s, d)`                   | type a string (fine for the minibuffer and documents)      |
| `slowtype(s, d)`               | type lowercase letters/spaces with real key codes          |
| `wait(d)` / `call(fn, d)`      | pause / run Lua                                            |
| `type_label(pattern, exclude)` | type the ace-window label of the window whose title matches |

Shifted keys are mods plus the base key: `(` is `{{"shift"}, "9"}`, `!` is
`{{"shift"}, "1"}`, `K` is `{{"shift"}, "k"}`.

## Pitfalls learned

- `hs.eventtap.keyStrokes` posts characters with key code 0. A macro recorded
  from it replays as `a`s, so use `slowtype` inside a macro recording.
- A leader item marked `:repeatable true` keeps the menu open after it runs, so
  keys the command posts itself (a replayed macro) hit the menu. Keep such
  commands non-repeatable.
- Saving any `.fnl` file reloads Hammerspoon and drops the `hs` IPC port for a
  few seconds ("message port invalid"); wait ~4 s and retry. A reload also wipes
  a running tour.
- Never touch the user's clipboard in a chapter (no `f y`), and back up any
  `hs.settings` key a chapter writes.
- Keep the caption at the top: the echo area, minibuffer and which-key are at the
  bottom.
- Web content needs a moment after a page opens before link hints find anything.
