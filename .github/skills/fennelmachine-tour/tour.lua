-- FennelMachine guided tour (see SKILL.md). Ctrl-Alt-N: next chapter, Ctrl-Alt-Q: quit.
if DEMO and DEMO.quit then DEMO.quit() end
DEMO = {timers = {}, chapter = 0}

local dir = "/tmp/fm-demo"
local doc = dir .. "/notes.txt"
local backup = {
  bookmarks = hs.settings.get("bookmarks"),
  shell = hs.settings.get("shell.history"),
  kmacro = hs.settings.get("kmacro.last"),
}
local textedit_was_running = hs.application.get("TextEdit") ~= nil
hs.execute("mkdir -p " .. dir)
local f = io.open(doc, "w") f:write("FennelMachine demo notes\n\n") f:close()

local screen = hs.screen.mainScreen():frame()
local cw, ch = 1000, 112
local caption = hs.canvas.new({x = screen.x + (screen.w - cw) / 2, y = screen.y + 40, w = cw, h = ch})
caption:level(hs.canvas.windowLevels.overlay)
caption:behavior({"canJoinAllSpaces", "transient"})
caption:appendElements(
  {type = "rectangle", action = "strokeAndFill", fillColor = {hex = "#161623", alpha = 0.95},
   strokeColor = {hex = "#bf90f5"}, strokeWidth = 2, roundedRectRadii = {xRadius = 12, yRadius = 12}},
  {type = "text", frame = {x = 20, y = 10, w = cw - 40, h = 32}, text = ""},
  {type = "text", frame = {x = 20, y = 44, w = cw - 40, h = 44}, text = ""},
  {type = "text", frame = {x = 20, y = 88, w = cw - 40, h = 20}, text = ""})
caption:show()

local function styled(s, size, color)
  return hs.styledtext.new(s, {font = {name = "Menlo", size = size}, color = {hex = color}})
end
local function say(title, body)
  caption[2].text = styled(title, 22, "#bf90f5")
  caption[3].text = styled(body or "", 15, "#c1cbf1")
  caption[4].text = styled(string.format("Chapter %d/%d   ·   Ctrl-Alt-N next   ·   Ctrl-Alt-Q quit",
                                         DEMO.chapter, #DEMO.chapters), 12, "#8187a1")
end

local function later(d, fn) table.insert(DEMO.timers, hs.timer.doAfter(d, fn)) end
local function wait(d) return function() return d end end
local function key(mods, k, d) return function() hs.eventtap.keyStroke(mods, k, 0) return d or 0.25 end end
local function text(s, d) return function() hs.eventtap.keyStrokes(s) return d or 0.6 end end
local function slowtype(s, d)
  return function()
    for c in s:gmatch(".") do
      if c == " " then hs.eventtap.keyStroke({}, "space", 0)
      else hs.eventtap.keyStroke({}, c, 0) end
    end
    return d or 0.6
  end
end
local function leader(...)
  local keys = {...}
  return function()
    hs.eventtap.keyStroke({"cmd"}, "space", 0)
    local i = 0
    local function step()
      i = i + 1
      local k = keys[i]
      if not k then return end
      if type(k) == "table" then hs.eventtap.keyStroke(k[1], k[2], 0) else hs.eventtap.keyStroke({}, k, 0) end
      later(1.1, step)
    end
    later(1.1, step)
    return 1.1 * (#keys + 1) + 0.3
  end
end
local function call(fn, d) return function() fn() return d or 0.5 end end

local function focus_doc()
  local app = hs.application.get("TextEdit")
  if app then
    app:activate()
    for _, w in ipairs(app:allWindows()) do if w:title():find("notes") then w:focus() end end
  end
end

local function run(actions, i)
  i = i or 1
  local a = actions[i]
  if not a then return end
  local delay = a() or 0.3
  later(delay, function() if DEMO.running == actions then run(actions, i + 1) end end)
end

local function ace_label_for(pattern, exclude_focused)
  local ow = require("lib.ordered-windows")["ordered-windows"]
  local focused = hs.window.focusedWindow()
  local wins = {}
  for _, w in ipairs(ow()) do
    if w:isStandard() and not (exclude_focused and focused and w:id() == focused:id()) then table.insert(wins, w) end
  end
  local labels = require("hints")["make-labels"](#wins, "asdfghjkl")
  for i, w in ipairs(wins) do if w:title():find(pattern) then return labels[i] end end
end
local function type_label(pattern, exclude)
  return function() local l = ace_label_for(pattern, exclude) if l then hs.eventtap.keyStrokes(l) end return 1.5 end
end

DEMO.chapters = {
  {title = "Welcome to the FennelMachine tour",
   body = "I press the real keys, so watch which-key and the minibuffer at the bottom.\nFirst, a demo document opens in TextEdit.",
   actions = {call(function() hs.execute("open -a TextEdit " .. doc) end, 2.5)}},
  {title = "Files: the window's document is the visited file",
   body = "SPC f j reveals notes.txt in Finder (dired-jump),\nthen SPC TAB jumps back to the last app.",
   actions = {call(focus_doc, 0.8), leader("f", "j"), wait(2.5), leader("tab")}},
  {title = "Files: rename the visited file",
   body = "SPC f R prefills the name; I type a new one.\nTextEdit follows the file: watch the window title.",
   actions = {call(focus_doc, 0.8), leader("f", {{"shift"}, "r"}), key({"ctrl"}, "u", 0.5),
              text("notes-renamed.txt", 1.2), key({}, "return", 2)}},
  {title = "Bookmarks",
   body = "SPC f b m bookmarks the document (the name defaults to its title).\nThen I go to Finder and jump back with SPC f b b.",
   actions = {call(focus_doc, 0.8), leader("f", "b", "m"), wait(0.8), key({}, "return", 1.5),
              call(function() hs.application.launchOrFocus("Finder") end, 1.5),
              leader("f", "b", "b"), wait(1.8), key({}, "return", 2)}},
  {title = "Buffers: switch apps, ibuffer",
   body = "SPC b b lists apps, most recent first. Then SPC b B, the ibuffer:\ns-m marks apps (shown with *); s-d / s-D / s-h would quit / kill / hide them.",
   actions = {call(focus_doc, 0.6), leader("b", "b"), wait(2.5), key({}, "escape", 0.8),
              leader("b", {{"shift"}, "b"}), wait(1), key({"cmd"}, "m", 1.2), key({}, "down", 0.5),
              key({"cmd"}, "m", 2.5), key({"cmd"}, "u", 1.5), key({}, "escape", 0.8)}},
  {title = "New window and ace-window",
   body = "SPC b c opens a new TextEdit window (Cmd-N).\nSPC w a labels every window; I type the label of notes-renamed.txt.",
   actions = {call(focus_doc, 0.6), leader("b", "c"), wait(1.5), leader("w", "a"), wait(2), type_label("notes")}},
  {title = "Ace swap and ace delete",
   body = "SPC w S swaps the focused window with a labelled one;\nSPC w D closes a labelled window (the empty Untitled one).",
   actions = {function() DEMO.l = ace_label_for("Untitled", true) return 0.1 end,
              leader("w", {{"shift"}, "s"}), wait(1.8),
              function() if DEMO.l then hs.eventtap.keyStrokes(DEMO.l) end return 2 end,
              leader("w", {{"shift"}, "d"}), wait(1.8), type_label("Untitled")}},
  {title = "Search the menus",
   body = "SPC s j: any menu item of the frontmost app, by name.\nI type \"show fonts\" and RET; Cmd-T closes the panel again.",
   actions = {call(focus_doc, 0.6), leader("s", "j"), wait(0.5), text("show fonts", 2), key({}, "return", 2.5),
              key({"cmd"}, "t", 1)}},
  {title = "Keyboard macros",
   body = "SPC K ( records (the menu bar shows ● Macro), I type a line, SPC K ) stops.\nThen SPC K e plays it back, twice.",
   actions = {call(focus_doc, 0.6), key({"cmd"}, "down", 0.3), leader({{"shift"}, "k"}, {{"shift"}, "9"}), wait(0.8),
              slowtype("hello from a macro"), key({}, "return", 0.8),
              leader({{"shift"}, "k"}, {{"shift"}, "0"}), wait(1.5),
              leader({{"shift"}, "k"}, "e"), wait(1.2), leader({{"shift"}, "k"}, "e"), wait(1)}},
  {title = "Shell command",
   body = "SPC ! runs a command: short output in the echo area,\nlong output in the help viewer (q closes it).",
   actions = {leader({{"shift"}, "1"}), wait(0.5), text("uname -sm", 1), key({}, "return", 3),
              leader({{"shift"}, "1"}), wait(0.5), text("ls -la /usr/bin | head -60", 1), key({}, "return", 4),
              key({}, "q", 1)}},
  {title = "Resume",
   body = "SPC r l reopens the last minibuffer session with its input,\nlike vertico-repeat.",
   actions = {leader("r", "l"), wait(3), key({}, "escape", 0.8)}},
  {title = "Link hints",
   body = "SPC j l labels only the links, like ace-link (Safari, example.com).",
   actions = {call(function() hs.execute("open -a Safari https://example.com") end, 3),
              leader("j", "l"), wait(3), key({}, "escape", 0.5), key({"cmd"}, "w", 0.5)}},
  {title = "Window numbers and the launcher",
   body = "SPC 1 focuses the leftmost window (winum). Everything is also in SPC SPC:\nI type \"macro\" to show the keyboard macro commands.",
   actions = {leader("1"), wait(1.5), leader("space"), wait(0.5), text("macro", 3.5), key({}, "escape", 0.5)}},
  {title = "That's the tour!",
   body = "Ctrl-Alt-N or Ctrl-Alt-Q cleans up: closes the demo document, removes the\ndemo bookmark, restores your shell and macro history.",
   actions = {}},
}

function DEMO.quit()
  for _, t in ipairs(DEMO.timers) do t:stop() end
  DEMO.running = nil
  if DEMO.next_key then DEMO.next_key:delete() end
  if DEMO.quit_key then DEMO.quit_key:delete() end
  caption:delete()
  hs.settings.set("bookmarks", backup.bookmarks or {})
  hs.settings.set("shell.history", backup.shell or {})
  if backup.kmacro then hs.settings.set("kmacro.last", backup.kmacro) else hs.settings.clear("kmacro.last") end
  local te = hs.application.get("TextEdit")
  if te and not textedit_was_running then te:kill9() end
  hs.execute("rm -rf " .. dir)
  require("lib.minibuffer").message("Demo over; cleaned up")
  DEMO.quit = nil
end

function DEMO.next()
  DEMO.chapter = DEMO.chapter + 1
  local c = DEMO.chapters[DEMO.chapter]
  if not c then return DEMO.quit() end
  for _, t in ipairs(DEMO.timers) do t:stop() end
  DEMO.timers = {}
  say(c.title, c.body)
  DEMO.running = c.actions
  run(c.actions)
end

DEMO.next_key = hs.hotkey.bind({"ctrl", "alt"}, "n", DEMO.next)
DEMO.quit_key = hs.hotkey.bind({"ctrl", "alt"}, "q", function() DEMO.quit() end)
DEMO.next()
