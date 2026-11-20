-- Give every mpv a JSON IPC socket, so Hammerspoon's media keys (mpv.fnl) can
-- control it: $TMPDIR/mpv-<pid>.sock, unless --input-ipc-server already set one
-- (as EMMS does). Install as ~/.config/mpv/scripts/ipc-socket.lua.

local utils = require 'mp.utils'

if mp.get_property('input-ipc-server', '') == '' then
  local dir = (os.getenv('TMPDIR') or '/tmp'):gsub('/$', '')
  local socket = dir .. '/mpv-' .. utils.getpid() .. '.sock'
  mp.set_property('input-ipc-server', socket)
  mp.register_event('shutdown', function() os.remove(socket) end)
end
