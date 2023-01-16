ctrlPressed = false
keyPressed = false

hs.eventtap.new({hs.eventtap.event.types.flagsChanged, hs.eventtap.event.types.keyUp}, function(e)
    local flags = e:getFlags()
    local keyCode = e:getKeyCode()
    if flags.cmd and keyCode == 0x36 and not (flags.alt or flags.shift or flags.ctrl or flags.fn) then
      ctrlPressed = true
      keyPressed = false
    elseif ctrlPressed and not (flags.cmd or flags.alt or flags.shift or flags.ctrl or flags.fn) and not keyPressed then
      ctrlPressed = false
      if keyCode == 0x36 then
	local lay = hs.keycodes.currentLayout()
	if lay == "U.S." then
	  hs.keycodes.setLayout("Korean")
	else
	  hs.keycodes.setLayout("U.S.")
	end
      end
    else
      keyPressed = true
    end
end):start()
