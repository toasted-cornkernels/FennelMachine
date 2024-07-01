(fn toggle-dark-mode []
  (hs.execute "osascript -e 'tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"))

{: toggle-dark-mode}
