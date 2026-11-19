"
Finder commands for finder-mode. Menu paths verified on macOS 26.7.3.

Commands select menu items like safari.fnl. A list of titles names the states
of an item Finder renames when toggled (Show Path Bar/Hide Path Bar), so those
commands toggle. Titles that embed the selection, such as
Copy “notes.txt” as Pathname, are matched by Lua pattern.
"

(local {: menu-item : menu-item-matching} (require :lib.modes))

(local menu-commands
       [;; General
        [:finder.back "Back" ["Go" "Back"]]
        [:finder.forward "Forward" ["Go" "Forward"]]
        [:finder.up "Enclosing folder" ["Go" "Enclosing Folder"]]
        [:finder.open "Open" ["File" "Open"]]
        [:finder.find "Find" ["File" "Find"]]
        [:finder.info "Get info" ["File" "Get Info"]]
        ;; Go
        [:finder.go.folder "Go to folder…" ["Go" "Go to Folder…"]]
        [:finder.go.home "Home" ["Go" "Home"]]
        [:finder.go.desktop "Desktop" ["Go" "Desktop"]]
        [:finder.go.documents "Documents" ["Go" "Documents"]]
        [:finder.go.downloads "Downloads" ["Go" "Downloads"]]
        [:finder.go.applications "Applications" ["Go" "Applications"]]
        [:finder.go.utilities "Utilities" ["Go" "Utilities"]]
        [:finder.go.library "Library" ["Go" "Library"]]
        [:finder.go.recents "Recents" ["Go" "Recents"]]
        [:finder.go.icloud "iCloud Drive" ["Go" "iCloud Drive"]]
        [:finder.go.computer "Computer" ["Go" "Computer"]]
        [:finder.go.network "Network" ["Go" "Network"]]
        [:finder.go.shared "Shared" ["Go" "Shared"]]
        [:finder.go.airdrop "AirDrop" ["Go" "AirDrop"]]
        [:finder.go.server "Connect to server…" ["Go" "Connect to Server…"]]
        ;; Files
        [:finder.file.new-folder "New folder" ["File" "New Folder"]]
        [:finder.file.rename "Rename" ["File" "Rename"]]
        [:finder.file.duplicate "Duplicate" ["File" "Duplicate"]]
        [:finder.file.alias "Make alias" ["File" "Make Alias"]]
        [:finder.file.original "Show original" ["File" "Show Original"]]
        [:finder.file.copy-link "Copy as link" ["Edit" "Copy as Link"]]
        [:finder.file.trash "Move to trash" ["File" "Move to Trash"]]
        [:finder.file.empty-trash "Empty trash…" ["Finder" "Empty Trash…"]]
        ;; Views
        [:finder.view.icons "As icons" ["View" "as Icons"]]
        [:finder.view.list "As list" ["View" "as List"]]
        [:finder.view.columns "As columns" ["View" "as Columns"]]
        [:finder.view.gallery "As gallery" ["View" "as Gallery"]]
        [:finder.view.groups "Use groups (toggle)" ["View" "Use Groups"]]
        [:finder.view.clean-up "Clean up" ["View" "Clean Up"]]
        [:finder.view.options "Show/hide view options" ["View" ["Show View Options" "Hide View Options"]]]
        [:finder.sort.none "Sort by none" ["View" "Sort By" "None"]]
        [:finder.sort.name "Sort by name" ["View" "Sort By" "Name"]]
        [:finder.sort.kind "Sort by kind" ["View" "Sort By" "Kind"]]
        [:finder.sort.opened "Sort by date last opened" ["View" "Sort By" "Date Last Opened"]]
        [:finder.sort.added "Sort by date added" ["View" "Sort By" "Date Added"]]
        [:finder.sort.modified "Sort by date modified" ["View" "Sort By" "Date Modified"]]
        [:finder.sort.created "Sort by date created" ["View" "Sort By" "Date Created"]]
        [:finder.sort.size "Sort by size" ["View" "Sort By" "Size"]]
        [:finder.sort.tags "Sort by tags" ["View" "Sort By" "Tags"]]
        ;; Bars
        [:finder.toolbar.show "Show/hide toolbar" ["View" ["Hide Toolbar" "Show Toolbar"]]]
        [:finder.toolbar.customize "Customize toolbar…" ["View" "Customize Toolbar…"]]
        [:finder.toolbar.tab-bar "Show/hide tab bar" ["View" ["Show Tab Bar" "Hide Tab Bar"]]]
        [:finder.toolbar.path-bar "Show/hide path bar" ["View" ["Show Path Bar" "Hide Path Bar"]]]
        [:finder.toolbar.status-bar "Show/hide status bar" ["View" ["Show Status Bar" "Hide Status Bar"]]]
        [:finder.sidebar.toggle "Show/hide sidebar" ["View" ["Hide Sidebar" "Show Sidebar"]]]
        [:finder.sidebar.preview "Show/hide preview" ["View" ["Show Preview" "Hide Preview"]]]
        ;; Tabs
        [:finder.tab.new "New tab" ["File" "New Tab"]]
        [:finder.tab.open "Open in new tab" ["File" "Open in New Tab"]]
        [:finder.tab.close "Close tab" ["File" ["Close Tab" "Close Window"]]]
        [:finder.tab.previous "Previous tab" ["Window" "Show Previous Tab"]]
        [:finder.tab.next "Next tab" ["Window" "Show Next Tab"]]
        [:finder.tab.move-to-window "Move tab to new window" ["Window" "Move Tab to New Window"]]
        [:finder.tab.overview "Show/hide all tabs" ["View" ["Show All Tabs" "Exit Tab Overview"]]]
        ;; Windows
        [:finder.window.new "New window" ["File" "New Finder Window"]]
        [:finder.window.close "Close window" ["File" "Close Window"]]
        [:finder.window.close-all "Close all windows" ["File" "Close All"]]
        [:finder.window.minimize "Minimize" ["Window" "Minimize"]]
        [:finder.window.zoom "Zoom" ["Window" "Zoom"]]
        [:finder.window.fill "Fill" ["Window" "Fill"]]
        [:finder.window.center "Center" ["Window" "Center"]]
        [:finder.window.merge "Merge all windows" ["Window" "Merge All Windows"]]
        [:finder.window.full-screen "Enter/exit full screen" ["View" ["Enter Full Screen" "Exit Full Screen"]]]])

(local matching-commands
       [[:finder.quick-look "Quick Look (toggle)" ["File"] "^[%w ]*Quick Look"]
        [:finder.file.new-folder-with "New folder with selection" ["File"] "^New Folder with "]
        [:finder.file.compress "Compress" ["File"] "^Compress “"]
        [:finder.file.copy-path "Copy as pathname" ["Edit"] "^Copy “.*” as Pathname$"]])

(local commands {})

(each [_ [id title path] (ipairs menu-commands)]
  (tset commands id {: title :fn (menu-item path)}))

(each [_ [id title parent pattern] (ipairs matching-commands)]
  (tset commands id {: title :fn (menu-item-matching parent pattern)}))

{: commands}
