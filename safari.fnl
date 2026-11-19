"
Safari commands for safari-mode. Menu paths verified on Safari 27.2.

Most commands select a menu item. A list of titles names the states of an item
that Safari renames when toggled (Show Sidebar/Hide Sidebar), so those commands
toggle, as do checkbox items such as Always Show Toolbar in Full Screen.
"

(local {: menu-item} (require :lib.modes))

(local close-tab (menu-item ["File" "Close Tab"]))
(local close-window (menu-item ["File" "Close Window"]))

(fn close-tab-or-window
  [app]
  "Close the current tab like Cmd-W; Safari disables Close Tab on the last tab."
  (if (?. (: app :findMenuItem ["File" "Close Tab"]) :enabled)
      (close-tab app)
      (close-window app)))

(local menu-commands
       [;; General
        [:safari.focus-address "Focus address/search field" ["File" "Open Location…"]]
        [:safari.find "Find in page" ["Edit" "Find" "Find…"]]
        [:safari.back "Back" ["History" "Back"]]
        [:safari.forward "Forward" ["History" "Forward"]]
        ;; Tabs
        [:safari.tab.new "New tab" ["File" "New Tab"]]
        [:safari.tab.new-at-end "New tab at end" ["File" "New Tab at End"]]
        [:safari.tab.close-others "Close other tabs" ["File" "Close Other Tabs"]]
        [:safari.tab.duplicate "Duplicate tab" ["Window" "Duplicate Tab"]]
        [:safari.tab.pin "Pin/unpin tab" ["Window" ["Pin Tab" "Unpin Tab"]]]
        [:safari.tab.mute "Mute/unmute tab" ["Window" ["Mute This Tab" "Unmute This Tab"]]]
        [:safari.tab.move-to-window "Move tab to new window" ["Window" "Move Tab to New Window"]]
        [:safari.tab.previous "Previous tab" ["Window" "Show Previous Tab"]]
        [:safari.tab.next "Next tab" ["Window" "Show Next Tab"]]
        [:safari.tab.overview "Show/hide tab overview" ["View" ["Show Tab Overview" "Hide Tab Overview"]]]
        [:safari.tab.arrange.title "Arrange tabs by title" ["Window" "Arrange Tabs By" "Title"]]
        [:safari.tab.arrange.website "Arrange tabs by website" ["Window" "Arrange Tabs By" "Website"]]
        ;; Windows
        [:safari.window.new "New window" ["File" "New Window"]]
        [:safari.window.new-private "New private window" ["File" "New Private Window"]]
        [:safari.window.close "Close window" ["File" "Close Window"]]
        [:safari.window.minimize "Minimize" ["Window" "Minimize"]]
        [:safari.window.zoom "Zoom" ["Window" "Zoom"]]
        [:safari.window.fill "Fill" ["Window" "Fill"]]
        [:safari.window.center "Center" ["Window" "Center"]]
        [:safari.window.merge "Merge all windows" ["Window" "Merge All Windows"]]
        [:safari.window.full-screen "Enter/exit full screen" ["View" ["Enter Full Screen" "Exit Full Screen"]]]
        ;; Toolbar and bars
        [:safari.toolbar.show "Always Show Toolbar in Full Screen" ["View" "Always Show Toolbar in Full Screen"]]
        [:safari.toolbar.customize "Customize Toolbar…" ["View" "Customize Toolbar…"]]
        [:safari.toolbar.tab-bar "Always Show Tab Bar" ["View" "Always Show Tab Bar"]]
        [:safari.toolbar.favorites-bar "Show/hide favorites bar" ["View" ["Show Favorites Bar" "Hide Favorites Bar"]]]
        [:safari.toolbar.status-bar "Show/hide status bar" ["View" ["Show Status Bar" "Hide Status Bar"]]]
        ;; Sidebar
        [:safari.sidebar.toggle "Show/hide sidebar" ["View" ["Show Sidebar" "Hide Sidebar"]]]
        [:safari.sidebar.bookmarks "Bookmarks sidebar" ["View" ["Show Bookmarks Sidebar" "Hide Bookmarks Sidebar"]]]
        [:safari.sidebar.reading-list "Reading list sidebar" ["View" ["Show Reading List Sidebar" "Hide Reading List Sidebar"]]]
        [:safari.sidebar.shared "Shared with You" ["View" ["Show Shared with You" "Hide Shared with You"]]]
        ;; Page view
        [:safari.view.reader "Show/hide Reader" ["View" ["Show Reader" "Hide Reader"]]]
        [:safari.view.downloads "Show downloads" ["View" "Show Downloads"]]
        [:safari.zoom.in "Zoom in" ["View" "Zoom In"]]
        [:safari.zoom.out "Zoom out" ["View" "Zoom Out"]]
        [:safari.zoom.reset "Actual size" ["View" "Actual Size"]]
        [:safari.zoom.text-bigger "Make text bigger" ["View" "Make Text Bigger"]]
        [:safari.zoom.text-smaller "Make text smaller" ["View" "Make Text Smaller"]]
        ;; Reload
        [:safari.reload.page "Reload page" ["View" "Reload Page"]]
        [:safari.reload.from-origin "Reload page from origin" ["View" "Reload Page From Origin"]]
        [:safari.reload.content-blockers "Reload without/with content blockers"
         ["View" ["Reload Without Content Blockers" "Reload With Content Blockers"]]]
        [:safari.reload.stop "Stop loading" ["View" "Stop"]]
        ;; History
        [:safari.history.show "Show/hide history" ["History" ["Show All History" "Hide History"]]]
        [:safari.history.home "Home" ["History" "Home"]]
        [:safari.history.search-results "Return to search results" ["History" "Return to Search Results"]]
        [:safari.history.reopen "Reopen last closed tab/window"
         ["History" ["Reopen Last Closed Tab" "Reopen Last Closed Window"]]]
        [:safari.history.reopen-session "Reopen all windows from last session"
         ["History" "Reopen All Windows from Last Session"]]
        [:safari.history.clear "Clear history…" ["History" "Clear History…"]]
        ;; Bookmarks
        [:safari.bookmark.add "Add bookmark…" ["Bookmarks" "Add Bookmark…"]]
        [:safari.bookmark.reading-list "Add to reading list" ["Bookmarks" "Add to Reading List"]]
        [:safari.bookmark.edit "Edit bookmarks" ["Bookmarks" "Edit Bookmarks"]]
        [:safari.bookmark.show "Show/hide bookmarks" ["Bookmarks" ["Show Bookmarks" "Hide Bookmarks"]]]
        [:safari.bookmark.start-page "Show start page" ["Bookmarks" "Show Start Page"]]
        ;; Develop (Settings → Advanced → Show features for web developers)
        [:safari.develop.inspector "Web inspector" ["Develop" ["Show Web Inspector" "Close Web Inspector"]]]
        [:safari.develop.console "Show JavaScript console" ["Develop" "Show JavaScript Console"]]
        [:safari.develop.source "Show page source" ["Develop" "Show Page Source"]]
        [:safari.develop.element "Start element selection" ["Develop" "Start Element Selection"]]
        [:safari.develop.responsive "Responsive design mode"
         ["Develop" ["Enter Responsive Design Mode" "Exit Responsive Design Mode"]]]
        [:safari.develop.empty-caches "Empty caches" ["Develop" "Empty Caches"]]])

(local commands
       {:safari.tab.close {:title "Close tab (window if last tab)"
                           :fn close-tab-or-window}})

(each [_ [id title path] (ipairs menu-commands)]
  (tset commands id {: title :fn (menu-item path)}))

{: commands}
