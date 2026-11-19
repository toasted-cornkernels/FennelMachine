"
NetNewsWire commands for netnewswire-mode. Menu paths verified on NetNewsWire 6.

Commands select menu items like safari.fnl; a list of titles names the states
of an item NetNewsWire renames when toggled (Mark as Read/Mark as Unread).

The three panes have no menu items, so focus-pane sets AXFocused on the pane's
AppKit view, which moves real keyboard focus (checked with arrow keys). The
search never descends into lists or the article web view: the timeline can
hold thousands of rows, and walking them over AX stalls Hammerspoon.
"

(local {: menu-item : notify} (require :lib.modes))

(local opaque-roles {:AXOutline true :AXTable true :AXWebArea true :AXToolbar true})

(fn find-pane
  [element role depth]
  (var found nil)
  (each [_ child (ipairs (or (: element :attributeValue :AXChildren) []))
         &until found]
    (let [child-role (: child :attributeValue :AXRole)]
      (if (= child-role role)
          (set found child)
          (and (not (. opaque-roles child-role)) (< depth 12))
          (set found (find-pane child role (+ depth 1))))))
  found)

(fn focus-pane
  [role name]
  "Returns a command fn focusing the main window's pane with AXRole ROLE."
  (fn [app]
    (let [window (: app :mainWindow)
          pane (and window (find-pane (hs.axuielement.windowElement window) role 0))]
      (if pane
          (: pane :setAttributeValue :AXFocused true)
          (notify (.. "Pane not found: " name))))))

(local menu-commands
       [;; General
        [:netnewswire.refresh "Refresh" ["File" "Refresh"]]
        [:netnewswire.search "Article search…" ["Edit" "Find" "Article Search…"]]
        ;; Articles
        [:netnewswire.article.read "Mark as read/unread" ["Article" ["Mark as Read" "Mark as Unread"]]]
        [:netnewswire.article.star "Mark as starred/unstarred" ["Article" ["Mark as Starred" "Mark as Unstarred"]]]
        [:netnewswire.article.read-all "Mark all as read" ["Article" "Mark All as Read"]]
        [:netnewswire.article.read-above "Mark above as read" ["Article" "Mark Above as Read"]]
        [:netnewswire.article.read-below "Mark below as read" ["Article" "Mark Below as Read"]]
        [:netnewswire.article.reader "Reader view (toggle)" ["Article" "Show Reader View"]]
        [:netnewswire.article.browser "Open in browser" ["Article" "Open in Browser"]]
        [:netnewswire.article.background "Open in browser in background" ["Article" "Open in Browser in Background"]]
        [:netnewswire.article.copy-url "Copy article URL" ["Edit" "Copy Article URL"]]
        [:netnewswire.article.copy-external-url "Copy external URL" ["Edit" "Copy External URL"]]
        ;; Go
        [:netnewswire.go.next-unread "Next unread" ["Go" "Next Unread"]]
        [:netnewswire.go.today "Today" ["Go" "Today"]]
        [:netnewswire.go.unread "All unread" ["Go" "All Unread"]]
        [:netnewswire.go.starred "Starred" ["Go" "Starred"]]
        ;; View
        [:netnewswire.view.read-articles "Hide/show read articles" ["View" ["Hide Read Articles" "Show Read Articles"]]]
        [:netnewswire.view.read-feeds "Hide/show read feeds" ["View" ["Hide Read Feeds" "Show Read Feeds"]]]
        [:netnewswire.view.columns "Use column layout (toggle)" ["View" "Use Column Layout"]]
        [:netnewswire.view.clean-up "Clean up articles" ["View" "Clean Up Articles"]]
        [:netnewswire.sort.date "Sort by date" ["View" "Sort Articles By" "Date"]]
        [:netnewswire.sort.feed "Sort by feed" ["View" "Sort Articles By" "Feed"]]
        [:netnewswire.sort.title "Sort by title" ["View" "Sort Articles By" "Title"]]
        [:netnewswire.sort.unread "Sort by unread" ["View" "Sort Articles By" "Unread"]]
        [:netnewswire.sort.starred "Sort by starred" ["View" "Sort Articles By" "Starred"]]
        [:netnewswire.sort.oldest "Oldest article on top" ["View" "Sort Articles By" "Oldest Article on Top"]]
        [:netnewswire.sort.newest "Newest article on top" ["View" "Sort Articles By" "Newest Article on Top"]]
        [:netnewswire.toolbar.show "Hide/show toolbar" ["View" ["Hide Toolbar" "Show Toolbar"]]]
        [:netnewswire.toolbar.customize "Customize toolbar…" ["View" "Customize Toolbar…"]]
        [:netnewswire.sidebar.toggle "Hide/show sidebar" ["View" ["Hide Sidebar" "Show Sidebar"]]]
        ;; Subscriptions
        [:netnewswire.feed.new "New feed…" ["File" "New Feed…"]]
        [:netnewswire.feed.new-folder "New folder…" ["File" "New Folder…"]]
        [:netnewswire.feed.import "Import subscriptions…" ["File" "Import Subscriptions…"]]
        [:netnewswire.feed.export "Export subscriptions…" ["File" "Export Subscriptions…"]]
        ;; Windows
        [:netnewswire.window.new "New window" ["File" "New Window"]]
        [:netnewswire.window.close "Close window" ["File" "Close Window"]]
        [:netnewswire.window.main "Main window" ["Window" "Main Window"]]
        [:netnewswire.window.minimize "Minimize" ["Window" "Minimize"]]
        [:netnewswire.window.zoom "Zoom" ["Window" "Zoom"]]
        [:netnewswire.window.fill "Fill" ["Window" "Fill"]]
        [:netnewswire.window.center "Center" ["Window" "Center"]]
        [:netnewswire.window.full-screen "Enter/exit full screen" ["View" ["Enter Full Screen" "Exit Full Screen"]]]
        [:netnewswire.window.info "Info" ["Window" "Info"]]
        [:netnewswire.window.activity "Activity log" ["Window" "Activity Log"]]
        [:netnewswire.window.errors "Error log" ["Window" "Error Log"]]])

(local commands
       {:netnewswire.pane.feeds {:title "Focus feeds" :fn (focus-pane :AXOutline "feeds")}
        :netnewswire.pane.timeline {:title "Focus timeline" :fn (focus-pane :AXTable "timeline")}
        :netnewswire.pane.article {:title "Focus article" :fn (focus-pane :AXWebArea "article")}})

(each [_ [id title path] (ipairs menu-commands)]
  (tset commands id {: title :fn (menu-item path)}))

{: commands}
