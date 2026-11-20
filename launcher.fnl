"
Launcher: open apps, files, settings and commands from the minibuffer.

Type to narrow, RET to run the selection. What you type can also be:

  2^10 / 3        a calculation (or =expr with math functions, e.g. =sqrt(2)),
                  RET copies the result
  g foo bar       a web search by keyword, see `web-searches`
  example.com     a URL to open
  'foo  open foo  file search with Spotlight (find foo reveals, in foo searches
                  contents)
  $ cmd           a shell command to run in a terminal

Every other input ends with fallback web searches.

  RET    run            s-RET  reveal in Finder     M-RET M-w  copy
  s-l    Large Type     s-y    Quick Look

The items you pick most often move to the top.
"
(local minibuffer (require :lib.minibuffer))
(local large-type (require :lib.large-type))
(local system-shortcuts (require :system-shortcuts))

(local home (os.getenv :HOME))

(local app-dirs ["/Applications"
                 "/System/Applications"
                 "/System/Library/CoreServices/Applications"
                 (.. home "/Applications")])

(local extra-apps ["/System/Library/CoreServices/Finder.app"])

(local web-searches
       [{:keyword :g :name :Google :fallback true
         :url "https://www.google.com/search?q=%s"}
        {:keyword :ddg :name :DuckDuckGo :fallback true
         :url "https://duckduckgo.com/?q=%s"}
        {:keyword :wiki :name :Wikipedia :fallback true
         :url "https://en.wikipedia.org/wiki/Special:Search?search=%s"}
        {:keyword :yt :name :YouTube
         :url "https://www.youtube.com/results?search_query=%s"}
        {:keyword :gh :name :GitHub :url "https://github.com/search?q=%s"}
        {:keyword :maps :name :Maps :url "maps://?q=%s"}
        {:keyword :nix :name "Nix packages"
         :url "https://search.nixos.org/packages?query=%s"}
        {:keyword :amazon :name :Amazon :url "https://www.amazon.com/s?k=%s"}
        {:keyword :imdb :name :IMDb :url "https://www.imdb.com/find?q=%s"}
        {:keyword :translate :name "Google Translate"
         :url "https://translate.google.com/?sl=auto&tl=en&text=%s"}
        {:keyword :define :name :Dictionary :url "dict://%s"}])

(local terminal "/Applications/Ghostty.app")
(local file-limit 200)
(local file-rows 50)
(local usage-key "launcher.usage")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn open [...]
  (: (hs.task.new "/usr/bin/open" nil [...]) :start))

(fn abbreviate [path]
  (if (= (path:sub 1 (length home)) home)
      (.. "~" (path:sub (+ (length home) 1)))
      path))

(fn dirname [path] (or (path:match "^(.*)/[^/]+/?$") "/"))

(fn basename [path] (or (path:match "([^/]+)/?$") path))

(fn trim [s] (s:match "^%s*(.-)%s*$"))

(fn copy [text]
  (hs.pasteboard.setContents text)
  (minibuffer.message (.. "Copied " text)))

(fn confirm [prompt f]
  (fn [] (minibuffer.y-or-n-p prompt f)))

(fn applescript [source]
  (fn []
    (let [(ok _ err) (hs.osascript.applescript source)]
      (when (not ok)
        (minibuffer.message (.. "Failed: " (hs.inspect err)))))))

(fn system-key [key]
  (fn []
    (each [_ down? (ipairs [true false])]
      (: (hs.eventtap.event.newSystemKeyEvent key down?) :post))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn scan-apps []
  "Apps in `app-dirs` and their subfolders, e.g. Utilities and Nix Apps."
  (let [acc []
        seen {}]
    (fn add [path]
      (let [real (or (hs.fs.pathToAbsolute path) path)]
        (when (not (. seen real))
          (tset seen real true)
          (table.insert acc {:text (: (basename path) :gsub "%.app$" "")
                             :annotation (abbreviate (dirname path))
                             :kind :app
                             : path
                             :id (.. "app:" real)}))))
    (fn scan [dir depth]
      (let [(ok iter state) (pcall hs.fs.dir dir)]
        (when (and ok iter)
          (each [name (values iter state)]
            (when (not (name:match "^%."))
              (let [path (.. dir "/" name)]
                (if (name:match "%.app$") (add path)
                    (and (< depth 2)
                         (= (hs.fs.attributes path :mode) :directory))
                    (scan path (+ depth 1)))))))))
    (each [_ dir (ipairs app-dirs)] (scan dir 0))
    (each [_ path (ipairs extra-apps)] (add path))
    acc))

(var panes-cache nil)

;; Panes whose extension has no usable name, or that should not be listed.
(local pane-names {"com.apple.Battery-Settings.extension" :Battery
                   "com.apple.HeadphoneSettings" :Headphones
                   "com.apple.settings.Storage" :Storage
                   "com.apple.SecurityImprovements-Settings.extension" false
                   "com.apple.FollowUpSettings.FollowUpSettingsExtension" false})

(fn settings-panes []
  "The panes of System Settings, including General's."
  (when (not panes-cache)
    (let [ext-dir "/System/Library/ExtensionKit/Extensions/"
          panes {"com.apple.systempreferences.GeneralSettings" :General}]
      (each [name (hs.fs.dir ext-dir)]
        (when (name:match "%.appex$")
          (let [contents (.. ext-dir name "/Contents/")
                info (or (hs.plist.read (.. contents "Info.plist")) {})
                id info.CFBundleIdentifier
                point (?. info :EXAppExtensionAttributes :EXExtensionPointIdentifier)]
            (when (and id (= point "com.apple.Settings.extension.ui"))
              (let [loc (hs.plist.read (.. contents "Resources/InfoPlist.loctable"))
                    en (or (?. loc :en) {})
                    override (. pane-names id)]
                (tset panes id (if (not= override nil) override
                                   (or en.CFBundleDisplayName en.CFBundleName
                                       info.CFBundleDisplayName))))))))
      (set panes-cache
           (icollect [id name (pairs panes)]
             (when name
               {:text name
                :annotation "System Settings"
                :keywords "settings preferences"
                :kind :url
                :url (.. "x-apple.systempreferences:" id)
                :id (.. "pane:" id)})))
      (table.sort panes-cache #(< $1.text $2.text))))
  panes-cache)

(fn command [text f keywords]
  {: text :annotation :System :kind :command :run f : keywords
   :id (.. "command:" text)})

(fn system-commands []
  [(command "Lock Screen" hs.caffeinate.lockScreen)
   (command "Sleep" hs.caffeinate.systemSleep)
   (command "Sleep Displays" #(hs.execute "pmset displaysleepnow"))
   (command "Start Screen Saver" hs.caffeinate.startScreensaver)
   (command "Restart…" (confirm "Restart?" hs.caffeinate.restartSystem) "reboot")
   (command "Shut Down…" (confirm "Shut down?" hs.caffeinate.shutdownSystem)
            "shutdown power off")
   (command "Log Out…" (confirm "Log out?" hs.caffeinate.logOut) "logout sign")
   (command "Open Trash" #(open (.. home "/.Trash")))
   (command "Empty Trash…"
            (confirm "Empty the Trash?"
                     (applescript "tell application \"Finder\" to empty trash")))
   (command "Eject All Disks"
            (applescript "tell application \"Finder\" to eject (every disk whose ejectable is true)")
            "unmount")
   (command "Toggle Dark Mode"
            (applescript "tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode")
            "appearance light")
   (command "Keep Awake"
            #(minibuffer.message
              (if (hs.caffeinate.toggle :displayIdle)
                  "Keeping the Mac awake"
                  "The Mac may sleep again"))
            "caffeinate prevent sleep")
   (command "Play/Pause" (system-key :PLAY) "music media")
   (command "Next Track" (system-key :NEXT) "music media")
   (command "Previous Track" (system-key :PREVIOUS) "music media")
   (command "Mute" (system-key :MUTE) "volume sound")
   (command "Volume Up" (system-key :SOUND_UP) "sound")
   (command "Volume Down" (system-key :SOUND_DOWN) "sound")
   (command "Clipboard History" #((. (require :clipboard) :show)) "paste")
   (command (if ((. (require :clipboard) :paused?))
                "Resume Clipboard History"
                "Pause Clipboard History")
            #((. (require :clipboard) :toggle-pause))
            "clipboard record incognito private")
   (command "Clear Clipboard History…"
            (confirm "Clear the clipboard history?"
                     #((. (require :clipboard) :clear))))])

(fn volume-commands []
  (let [acc []]
    (each [path info (pairs (or (hs.fs.volume.allVolumes) {}))]
      (when (or info.NSURLVolumeIsEjectableKey info.NSURLVolumeIsRemovableKey)
        (table.insert acc
                      (command (.. "Eject " (or info.NSURLVolumeLocalizedNameKey
                                                (basename path)))
                               #(let [(ok err) (hs.fs.volume.eject path)]
                                  (minibuffer.message
                                   (if ok (.. "Ejected " path)
                                       (.. "Could not eject " path ": "
                                           (tostring err)))))
                               "unmount"))))
    acc))

(fn running-app-commands []
  (let [acc []]
    (each [_ app (ipairs (hs.application.runningApplications))]
      (when (= (: app :kind) 1)
        (let [name (or (: app :name) "?")]
          (each [_ [verb f] (ipairs [[:Quit #(: app :kill)]
                                     [:Hide #(: app :hide)]
                                     ["Force Quit" #(: app :kill9)]])]
            (table.insert acc {:text (.. verb " " name)
                               :annotation "Running"
                               :kind :command
                               :run f
                               :id (.. "running:" verb ":" name)})))))
    acc))

(fn shortcut-commands []
  (let [(ok cmds) (pcall system-shortcuts.commands)]
    (icollect [_ c (ipairs (if ok cmds []))]
      {:text c.name
       :annotation c.category
       :kind :command
       :run #(system-shortcuts.run c.id)
       :id (.. "shortcut:" c.id)})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn usage [] (or (hs.settings.get usage-key) {}))

(fn record-use [candidate]
  (when candidate.id
    (let [counts (usage)]
      (tset counts candidate.id (+ 1 (or (. counts candidate.id) 0)))
      (hs.settings.set usage-key counts))))

(fn by-usage [candidates]
  "CANDIDATES, most used first, otherwise in their original order."
  (let [counts (usage)
        indexed (icollect [i c (ipairs candidates)]
                  {: c : i :n (or (. counts (or c.id "")) 0)})]
    (table.sort indexed #(if (= $1.n $2.n) (< $1.i $2.i) (> $1.n $2.n)))
    (icollect [_ x (ipairs indexed)] x.c)))

(fn collection []
  (let [acc []]
    (each [_ source (ipairs [scan-apps settings-panes system-commands
                             volume-commands shortcut-commands
                             running-app-commands])]
      (let [(ok items) (pcall source)]
        (if ok
            (each [_ c (ipairs items)] (table.insert acc c))
            (print (.. "launcher: " (tostring items))))))
    (by-usage acc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Candidates computed from the input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local calc-env (setmetatable {:e (math.exp 1) :ln math.log}
                              {:__index math}))

(fn calculate [input]
  "The value of INPUT as arithmetic, or as any math expression after =."
  (let [explicit (input:match "^=%s*(.+)$")
        expr (or explicit input)]
    (when (and (or explicit
                   (and (expr:match "%d") (expr:match "[%+%-%*/%%%^]")))
               (not (expr:match "[^%w%s%.%+%-%*/%%%^%(%),_]"))
               (accumulate [ok true id (expr:gmatch "%a[%w_]*")]
                 (and ok (or (= (type (. calc-env id)) :number)
                             (= (type (. calc-env id)) :function)))))
      (let [f (load (.. "return " expr) :calc :t calc-env)
            (ok value) (when f (pcall f))]
        (when (and ok (= (type value) :number) (= value value))
          (if (and (= (math.type value) :float)
                   (= value (math.floor value))
                   (< (math.abs value) 1e15))
              (string.format "%d" value)
              (= (math.type value) :integer) (tostring value)
              (string.format "%.12g" value)))))))

(fn url-of [input]
  (if (input:match "^%a[%w+.-]*://%S+$") input
      (let [host (or (input:match "^([%w.-]+)$") (input:match "^([%w.-]+)[:/]%S*$"))]
        (and host (host:match "^[%w-]+%.[%w.-]*%a%a$")))
      (.. "https://" input)))

(fn search-candidate [search query]
  (let [encoded (: (hs.http.encodeForQuery query) :gsub "%%" "%%%%")
        url (search.url:gsub "%%s" encoded)]
    {:text (.. "Search " search.name " for " query)
     :annotation search.keyword
     :kind :url
     : url}))

;; Spotlight file search, run in the background as the input changes.
(local files {:query nil :results [] :done false :task nil :timer nil})

(fn file-query [terms content?]
  (if content?
      terms
      (table.concat
       (icollect [term (terms:gmatch "%S+")]
         (.. "kMDItemFSName == \"*" (term:gsub "[\"\\*]" "\\%0") "*\"cd"))
       " && ")))

(fn file-rank [path]
  "Lower is better: files in the home folder first, then shorter paths."
  (+ (if (= (path:sub 1 (+ (length home) 1)) (.. home "/")) 0 1000)
     (if (path:match "/Library/") 500 0)
     (length path)))

(fn finish-file-search [key paths]
  (when (= files.query key)
    (table.sort paths #(< (file-rank $1) (file-rank $2)))
    (set files.results
         (icollect [i path (ipairs paths)]
           (when (<= i file-rows)
             {:text (basename path)
              :annotation (abbreviate (dirname path))
              :kind :file
              : path})))
    (set files.done true)
    (set files.task nil)
    (minibuffer.refresh)))

(fn start-file-search [key query content?]
  (let [paths []
        seen {}
        buffer {:rest ""}]
    (fn add-lines [data]
      (let [text (.. buffer.rest (or data ""))]
        (set buffer.rest (or (text:match "[^\n]*$") ""))
        (each [line (text:gmatch "([^\n]*)\n")]
          (when (and (not= line "") (not (. seen line)) (not (line:match "/%."))
                     (< (length paths) file-limit))
            (tset seen line true)
            (table.insert paths line)))))
    (set files.task
         (hs.task.new "/usr/bin/mdfind"
                      (fn [_ stdout]
                        (add-lines stdout)
                        (finish-file-search key paths))
                      (fn [task stdout]
                        (add-lines stdout)
                        (when (>= (length paths) file-limit)
                          (: task :terminate))
                        true)
                      [(file-query query content?)]))
    (: files.task :start)))

(fn file-search [query content?]
  "The file search results for QUERY so far, starting the search if needed."
  (let [key (.. (if content? "in:" "name:") query)]
    (when (not= files.query key)
      (when files.task (: files.task :terminate) (set files.task nil))
      (when files.timer (: files.timer :stop))
      (set files.query key)
      (set files.results [])
      (set files.done false)
      (set files.timer (hs.timer.doAfter 0.15 #(when (= files.query key)
                                                 (start-file-search key query content?)))))
    (if (and files.done (= (length files.results) 0))
        [{:text (.. "No files match " query) :kind :none}]
        (not files.done)
        (doto (icollect [_ c (ipairs files.results)] c)
          (table.insert {:text "Searching…" :kind :none}))
        files.results)))

(fn dynamic [input]
  (let [input (trim input)
        (keyword query) (input:match "^(%S+)%s+(.+)$")
        search (when keyword
                 (accumulate [found nil _ s (ipairs web-searches)]
                   (or found (when (= s.keyword keyword) s))))
        file-mode (or (when (input:match "^'") [(trim (input:sub 2)) :open])
                      (when (= keyword :open) [query :open])
                      (when (= keyword :find) [query :find])
                      (when (= keyword :in) [query :in]))]
    (if (= input "") {}
        (input:match "^%$")
        (let [cmd (trim (input:sub 2))]
          {:only [(if (= cmd "")
                      {:text "Type a shell command to run in a terminal" :kind :none}
                      {:text (.. "Run " cmd) :annotation "Terminal" :kind :shell
                       :command cmd})]})
        file-mode
        (let [[q mode] file-mode]
          {:only (if (= q "")
                     [{:text "Type a file name" :kind :none}]
                     (icollect [_ c (ipairs (file-search q (= mode :in)))]
                       (if (and (= c.kind :file) (= mode :find))
                           {:text c.text :annotation c.annotation :kind :reveal
                            :path c.path}
                           c)))})
        (let [before []
              result (calculate input)
              url (url-of input)]
          (when result
            (table.insert before {:text (.. "= " result) :annotation "Copy"
                                  :kind :copy :value result}))
          (when search
            (table.insert before (search-candidate search query)))
          (when url
            (table.insert before {:text (.. "Open " url) :annotation "URL"
                                  :kind :url : url}))
          {: before
           :after (if search []
                      (icollect [_ s (ipairs web-searches)]
                        (when s.fallback (search-candidate s input))))}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn run-in-terminal [cmd]
  (let [shell (or (os.getenv :SHELL) "/bin/zsh")
        script (.. cmd "; exec " shell " -l")]
    (if (hs.fs.attributes terminal)
        (open "-na" terminal "--args" "-e" shell "-lc" script)
        (hs.osascript.applescript
         (.. "tell application \"Terminal\"\nactivate\ndo script "
             (hs.inspect script) "\nend tell")))))

(fn value-of [candidate]
  "What copying or Large Type use for CANDIDATE."
  (or candidate.value candidate.path candidate.url candidate.command
      candidate.text))

(fn run [candidate]
  (let [kind candidate.kind]
    (if (= kind :app) (open candidate.path)
        (= kind :file) (open candidate.path)
        (= kind :reveal) (open "-R" candidate.path)
        (= kind :url) (open candidate.url)
        (= kind :command) (candidate.run)
        (= kind :copy) (copy candidate.value)
        (= kind :shell) (run-in-terminal candidate.command))))

(fn accept [candidate input flags]
  (when (and candidate (not= candidate.kind :none))
    (record-use candidate)
    (if (and flags flags.cmd candidate.path) (open "-R" candidate.path)
        (and flags flags.alt) (copy (value-of candidate))
        (run candidate))))

(fn quick-look [candidate]
  (when (and candidate candidate.path)
    (let [task (hs.task.new "/usr/bin/qlmanage" nil ["-p" candidate.path])]
      (: task :start)
      ;; The preview opens in the background; bring it forward so ESC or
      ;; Space close it.
      (hs.timer.doAfter 0.5
                        #(let [app (hs.application.applicationForPID (: task :pid))]
                           (when app (: app :activate true)))))))

(local actions
       {"M-w" #(when $1 (copy (value-of $1)))
        "s-l" #(large-type.show (if (and $1 (not= $1.kind :none)) (value-of $1) $2))
        "s-y" quick-look})

(fn show []
  "Open the launcher in the minibuffer."
  (minibuffer.completing-read "Run: " nil
                              {:on-accept accept
                               : dynamic
                               : actions})
  (hs.timer.doAfter 0.01
                    #(when (minibuffer.active?)
                       (minibuffer.set-collection (collection)))))

{: show
 : system-commands
 : collection
 : calculate
 : url-of
 : dynamic}
