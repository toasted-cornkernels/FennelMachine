hs.alert.show("(λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x)))))")

-- Support upcoming 5.4 release and also use luarocks' local path
package.path = package.path .. ";" .. os.getenv("HOME") .. "/.luarocks/share/lua/5.4/?.lua;" .. os.getenv("HOME") .. "/.luarocks/share/lua/5.4/?/init.lua"
package.cpath = package.cpath .. ";" .. os.getenv("HOME") .. "/.luarocks/lib/lua/5.4/?.so"
package.path = package.path .. ";" .. os.getenv("HOME") .. "/.luarocks/share/lua/5.3/?.lua;" .. os.getenv("HOME") .. "/.luarocks/share/lua/5.3/?/init.lua"
package.cpath = package.cpath .. ";" .. os.getenv("HOME") .. "/.luarocks/lib/lua/5.3/?.so"
package.path = package.path .. ";" .. os.getenv("HOME") .. "/.nix-profile/share/lua/5.3/?.lua;" .. os.getenv("HOME") .. "/.nix-profile/share/lua/5.3/?/init.lua"

fennel = require("fennel")
table.insert(package.loaders or package.searchers, fennel.searcher)

require "core"
