-- Andrew Apollonsky xmonad configuration
-- Source for most of this:
-- https://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration
import qualified Data.Map as M
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Util.Run
import System.IO

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/aapollon/.dotfiles/xmobarrc"
    xmonad $ defaultConfig
        {
-------------------- basics
          terminal = "urxvtc"
        , modMask = mod4Mask -- windows as mod key
        , focusedBorderColor = "#AAAAFF"
        , normalBorderColor = "#555599"
        , borderWidth = 1
-------------------- xmobar                               
        , manageHook = manageDocks <+> manageHook defaultConfig
--        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , layoutHook = myLayouts
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#A0E030" "" . shorten 50
                        , ppHiddenNoWindows = xmobarColor "grey" ""
                        }
-------------------- other
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        }

-------------------- layouts
myLayouts = avoidStruts
            $ mkToggle (REFLECTX ?? REFLECTY ?? MIRROR ?? FULL ?? EOT)
            $ Tall 1 (3/1000) (1/2)
--            ||| ThreeCol 1 (3/100) (1/2)
            ||| renamed [Replace "ThreeColMid"] (ThreeColMid 1 (3/100) (1/2))
            ||| Grid
            ||| spiral (6/7)

-------------------- keybindings
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList
  $     [ ((modm .|. shiftMask, xK_l), spawn "xscreensaver-command -lock") -- super+shift+l = lock
        , ((modm, xK_Print), spawn "sleep 0.2; scrot -s") -- super + printscreen = screenshot of window
        , ((0, xK_Print), spawn "scrot") -- printscreen = screenshot of everything. screenshot reqs "scrot"
        , ((modm, xK_a), spawn "emacs")
        , ((modm, xK_s), spawn "conkeror")
        , ((modm, xK_d), spawn "dmenu_run")
          -- direction navigation of windows
        , ((modm,                 xK_Right), windowGo R True)
        , ((modm,                 xK_Left ), windowGo L True)
        , ((modm,                 xK_Up   ), windowGo U True)
        , ((modm,                 xK_Down ), windowGo D True)
        , ((modm,               xK_m), sendMessage $ Toggle MIRROR)
        --, ((modm,               xK_f), sendMessage $ Toggle TABBED)
        , ((modm,               xK_f), sendMessage $ Toggle FULL)
        , ((modm,               xK_x), sendMessage $ Toggle REFLECTX)
        , ((modm,               xK_y), sendMessage $ Toggle REFLECTY)
        ]
