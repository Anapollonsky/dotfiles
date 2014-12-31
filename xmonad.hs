-- Andrew Apollonsky xmonad configuration
-- Source for most of this:
-- https://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/aapollon/.dotfiles/xmobarrc"
    xmonad $ defaultConfig
        { terminal = "urxvt"
        , modMask = mod4Mask -- windows as mod key
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#A0E030" "" . shorten 50
                        }
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock") -- super+shift+l = lock
        , ((mod4Mask, xK_Print), spawn "sleep 0.2; scrot -s") -- super + printscreen = screenshot of window
        , ((0, xK_Print), spawn "scrot") -- printscreen = screenshot of everything. screenshot reqs "scrot"
        ]
