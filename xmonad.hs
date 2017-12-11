{-# LANGUAGE
     DeriveDataTypeable,
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     PatternGuards,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances
     #-}
{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-}

-- Andrew Apollonsky xmonad configuration
-- Sources:
-- https://github.com/nicolasavru/dotfiles/blob/master/xmonad/xmonad.hs

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle -- requires DeriveDataTypeable
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.UpdatePointer
import XMonad.Util.WorkspaceCompare

import XMonad.Hooks.EwmhDesktops        (ewmh)

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

--import System.Taffybar.Hooks.PagerHints (pagerHints)


main = do

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad $ withUrgencyHook NoUrgencyHook
           $ ewmh
           $ docks
           $ def
        {
-------------------- basics
          terminal = "termite"
        , modMask = mod4Mask -- windows as mod key
        , focusedBorderColor = "#AAAAFF"
        , normalBorderColor = "#222255"
        , borderWidth = 1

-------------------- dzen
        , layoutHook = avoidStruts myLayouts
        , logHook =  dynamicLogWithPP (myLogHook dbus) >> updatePointer (0.5, 0.5) (0, 0)
        , startupHook = setWMName "LG3D"
-------------------- other
        , workspaces = myWorkspaces
        , keys = \c -> myKeys c `M.union` keys def c
        }


-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
myLogHook :: D.Client -> PP
myLogHook dbus = def { ppOutput = dbusOutput dbus }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

--  myLogHook :: D.Client -> PP
-- myLogHook dbus = def
--     { ppOutput = dbusOutput dbus
--     , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
--     , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
--     , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
--     , ppHidden = wrap " " " "
--     , ppWsSep = ""
--     , ppSep = " : "
--     , ppTitle = shorten 40
--     }
 
--  workspaceNamesPP dzenPP
--                         {ppCurrent = dzenColor "#AAEE33" "" . pad
--                         , ppVisible = dzenColor "#BBBBBB" "" . pad
--                         , ppTitle = dzenColor "#AAEE33" "" . shorten 80
--                         , ppLayout = dzenColor "orange" "" . pad
--                         , ppSort = getSortByTag
--                         , ppHidden = dzenColor "#558855" "" . pad
--                         , ppHiddenNoWindows = const ""
--                         , ppUrgent = dzenColor "yellow" "red" . pad . dzenStrip -- urgency hook
--                         }  
-------------------- workspaces
myWorkspaces = withScreens 3 ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

-------------------- layouts
myLayouts = mkToggle (REFLECTX ?? REFLECTY ?? MIRROR ?? TABBED ?? FULL ?? EOT)
            $ Tall 1 (3/1000) (1/2)
            ||| renamed [Replace "ThreeColMid"] (ThreeColMid 1 (3/100) (1/2))
            ||| Grid


---------- TABBED layout
myTabConfig = def {
  inactiveBorderColor = "#77AA11"
, activeBorderColor = "#AAEE33"
, activeColor = "#303030"
, inactiveColor = "#101010"
, decoHeight = 20
, fontName = "xft:DejaVu Sans Mono:size=11:antialias=true"
}

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
  transform _ x k = k (tabbed shrinkText myTabConfig) (const x)

-------------------- keybindings
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $ [ -- run stuff
          ((modm .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock") -- super+shift+l = lock
        , ((modm, xK_Print), spawn "sleep 0.2; scrot -s -e 'mv $f ~/screenshots/'") -- super + printscreen = screenshot of window
        , ((0, xK_Print), spawn "scrot -e 'mv $f ~/screenshots/'") -- printscreen = screenshot of everything. screenshot reqs "scrot"
        , ((modm, xK_a), runOrRaise "emacs" (className =? "Emacs")) -- Go to window if it exists, or open new one.
        , ((modm .|. shiftMask, xK_a), spawn "emacsclient -c") -- emacsclient
        , ((modm, xK_s), runOrRaise "google-chrome-stable" (className =? "Firefox" <||> className =? "Firefox-bin" <||> className =? "Navigator"))
        , ((modm .|. shiftMask, xK_s), spawn "google-chrome-stable")
        , ((modm, xK_d), spawn "rofi -show run")
        , ((modm .|. shiftMask, xK_d), spawn "dmenu_run")
        -- , ((modm .|. shiftMask, xK_r), renameWorkspace defaultXPConfig) -- Rename a workspace

         -- Bring/Goto open windows
        , ((modm, xK_g), gotoMenu' "rofi_dmenu")
        , ((modm .|. shiftMask, xK_g), bringMenu' "rofi_dmenu")

          -- directional navigation of windows
        , ((modm,                 xK_Right), windowGo R True)
        , ((modm,                 xK_Left ), windowGo L True)
        , ((modm,                 xK_Up   ), windowGo U True)
        , ((modm,                 xK_Down ), windowGo D True)
        , ((modm .|. controlMask, xK_l),     windowGo R True)
        , ((modm .|. controlMask, xK_h),     windowGo L True)
        , ((modm .|. controlMask, xK_k),     windowGo U True)
        , ((modm .|. controlMask, xK_j),     windowGo D True)
        , ((modm .|. shiftMask,   xK_Right), windowToScreen R True)
        , ((modm .|. shiftMask,   xK_Left ), windowToScreen L True)
        , ((modm .|. shiftMask,   xK_Up   ), windowToScreen U True)
        , ((modm .|. shiftMask,   xK_Down ), windowToScreen D True)

          -- Multi-Toggle Modes
        , ((modm,                 xK_n), sendMessage $ Toggle MIRROR)
        , ((modm,                 xK_f), sendMessage $ Toggle TABBED)
        , ((modm .|. shiftMask,   xK_f), sendMessage $ Toggle FULL)
        , ((modm,                 xK_x), sendMessage $ Toggle REFLECTX)
        , ((modm,                 xK_y), sendMessage $ Toggle REFLECTY)
        ]


      ++ [((m .|. modm, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]
     ++ [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
        ]
