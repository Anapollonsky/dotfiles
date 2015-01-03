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
-- https://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration
-- https://github.com/nicolasavru/dotfiles/blob/master/xmonad/xmonad.hs

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified Data.List as L
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/aapollon/dotfiles/xmobarrc"
    xmonad $ def
        {
-------------------- basics
          terminal = "urxvtc"
        , modMask = mod4Mask -- windows as mod key
        , focusedBorderColor = "#AAAAFF"
        , normalBorderColor = "#555599"
        , borderWidth = 1
                        
-------------------- xmobar                               
        -- , manageHook = myManageHook <+> manageHook def
        , manageHook = manageDocks <+> manageHook def
        , layoutHook = myLayouts
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "#AAEE33" ""
                        , ppVisible = xmobarColor "#BBBBBB" ""
                        , ppTitle = xmobarColor "#AAEE33" "" . shorten 150
                        , ppLayout = xmobarColor "orange" ""
                        , ppSort = getSortByTag
                        , ppHidden = const ""
                        } 
                        
-------------------- other
        , workspaces = myWorkspaces
        , keys = \c -> myKeys c `M.union` keys def c
        }

-------------------- workspaces
myWorkspaces = withScreens 4 ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

-------------------- layouts
myLayouts = avoidStruts
            $ mkToggle (REFLECTX ?? REFLECTY ?? MIRROR ?? TABBED ?? FULL ?? EOT)
            $ Tall 1 (3/1000) (1/2)
            ||| renamed [Replace "ThreeColMid"] (ThreeColMid 1 (3/100) (1/2))
            ||| Grid

---------- TABBED layout
myTabConfig = defaultTheme {
  inactiveBorderColor = "#77AA11"
, activeBorderColor = "#AAEE33"
, activeColor = "#303030"
, inactiveColor = "#101010"
, decoHeight = 18
, fontName = "xft:DejaVu Sans Mono:size=9:bold:antialias=true"
}

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
  transform _ x k = k (tabbed shrinkText myTabConfig) (const x)

-------------------- keybindings
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList
  $     [ -- run stuff
          ((modm .|. shiftMask, xK_l), spawn "xscreensaver-command -lock") -- super+shift+l = lock
        , ((modm, xK_Print), spawn "sleep 0.2; scrot -s") -- super + printscreen = screenshot of window
        , ((0, xK_Print), spawn "scrot") -- printscreen = screenshot of everything. screenshot reqs "scrot"
        , ((modm, xK_a), spawn "emacs")
        , ((modm, xK_s), spawn "conkeror")
        , ((modm, xK_d), spawn "dmenu_run")
          -- directional navigation of windows
        , ((modm,                 xK_Right), windowGo R True)
        , ((modm,                 xK_Left ), windowGo L True)
        , ((modm,                 xK_Up   ), windowGo U True)
        , ((modm,                 xK_Down ), windowGo D True)
        , ((modm .|. shiftMask,   xK_Right), windowToScreen R True)
        , ((modm .|. shiftMask,   xK_Left ), windowToScreen L True)
        , ((modm .|. shiftMask,   xK_Up   ), windowToScreen U True)
        , ((modm .|. shiftMask,   xK_Down ), windowToScreen D True)          
          -- Multi-Toggle Modes
        , ((modm,               xK_n), sendMessage $ Toggle MIRROR)
        , ((modm,               xK_f), sendMessage $ Toggle TABBED)
        , ((modm .|. shiftMask, xK_f), sendMessage $ Toggle FULL)
        , ((modm,               xK_x), sendMessage $ Toggle REFLECTX)
        , ((modm,               xK_y), sendMessage $ Toggle REFLECTY)
        ]
          -- workspace control
      ++ [((m .|. modm, k), windows $ onCurrentScreen f i)
         | (i, k) <- zip (workspaces' conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0]
         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]
     ++ [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r, xK_t] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
        ]