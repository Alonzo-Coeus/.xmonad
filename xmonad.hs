import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.Minimize
import qualified Data.Map as M
import System.Exit -- exitWith
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Spiral

import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell (shellPrompt)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as W
import Control.Monad
import Control.Applicative
import Data.Monoid (All (All))

import XMonad.Util.EZConfig (additionalKeysP)

myFont = "xft:inconsolatalgc:pixelsize=12:antialias=true"

xmobarTitleColor            = "#FFB6B0"
xmobarCurrentWorkspaceColor = "#CEFFAC"

myEditor     = "emacs -a '' -c"
myBrowser    = "chromium"
myAudioMixer = "pavucontrol"
myTerminal   = "alacritty"
myWallpaper  = "$HOME/.config/background.jpg"
myXmobarrc   = "~/.xmonad/xmobar.hs"

promptConfig :: XPConfig
promptConfig = def
      { font                = myFont
      , bgColor             = "#002b36"
      , fgColor             = "#657b83"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , position            = Top
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Nothing
      }

myKeys :: [(String, X ())]
myKeys =
  [
    ("M-r", shellPrompt promptConfig  )
  , ("M-e", spawn myEditor )
  , ("M-a", spawn myAudioMixer)
  , ("M-w", spawn myBrowser)
  , ("M-z", sendMessage Shrink)
  , ("M-x", sendMessage Expand)
  ]

startup :: X ()
startup = do
  spawnOn "1" myBrowser
  spawnOn "2" myTerminal
  spawnOn "3" myEditor

  spawnOn "8" "Discord"
  spawnOn "9" "spotify"

main :: IO ()
main = do
  spawn $ "feh --bg-scale " ++ myWallpaper
  xmproc <- spawnPipe $ (++) "xmobar " myXmobarrc
  xmonad $ ewmh def
    { terminal    = myTerminal
    , modMask     = mod4Mask
    , focusFollowsMouse = False
    , borderWidth = 2
    , normalBorderColor  = "#888888"
    , focusedBorderColor = "#000000"
    , manageHook   = manageSpawn <+> manageDocks <+> manageHook def
    , startupHook = startup
    , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
    , layoutHook = avoidStruts (spiral (6/7))
    , logHook = dynamicLogWithPP $ def {
          ppOutput   = hPutStrLn xmproc
        , ppTitle    = xmobarColor xmobarTitleColor "" . shorten 100
        , ppCurrent  = xmobarColor xmobarCurrentWorkspaceColor ""
        , ppSep      = "   "
      }
    } `additionalKeysP` myKeys
