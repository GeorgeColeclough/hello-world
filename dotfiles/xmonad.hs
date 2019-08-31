import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig
import XMonad.Actions.Submap
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdateFocus
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.Input
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicProperty
import XMonad.Layout.NoBorders

import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

import Graphics.X11.Xlib
import Graphics.X11.Xinerama
-- import Graphics.X11.Xlib.Extras
-- import Graphics.X11.Xlib.Event

import qualified Data.Map        as M

import Data.List
import Data.Function

main =
    xmonad =<< statusBar myBar myPP toggleStrutsKey configa

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP {
    ppCurrent= xmobarColor "#edaddd" "" . wrap "~" "~"
    , ppVisible= xmobarColor "#ffffff" "" . wrap "-" "-"
    , ppHiddenNoWindows= xmobarColor "#ffffff" ""
    , ppTitle= xmobarColor "#edaddd" ""
    }
--
-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_x)

--startupHook
myStartupHook :: X ()
myStartupHook=do
    spawn "xrandr --dpi 96"
    spawn "redshift"
    spawn "compton --backend glx --xrender-sync --xrender-sync-fence"
    spawn "xrdb -merge ~/.Xrecources"

--layoutHook
myLayoutHook= spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
    Grid ||| noBorders Full


--workspaces
myWorkspaces= ["1:general","2:work0","3:work1","4:work2","5:social","6:system"]



--scratchpads

myScratchpads=
    [
    NS "spotify" "/snap/bin/spotify" (appName =? "spotify") customCenter,
    --NS "qalc" "qalculate" (appName =? "qalculate") (customFloating $ W.RationalRect (1/9) (1/9) (2/3) (2/3))
    NS "qalc" "qalculate" (appName =? "qalculate") customCenter,
    NS "vim" "xterm -xrm 'XTerm.vt100.allowTitleOps: false' -T vimS -e vim" (title =? "vimS") customCenter,
    NS "qute" "qutebrowser --basedir ~/.config/qutebrowser/scratchProfile :set' window.title_format quteS'" (title =? "quteS") customCenter,
    NS "ranger" "xterm -xrm 'XTerm.vt100.allowTitleOps: false' -T rangerS -e ranger --cmd='shell transset 0.9 -a'" (title =? "rangerS") customCenter,
    NS "term" "xterm -xrm 'XTerm.vt100.allowTitleOps: false' -T termS" (title =? "termS") customCenter
    ] where customCenter= (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))

--manageHook

myManageHook= composeAll
    [
    className =? "Spotify"  --> doFloat,
    appName =? "PureRef"  --> doFloat,
    title =? "Edit Text" --> customCenter
    ] <+> namedScratchpadManageHook myScratchpads
    where customCenter= (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4))

--handle spotify weirdness

myHandleEventHook =
    spotifyHandle <+> quteHandle
    where
        spotifyHandle=dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4)))
        quteHandle=dynamicPropertyChange "WM_NAME" (title =? "quteS" --> (customFloating $ W.RationalRect (1/8) (1/8) (3/4) (3/4)))
--keybinds P
myKeys=
    -- user Funcs
    [
    ("M4-h", spawn "xterm -e ~/scripts/hdkEnv"),
    ("M4-S-b", spawn "qutebrowser --basedir ~/.config/qutebrowser/std"),
    ("M4-S-f", spawn "xterm -e ranger --cmd='shell transset 0.9 -a'"),
    ("M4-S-t", spawn "xterm"),
    ("M4-r", spawn "rofi -show drun"),
    ("M4-q", spawn "qtcreator"),
    ("M4-p", spawn "PureRef"),
    ("M4-S-v", spawn "xterm -e vim"),
    ("M4-s", namedScratchpadAction myScratchpads "spotify"),
    ("M4-c", namedScratchpadAction myScratchpads "qalc"),
    ("M4-v", namedScratchpadAction myScratchpads "vim"),
    ("M4-b", namedScratchpadAction myScratchpads "qute"),
    ("M4-f", namedScratchpadAction myScratchpads "ranger"),
    ("M4-t", namedScratchpadAction myScratchpads "term"),

    -- xmonad Funcs
    ("M-c", kill),
    ("M-t", withFocused $ windows . W.sink),
    ("M-p", sequence_ [spawn "thunar", spawn "thunar"]),
    --("M-q", sequence_ [spawn "pkill compton", spawn "pkill redshift", spawn "xmonad --recompile", spawn "xmonad --restart"]),
    ("M-k", windows W.focusUp),
    ("M-j", windows W.focusDown),
    ("M-S-k", windows W.swapUp),
    ("M-S-j", windows W.swapDown),

    ("M-l", nextWS),
    ("M-h",    prevWS),
    ("M-S-l",  shiftToNext),
    ("M-S-h",    shiftToPrev),
    ("M-<F7>", spawn "~/scripts/brightnessUpdate -d 10"),
    ("M-<F8>", spawn "~/scripts/brightnessUpdate -u 10"),
    ("M-<F10>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
    ("M-<F11>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
    ("M-<F12>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"),
    ("M-<Print>", spawn "flameshot gui"),

    ("M5-<Print>", spawn "flameshot full -p ~/Pictures/screenshots")
    ]

--keyRemovals P
myKeyRemovals=
    [
    ("M-e")
    ]

--mousebinds
myMouseBinds=
    [
    ((mod4Mask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
    ((mod4Mask, button2), \w -> focus w >> windows W.shiftMaster),
    ((mod4Mask, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

--mouseRemovals

myMouseRemovals=
    [
    ((mod1Mask, button1)),
    ((mod1Mask, button2)),
    ((mod1Mask, button3))
    ]


--config
configa = ewmh def
        {
        modMask= mod1Mask
        , borderWidth= 2
        , focusedBorderColor= "#e08dcc"
        , layoutHook= myLayoutHook
        , workspaces= myWorkspaces
        , startupHook= myStartupHook
        , manageHook= myManageHook
	, handleEventHook= focusOnMouseMove <+> myHandleEventHook <+> handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
        }
        `additionalKeysP` myKeys
        `removeKeysP` myKeyRemovals
        `additionalMouseBindings` myMouseBinds
        `removeMouseBindings` myMouseRemovals
