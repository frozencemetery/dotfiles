import Data.Maybe
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.Run(spawnPipe)

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Loggers as L

spawnterm :: String -> X ()
spawnterm c = spawn $ "urxvtcd -e " ++ c

mpc :: String -> X ()
mpc c = spawn $ "MPD_HOST=/run/mpd/socket mpc " ++ c

dmenu :: String -> String -> X ()
dmenu command prompt = let
  cfg = " -b -fn Terminus:size=8 -nb black -nf grey -sb orange -sf black -p "
  in spawn $ command ++ cfg ++ prompt

-- Layouts are designed for a 1080p screen.  With 8-point terminus, I can get
-- four terminals across at 79 characters each, or three terminals across at
-- 104 each.
widthdelta = 3/100 :: Rational
even_three = multiCol [1, 1] 0 widthdelta (1/3) :: MultiCol a
four = multiCol [1, 1, 1] 0 widthdelta (1/4) :: MultiCol a
myLayouts = avoidStruts $ smartBorders $
            mkToggle (single FULL) . mkToggle (single MIRROR) $
            onWorkspace "1" (even_three ||| four) $
            (four ||| even_three)

-- Rebind some keys because firefox is too uppity to support that.
onFirefox :: X Bool
onFirefox = withWindowSet (
  \wset ->
    case W.peek wset of
      Nothing -> return False
      Just w -> runQuery (className =? "Firefox") w
  )

ifFirefox :: (KeyMask, KeySym) -> (KeyMask, KeySym) -> X ()
ifFirefox (fm, fs) (nm, ns) =
  (onFirefox --> sendKey fm fs) >> ((fmap not onFirefox) --> sendKey nm ns)

keymap :: XConfig Layout -> M.Map (KeyMask, KeySym) (X())
keymap conf@XConfig {XMonad.modMask = modm} = let
  ordered_keysyms = [xK_1 .. xK_9] ++ [xK_0]
  wspacekeys =
    [ ((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) ordered_keysyms
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ] ++
    [ ( (m .|. modm .|. mod1Mask, key)
      , screenWorkspace sc >>= flip whenJust (windows . f)
      )
    | (key, sc) <- zip ordered_keysyms [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
  keyconf =
    [ ("M-S-c", kill)
    , ("M-q",   spawn "pkill .mpdmonitor.sh; xmonad --restart")
    , ("M-b",   sendMessage ToggleStruts)
    , ("M-r",   refresh)

    -- firefox is trash (see above)
    , ("C-s",  ifFirefox (0, xK_F3)          (controlMask, xK_s))
    , ("C-r",  ifFirefox (shiftMask, xK_F3)  (controlMask, xK_r))
    , ("C-n",  ifFirefox (0, xK_Down)        (controlMask, xK_n))
    , ("C-p",  ifFirefox (0, xK_Up)          (controlMask, xK_p))
    , ("C-f",  ifFirefox (0, xK_Right)       (controlMask, xK_f))
    , ("C-b",  ifFirefox (0, xK_Left)        (controlMask, xK_b))
    , ("C-g",  ifFirefox (0, xK_Escape)      (controlMask, xK_g))
    , ("C-y",  ifFirefox (controlMask, xK_v) (controlMask, xK_y))
    , ("M1-w", ifFirefox (controlMask, xK_c) (mod1Mask, xK_w))
    , ("C-/",  ifFirefox (controlMask, xK_z) (controlMask, xK_slash))
    , ("C-v",  ifFirefox (0, xK_Page_Down)   (controlMask, xK_v))
    , ("M1-v", ifFirefox (0, xK_Page_Up)     (mod1Mask,    xK_v))

    -- windows
    , ("M-n",   windows W.focusDown)
    , ("M-S-n", windows W.swapDown)
    , ("M-p",   windows W.focusUp)
    , ("M-S-p", windows W.swapUp)
    , ("M-h",   sendMessage Shrink)
    , ("M-l",   sendMessage Expand)
    , ("M-m",   windows W.focusMaster)

    -- layouts
    , ("M-,",         sendMessage $ IncMasterN 1)
    , ("M-.",         sendMessage $ IncMasterN $ -1)
    , ("M-<Space>",   sendMessage NextLayout)
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
    , ("M-f",         sendMessage $ Toggle FULL)
    , ("M-a",         sendMessage $ Toggle MIRROR)
    , ("M-t",         withFocused $ windows . W.sink)

    -- spawn all the things
    , ("M-S-<Return>", spawn $ terminal conf)
    , ("M-<Return>", spawnterm "emacsclient -nw -a emacs")
    , ("M-S-x", spawn "gmrun")
    , ("M-x", dmenu "dmenu_run" "cmd: ")
    , ("M-u", spawn "idcpass x")
    , ("M-S-/", spawnterm "less /usr/share/X11/locale/en_US.UTF-8/Compose")
    , ("M-w", spawnterm "emacsclient -nw $(mktemp)")
    , ("M-S-a", spawnterm "htop")

    -- "regular" bindings
    , ("<XF86AudioPrev>",        mpc "pause")
    , ("<XF86AudioPlay>",        spawnterm "alsamixer")
    , ("<XF86AudioNext>",        mpc "play")
    , ("S-<XF86AudioPrev>",      mpc "prev")
    , ("S-<XF86AudioPlay>",      spawnterm "ncmpcpp")
    , ("S-<XF86AudioNext>",      mpc "clear")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 3%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 3%+")
    , ("<XF86ScreenSaver>",      spawn "xscreensaver-command --lock")
    , ("<Pause>",                spawn "xscreensaver-command --lock")

    -- P1 bindings.  See below for Bluetooth workaround.  XF86Keyboard doesn't
    -- pass through because it's value is greater than 255 so X can't handle
    -- it.  There's an additional rule in /etc/udev/hwdb.d that tries to turn
    -- it into playpause, and then magic in xmodmap to turn it into
    -- XF87KAudioPlay.  Not worth the effort; get a different computer.
--
--    , ("<XF86Bluetooth>",   mpc "pause")
--    , ("<XF86Keyboard>", spawnterm "alsamixer")
    , ("<XF86Favorites>",   mpc "play")
--    , ("S-<XF86Bluetooth>", mpc "prev")
--    , ("S-<XF86Keyboard>", spawnterm "ncmpcpp")
    , ("S-<XF86Favorites>", mpc "clear")

    -- Apple Extended II bindings
    , ("C-<F2>",    spawn "amixer set Master 3%-")
    , ("C-<F3>",    spawn "amixer set Master 3%+")
    , ("C-<F10>",   mpc "pause")
    , ("C-<F11>",   spawnterm "alsamixer")
    , ("C-<F12>",   mpc "play")
    , ("C-S-<F10>", mpc "prev")
    , ("C-S-<F11>", spawnterm "ncmpcpp")
    , ("C-S-<F12>", mpc "clear")
    ]
  -- EZConfig doesn't have all the XF86 keys.  Maybe I should stop using it?
  -- https://github.com/xmonad/xmonad-contrib/pull/365
  workaround = [ ((0, xF86XK_Bluetooth), mpc "pause")
               , ((shiftMask, xF86XK_Bluetooth), mpc "prev")
               , ((0, 0x1008ffb3), spawnterm "alsamixer")
               , ((shiftMask, 0x1008ffb3), spawnterm "ncmpcpp")
               ]
  keylists = wspacekeys ++ workaround
  in mkKeymap conf keyconf `M.union` M.fromList keylists

mice :: XConfig l0 -> M.Map (KeyMask, Button) (Window -> X())
mice XConfig {XMonad.modMask = modm} = let
  move w = focus w >> mouseMoveWindow w >> windows W.shiftMaster
  resize w = focus w >> mouseResizeWindow w >> windows W.shiftMaster
  master w = focus w >> windows W.shiftMaster
  actions = [ ((modm, button1), move)
            , ((modm, button2), resize)
            , ((modm, button3), master)
            ]
  in M.fromList actions

logger :: Handle -> X ()
logger xmproc = let
  workspacePP "MultiCol" = "m │"
  workspacePP "Full" = "ƒ │"
  workspacePP "ResizableTall" = "t │"
  workspacePP "Mirror MultiCol" = "M │"
  workspacePP "Mirror ResizableTall" = "T │"
  workspacePP s = s

  xmpp = xmobarPP
    { ppCurrent = xmobarColor "black" "orange" . xmobarStrip
    , ppVisible = xmobarColor "black" "white" . xmobarStrip
    , ppWsSep = ""
    , ppHidden = xmobarColor "black" "grey" . xmobarStrip
    , ppHiddenNoWindows = id
    , ppUrgent = xmobarColor "orange" "black" . xmobarStrip
    , ppSep = " "
    , ppTitle = xmobarColor "purple" "black" . xmobarStrip
    , ppLayout = workspacePP
    , ppOutput = hPutStrLn xmproc
    }
  in dynamicLogWithPP xmpp

manip :: ManageHook
manip = let
  action = [ resource =? "Dialog" --> doFloat
           , className =? "pinentry" --> doFloat
           ]
  in manageDocks <+> composeAll action

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0"
  let xmconfig = withUrgencyHook NoUrgencyHook def
        { terminal = "urxvtcd"
        , focusFollowsMouse = False
        , borderWidth = 1
        , modMask = mod4Mask
        , workspaces = map show $ [1..9] ++ [0]
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#ff7228"
        , keys = keymap
        , mouseBindings = mice
        , layoutHook = myLayouts
        , logHook = logger xmproc0
        , manageHook = manip
        , handleEventHook = docksEventHook
        , startupHook = spawnterm "zsh"
        }
  xmonad $ xmconfig
