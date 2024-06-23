import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.Run

import qualified Data.Map as Map
import qualified XMonad.StackSet as W

spawnterm :: String -> X ()
spawnterm c = spawn $ "urxvtcd -e " ++ c

mpc :: String -> X ()
mpc c = spawn $ "MPD_HOST=~/.mpd/socket mpc " ++ c

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
myLayouts = avoidStruts $
            mkToggle (single NBFULL) . mkToggle (single MIRROR) $
            smartBorders $
            onWorkspace "1" (even_three ||| four) $
            (four ||| even_three)

-- Rebind some keys because firefox is too uppity to support that.  See
-- "firefox" below for the bindings.
fffix :: (KeyMask, KeySym, KeyMask, KeySym) -> ((KeyMask, KeySym), X())
fffix (inm, ins, ffm, ffs) = let
  onFirefox = withWindowSet (
    \wset ->
      case W.peek wset of
        Nothing -> return False
        Just w -> do
          lower <- runQuery (className =? "firefox") w
          upper <- runQuery (className =? "Firefox") w
          return $ lower || upper
    )
  ffev = onFirefox --> sendKey ffm ffs
  nfev = (fmap not onFirefox) --> sendKey inm ins
  in ((inm, ins), ffev >> nfev)

keymap :: XConfig Layout -> Map.Map (KeyMask, KeySym) (X())
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
    , ("M-q",   spawn $ "pkill .mpdmonitor.py; pkill .alsamonitor.py; " ++
        "pkill alsactl; xmonad --restart")
    , ("M-b",   sendMessage ToggleStruts)
    , ("M-r",   refresh)

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
    , ("M-f",         sendMessage $ Toggle NBFULL)
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

    -- audio bindings - use with mpdmonitor
    , ("M-[",    mpc "pause")
    , ("M-]",    mpc "play")
    , ("M-\\",   spawnterm "alsamixer")
    , ("M-S-[",  mpc "prev")
    , ("M-S-]",  mpc "clear")
    , ("M-S-\\", spawnterm "ncmpcpp")
    , ("M-;",    spawn "amixer set Master 3%-")
    , ("M-'",    spawn "amixer set Master 3%+")
    -- , ("M-S-=",  spawn "amixer set Headphone mute; amixer set Speaker unmute")
    -- , ("M-=",    spawn "amixer set Speaker mute; amixer set Headphone unmute")

    -- Works with many keyboards.  On Thinkpads, Pause is often Fn-p.
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 3%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 3%+")
    , ("<XF86ScreenSaver>",      spawn "slock xset dpms force off")
    , ("<Pause>",                spawn "slock xset dpms force off")

    -- Apple Extended II, which has no Fn or multimedia keys
    , ("C-<F2>", spawn "amixer set Master 3%-")
    , ("C-<F3>", spawn "amixer set Master 3%+")
    ]

  -- emacs/readline-like bindings
  firefox = Map.fromList $ map fffix
    [ (controlMask, xK_s,     0, xK_F3)
    , (controlMask, xK_r,     shiftMask, xK_F3)
    , (controlMask, xK_n,     0, xK_Down)
    , (controlMask, xK_p,     0, xK_Up)
    , (controlMask, xK_f,     0, xK_Right)
    , (controlMask, xK_b,     0, xK_Left)
    , (controlMask, xK_g,     0, xK_Escape)
    , (controlMask, xK_y,     controlMask, xK_v)
    , (controlMask, xK_a,     0, xK_Home)
    , (mod1Mask, xK_w,        controlMask, xK_c)
    , (controlMask, xK_slash, controlMask, xK_z)
    , (controlMask, xK_v,     0, xK_Page_Down)
    , (mod1Mask, xK_v,        0, xK_Page_Up)
    ]
  in Map.unions [mkKeymap conf keyconf, Map.fromList wspacekeys, firefox]

mice :: XConfig l0 -> Map.Map (KeyMask, Button) (Window -> X())
mice XConfig {XMonad.modMask = modm} = let
  move w = focus w >> mouseMoveWindow w >> windows W.shiftMaster
  resize w = focus w >> mouseResizeWindow w >> windows W.shiftMaster
  master w = focus w >> windows W.shiftMaster
  actions = [ ((modm, button1), move)
            , ((modm, button2), resize)
            , ((modm, button3), master)
            ]
  in Map.fromList actions

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
    , ppHidden = xmobarColor "black" "lightgrey" . xmobarStrip
    , ppHiddenNoWindows = id
    , ppUrgent = xmobarColor "orange" "black" . xmobarStrip
    , ppSep = " "
    , ppTitle = xmobarColor "#a020f0" "black" . xmobarStrip
    , ppLayout = workspacePP
    , ppOutput = hPutStrLn xmproc
    }
  in dynamicLogWithPP xmpp

manip :: ManageHook
manip = let
  floats = map (--> doFloat) [ resource =? "Dialog"
                             , className =? "pinentry"
                             , className =? "magpie"
                             , isDialog]
  shifts = [ className =? "Keybase" --> doShift "9" ]
  in manageDocks <+> composeAll (floats ++ shifts)

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
        , startupHook = do
            spawnterm "zsh"
            spawn "xmodmap .Xmodmap"
            spawn "emacs --daemon"
            spawn "firefox"
            spawn "redshift"
        }
  xmonad $ docks $ xmconfig
