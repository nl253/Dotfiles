-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import           Data.Monoid
import           System.Directory            (findExecutable)
import           System.Exit
import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.FixedColumn
import           XMonad.Layout.HintedGrid
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.TwoPane

import qualified Data.Map                    as M
import qualified XMonad.StackSet             as W

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal :: String
myTerminal = "alacritty -e tmux"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces =
  ["1:term", "2:chrome", "3:ide", "4:spotify"] ++ fmap show [5 .. 9]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor = "#000000"

myFocusedBorderColor = "#ffc251"

------------------------------------------------------------------------
-- Key bindings.
myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch a dmenu
  , ((modm, xK_p), spawn "dmenu_run")
    -- close focused window
  , ((modm .|. shiftMask, xK_c), kill)
  , ((mod1Mask, xK_F4), kill)
     -- Rotate through the available layout algorithms
  , ((modm, xK_space), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  -- Brightness and  volume.  (notify-send, xbacklight, amixer, scrot)
  , ( (0, 0x1008ff02)
    , spawn $ "backlight +2; notify-send " ++ show "Brightness up +5")
  , ( (0, 0x1008ff03)
    , spawn $ "backlight -2; notify-send " ++ show "Brightness down -5")
  , ( (0, 0x1008ff12)
    , spawn $
      "amixer -q set Master toggle; notify-send " ++ show "Toggle volume")
  , ( (0, 0x1008ff13)
    , spawn $
      "amixer -D pulse sset Master 5%+; notify-send " ++ show "Volume up +5")
  , ( (0, 0x1008ff11)
    , spawn $
      "amixer -D pulse sset Master 5%-; notify-send " ++ show "Volume down -5")
  , ( (0, xK_Print)
    , spawn $
      "sleep 0.2; scrot '%Y-%m-%d_%H-%M_$wx$h.png' -e 'mv $f ~/Pictures/' ; notify-send " ++
      show "Print Screen Saved to ~/Pictures")
    -- Resize viewed windows to the correct size
  , ((modm, xK_n), refresh)
    -- Move focus to the next window
  , ((modm, xK_Tab), windows W.focusDown)
    -- Move focus to the next window
  , ((modm, xK_j), windows W.focusDown)
    -- Move focus to the previous window
  , ((modm, xK_k), windows W.focusUp)
    -- Move focus to the master window
  , ((modm, xK_m), windows W.focusMaster)
    -- Swap the focused window and the master window
  , ((modm, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- Shrink the master area
  , ((modm, xK_h), sendMessage Shrink)
    -- Expand the master area
  , ((modm, xK_l), sendMessage Expand)
    -- Push window back into tiling
  , ((modm, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- Quit xmonad
  , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    -- Restart xmonad
  , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
  ] ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = one ||| two ||| three ||| four
  where
    one = Full
    two = spiral (4 / 8)
    three = TwoPane (7 / 100) (3 / 7)
    four = FixedColumn 3 20 80 10

------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "Alacritty" --> doShift "1:term"
    , className =? "spotify" --> doShift "4:spotify"
    , className =? "Google-chrome" --> doShift "2:chrome"
    , className =? "Google-chrome-beta" --> doShift "2:chrome"
    , className =? "Google-chrome-nightly" --> doShift "2:chrome"
    , resource =? "desktop_window" --> doIgnore
    , resource =? "kdesktop" --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

---------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook
  -- Make Java GUIs work with XMonad
 = do
  setWMName "LG3D"
  spawn "/usr/bin/feh --bg-fill /home/norbert/Pictures/desktop.png"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
main :: IO ()
main = xmonad defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
defaults =
  def
  { terminal = myTerminal
  , focusFollowsMouse = myFocusFollowsMouse
  , clickJustFocuses = myClickJustFocuses
  , borderWidth = myBorderWidth
  , modMask = myModMask
  , workspaces = myWorkspaces
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , keys = myKeys
  , layoutHook = myLayout
  , manageHook = myManageHook
  , handleEventHook = myEventHook
  , logHook = myLogHook
  , startupHook = myStartupHook
  }
