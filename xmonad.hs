-- rupa's xmonad.hs (darcs/gnome)
--
-- # .gnomerc
-- export WINDOW_MANAGER=/home/rupa/bin/xmonad
-- # easy way to get binary into gnome's path
-- ln -s /home/rupa/bin/xmonad /usr/local/bin

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Monoid

import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Submap as SM

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.XSelection

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- terminals
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_Return), spawn "urxvt -pe tabbed")

    -- quake terminal
    , ((modMask,               xK_Down  ), scratchpadSpawnAction conf)

    -- file manager
    , ((modMask,               xK_Up    ), spawn "nautilus ~")

    -- shorturl
    , ((modMask,               xK_c     ), spawn "/home/rupa/bin/short")

    -- shell/window prompts
    , ((modMask,               xK_space ), runOrRaisePrompt mySP)
    , ((modMask .|. shiftMask,   xK_space ), shellPrompt mySP)
    , ((modMask .|. controlMask, xK_space), windowPromptGoto mySP)

    -- browser
    , ((modMask,               xK_b     ), runOrRaise 
        "firefox" (className =? "Firefox"))

    -- send a mail
    , ((modMask,               xK_x     ), emailPrompt mySP ["rupa@lrrr.us"])

    -- search prompt
    , ((modMask,               xK_p     ), SM.submap $ searchEngineMap $ promptSearch mySP)
    -- search selection
    , ((modMask .|. controlMask, xK_p   ), SM.submap $ searchEngineMap $ selectSearch)
    -- open selection as URL
    , ((modMask .|. shiftMask, xK_p     ), safePromptSelection "firefox")

    -- print screen
    , ((0,                     xK_Print ), unsafeSpawn "scrot -e 'mv $f ~/Pictures'")
    -- cap screen
    , ((modMask,               xK_Print ), unsafeSpawn "/home/rupa/ubin/cap")

    -- cycle through workspaces
    , ((modMask,               xK_Right ), moveTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask,               xK_Left  ), moveTo Prev (WSIs (return $ not . (=="SP") . W.tag)))

    -- move windows through workspaces
    , ((modMask .|. shiftMask, xK_Right ), shiftTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask .|. shiftMask, xK_Left  ), shiftTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask .|. controlMask, xK_Right), shiftTo Next EmptyWS)
    , ((modMask .|. controlMask, xK_Left), shiftTo Prev EmptyWS)

    -- Rotate through layouts
    , ((modMask,               xK_grave ), sendMessage NextLayout
    >> (dynamicLogString myPP >>= \d->safeSpawn "gnome-osd-client" d))

    -- Move focus to the next/previous window
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)

    -- Swap the focused window with next/prev window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)

    -- Swap the focused window and the master window
    , ((modMask,            xK_semicolon), windows W.swapMaster)

    -- Shrink/Expand the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- Increment/Deincrement the number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- toggle focused window fullscreen
    , ((modMask,               xK_m     ), sendMessage (Toggle "Full")
    >> (dynamicLogString myPP >>= \d->safeSpawn "gnome-osd-client" d))

    -- Push window back into tiling
    , ((modMask,               xK_s     ), withFocused $ windows . W.sink)
    --, ((modMask .|. shiftMask, xK_s     ), sendMessage Arrange)

    -- toggle the status bar gap
    , ((modMask,               xK_f     ), sendMessage ToggleStruts)

    -- close focused window 
    , ((modMask              , xK_w     ), kill)

    -- Restart xmonad
    , ((modMask              , xK_q     ),
        broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]

    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]

    -- ++
    -- mod-{e,r,t}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{e,r,t}, Move client to screen 1, 2, or 3
    -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_e, xK_r, xK_t] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ]

-- search engines
delicious = searchEngine "delicious" "http://del.icio.us/tag/"
searchEngineMap method = M.fromList $
    [ ((0, xK_d), method delicious)
    , ((0, xK_g), method google)
    , ((0, xK_i), method imdb)
    , ((0, xK_w), method wikipedia) ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    --, ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]

-- decoration theme
myDeco = defaultTheme
    { activeColor         = "orange"
    , inactiveColor       = "#222222"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#222222"
    , urgentBorderColor   = "yellow"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#222222"
    , urgentTextColor     = "yellow"
    , decoHeight          = 10 }

-- tab theme
myTab = defaultTheme
    { activeColor         = "black"
    , inactiveColor       = "black"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#222222"
    , urgentBorderColor   = "black"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#222222"
    , urgentTextColor     = "yellow" }

-- shell prompt theme
mySP = defaultXPConfig
    { bgColor           = "black"
    , fgColor           = "white"
    , bgHLight          = "gray"
    , fgHLight          = "black"
    , borderColor       = "orange"
    , promptBorderWidth = 1
    , position          = Bottom
    , height            = 20
    --, autoComplete      = ?
    , historySize       = 1000 }

-- dynamicLog theme (suppress everything but layout)
myPP = defaultPP
    { ppLayout  = (\ x -> case x of
      "Hinted ResizableTall"        -> "[|]"
      "Mirror Hinted ResizableTall" -> "[-]"
      "Hinted Tabbed Simplest"      -> "[T]"
      "Full"                 -> "[ ]"
      _                      -> x )
    , ppCurrent         = const ""
    , ppVisible         = const ""
    , ppHidden          = const ""
    , ppHiddenNoWindows = const ""
    , ppUrgent          = const ""
    , ppTitle           = const ""
    , ppWsSep           = ""
    , ppSep             = "" }

-- layouts
myLayout = ewmhDesktopsLayout $ avoidStruts $ toggleLayouts (noBorders Full)
    --(smartBorders (tiled ||| Mirror tiled ||| tabbed shrinkText myTab))
    (smartBorders (tiled ||| Mirror tiled ||| layoutHints (tabbed shrinkText myTab)))
    where
        tiled   = layoutHints $ ResizableTall nmaster delta ratio []
        nmaster = 1
        delta   = 3/100
        ratio   = 1/2

-- special windows
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    --, className =? "Gimp"           --> doF (W.shift "7")
    --, className =? "Gimp"           --> (liftX (findWorkspace getSortByIndex Next EmptyWS 1) >>= doF . W.shift)
    --, className =? "Gimp"           --> (liftX (findWorkspace getSortByIndex Next (WSIs myGimpFindingFunction) 1) >>= doF . W.shift)
    , title     =? "glxgears"       --> doFloat
    , className =? "Gnome-panel"    --> doIgnore
    , className =? "XVkbd"          --> doIgnore
    , className =? "Cellwriter"     --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , className =? "wmforkplop"     --> doFloat
    , className =? "wmhdplop"       --> doFloat
    , className =? "Conky"          --> doIgnore
    , isFullscreen                  --> doFullFloat
    --                                      x y w h
    , scratchpadManageHook $ W.RationalRect 0 0 1 0.3
    , manageDocks ]

-- let Gnome know about Xmonad actions
myLogHook = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace >> updatePointer Nearest

-- mailPrompt
emailPrompt :: XPConfig -> [String] -> X( )
emailPrompt c addrs =
    inputPromptWithCompl c "@" (mkComplFunFromList addrs) ?+ \to ->
    inputPrompt c "s" ?+ \subj ->
    inputPrompt c ">" ?+ \body ->
    io $ runProcessWithInput "mutt" ["-F", "~/.gmailrc", "-s", subj, to] (body ++ "\n")
         >> return ()

main = xmonad $ defaultConfig
    { terminal           = "urxvt"
    , borderWidth        = 2
    , normalBorderColor  = "black"
    , focusedBorderColor = "orange"
    , focusFollowsMouse  = True
    , modMask            = mod4Mask
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = myLayout
    , logHook            = myLogHook
    , manageHook         = myManageHook }
