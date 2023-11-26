{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable, PatternGuards, ViewPatterns, OverloadedStrings, FlexibleContexts #-}

-- | Applications group is defined by set of conditions for windows.
-- This module allows to define ManageHooks and keyboard shortcuts
-- for applications group in one place. Moreover, one can associate an `action'
-- with a group. This action is run instead of switching to group, in case there are
-- no opened windows of group at this moment.

module XMonad.AppGroups
  (Key, App, AppsConfig (..), Condition (..), Cond (..),
   group, on, full, float,
   nofocus, named, orSpawn, orRun,
   (~>), (~>>),
   fromGroups,
   doFullscreen, query,
   apps2hooks, apps2keys,
   moveToOwnWorkspace,
   readConfig, useAppGroupsConfig,
   selectAppGroup, switchToApp, selectWorkspaceOn
  ) where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Default
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (toLower, isAlphaNum)
import Data.Yaml
import Data.Aeson.Types (typeMismatch)
import System.FilePath
import System.Environment

import XMonad hiding (float)
import qualified XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.GridSelect
import XMonad.Actions.OnScreen
import XMonad.Layout.Minimize
import XMonad.Util.WindowProperties
import XMonad.Util.WindowPropertiesRE
import XMonad.Util.NamedWindows
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.ExtensibleState as XS
import XMonad.Hooks.ManageHelpers hiding (C)

import XMonad.Utils

-- | Anything that can be turned into @Query@ is a condition.
class Condition c where
  toQuery :: c -> Query Bool

-- | In case just a string is used as condition, we assume that this string
-- must be in window class string.
instance Condition String where
  toQuery s = className =? s

instance Condition Property where
  toQuery p = propertyToQuery p

instance Condition PropertyRE where
  toQuery (RE p) = propertyToQueryRE p

instance Condition Bool where
  toQuery bool = return bool

data AndC = AndC Cond Cond

instance Condition AndC where
  toQuery (AndC (C c1) (C c2)) = liftM2 (&&) (toQuery c1) (toQuery c2)

-- | Keyboard shortcut
type Key = String

-- | List of application groups
data AppsConfig = AppsConfig {
    appsList :: [App],
    windowsGSC :: GSConfig Window,
    appsGSC :: GSConfig App,
    workspacesGSC :: GSConfig WorkspaceId,
    prohibitOwnWorkspaces :: [WorkspaceId],
    workspacesMapping :: [(WorkspaceId, ScreenId)] }

instance Default AppsConfig where
  def = AppsConfig [] def def def [] []

instance FromJSON AppsConfig where
  parseJSON (Object v) = do
    apps <- v .: "applications"
    screens <- v .:? "screens" .!= []
    prohibit <- v .:? "prohibit-workspaces" .!=  []
    let mapping = fromGroups screens
    return $ AppsConfig apps def def def prohibit mapping

  parseJSON invalid = typeMismatch "AppsConfig" invalid

instance ExtensionClass AppsConfig where
  initialValue = def

-- | Application group configuration
data App = App {
     hotkey :: Maybe Key     -- ^ Keyboard shortcut to switch to this group
   , action  :: X ()         -- ^ Action to be run when there are no windows in group
   , conditions :: Conds     -- ^ Conditions define which windows are included into group
   , makeFullscreen :: Bool  -- ^ Fullscreen
   , makeFloat :: Bool       -- ^ Whether windows must be made floating
   , noFocus :: Bool
   , moveToWksp :: Maybe WorkspaceId -- ^ Specify workspace
   , jumpToWksp :: Bool
   , shortName :: Maybe String -- ^ Group short name
}

instance FromJSON App where
  parseJSON (Object v) = do
    key <- v .:? "key"
    mbCommand <- v .:? "command"
    conds <- v .: "conditions"
    full <- v .:? "fullscreen" .!= False
    float <- v .:? "float" .!= False
    nofocus <- v .:? "no-focus" .!= False
    wksp <- v .:? "workspace"
    jump <- v .:? "jump" .!= True
    name <- v .:? "name" .!= wksp

    let action = case mbCommand of
                   Nothing -> return ()
                   Just command -> spawn command
    return $ App key action conds full float nofocus wksp jump name

  parseJSON invalid = typeMismatch "App" invalid

-- | Container type for any condition
data Cond = forall c. Condition c => C c

-- | List of conditions
type Conds = [Cond]

instance FromJSON Cond where
  parseJSON (Object v) = do
      mbClass <- v .:? "class"
      mbTitle <- v .:? "title"

      let clsCond =     case mbClass of
                          Nothing -> C True
                          Just cls -> if isRegexp cls
                                        then C $ RE $ ClassName (stripRegexp cls)
                                        else C $ ClassName cls
      let titleCond =   case mbTitle of
                          Nothing -> C True
                          Just title -> if isRegexp title
                                          then C $ RE $ Title (stripRegexp title)
                                          else C $ Title (stripRegexp title)
      return $ C $ AndC clsCond titleCond

    where
      isRegexp :: String -> Bool
      isRegexp str = "/" `isPrefixOf` str && "/" `isSuffixOf` str

      stripRegexp :: String -> String
      stripRegexp str = tail $ init str

  parseJSON invalid = typeMismatch "Cond" invalid

readConfig :: IO AppsConfig
readConfig = do
  home <- getEnv "HOME"
  let path = home </> ".xmonad" </> "applications.yaml"
  x <- decodeFileEither path
  case x of
    Left err -> fail $ show err
    Right cfg -> return cfg

useAppGroupsConfig :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO (XConfig l)
useAppGroupsConfig xcfg = do
  cfg <- readConfig
  return $ xcfg {
             manageHook = composeOne (apps2hooks cfg) <+> moveToOwnWorkspace cfg <+> manageHook xcfg
          } `additionalKeysP` apps2keys cfg


{-
EDSL для описания групп приложений
----------------------------------

Описывать группы приложений с использованием непосредственно конструктора +App+
и record syntax длинно, неудобно и не наглядно. Поэтому ниже определяем набор
комбинаторов для конструирования значений типа +App+.

Начальное значение. Определяет только набор условий на окна.
-}

-- | Initial value. Defines only window matching conditions.
group :: Conds -> App
group conds = App {
  hotkey = Nothing,
  action = return (),
  conditions = conds,
  makeFullscreen = False,
  makeFloat = False,
  noFocus = False,
  moveToWksp = Nothing,
  jumpToWksp = False,
  shortName = Nothing }

-- | Associate keyboard shortcut with a group.
on :: App -> Key -> App
on app key = app {hotkey = Just key}

-- | `Or spawn'. Constructs a group from set of conditions and a command to be run
-- when there is no such windows.
orSpawn :: Conds -> String -> App
orSpawn conds command = (group conds) {action = spawn command}

-- | `Or run'. Analogous to @orSpawn@, but for any @X@ action.
orRun :: Conds -> X () -> App
orRun conds x = (group conds) {action = x} 

-- | Says that windows of this group are to be made fullscreen.
full :: App -> App
full app = app {makeFullscreen = True}

-- | Says that windows of this group are to be made floating.
float :: App -> App
float app = app {makeFloat = True}

nofocus :: App -> App
nofocus app = app {noFocus = True}

-- | Sets the short name for group.
named :: App -> String -> App
named app name = app {shortName = Just name}

-- | Windows of this group are to be opened on specified workspace.
(~>) :: App -> WorkspaceId -> App
app ~> wksp = app {moveToWksp = Just wksp}

(~>>) :: App -> WorkspaceId -> App
app ~>> wksp = app {moveToWksp = Just wksp, jumpToWksp = True}

-- Утилиты

fromGroups :: [[WorkspaceId]] -> [(WorkspaceId, ScreenId)]
fromGroups lists = concat $ zipWith toPairs lists [0..]
  where
    toPairs list i = [(wksp, i) | wksp <- list]

-- | Make window floating and fullscreen.
doFullscreen :: ManageHook
doFullscreen = fromWindowOp (withDisplay . fullscreen)
    where
      fullscreen :: Window -> Display -> X ()
      fullscreen w d = let wd = widthOfScreen s
                           ht = heightOfScreen s
                           s = defaultScreenOfDisplay d
                        in do XMonad.float w
                              io $ resizeWindow d w wd ht

-- | Returns @True@, if the window is not transient.
isNotTransient :: Query Bool
isNotTransient = do
  mbw <- transientTo
  case mbw of
    Nothing -> return True
    Just _  -> return False

query :: App -> Query Bool
query app = isNotTransient <&&> oneOf [toQuery c | (C c) <- conditions app]

appHook :: Maybe ScreenId -> App -> MaybeManageHook
appHook mbSID app = query app -?> mconcat $ [
                              whenH (makeFloat app)      doFloat,
                              whenH (makeFullscreen app) doFullscreen, 
                              whenJustH (moveToWksp app) (createAndMove (jumpToWksp app) mbSID) ]

-- | Group name.
groupName :: App -> String
groupName app
        | Just name <- shortName app  = name
        | Just wksp <- moveToWksp app = wksp
        | otherwise                   = "unknown"
 
apps2hooks :: AppsConfig -> [MaybeManageHook]
apps2hooks apps = map hook (appsList apps)
   where hook app = appHook (scr app) app
         scr app = moveToWksp app >>= flip lookup (workspacesMapping apps)

{-
Список привязок сочетаний клавиш.
Каждая комбинация клавиш будет переключать к одному из окон соответствующей
группы (окна выбираются с помощью +X.A.GridSelect+), или запускать связанное
с группой действие, если подходящих окон нет.
-}

-- | Hotkeys list.
apps2keys :: AppsConfig -> [(String, X ())]
apps2keys apps = mapMaybe getHotkey (appsList apps)
  where
    getHotkey app
      | Just key <- hotkey app = Just (key, selectWithQuery apps (query app) (action app))
      | otherwise              = Nothing

isVisible :: Window -> X Bool
isVisible w =  withWindowSet $ \ws -> do
    let maybeStacks = map W.stack $ map W.workspace $ W.screens ws
    return $ any good maybeStacks
  where good Nothing              = False
        good (Just (W.Stack t l r)) = w `elem` (t: l ++ r)

myFocus :: AppsConfig -> Window -> X ()
myFocus apps w = do
  visible <- isVisible w
  if visible
    then focus w
    else withWindowSet $ \ws -> do
           whenJust (W.findTag w ws) $ \wksp -> do
             let sid = fromMaybe (W.screen $ W.current ws) $ lookup wksp (workspacesMapping apps)
             targetWksp <- screenWorkspace sid
             whenJust targetWksp (windows . W.view)
             focus w

-- | Select windows from ones matching the query (using @X.A.GridSelect@),
-- or run specified action if there are no such windows.
selectWithQuery :: AppsConfig -> Query Bool -> X () -> X ()
selectWithQuery apps qry run = do
    wins <- matchingWindows qry
    case wins of
      [] -> run
      [w] -> myFocus apps w
      _ -> do
          titles <- mapM windowTitle wins
          selected <- gridselect (windowsGSC apps) $ zip titles wins
          whenJust selected $ \w -> do
             myFocus apps w
             -- sendMessage (RestoreMinimizedWin w)
  where
    windowTitle w = show `fmap` getName w

-- | Select application group: switch to one of windows of group or execute an action.
selectAppGroup :: AppsConfig -> X ()
selectAppGroup apps = do
    nonempty <- filterM isNotEmpty (appsList apps)
    let names = map groupName nonempty
    group <- gridselect (appsGSC apps) $ zip names nonempty
    whenJust group $ \app ->
      selectWithQuery apps (query app) (action app)
  where
    isNotEmpty :: App -> X Bool
    isNotEmpty group = (not . null) `fmap` matchingWindows (query group)

switchToApp :: AppsConfig -> String -> X ()
switchToApp apps name = 
  case filter (\a -> groupName a == name) (appsList apps) of
    [app] -> do
      ws <- matchingWindows (query app)
      case ws of
        []  -> action app
        [w] -> myFocus apps w
        _   -> selectOneWindow (windowsGSC apps) ws
    _     -> return ()

selectWorkspaceOn :: AppsConfig -> Maybe ScreenId -> X ()
selectWorkspaceOn conf Nothing = withWindowSet $ \ws -> do
    let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    selected <- gridselect (workspacesGSC conf) (zip wss wss)
    whenJust selected $ \wksp -> do
        case lookup wksp $ workspacesMapping conf of
          Nothing  -> windows (W.view wksp)
          Just sid -> windows (viewOnScreen sid wksp)
selectWorkspaceOn conf (Just sid) = withWindowSet $ \ws -> do
    let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    screenWorkspace sid >>= flip whenJust (windows . W.view)
    selected <- gridselect (workspacesGSC conf) (zip wss wss)
    whenJust selected $ \wksp ->
        windows (viewOnScreen sid wksp)

moveToOwnWorkspace :: AppsConfig -> ManageHook
moveToOwnWorkspace apps = do
  window <- ask
  matching <- liftX $ runQuery (oneOf $ map query $ appsList apps) window
  floating <- liftX $ isFloat window
  transient <- liftX $ runQuery transientTo window
  unlessH (matching || floating || isJust transient) $ do
      wksp <- liftX (windowWorkspace window)
      whenH (wksp `notElem` prohibitOwnWorkspaces apps) $
          createAndMove True (lookup wksp $ workspacesMapping apps) wksp

windowWorkspace :: Window -> X WorkspaceId
windowWorkspace win = do
    cls <- withDisplay $ \d -> fmap resName $ io $ getClassHint d win
    let cls' = map (anySeparatorToSpace . toLower) cls
        wksp = head $ words cls'
    return wksp
  where
    anySeparatorToSpace c | isAlphaNum c = c
                          | otherwise    = ' '

