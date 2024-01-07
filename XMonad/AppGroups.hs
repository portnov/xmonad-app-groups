{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable, PatternGuards, ViewPatterns, OverloadedStrings, FlexibleContexts #-}

-- | Applications group is defined by set of conditions for windows.
-- This module allows to define ManageHooks and keyboard shortcuts
-- for applications group in one place. Moreover, one can associate an `action'
-- with a group. This action is run instead of switching to group, in case there are
-- no opened windows of group at this moment.

module XMonad.AppGroups
  (Key, App, AppsConfig (..), Condition (..), Cond (..),
   initializeAppGroups,
   group, on, full, float,
   tag,
   nofocus, named, orSpawn, orRun,
   (~>), (~>>),
   handleDynamic,
   dynamicHooks,
   fromGroups,
   doFullscreen, query,
   apps2hooks, apps2keys,
   moveToOwnWorkspace,
   readConfig, useAppGroupsConfig,
   selectAppGroup, switchToApp, selectWorkspaceOn,
   addTagToWindow, removeTagFromWindow, setTagsForWindow,
   addTagToWorkspace, addTagToWorkspace', removeTagFromWorkspace, setTagOnWorkspace,
   createWorkspaceForTag
  ) where

import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Default
import Data.List (isPrefixOf, isSuffixOf, delete)
import qualified Data.Map as M
import qualified Data.Set as S
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
import XMonad.Actions.Minimize
import XMonad.Actions.DynamicWorkspaces
import XMonad.Layout.Minimize
import XMonad.Util.WindowProperties
import XMonad.Util.WindowPropertiesRE
import XMonad.Util.NamedWindows
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.ManageHelpers hiding (C)
import XMonad.Hooks.DynamicProperty
import qualified XMonad.Actions.TagWindows as Tag
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Actions.CopyWindow as Copy
import XMonad.Util.Stack
import XMonad.Layout.WindowArranger (diff)

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
    modeKeyMod :: String,
    bringHereKeyMod :: Maybe String,
    removeFromHereKeyMod :: Maybe String,
    windowsGSC :: GSConfig Window,
    appsGSC :: GSConfig App,
    workspacesGSC :: GSConfig WorkspaceId,
    prohibitOwnWorkspaces :: [WorkspaceId],
    workspacesMapping :: [(WorkspaceId, ScreenId)] }

instance Default AppsConfig where
  def = AppsConfig [] "" Nothing Nothing def def def [] []

instance FromJSON AppsConfig where
  parseJSON (Object v) = do
    apps <- v .: "applications"
    modeKey <- v .:? "mode_key" .!= ""
    addKey <- v .:? "bring_here_key"
    removeKey <- v .:? "remove_key"
    screens <- v .:? "screens" .!= []
    prohibit <- v .:? "prohibit-workspaces" .!=  []
    let mapping = fromGroups screens
    return $ AppsConfig apps modeKey addKey removeKey def def def prohibit mapping

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
   , dynamicProperty :: Maybe String
   , setTag :: Maybe String
   , shortName :: Maybe String -- ^ Group short name
}

instance Show App where
  show app
    | Just name <- shortName app = name
    | Just wksp <- moveToWksp app = wksp
    | Just tag <- setTag app = tag
    | Just key <- hotkey app = key
    | otherwise = "<App>"

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
    dynProp <- v .:? "dynamic_property"
    tags <- v .:? "tag"
    name <- v .:? "name" .!= wksp

    let action = case mbCommand of
                   Nothing -> return ()
                   Just command -> spawn command
    return $ App key action conds full float nofocus wksp jump dynProp tags name

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
  dynamicProperty = Nothing,
  setTag = Nothing,
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

handleDynamic :: App -> String -> App
handleDynamic app property = app {dynamicProperty = Just property}

tag :: App -> String -> App
tag app name = app {setTag = Just name}

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
                              whenJustH (appTag app) setTagForWindowH,
                              whenJustH (moveToWksp app) (\mbWksp -> do
                                  alreadyThere <- isAlreadyThere
                                  createAndMove (not alreadyThere && jumpToWksp app) mbSID mbWksp)]
  where
    isAlreadyThere :: Query Bool
    isAlreadyThere = do
      case appTag app of
        Nothing -> return False
        Just tagName -> liftX $ do
          wksp <- gets (W.tag . W.workspace . W.current . windowset)
          tags <- getTagsByWorkspace wksp
          return $ tagName `elem` tags

appHookWithScreen :: AppsConfig -> App -> MaybeManageHook
appHookWithScreen apps app = appHook (scr app) app
  where
    scr app = moveToWksp app >>= flip lookup (workspacesMapping apps)

-- | Group name.
groupName :: App -> String
groupName app
        | Just name <- shortName app  = name
        | Just wksp <- moveToWksp app = wksp
        | otherwise                   = "unknown"
 
appTag :: App -> Maybe String
appTag app
  | Just tag <- setTag app = Just tag
  | Just name <- shortName app = Just name
  | Just wksp <- moveToWksp app = Just wksp
  | otherwise = Nothing

apps2hooks' :: AppsConfig -> [App] -> [MaybeManageHook]
apps2hooks' apps lst = map (appHookWithScreen apps) lst

apps2hooks :: AppsConfig -> [MaybeManageHook]
apps2hooks apps = apps2hooks' apps (appsList apps)

dynamicHooks :: AppsConfig -> Event -> X All
dynamicHooks apps event = do
    results <- forM (M.assocs $ compose (appsList apps)) $ \(property, hook) -> do
                   dynamicPropertyChange property (maybeToDefinite hook) event
    return $ mconcat results
  where
    compose lst = M.fromListWith (<+>) [(property, appHookWithScreen apps app)
                    | app@(App {dynamicProperty = Just property}) <- lst]

dfltWorkspacesByTag :: AppsConfig -> M.Map String (S.Set WorkspaceId)
dfltWorkspacesByTag apps = foldr go M.empty (appsList apps)
  where
    go app m =
      case (appTag app, moveToWksp app) of
        (Just tagName, Just wksp) -> M.insertWith S.union tagName (S.singleton wksp) m
        _ -> m

initializeAppGroups :: AppsConfig -> X ()
initializeAppGroups apps = do
  let m = dfltWorkspacesByTag apps
  XS.put $ TagsMap m

{-
Список привязок сочетаний клавиш.
Каждая комбинация клавиш будет переключать к одному из окон соответствующей
группы (окна выбираются с помощью +X.A.GridSelect+), или запускать связанное
с группой действие, если подходящих окон нет.
-}

-- | Hotkeys list.
apps2keys :: AppsConfig -> [(String, X ())]
apps2keys apps = concatMap getHotkey (appsList apps)
  where
    getHotkey app
      | Just key <- hotkey app = mkSelectKey key app
                                 ++ mkBringKey key app
                                 ++ mkRemoveKey key app
      | otherwise              = []

    mkSelectKey key app = [(modeKeyMod apps ++ key, selectWithQuery apps (query app) (action app))]

    mkBringKey key app
      | Just shift <- bringHereKeyMod apps,
        Just tag <- appTag app = [(modeKeyMod apps ++ shift ++ key, addTagToWorkspace tag)]
      | otherwise = []

    mkRemoveKey key app
      | Just ctrl <- removeFromHereKeyMod apps,
        Just tag <- appTag app = [(modeKeyMod apps ++ ctrl ++ key, removeTagFromWorkspace tag)]
      | otherwise = []

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
    then do
      maximizeWindowAndFocus w
      focus w
    else withWindowSet $ \ws -> do
           whenJust (W.findTag w ws) $ \wksp -> do
             let sid = fromMaybe (W.screen $ W.current ws) $ lookup wksp (workspacesMapping apps)
             targetWksp <- screenWorkspace sid
             whenJust targetWksp (windows . W.view)
             maximizeWindowAndFocus w
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


newtype TagsMap = TagsMap (M.Map String (S.Set WorkspaceId))
  deriving (Eq, Read, Show, Typeable)

instance ExtensionClass TagsMap where
  initialValue = TagsMap M.empty
  extensionType = PersistentExtension

getWorkspacesByTag :: String -> X (S.Set WorkspaceId)
getWorkspacesByTag tagName = do
  TagsMap m <- XS.get
  return $ fromMaybe S.empty $ M.lookup tagName m

getTagsByWorkspace :: WorkspaceId -> X [String]
getTagsByWorkspace wksp = do
  TagsMap m <- XS.get
  return $ M.keys $ M.filter (wksp `S.member`) m

addTagToWorkspace :: String -> X ()
addTagToWorkspace tagName = do
  wksp <- gets (W.tag . W.workspace . W.current . windowset)
  addTagToWorkspace' wksp tagName

addTagToWorkspace' :: WorkspaceId -> String -> X ()
addTagToWorkspace' wksp tagName = do
  TagsMap m <- XS.get
  let m' = M.insertWith S.union tagName (S.singleton wksp) m
  XS.put $ TagsMap m'
  Tag.withTaggedGlobal tagName $ \w -> windows (Copy.copyWindow w wksp)

removeTagFromWorkspace :: String -> X ()
removeTagFromWorkspace tagName = do
  wksp <- gets (W.tag . W.workspace . W.current . windowset)
  TagsMap m <- XS.get
  let up ws =
        let ws' = S.delete wksp ws
        in  if S.null ws'
              then Nothing
              else Just ws'
      m' = M.update up tagName m 
  XS.put $ TagsMap m'
  Tag.withTagged tagName killCurrentCopy

setTagOnWorkspace :: String -> X ()
setTagOnWorkspace tagName = do
  wksp <- gets (W.tag . W.workspace . W.current . windowset)
  oldTags <- getTagsByWorkspace wksp
  TagsMap m <- XS.get
  let up t ws 
        | t == tagName = S.insert wksp ws
        | otherwise    = S.delete wksp ws
      m' = M.mapWithKey up m
  XS.put $ TagsMap m'
  forM_ (delete tagName oldTags) $ \oldTag ->
    Tag.withTagged oldTag killCurrentCopy
  Tag.withTaggedGlobal tagName $ \w -> windows (Copy.copyWindow w wksp)

addTagToWindow :: String -> Window -> X ()
addTagToWindow tagName win = do
  Tag.addTag tagName win
  wksps <- getWorkspacesByTag tagName
  let fns = [Endo $ Copy.copyWindow win wksp | wksp <- S.toList wksps]
  windows $ appEndo $ mconcat fns

setTagForWindowH :: String -> ManageHook
setTagForWindowH tagName = do
  win <- ask
  liftX $ Tag.setTags [tagName] win
  wksps <- liftX $ getWorkspacesByTag tagName
  let fns = [Endo $ Copy.copyWindow win wksp | wksp <- S.toList wksps]
  doF $ appEndo $ mconcat fns

setTagsForWindow :: [String] -> Window -> X ()
setTagsForWindow tagNames win = do
  oldTags <- Tag.getTags win
  Tag.setTags tagNames win
  let (tagsToAdd, tagsToRemove) = diff (tagNames, oldTags)
  forM_ tagsToAdd $ \tagToAdd -> do
    wksps <- getWorkspacesByTag tagToAdd
    let fns = [Endo $ Copy.copyWindow win wksp | wksp <- S.toList wksps]
    windows $ appEndo $ mconcat fns
  forM_ tagsToRemove $ \tagToRemove -> do
    wksps <- getWorkspacesByTag tagToRemove
    killCopies win wksps

removeTagFromWindow :: String -> Window -> X ()
removeTagFromWindow tagName win = do
  Tag.delTag tagName win
  wksps <- getWorkspacesByTag tagName
  killCopies win wksps

createWorkspaceForTag :: String -> X ()
createWorkspaceForTag tagName = do
  addHiddenWorkspace tagName
  addTagToWorkspace' tagName tagName

copyHere :: Window -> X ()
copyHere win = do
  wksp <- gets (W.tag . W.workspace . W.current . windowset)
  windows $ Copy.copyWindow win wksp

killCurrentCopy :: Window -> X ()
killCurrentCopy win = do
    ws <- gets windowset
    if W.member win $ delete'' win ws
      then windows $ delete'' win
      else minimizeWindow win
  where
    delete'' w = W.modify Nothing (W.filter (/= w))

killCopy :: Window -> WorkspaceId -> X ()
killCopy win wkspId = killCopies win (S.singleton wkspId)

killCopies :: Window -> S.Set WorkspaceId -> X ()
killCopies win wkspIds = do
    ws <- gets windowset
    if W.member win $ delete' ws
      then windows delete' 
      else minimizeWindow win
  where
    delete' = W.mapWorkspace $ \wksp ->
                if W.tag wksp `S.member` wkspIds
                  then wksp {
                              W.stack = filterZ (\_ w -> w /= win) (W.stack wksp)
                            }
                  else wksp

