{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ()
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.List (intersperse)
import GHC.IO.Exception (ExitCode(..))
import qualified Graphics.Vty as Vty
import qualified Data.Vector as Vector

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( vBox
  , str
  , viewport
  , (<=>)
  )

import qualified System.Process as P
import System.IO
  ( hGetContents
  )

data WeaverEvent =
  VtyEvent Vty.Event
  | CommandOutput Int String
  | CommandFinished Int ExitCode

data History =
  History { _cmd :: String
          , _returnValue :: Maybe ExitCode
          , _output :: String
          }

makeLenses ''History

-- XXX TODO replace the List with a viewport of Historys
data Weaver =
  Weaver { _input :: E.Editor
         , _history :: [History]
         , _eventChannel :: Chan WeaverEvent
         }

makeLenses ''Weaver

historyName :: T.Name
historyName = "history"

inputName :: T.Name
inputName = "input"

mainUI :: Weaver -> [Widget]
mainUI w = [ui]
  where
    ui = _history <=> i
    _history = viewport historyName T.Vertical $ vBox $ concat $ map renderHistoryElement (w ^. history)
    i = E.renderEditor $ w ^. input

renderHistoryElement :: History -> [Widget]
renderHistoryElement h = [(str $ h ^. cmd), (str $ h ^. output)]

histScroll :: M.ViewportScroll
histScroll = M.viewportScroll historyName

weaverEvent :: Weaver -> WeaverEvent -> T.EventM (T.Next Weaver)
weaverEvent w (VtyEvent v) = appEvent w v
weaverEvent w (CommandOutput i t) = M.continue $ w & (history . ix i . output) %~ (++ t)
weaverEvent w (CommandFinished i rv) = M.continue $ w & (history . ix i . returnValue) .~ Just rv

appEvent :: Weaver -> Vty.Event -> T.EventM (T.Next Weaver)
appEvent w (Vty.EvKey Vty.KDown [])  = M.vScrollBy histScroll 1 >> M.continue w
appEvent w (Vty.EvKey Vty.KUp [])    = M.vScrollBy histScroll (-1) >> M.continue w
appEvent w (Vty.EvKey Vty.KEnter []) = forkRunCommand w >>= M.continue
appEvent w (Vty.EvKey Vty.KEsc []) = M.halt w
appEvent w ev = T.handleEventLensed w input ev >>= M.continue

launchShellProcess cmd =  P.createProcess (P.shell cmd){P.std_out = P.CreatePipe, P.std_in = P.CreatePipe}

forkRunCommand :: Weaver -> T.EventM Weaver
forkRunCommand w = do
  pid <- liftIO $ forkIO $ do
    (_, so, _, h) <- launchShellProcess t
    rv <- P.waitForProcess h
    out <- maybe (return "") hGetContents so
    writeChan (w ^. eventChannel) (CommandOutput i out)
    writeChan (w ^. eventChannel) (CommandFinished i rv)
  return emptied
  where
    appended = w & history %~ (++ [ History t Nothing "" ])
    emptied = appended & input .~ emptyInput
    i = length ( w ^. history)
    t = unlines $ E.getEditContents $ w ^. input

emptyInput :: E.Editor
emptyInput = E.editor inputName (str . unlines) (Just 1) ""

initialState :: IO Weaver
initialState = do
  ec <- Control.Concurrent.newChan
  return $ Weaver emptyInput [] ec

appCursor :: Weaver -> [T.CursorLocation] -> Maybe T.CursorLocation
appCursor _ = M.showCursorNamed inputName

app :: M.App Weaver WeaverEvent
app =
    M.App { M.appDraw = mainUI
          , M.appStartEvent = return
          , M.appHandleEvent = weaverEvent
          , M.appAttrMap = const def
          , M.appLiftVtyEvent = VtyEvent
          , M.appChooseCursor = appCursor
          }

main :: IO ()
main = do
  is <- initialState
  finalState <- M.customMain (Vty.mkVty Data.Default.def) (is ^. eventChannel) app is
  print "done"
-- main = do
--   finished <- newChan
--   let cmd = (P.shell "cat /etc/hosts"){P.std_out = P.CreatePipe, P.std_in = P.CreatePipe}
--   _ <- forkRunCommand finished cmd
--   rv <- readChan finished
--   print rv
