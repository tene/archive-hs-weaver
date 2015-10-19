{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ()
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad (void)
import Data.Default
import GHC.IO.Exception (ExitCode(..))
import qualified Graphics.Vty as Vty
import qualified Data.Vector as Vector

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( str
  , (<=>)
  )

import qualified System.Process as P
import System.IO
  ( hGetContents
  )

-- XXX TODO replace the List with a viewport of Historys
data Weaver =
  Weaver { _input :: E.Editor
         , _history :: L.List String
         }

makeLenses ''Weaver

data History =
  History { _returnValue :: Maybe ExitCode
          , _output :: [String]
          }

makeLenses ''History

data WeaverEvent =
  VtyEvent Vty.Event
  | CommandOutput [String]
  | CommandFinished ExitCode

historyName :: T.Name
historyName = "history"

inputName :: T.Name
inputName = "input"

mainUI :: Weaver -> [Widget]
mainUI w = [ui]
  where
    ui = _history <=> i
    _history = L.renderList (w ^. history) renderHistoryElement
    i = E.renderEditor $ w ^. input

renderHistoryElement :: Bool -> String -> Widget
renderHistoryElement _ i = str i

histScroll :: M.ViewportScroll
histScroll = M.viewportScroll historyName

weaverEvent :: Weaver -> WeaverEvent -> T.EventM (T.Next Weaver)
weaverEvent w (VtyEvent v) = appEvent w v

appEvent :: Weaver -> Vty.Event -> T.EventM (T.Next Weaver)
appEvent w (Vty.EvKey Vty.KDown [])  = M.vScrollBy histScroll 1 >> M.continue w
appEvent w (Vty.EvKey Vty.KUp [])    = M.vScrollBy histScroll (-1) >> M.continue w
appEvent w (Vty.EvKey Vty.KEnter []) = M.continue $ runCommand w
appEvent w (Vty.EvKey Vty.KEsc []) = M.halt w
appEvent w ev = T.handleEventLensed w input ev >>= M.continue

runCommand :: Weaver -> Weaver
runCommand w = emptied
  where
    appended = w & history %~ (L.listInsert i t)
    emptied = appended & input .~ emptyInput
    i = Vector.length $ w ^. history ^. L.listElementsL
    t = unlines $ E.getEditContents $ w ^. input

emptyInput :: E.Editor
emptyInput = E.editor inputName (str . unlines) (Just 1) ""

initialState :: Weaver
initialState = Weaver emptyInput (L.list historyName (Vector.fromList ["ohai","lol"]) 1)

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

forkRunCommand :: Chan (ExitCode, String) -> P.CreateProcess -> IO ThreadId
forkRunCommand chan cmd = forkIO $ do
  (_, so, _, h) <- P.createProcess cmd
  rv <- P.waitForProcess h
  out <- maybe (return "") hGetContents so
  writeChan chan (rv, out)

main :: IO ()
-- main = void $ M.defaultMain app initialState
main = do
  finished <- newChan
  let cmd = (P.shell "cat /etc/hosts"){P.std_out = P.CreatePipe, P.std_in = P.CreatePipe}
  _ <- forkRunCommand finished cmd
  rv <- readChan finished
  print rv
