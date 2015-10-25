{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent
  ( writeChan
  , forkIO
  , newChan
  , Chan
  )
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.Default (def)
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import Foreign.Marshal.Array
  ( allocaArray
  , peekArray
  )
import GHC.IO.Exception (ExitCode(..))
import GHC.IO.Handle.Types (Handle(..))
import qualified Graphics.Vty as Vty

import Brick.AttrMap
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import Brick.Types
  ( Widget
  )
import Brick.Util
  ( on
  , fg
  , bg
  )
import Brick.Widgets.Core
  ( padRight
  , str
  , vBox
  , viewport
  , withAttr
  , (<=>)
  )

import qualified System.Process as P
import System.IO
  ( hSetBinaryMode
  , hGetBufSome
  )

data WeaverEvent =
  VtyEvent Vty.Event
  | CommandOutput Int String
  | CommandFinished Int ExitCode
  deriving Show

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
renderHistoryElement h = [withAttr (attrName "command") (padRight T.Max $ str $ h ^. cmd), withAttr (attrName "output") (str $ h ^. output)]

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

launchShellProcess :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
launchShellProcess shellCommandText =  P.createProcess (P.shell shellCommandText){P.std_out = P.CreatePipe, P.std_in = P.CreatePipe}

readBufferSize :: Int
readBufferSize = 1024

streamOutput :: Handle -> Chan WeaverEvent -> Int -> IO ()
streamOutput h c i = do
  hSetBinaryMode h True
  allocaArray readBufferSize innerloop
    where
      innerloop buffer = do
        count <- hGetBufSome h buffer readBufferSize
        bytes <- peekArray count buffer
        let result = BS8.toString $ BS.pack bytes
        if count > 0
           then writeChan c (CommandOutput i result) >> innerloop buffer
           else return ()

forkRunCommand :: Weaver -> T.EventM Weaver
forkRunCommand w = do
  _ <- liftIO $ forkIO $ do
    (_, so, _, h) <- launchShellProcess t
    _ <- forkIO $ streamOutput (fromJust so) (w ^. eventChannel) i
    rv <- P.waitForProcess h
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

weaverAttrMap :: AttrMap
weaverAttrMap = attrMap ((Vty.Color240 239) `on` (Vty.Color240 216))
  [ ("command", bg $ Vty.Color240 217)
  , ("error", fg Vty.red)
  ]

app :: M.App Weaver WeaverEvent
app =
    M.App { M.appDraw = mainUI
          , M.appStartEvent = return
          , M.appHandleEvent = weaverEvent
          , M.appAttrMap = const weaverAttrMap
          , M.appLiftVtyEvent = VtyEvent
          , M.appChooseCursor = appCursor
          }

main :: IO ()
main = do
  is <- initialState
  _ <- M.customMain (Vty.mkVty Data.Default.def) (is ^. eventChannel) app is
  return ()
-- main = do
--   finished <- newChan
--   let cmd = (P.shell "cat /etc/hosts"){P.std_out = P.CreatePipe, P.std_in = P.CreatePipe}
--   _ <- forkRunCommand finished cmd
--   rv <- readChan finished
--   print rv
