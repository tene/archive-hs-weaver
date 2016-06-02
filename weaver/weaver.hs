{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Concurrent     (Chan, forkIO, newChan, writeChan)
import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.UTF8   as BSU8
import qualified Data.Default           (def)
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Foreign.Marshal.Array  (allocaArray, peekArray)
import           GHC.IO.Exception       (ExitCode (..))
import           GHC.IO.Handle.Types    (Handle (..))
import qualified Graphics.Vty           as Vty

import           Brick.AttrMap
import qualified Brick.Main             as M
import           Brick.Types            (Name, Padding (..), ViewportType (..),
                                         Widget)
import qualified Brick.Types            as Types
import           Brick.Util             (bg, fg, on)
import           Brick.Widgets.Core     (padRight, str, vBox, vLimit, viewport,
                                         withAttr, (<+>), (<=>))
import qualified Brick.Widgets.Edit     as E

import           System.IO              (hGetBufSome, hSetBinaryMode)
import qualified System.Process         as P

import           Weaver

data UIEvent =
  VtyEvent Vty.Event
  | WEvent WeaverEvent
  deriving Show

data History =
  History { _cmd         :: String
          , _returnValue :: Maybe Integer
          , _output      :: BS.ByteString
          }

makeLenses ''History

-- XXX TODO replace the List with a viewport of Historys
data Weaver =
  Weaver { _input        :: E.Editor
         , _history      :: [History]
         , _eventChannel :: Chan UIEvent
         }

makeLenses ''Weaver

historyName :: Name
historyName = "history"

inputName :: Name
inputName = "input"

mainUI :: Weaver -> [Widget]
mainUI w = [ui]
  where
    ui = _history <=> i
    _history = viewport historyName Vertical $ vBox $ concat $ zipWith renderHistoryElement (w ^. history) [1..]
    i = E.renderEditor $ w ^. input

outputViewSize :: Int
outputViewSize = 5

renderHistoryElement :: History -> Integer -> [Widget]
renderHistoryElement h i =
  [ withAttr (attrName "command") $ sigil <+> (padRight Max $ str $ h ^. cmd)
  , withAttr (attrName "output") $ vLimit outputViewSize $ viewport (Types.Name $ "output" ++ show i) Vertical (str $ BSU8.toString$ h ^. output)
  ]
    where
      sigil = case (h ^. returnValue) of
                Just 0  -> withAttr (attrName "success") $ str "✔"
                Just _  -> withAttr (attrName "failure") $ str "✘"
                Nothing -> str "…"

histScroll :: M.ViewportScroll
histScroll = M.viewportScroll historyName

weaverEvent :: Weaver -> UIEvent -> Types.EventM (Types.Next Weaver)
weaverEvent w (VtyEvent v) = appEvent w v
weaverEvent w (WEvent (ProcessOutput i t)) = M.continue $ w & (history . ix (getProcessId i) . output) %~ (\ x -> BS.append x t)
weaverEvent w (WEvent (ProcessTerminated i rv)) = M.continue $ w & (history . ix (getProcessId i) . returnValue) .~ Just rv

appEvent :: Weaver -> Vty.Event -> Types.EventM (Types.Next Weaver)
appEvent w (Vty.EvKey Vty.KDown [])  = M.vScrollBy histScroll 1 >> M.continue w
appEvent w (Vty.EvKey Vty.KUp [])    = M.vScrollBy histScroll (-1) >> M.continue w
appEvent w (Vty.EvKey Vty.KEnter []) = forkRunCommand w >>= M.continue
appEvent w (Vty.EvKey Vty.KEsc []) = M.halt w
appEvent w ev = Types.handleEventLensed w input ev >>= M.continue

launchShellProcess :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, P.ProcessHandle)
launchShellProcess shellCommandText =  P.createProcess (P.shell shellCommandText){P.std_out = P.CreatePipe, P.std_in = P.CreatePipe, P.std_err = P.CreatePipe}

readBufferSize :: Int
readBufferSize = 1024

streamOutput :: Handle -> Chan UIEvent -> Int -> IO ()
streamOutput h c i = do
  hSetBinaryMode h True
  allocaArray readBufferSize innerloop
    where
      innerloop buffer = do
        count <- hGetBufSome h buffer readBufferSize
        bytes <- peekArray count buffer
        let result = BS.pack bytes
        if count > 0
           then writeChan c (WEvent $ ProcessOutput (ProcessId i) result) >> innerloop buffer
           else return ()

unExitCode :: ExitCode -> Integer
unExitCode ExitSuccess = 0
unExitCode (ExitFailure n) = toInteger n

forkRunCommand :: Weaver -> Types.EventM Weaver
forkRunCommand w = do
  _ <- liftIO $ forkIO $ do
    (_, so, _, h) <- launchShellProcess cmd
    _ <- forkIO $ streamOutput (fromJust so) (w ^. eventChannel) i
    rv <- P.waitForProcess h
    writeChan (w ^. eventChannel) (WEvent $ ProcessTerminated (ProcessId i) (unExitCode rv))
  return emptied
  where
    appended = w & history %~ (++ [ History t Nothing "" ])
    emptied = appended & input .~ emptyInput
    i = length ( w ^. history)
    cmd = unlines $ E.getEditContents $ w ^. input
    t = unlines $ E.getEditContents $ w ^. input

emptyInput :: E.Editor
emptyInput = E.editor inputName (str . unlines) (Just 1) ""

initialState :: IO Weaver
initialState = do
  ec <- Control.Concurrent.newChan
  return $ Weaver emptyInput [] ec

appCursor :: Weaver -> [Types.CursorLocation] -> Maybe Types.CursorLocation
appCursor _ = M.showCursorNamed inputName

weaverAttrMap :: AttrMap
weaverAttrMap = attrMap ((Vty.Color240 239) `on` (Vty.Color240 216))
  [ ("command", bg $ Vty.Color240 217)
  , ("success", fg Vty.green)
  , ("failure", fg Vty.red)
  ]

app :: M.App Weaver UIEvent
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
