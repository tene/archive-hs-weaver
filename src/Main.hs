{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ()
import Control.Lens
import Control.Monad (void)
import Data.Default
import qualified Graphics.Vty as V
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

data Weaver = 
  Weaver { _input :: E.Editor
         , _history :: L.List String
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
    _history = L.renderList (w ^. history) renderHistoryElement
    i = E.renderEditor $ w ^. input

renderHistoryElement :: Bool -> String -> Widget
renderHistoryElement _ i = str i

histScroll :: M.ViewportScroll
histScroll = M.viewportScroll historyName

appEvent :: Weaver -> V.Event -> T.EventM (T.Next Weaver)
appEvent w (V.EvKey V.KDown [])  = M.vScrollBy histScroll 1 >> M.continue w
appEvent w (V.EvKey V.KUp [])    = M.vScrollBy histScroll (-1) >> M.continue w
appEvent w (V.EvKey V.KEnter []) = M.continue $ runCommand w
appEvent w (V.EvKey V.KEsc []) = M.halt w
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

app :: M.App Weaver V.Event
app =
    M.App { M.appDraw = mainUI
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const def
          , M.appLiftVtyEvent = id
          , M.appChooseCursor = appCursor
          }

main :: IO ()
main = void $ M.defaultMain app initialState
