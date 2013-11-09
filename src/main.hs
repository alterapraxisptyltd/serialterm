{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Graphics.Vty hiding (Button)
import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All

import System.Exit ( exitSuccess )
-- import System.Locale
-- import Control.Monad
import qualified Data.Text as T

--
-- ..
data StatusT = Connected | Disconnected deriving Show

--
-- ..
appName :: T.Text
appName = "HardTerm"

historySize :: Int
historySize = 100

-- Visual attributes.
fg, bg :: Color
fg = white
bg = black
focAttr, headerAttr, msgAttr :: Attr
focAttr = black `on` yellow
headerAttr = fgColor bright_green
msgAttr = fgColor blue

--
-- ..
main :: IO ()
main = do
--  let columns = [ column (ColFixed 25) `pad` padAll 1
--                , column ColAuto `pad` padAll 1
--                ]
--
--  table <- newTable columns BorderFull >>=
--           withNormalAttribute (bgColor blue) >>=
--           withBorderAttribute (fgColor green)
--

  -- --------------------- --
  -- Status & Command Line --
  -- --------------------- --
  -- Single-line text editor, sends commands to UART
  e <- editWidget
  -- XXX: replace "UART" with name of device
  b <- plainText "[UART] :: >> " <++> return e

  sbox1 <- plainText "Speed : 15200 bps" <++> vBorder <++> plainText "Device : .." >>= withBoxSpacing 1
  sbox2 <- plainText "Status : Disconnected" <++> plainText ".." >>= withBoxSpacing 1
  sbox <- return sbox1 <++> vBorder <++> return sbox2
  sbar <- boxFixed 40 1 sbox

  cbox <- return sbar <--> hBorder <--> return b
  cui <- bordered cbox

  -- --------- --
  -- Twin View --
  -- --------- --
  -- XXX: split (or n?) view stream..
  -- XXX: geometry ok? 40+40=80 colums boxFixed change to vBox ?
  edit1 <- multiLineEditWidget >>= withFocusAttribute (white `on` red)
  edit2 <- multiLineEditWidget
  edit1box <- boxFixed 40 historySize edit1
  edit2box <- boxFixed 40 historySize edit2
  setEditLineLimit edit1 $ Just historySize
  setEditLineLimit edit2 $ Just historySize

  tv <- bordered =<< return edit1box <++> vBorder <++> return edit2box
  vui <- centered =<< hLimit 95 tv

  -- ------------- --
  -- Build main UI --
  -- ------------- --

  b2 <- return vui <--> hBorder <--> return cui
  mainBox <- bordered b2
  setBorderedLabel mainBox appName
  ui <- centered mainBox

  -- ------------ --
  -- Focus Groups --
  -- ------------ --
  fgr <- newFocusGroup -- main focus
  fgh <- newFocusGroup -- help menu focus
  addToFocusGroup fgr b
  -- addToFocusGroup fgr edit1
  -- addToFocusGroup fgr edit2

  -- --------- --
  -- Help Menu --
  -- --------- --
  let msg = "- <TAB> switches input elements\n\n\
            \- ordinary keystrokes edit\n\n\
            \- <SPC> toggles checkboxes\n\n\
            \- <ESC> quits"

  helpBox <- bordered =<< (textWidget wrap msg >>= withNormalAttribute msgAttr)
  setBorderedLabel helpBox "Help"
  hui <- centered helpBox

  addToFocusGroup fgh hui

  -- ---------------- --
  -- Main Collections --
  -- ---------------- --
  c <- newCollection
  hc <- newCollection
  changeToHelp <- addToCollection hc hui fgh
  changeToMain <- addToCollection c ui fgr

  -- -------------- --
  -- Event Handlers --
  -- -------------- --

  -- XXX: event handle twin view streams..
  -- edit1 `onChange` setText edit1Header
  -- edit2 `onChange` setText edit2Header

  fgr `onKeyPressed` \_ k _ ->
    case k of
      KEsc -> exitSuccess
      KASCII 'z' -> return True
      KASCII 'h' -> changeToHelp >> return True
      _ -> return False

  fgh `onKeyPressed` \_ k _ ->
    case k of
      KEsc -> changeToMain >> return True

  -- ------------- --
  -- Enumlicate UI --
  -- ------------- --
  runUi c $ defaultContext { focusAttr = focAttr
                           , normalAttr = fg `on` bg
                           }
