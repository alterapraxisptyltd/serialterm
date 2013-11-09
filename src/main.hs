{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Graphics.Vty hiding (Button)
import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All

import System.Exit ( exitSuccess )
-- import System.Locale
import qualified Data.Text as T

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


  -- ------------ --
  -- Help Message --
  -- ------------ --
--  let msg = "- <TAB> switches input elements\n\n\
--            \- ordinary keystrokes edit\n\n\
--            \- <SPC> toggles checkboxes\n\n\
--            \- <ESC> quits"
--

-- Multi-state checkbox value type
data FlowControlType = Hardware
                  | Software
                    deriving (Eq, Show)


main :: IO ()
main = do
  let columns = [ column (ColFixed 25) `pad` padAll 1
                , column ColAuto `pad` padAll 1
                ]

  table <- newTable columns BorderFull >>=
           withNormalAttribute (bgColor blue) >>=
           withBorderAttribute (fgColor green)

  -- tw <- textWidget wrap msg >>= withNormalAttribute msgAttr

  -- --------------------- --
  -- Status & Command Line --
  -- --------------------- --
  -- Single-line text editor, sends commands to UART
  e <- editWidget
  -- XXX: replace "UART" with name of device
  b <- plainText "[UART] :: >> " <++> return e

  -- XXX: FIX status bar to be like vim??? dont use a table..??
  -- status widgits
  sw1 <- editWidget
  sw2 <- editWidget
  swbox1 <- boxFixed 5 1 sw1
  swbox2 <- boxFixed 5 1 sw2
  r1 <- newMultiStateCheckbox "FlowControl" [ (Hardware, 'H')
                                            , (Software, 'S')
                                            ]

  cbHeader <- plainText T.empty
  addHeadingRow_ table headerAttr ["Device", "Status"]
  addRow table $ cbHeader .|. r1
  addRow table $ swbox1 .|. swbox2

  setCheckboxState r1 Software
  -- It would be nice if we didn't have to do this, but the
  -- setCheckboxState call above will not notify any state-change
  -- handlers because the state isn't actually changing (from its
  -- original value of Chocolate, the first value in its state list).
  setText cbHeader "you chose: Software"

  cbox <- return table <--> hBorder <--> return b
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

  b1 <- return edit1box <++> vBorder <++> return edit2box
  tv <- bordered b1
  setBorderedLabel tv "Twin View"
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
  fgr <- newFocusGroup
  addToFocusGroup fgr b
  addToFocusGroup fgr r1
  addToFocusGroup fgr edit1
  addToFocusGroup fgr edit2

  -- -------------- --
  -- Event Handlers --
  -- -------------- --
  r1 `onCheckboxChange` \v ->
      setText cbHeader $ T.pack ("you chose: " ++ show v)

  -- XXX: event handle twin view streams..
  -- edit1 `onChange` setText edit1Header
  -- edit2 `onChange` setText edit2Header

  -- XXX: Handle the editor events so that onactivate we send to the serial device
  -- XXX: event handler for entering text in editwidget
  -- e `onActivate` \this ->
  --   getEditText this >>= (error . ("You entered: " ++) . T.unpack)

--  efg `onKeyPressed` \_ key _ ->
--    if key == KASCII 'q' then
--      exitSuccess else return False
  fgr `onKeyPressed` \_ k _ ->
         case k of
           KEsc -> exitSuccess
           _ -> return False

  -- ---------------- --
  -- Main Collections --
  -- ---------------- --
  c <- newCollection
  _ <- addToCollection c ui fgr

  -- ------------- --
  -- Enumlicate UI --
  -- ------------- --
  runUi c $ defaultContext { focusAttr = focAttr
                           , normalAttr = fg `on` bg
                           }
