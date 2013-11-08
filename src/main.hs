{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Graphics.Vty hiding (Button)
import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All

import System.Exit ( exitSuccess )
import System.Locale
import qualified Data.Text as T

-- Visual attributes.
fg = white
bg = black
focAttr = black `on` yellow
headerAttr = fgColor bright_green
msgAttr = fgColor blue

-- Multi-state checkbox value type
data FlowControlType = Hardware
                  | Software
                    deriving (Eq, Show)


main :: IO ()
main = do
  -- ------------ --
  -- Help Message --
  -- ------------ --
  let msg = "- <TAB> switches input elements\n\n\
            \- ordinary keystrokes edit\n\n\
            \- <SPC> toggles checkboxes\n\n\
            \- <ESC> quits"

  -- ---------------- --
  -- Build Table View --
  -- ---------------- --
      columns = [ column (ColFixed 25) `pad` (padAll 1)
                , column ColAuto `pad` (padAll 1)
                ]

  table <- newTable columns BorderFull >>=
           withNormalAttribute (bgColor blue) >>=
           withBorderAttribute (fgColor green)

  -- -------------- --
  -- Create Widgits --
  -- -------------- --
  tw <- (textWidget wrap msg) >>= withNormalAttribute msgAttr
  mainBox <- vBox table tw >>= withBoxSpacing 1

  r1 <- newMultiStateCheckbox "FlowControl" [ (Hardware, 'H')
                                            , (Software, 'S')
                                            ]

  -- XXX: split view stream..
  edit1 <- multiLineEditWidget >>= withFocusAttribute (white `on` red)
  edit2 <- multiLineEditWidget
  edit1box <- boxFixed 30 3 edit1
  edit2box <- boxFixed 30 3 edit2
  setEditLineLimit edit1 $ Just 3
  setEditLineLimit edit2 $ Just 3

  edit1Header <- textWidget wrap T.empty >>= withNormalAttribute headerAttr
  edit2Header <- textWidget wrap T.empty >>= withNormalAttribute headerAttr

  -- Single-line text editor, sends commands to UART
  e <- editWidget
  -- XXX: replace "UART" with name of device
  b <- (plainText "[UART] :: >> ") <++> (return e)

  -- ---------------------- --
  -- Configure Table Layout --
  -- ---------------------- --

  cbHeader <- plainText T.empty
  addHeadingRow_ table headerAttr ["Column 1", "Column 2"]
  addRow table $ edit1Header .|. edit1box
  addRow table $ edit2Header .|. edit2box
  addRow table $ cbHeader .|. r1
  addRow table $ b .|. b

  setCheckboxState r1 Software
  -- It would be nice if we didn't have to do this, but the
  -- setCheckboxState call above will not notify any state-change
  -- handlers because the state isn't actually changing (from its
  -- original value of Chocolate, the first value in its state list).
  setText cbHeader $ "you chose: Software"

  -- ------------ --
  -- Focus Groups --
  -- ------------ --
  fgr <- newFocusGroup
  addToFocusGroup fgr b
  addToFocusGroup fgr r1
  addToFocusGroup fgr edit1
  addToFocusGroup fgr edit2

  -- ui <- centered =<< hLimit 95 mainBox

  -- -------------- --
  -- Event Handlers --
  -- -------------- --
  r1 `onCheckboxChange` \v ->
      setText cbHeader $ T.pack $ concat ["you chose: ", show v]

  -- XXX: event handle twin view streams..
  edit1 `onChange` (setText edit1Header)
  edit2 `onChange` (setText edit2Header)

-- XXX: Handle the editor events so that onactivate we send to the serial device
--  cmd `Activation` \this ->
--    -- ?

  -- XXX: event handler for entering text in editwidget
  e `onActivate` \this ->
    getEditText this >>= (error . ("You entered: " ++) . T.unpack)

--  efg `onKeyPressed` \_ key _ ->
--    if key == KASCII 'q' then
--      exitSuccess else return False
  fgr `onKeyPressed` \_ k _ -> do
         case k of
           KEsc -> exitSuccess
           _ -> return False

  -- ---------------- --
  -- Main Collections --
  -- ---------------- --
  c <- newCollection
  -- _ <- addToCollection c ui fgr
  _ <- addToCollection c mainBox fgr

  -- ------------- --
  -- Enumlicate UI --
  -- ------------- --
  runUi c $ defaultContext { focusAttr = focAttr
                           , normalAttr = fg `on` bg
                           }
