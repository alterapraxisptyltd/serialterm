{-# LANGUAGE OverloadedStrings #-}

module ConfigMenu where

-- import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All
-- import System.Locale
-- import Control.Monad
import qualified Data.Text as T

-- Multi-state checkbox value type
data FlowControlType = Hardware | Software deriving (Eq, Show)

-- data SpeedT = 15220 | 9600

data Config = Config FlowControlType T.Text T.Text
                   deriving (Show)

type ConfigBoxT = Box (Box
              (Box (Box (HFixed (Box FormattedText (CheckBox FlowControlType))) FormattedText) (HFixed Edit))
              FormattedText) (HFixed Edit)

data ConfigInput = ConfigInput { configInputWidgit :: Widget ConfigBoxT
                               , speed :: Widget Edit
                               , device :: Widget Edit
                               , activateHandlers :: Handlers Config
                               }

-- ------------------ --
-- Configuration Menu --
-- ------------------ --
configMenu :: IO (ConfigInput, Widget FocusGroup)
configMenu = do
  ahs <- newHandlers
  r1 <- newMultiStateCheckbox "FlowControl" [ (Hardware, 'H')
                                           , (Software, 'S')
                                           ]
  r1State <- plainText T.empty
  rbox <- return r1State <++> return r1
  e1 <- editWidget
  e2 <- editWidget
  ui <- hFixed 4 rbox <++>
        plainText "-" <++>
        hFixed 4 e1 <++>
        plainText "-" <++>
        hFixed 4 e2

  let w = ConfigInput ui e1 e2 ahs
      doFireEvent = const $ do
        conf <- mkConfig
        fireEvent w (return . activateHandlers) conf

      mkConfig = do
        h1 <- getCheckboxState r1
        s1 <- getEditText e1
        s2 <- getEditText e2
        return $ Config h1 s1 s2

  setCheckboxState r1 Software
  -- It would be nice if we didn't have to do this, but the
  -- setCheckboxState call above will not notify any state-change
  -- handlers because the state isn't actually changing (from its
  -- original value of Chocolate, the first value in its state list).
  setText r1State "you chose: Software"

  e1 `onActivate` doFireEvent
  -- e2 `onActivate` doFireEvent

  -- r1 `onChange` \s -> when (T.length s == 3) $ focus e1
  -- e1 `onChange` \s -> when (T.length s == 3) $ focus e2

  r1 `onCheckboxChange` \v ->
      setText r1State $ T.pack ("you chose: " ++ show v)

  fgc <- newFocusGroup
  mapM_ (addToFocusGroup fgc) [e1, e2]

  return (w, fgc)
