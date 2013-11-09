{-# LANGUAGE OverloadedStrings #-}

module HelpMenu (helpMenu, HelpT, helpWidgit) where

import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All

import System.Exit ( exitSuccess )

-- Visual attributes.
msgAttr :: Attr
msgAttr = fgColor blue

data HelpT = HelpT { helpWidgit :: Widget (VCentered (HCentered
                                        (Bordered FormattedText)))
                   }

-- --------- --
-- Help Menu --
-- --------- --
helpMenu :: IO (HelpT, Widget FocusGroup)
helpMenu = do
  -- ahs <- newHandlers
  -- .
  let msg = "- <TAB> switches input elements\n\n\
            \- ordinary keystrokes edit\n\n\
            \- <SPC> toggles checkboxes\n\n\
            \- <ESC> quits"

  helpBox <- bordered =<< (textWidget wrap msg >>= withNormalAttribute msgAttr)
  setBorderedLabel helpBox "Help"
  ui <- centered helpBox
  let w = HelpT ui

  fg <- newFocusGroup
  addToFocusGroup fg ui

  fg `onKeyPressed` \_ k _ ->
    case k of
      KEsc -> exitSuccess
      _ -> return False

  return (w, fg)
