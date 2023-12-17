{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module XMonad.Layout.DecorationEx.Cairo.Theme (
    ImageUsage (..),
    Fill (..),
    PanelBackground (..),
    GradientStops (..), GradientType (..),
    FontWeight (..), FontSlant (..),
    CairoStyle (..),
    CairoTheme (..),
    simpleGradient,
    stripesGradient,
    themeC,
    stringToColor,
    getActiveBorderColor, getInactiveBorderColor
  ) where

import Data.Word
import Data.Bits
import qualified Data.Map as M
import Data.Default
import Numeric (readHex)
import Graphics.Rendering.Cairo as Cairo

import XMonad
import XMonad.Prelude (fi)
import qualified XMonad.Layout.Decoration as D
import XMonad.Layout.DecorationEx

deriving instance Read FontWeight
deriving instance Read FontSlant

type CairoColor = (Double, Double, Double)

parseHex :: String -> Word32
parseHex s = case readHex s of
               [(result, "")] -> result
               _ -> 0

stringToColor :: String -> CairoColor
stringToColor ('#' : hx) =
  let x = parseHex hx
      b = x .&. 0x0000ff
      g = (x .&. 0x00ff00) `shiftR` 8
      r = (x .&. 0xff0000) `shiftR` 16
  in  (fromIntegral r / 255.0, fromIntegral g / 255.0, fromIntegral b / 255.0)
stringToColor _ = (0.0, 0.0, 0.0)

data CairoTheme widget = CairoTheme {
    ctActive :: !CairoStyle
  , ctInactive :: !CairoStyle
  , ctUrgent :: !CairoStyle
  , ctPadding :: !(BoxBorders Dimension)
  , ctFontName :: !String
  , ctFontSize :: !Int
  , ctFontWeight :: !FontWeight
  , ctFontSlant :: !FontSlant
  , ctOnDecoClick :: !(M.Map Int (WidgetCommand widget))
  , ctDragWindowButtons :: ![Int]
  , ctIconsPath :: !FilePath
  , ctWidgetsLeft :: ![widget]
  , ctWidgetsCenter :: ![widget]
  , ctWidgetsRight :: ![widget]
  }

deriving instance (Show widget, Show (WidgetCommand widget)) => Show (CairoTheme widget)
deriving instance (Read widget, Read (WidgetCommand widget)) => Read (CairoTheme widget)

instance HasWidgets CairoTheme widget where
  themeWidgets t = WidgetLayout (ctWidgetsLeft t) (ctWidgetsCenter t) (ctWidgetsRight t)

data GradientType = Vertical | Horizontal
  deriving (Eq, Show, Read)

type GradientStops = [(Double, String)]

data ImageUsage = TileImage | ScaleImage
  deriving (Eq, Show, Read)

data Fill =
    Flat !String
  | Gradient !GradientType !GradientStops
  | Image !ImageUsage !FilePath
  deriving (Eq, Show, Read)

data PanelBackground = PanelBackground {
    cpLeftImage :: !(Maybe FilePath)
  , cpMiddle :: !(Maybe Fill)
  , cpRightImage :: !(Maybe FilePath)
  }
  deriving (Eq, Show, Read)

instance Default PanelBackground where
  def = PanelBackground Nothing Nothing Nothing

data CairoStyle = CairoStyle {
    csBackground :: !Fill
  , csWidgetsBackground :: !(Maybe PanelBackground, Maybe PanelBackground, Maybe PanelBackground)
  , csPadCentralPanelForWidgets :: !Bool
  , csCentralPanelBackground :: !PanelBackground
  , csTextColor :: !String
  , csDecoBorderWidth :: !Dimension
  , csDecorationBorders :: !BorderColors
  }
  deriving (Show, Read)

simpleGradient :: String -> String -> GradientStops
simpleGradient from to = [(0.0, from), (1.0, to)]

stripesGradient :: Int -> String -> String -> GradientStops
stripesGradient nStops color1 color2 =
  let stops = [fi i / fi nStops | i <- [0 .. nStops]]
      colors = take nStops $ cycle [color1, color2]
  in  zip stops colors

themeC :: D.Theme -> CairoTheme StandardWidget
themeC t =
    CairoTheme {
          ctActive = style (D.activeColor t) (D.activeBorderColor t) (D.activeTextColor t) (D.activeBorderWidth t)
        , ctInactive = style (D.inactiveColor t) (D.inactiveBorderColor t) (D.inactiveTextColor t) (D.inactiveBorderWidth t) 
        , ctUrgent = style (D.urgentColor t) (D.urgentBorderColor t) (D.urgentTextColor t) (D.urgentBorderWidth t)
        , ctPadding = BoxBorders 0 4 0 4
        , ctFontName = D.fontName t
        , ctFontSize = 12
        , ctFontWeight = FontWeightNormal
        , ctFontSlant = FontSlantNormal
        , ctOnDecoClick = M.fromList [(1, FocusWindow)]
        , ctDragWindowButtons = [1]
        , ctIconsPath = "."
        , ctWidgetsLeft = []
        , ctWidgetsCenter = []
        , ctWidgetsRight = []
      }
  where
    style bgColor brdColor textColor borderWidth =
      CairoStyle {
        csBackground = Flat bgColor,
        csWidgetsBackground = (Nothing, Nothing, Nothing),
        csPadCentralPanelForWidgets = True,
        csCentralPanelBackground = def,
        csTextColor = textColor,
        csDecoBorderWidth = borderWidth,
        csDecorationBorders = borderColor brdColor
      }

instance ClickHandler CairoTheme StandardWidget where
  onDecorationClick theme button = M.lookup button (ctOnDecoClick theme)
  isDraggingEnabled theme button = button `elem` ctDragWindowButtons theme

instance (Show widget, Read widget, Read (WidgetCommand widget), Show (WidgetCommand widget),
          Read (CairoTheme widget), Show (CairoTheme widget))
        => ThemeAttributes (CairoTheme widget) where
  type Style (CairoTheme widget) = CairoStyle

  selectWindowStyle theme win = do
    styleType <- windowStyleType win
    return $ case styleType of
               ActiveWindow -> ctActive theme
               InactiveWindow -> ctInactive theme
               UrgentWindow -> ctUrgent theme

  defaultBgColor t = "#888888"
  widgetsPadding = ctPadding
  themeFontName = ctFontName

getActiveBorderColor :: CairoTheme widget -> String
getActiveBorderColor theme = bxBottom $ csDecorationBorders $ ctActive theme

getInactiveBorderColor :: CairoTheme widget -> String
getInactiveBorderColor theme = bxBottom $ csDecorationBorders $ ctInactive theme

