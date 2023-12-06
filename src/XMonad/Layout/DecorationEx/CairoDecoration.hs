{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module XMonad.Layout.DecorationEx.CairoDecoration where

import Data.Word
import Data.Bits
import qualified Data.Map as M
import Numeric (readHex)
import Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Internal

import XMonad
import XMonad.Prelude
import XMonad.Layout.Decoration (ModifiedLayout, Shrinker (..))
import qualified XMonad.Layout.Decoration as D
import XMonad.Util.NamedWindows (getName)

import XMonad.Layout.DecorationEx.LayoutModifier
import XMonad.Layout.DecorationEx.Types
import XMonad.Layout.DecorationEx.DecorationStyleEx
import XMonad.Layout.DecorationEx.Widgets

import Graphics.X11.Cairo.CairoSurface

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

data CairoDecoration a = CairoDecoration
  deriving (Show, Read)

data CairoStyle = CairoStyle {
    csBgColor :: String
  , csBorderColor :: String
  , csTextColor :: String
  , csDecoBorderWidth :: Dimension
  , csDecorationBorders :: BorderColors
  }
  deriving (Show, Read)

themeC :: D.Theme -> ThemeC widget
themeC t =
    GenericTheme {
          exActive = CairoStyle (D.activeColor t) (D.activeBorderColor t) (D.activeTextColor t) (D.activeBorderWidth t) (borderColor $ D.activeColor t)
        , exInactive = CairoStyle (D.inactiveColor t) (D.inactiveBorderColor t) (D.inactiveTextColor t) (D.inactiveBorderWidth t) (borderColor $ D.inactiveColor t)
        , exUrgent = CairoStyle (D.urgentColor t) (D.urgentBorderColor t) (D.urgentTextColor t) (D.urgentBorderWidth t) (borderColor $ D.urgentColor t)
        , exPadding = BoxBorders 0 4 0 4
        , exFontName = D.fontName t
        , exDecoWidth = D.decoWidth t
        , exDecoHeight = D.decoHeight t
        , exOnDecoClick = M.empty
        , exDragWindowButtons = [1]
        , exWidgetsLeft = []
        , exWidgetsCenter = []
        , exWidgetsRight = []
      }

type ThemeC widget = GenericTheme CairoStyle widget

instance ClickHandler (GenericTheme CairoStyle) StandardWidget where
  onDecorationClick theme button = M.lookup button (exOnDecoClick theme)
  isDraggingEnabled theme button = button `elem` exDragWindowButtons theme

instance (Show widget, Read widget, Read (WidgetCommand widget), Show (WidgetCommand widget),
          Read (ThemeC widget), Show (ThemeC widget))
        => ThemeAttributes (ThemeC widget) where
  type Style (ThemeC widget) = CairoStyle
  selectWindowStyle theme w = windowStyle w theme
  defaultBgColor t = csBgColor $ exInactive t
  decorationSize t = (exDecoWidth t, exDecoHeight t)
  widgetsPadding = exPadding
  themeFontName = exFontName

instance DecorationStyleEx CairoDecoration Window where
  type Theme CairoDecoration = GenericTheme CairoStyle
  type Widget CairoDecoration = StandardWidget
  type DecorationPaintingContext CairoDecoration = Surface
  type DecorationStyleState CairoDecoration = String

  describeDecoration _ = "CairoDecoration"

  initializeState _ theme = return $ themeFontName theme
  releaseStateResources _ _ = return ()

  getShrinkedWindowName dstyle shrinker font win wh ht = do
    let calcWidth text =
          io $ withImageSurface FormatARGB32 (fi wh) (fi ht) $ \surface ->
            renderWith surface $ do
              setFontSize 12
              selectFontFace font FontSlantNormal FontWeightNormal
              ext <- Cairo.textExtents text
              return $ round $ textExtentsWidth ext
    -- xmonad-contrib #809
    -- qutebrowser will happily shovel a 389K multiline string into @_NET_WM_NAME@
    -- and the 'defaultShrinker' (a) doesn't handle multiline strings well (b) is
    -- quadratic due to using 'init'
    nw  <- fmap (take 2048 . takeWhile (/= '\n') . show) (getName win)
    let s = shrinkIt shrinker
    D.shrinkWhile s (\n -> do
                           size <- calcWidth n
                           return $ size > fromIntegral wh - fromIntegral (ht `div` 2)) nw

  calcWidgetPlace dstyle dd widget = do
    let decoRect = ddDecoRect dd
        decoWidth = fi $ rect_width decoRect
        decoHeight = fi $ rect_height decoRect
    str <- widgetString dd widget
    io $ withImageSurface FormatARGB32 decoWidth decoHeight $ \surface ->
      renderWith surface $ do
        setFontSize 12
        ext <- Cairo.textExtents str
        let textWidth = textExtentsWidth ext
            textHeight = textExtentsHeight ext
            y = (fi decoHeight - textHeight) / 2.0
            y0 = round $ y - textExtentsYbearing ext
            rect = Rectangle 0 (round y) (round textWidth) (round textHeight)
        return $ WidgetPlace y0 rect

  placeWidgets = defaultPlaceWidgets

  paintDecoration dstyle win windowWidth windowHeight dd = do
    dpy <- asks display
    let widgets = widgetLayout $ ddLabels dd
        style = ddStyle dd
        scr = defaultScreenOfDisplay dpy
        visual = defaultVisualOfScreen scr
        (bgR, bgG, bgB) = stringToColor (csBgColor style)
    surface <- io $ createXlibSurface dpy win visual (fi windowWidth) (fi windowHeight)
    io $ renderWith surface $ do
      setSourceRGB bgR bgG bgB
      rectangle 0 0 (fi windowWidth) (fi windowHeight)
      fill
    forM_ (zip widgets $ ddWidgetPlaces dd) $ \(widget, place) ->
      paintWidget dstyle surface place dd widget
    io $ Internal.surfaceDestroy surface

  paintWidget dstyle surface place dd widget = do
    dpy <- asks display
    let style = ddStyle dd
        x = rect_x (wpRectangle place)
        y = wpTextYPosition place
        decoRect = ddDecoRect dd
        decoWidth = fi $ rect_width decoRect
        decoHeight = fi $ rect_height decoRect
        (textR, textG, textB) = stringToColor (csTextColor style)
    str <- widgetString dd widget
    renderWith surface $ do
      setSourceRGB textR textG textB
      setFontSize 12
      selectFontFace (ddStyleState dd) FontSlantNormal FontWeightNormal
      moveTo (fi x) (fi y)
      showText str
      
cairoDecoration :: (Shrinker shrinker) => shrinker -> ThemeC StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx CairoDecoration shrinker) l Window
cairoDecoration s theme = decorationEx s theme CairoDecoration

