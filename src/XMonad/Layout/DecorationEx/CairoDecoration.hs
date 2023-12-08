{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module XMonad.Layout.DecorationEx.CairoDecoration where

import Control.Concurrent
import Data.Word
import Data.Bits
import qualified Data.Map as M
import Numeric (readHex)
import Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Internal
import System.FilePath

import XMonad
import XMonad.Prelude
import XMonad.Layout.Decoration (ModifiedLayout, Shrinker (..))
import qualified XMonad.Layout.Decoration as D
import XMonad.Util.Types

import XMonad.Layout.DecorationEx

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

data CairoTheme widget = CairoTheme {
    ctActive :: CairoStyle
  , ctInactive :: CairoStyle
  , ctUrgent :: CairoStyle
  , ctPadding :: BoxBorders Dimension
  , ctFontName :: String
  , ctFontSize :: Int
  , ctDecoWidth :: Dimension
  , ctDecoHeight :: Dimension
  , ctOnDecoClick :: M.Map Int (WidgetCommand widget)
  , ctDragWindowButtons :: [Int]
  , ctIconsPath :: FilePath
  , ctWidgetsLeft :: [widget]
  , ctWidgetsCenter :: [widget]
  , ctWidgetsRight :: [widget]
  }

deriving instance (Show widget, Show (WidgetCommand widget)) => Show (CairoTheme widget)
deriving instance (Read widget, Read (WidgetCommand widget)) => Read (CairoTheme widget)

instance HasWidgets CairoTheme widget where
  themeWidgets t = WidgetLayout (ctWidgetsLeft t) (ctWidgetsCenter t) (ctWidgetsRight t)

data CairoDecoration a = CairoDecoration
  deriving (Show, Read)

data GradientType = Vertical | Horizontal
  deriving (Eq, Show, Read)

type GradientStops = [(Double, String)]

data Background =
    Flat String
  | Gradient GradientType GradientStops
  deriving (Eq, Show, Read)

data CairoStyle = CairoStyle {
    csBackground :: Background
  , csBorderColor :: String
  , csTextColor :: String
  , csDecoBorderWidth :: Dimension
  , csDecorationBorders :: BorderColors
  }
  deriving (Show, Read)

simpleGradient :: String -> String -> GradientStops
simpleGradient from to = [(0.0, from), (1.0, to)]

stripesGradient :: Int -> String -> String -> GradientStops
stripesGradient nStops color1 color2 =
  let stops = [fi i / fi nStops | i <- [0 .. nStops]]
      colors = take nStops $ cycle [color1, color2]
  in  zip stops colors

themeC :: D.Theme -> CairoTheme widget
themeC t =
    CairoTheme {
          ctActive = CairoStyle (Flat $ D.activeColor t) (D.activeBorderColor t) (D.activeTextColor t) (D.activeBorderWidth t) (borderColor $ D.activeColor t)
        , ctInactive = CairoStyle (Flat $ D.inactiveColor t) (D.inactiveBorderColor t) (D.inactiveTextColor t) (D.inactiveBorderWidth t) (borderColor $ D.inactiveColor t)
        , ctUrgent = CairoStyle (Flat $ D.urgentColor t) (D.urgentBorderColor t) (D.urgentTextColor t) (D.urgentBorderWidth t) (borderColor $ D.urgentColor t)
        , ctPadding = BoxBorders 0 4 0 4
        , ctFontName = D.fontName t
        , ctFontSize = 12
        , ctDecoWidth = D.decoWidth t
        , ctDecoHeight = D.decoHeight t
        , ctOnDecoClick = M.empty
        , ctDragWindowButtons = [1]
        , ctIconsPath = "."
        , ctWidgetsLeft = []
        , ctWidgetsCenter = []
        , ctWidgetsRight = []
      }

instance ClickHandler CairoTheme StandardWidget where
  onDecorationClick theme button = M.lookup button (ctOnDecoClick theme)
  isDraggingEnabled theme button = button `elem` ctDragWindowButtons theme

instance HasDecorationSize (CairoTheme widget) where
  decorationSize t = (ctDecoWidth t, ctDecoHeight t)

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

type ImagesCache = M.Map String Surface

data CairoDecoStyleState = CairoDecoStyleState {
    cdssFontName :: String
  , cdssFontSize :: Int
  , cdssIconsPath :: FilePath
  , cdssImages :: MVar ImagesCache
  }

instance DecorationEngine CairoDecoration Window where
  type Theme CairoDecoration = CairoTheme
  type Widget CairoDecoration = StandardWidget
  type DecorationPaintingContext CairoDecoration = Surface
  type DecorationEngineState CairoDecoration = CairoDecoStyleState

  describeEngine _ = "CairoDecoration"

  initializeState _ _ theme = do
    var <- io $ newMVar M.empty
    return $ CairoDecoStyleState {
      cdssFontName = ctFontName theme,
      cdssFontSize = ctFontSize theme,
      cdssIconsPath = ctIconsPath theme,
      cdssImages = var
    }

  releaseStateResources _ st = do
    mbCache <- io $ tryTakeMVar (cdssImages st)
    whenJust mbCache $ \cache ->
      forM_ (M.elems cache) $ \surface ->
        io $ Internal.surfaceDestroy surface

  getShrinkedWindowName dstyle shrinker st name wh ht = do
    let calcWidth text =
          io $ withImageSurface FormatARGB32 (fi wh) (fi ht) $ \surface ->
            renderWith surface $ do
              setFontSize (fi $ cdssFontSize st)
              selectFontFace (cdssFontName st) FontSlantNormal FontWeightNormal
              ext <- Cairo.textExtents text
              return $ round $ textExtentsWidth ext
    let s = shrinkIt shrinker
    D.shrinkWhile s (\n -> do
                           size <- calcWidth n
                           return $ size > fromIntegral wh) name

  placeWidgets = defaultPlaceWidgets

  paintDecoration dstyle win windowWidth windowHeight shrinker dd = do
      dpy <- asks display
      let widgets = widgetLayout $ ddLabels dd
          style = ddStyle dd
          borders = csDecorationBorders style
          borderWidth = fi $ csDecoBorderWidth style
          scr = defaultScreenOfDisplay dpy
          visual = defaultVisualOfScreen scr
          widthD = fi windowWidth :: Double
          heightD = fi windowHeight :: Double
      surface <- io $ createXlibSurface dpy win visual (fi windowWidth) (fi windowHeight)
      io $ renderWith surface $ do
        paintBackground (csBackground style) widthD heightD
        drawLineWith 0 0 widthD borderWidth (bxTop borders)
        drawLineWith 0 0 borderWidth widthD (bxLeft borders)
        drawLineWith 0 (heightD - borderWidth) widthD borderWidth (bxBottom borders)
        drawLineWith (widthD - borderWidth) 0 borderWidth heightD (bxRight borders)
      forM_ (zip widgets $ ddWidgetPlaces dd) $ \(widget, place) ->
        paintWidget dstyle surface place shrinker dd widget
      io $ Internal.surfaceDestroy surface
    where
      drawLineWith x y w h color = do
        let (r,g,b) = stringToColor color
        setSourceRGB r g b
        rectangle x y w h
        fill

  calcWidgetPlace dstyle dd widget = do
    let decoRect = ddDecoRect dd
        decoWidth = fi $ rect_width decoRect
        decoHeight = fi $ rect_height decoRect
    res <- getWidgetImage dd widget
    case res of
      Left str -> do
        io $ withImageSurface FormatARGB32 decoWidth decoHeight $ \surface ->
          renderWith surface $ do
            setFontSize (fi $ cdssFontSize $ ddStyleState dd)
            selectFontFace (cdssFontName $ ddStyleState dd) FontSlantNormal FontWeightNormal
            ext <- Cairo.textExtents str
            let textWidth = textExtentsWidth ext
                textHeight = textExtentsHeight ext
                y = (fi decoHeight - textHeight) / 2.0
                y0 = round $ y - textExtentsYbearing ext
                rect = Rectangle 0 (round y) (round textWidth) (round textHeight)
            return $ WidgetPlace y0 rect
      Right image -> do
        imgWidth <- io $ imageSurfaceGetWidth image
        imgHeight <- io $ imageSurfaceGetHeight image
        let width' = (fi imgWidth / fi imgHeight) * fi decoHeight
        return $ WidgetPlace 0 $ Rectangle 0 0 (round width') (fi decoHeight)

  paintWidget engine surface place shrinker dd widget = do
    dpy <- asks display
    let style = ddStyle dd
        rect = wpRectangle place
        decoRect = ddDecoRect dd
        decoWidth = fi $ rect_width decoRect
        decoHeight = fi $ rect_height decoRect
        (textR, textG, textB) = stringToColor (csTextColor style)
    res <- getWidgetImage dd widget 
    case res of
      Left str -> do
        let x = rect_x rect
            y = wpTextYPosition place
        str' <- if isShrinkable widget
                  then getShrinkedWindowName engine shrinker (ddStyleState dd) str (rect_width rect) (rect_height rect) 
                  else return str
        renderWith surface $ do
          setSourceRGB textR textG textB
          setFontSize (fi $ cdssFontSize $ ddStyleState dd)
          selectFontFace (cdssFontName $ ddStyleState dd) FontSlantNormal FontWeightNormal
          moveTo (fi x) (fi y)
          showText str'
      Right image -> do
        renderWith surface $ do
          setSourceSurface image (fi $ rect_x rect) (fi $ rect_y rect)
          rectangle (fi $ rect_x rect) (fi $ rect_y rect) (fi $ rect_width rect) (fi $ rect_height rect)
          fill

paintBackground :: Background -> Double -> Double -> Render ()
paintBackground (Flat bgColor) width height = do
  let (bgR, bgG, bgB) = stringToColor bgColor
  setSourceRGB bgR bgG bgB
  rectangle 0 0 width height
  fill
paintBackground (Gradient gradType stops) width height = do
  let (x0, y0, x1, y1) =
        case gradType of
          Vertical -> (0, 0, 0, height)
          Horizontal -> (0, 0, width, 0)
  withLinearPattern x0 y0 x1 y1 $ \pattern -> do
    forM_ stops $ \(offset, color) -> do
      let (r,g,b) = stringToColor color
      patternAddColorStopRGB pattern offset r g b
    setSource pattern
    rectangle 0 0 width height
    fill

getWidgetImage :: DrawData CairoDecoration -> StandardWidget -> X (Either String Surface)
getWidgetImage dd TitleWidget = return $ Left $ ddWindowTitle dd
getWidgetImage dd widget = do
  checked <- isWidgetChecked widget (ddOrigWindow dd)
  let imageName = if checked
                    then swCheckedText widget
                    else swUncheckedText widget
  let path = cdssIconsPath (ddStyleState dd) </> imageName
  Right <$> getImageSurface (cdssImages $ ddStyleState dd) path

getImageSurface :: MVar ImagesCache -> String -> X Surface
getImageSurface var path = do
  io $ modifyMVar var $ \cache -> do
    case M.lookup path cache of
      Just surface -> return (cache, surface)
      Nothing -> do
        surface <- imageSurfaceCreateFromPNG path
        let cache' = M.insert path surface cache
        return (cache', surface)
      
cairoDecoration :: (Shrinker shrinker) => shrinker -> CairoTheme StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx CairoDecoration DefaultGeometry shrinker) l Window
cairoDecoration s theme = decorationEx s theme CairoDecoration DefaultGeometry

cairoTabDecoration :: (Shrinker shrinker) => shrinker -> CairoTheme StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx CairoDecoration TabbedGeometry shrinker) l Window
cairoTabDecoration s theme = decorationEx s theme CairoDecoration (TabbedGeometry U)

cairoDwmDecoration :: (Shrinker shrinker) => shrinker -> CairoTheme StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx CairoDecoration DwmGeometry shrinker) l Window
cairoDwmDecoration s theme = decorationEx s theme CairoDecoration (DwmGeometry True)

toggleStickyC = StandardWidget "sticky.png" "sticky.png" ToggleSticky
minimizeC = StandardWidget "minimize.png" "minimize.png" Minimize
maximizeC = StandardWidget "maximize.png" "maximize.png" ToggleMaximize
closeC = StandardWidget "close.png" "close.png" CloseWindow
dwmpromoteC = StandardWidget "demote.png" "promote.png" DwmPromote
moveToNextGroupC = StandardWidget "" "right.png" MoveToNextGroup
moveToPrevGroupC = StandardWidget "" "left.png" MoveToPrevGroup

