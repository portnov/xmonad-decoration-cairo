{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module XMonad.Layout.DecorationEx.CairoDecoration (
    ImageUsage (..),
    Fill (..),
    CentralPanelBackground (..),
    GradientStops (..), GradientType (..),
    FontWeight (..), FontSlant (..),
    CairoStyle (..),
    CairoTheme (..),
    CairoDecoration (..),
    simpleGradient,
    stripesGradient,
    themeC,
    cairoDecoration,
    cairoTabDecoration,
    cairoDwmDecoration,
    toggleStickyC, minimizeC, maximizeC,
    closeC, dwmpromoteC,
    moveToNextGroupC, moveToPrevGroupC
  ) where

import Control.Concurrent
import Data.Word
import Data.Bits
import qualified Data.Map as M
import qualified Data.Text as T
import Numeric (readHex)
import Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Internal as Internal
-- import qualified GI.Cairo.Render.Connector as Connector
-- import qualified GI.Rsvg as Rsvg
import System.FilePath

import XMonad
import XMonad.Prelude
import XMonad.Layout.Decoration (ModifiedLayout, Shrinker (..))
import qualified XMonad.Layout.Decoration as D
import XMonad.Util.Types
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Layout.DecorationEx

import Graphics.X11.Cairo.CairoSurface

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
    ctActive :: CairoStyle
  , ctInactive :: CairoStyle
  , ctUrgent :: CairoStyle
  , ctPadding :: BoxBorders Dimension
  , ctFontName :: String
  , ctFontSize :: Int
  , ctFontWeight :: FontWeight
  , ctFontSlant :: FontSlant
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

data ImageUsage = TileImage | ScaleImage
  deriving (Eq, Show, Read)

data Fill =
    Flat String
  | Gradient GradientType GradientStops
  | Image ImageUsage FilePath
  deriving (Eq, Show, Read)

data CentralPanelBackground = CentralPanelBackground {
    cpPadForWidgets :: Bool
  , cpLeftImage :: Maybe FilePath
  , cpMiddle :: Maybe Fill
  , cpRightImage :: Maybe FilePath
  }
  deriving (Eq, Show, Read)

instance Default CentralPanelBackground where
  def = CentralPanelBackground True Nothing Nothing Nothing

data CairoStyle = CairoStyle {
    csBackground :: Fill
  , csWidgetsBackground :: (Maybe Fill, Maybe Fill, Maybe Fill)
  , csCentralPanelBackground :: CentralPanelBackground
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
          ctActive = style (D.activeColor t) (D.activeBorderColor t) (D.activeTextColor t) (D.activeBorderWidth t)
        , ctInactive = style (D.inactiveColor t) (D.inactiveBorderColor t) (D.inactiveTextColor t) (D.inactiveBorderWidth t) 
        , ctUrgent = style (D.urgentColor t) (D.urgentBorderColor t) (D.urgentTextColor t) (D.urgentBorderWidth t)
        , ctPadding = BoxBorders 0 4 0 4
        , ctFontName = D.fontName t
        , ctFontSize = 12
        , ctFontWeight = FontWeightNormal
        , ctFontSlant = FontSlantNormal
        , ctDecoWidth = D.decoWidth t
        , ctDecoHeight = D.decoHeight t
        , ctOnDecoClick = M.empty
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
        csCentralPanelBackground = def,
        csTextColor = textColor,
        csDecoBorderWidth = borderWidth,
        csDecorationBorders = borderColor brdColor
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

type ImagesData = M.Map String Surface

newtype ImagesCache = ImagesCache ImagesData
  deriving Typeable

instance ExtensionClass ImagesCache where
  initialValue = ImagesCache M.empty

getImagesCache :: X ImagesData
getImagesCache = do
  ImagesCache cache <- XS.get
  return cache

data CairoEngineState = CairoEngineState {
    cdssFontName :: String
  , cdssFontSize :: Int
  , cdssFontWeight :: FontWeight
  , cdssFontSlant :: FontSlant
  , cdssIconsPath :: FilePath
  }

instance DecorationEngine CairoDecoration Window where
  type Theme CairoDecoration = CairoTheme
  type Widget CairoDecoration = StandardWidget
  type DecorationPaintingContext CairoDecoration = Surface
  type DecorationEngineState CairoDecoration = CairoEngineState

  describeEngine _ = "CairoDecoration"

  initializeState _ _ theme = do
    -- cache <- getImagesCache
    XS.put $ ImagesCache M.empty
    return $ CairoEngineState {
      cdssFontName = ctFontName theme,
      cdssFontSize = ctFontSize theme,
      cdssFontWeight = ctFontWeight theme,
      cdssFontSlant = ctFontSlant theme,
      cdssIconsPath = ctIconsPath theme
    }

  releaseStateResources _ st = do
    cache <- getImagesCache
    forM_ (M.elems cache) $ \surface -> do
      io $ surfaceFinish surface
      io $ Internal.surfaceDestroy surface

  getShrinkedWindowName dstyle shrinker st name wh ht = do
    let calcWidth text =
          io $ withImageSurface FormatARGB32 (fi wh) (fi ht) $ \surface ->
            renderWith surface $ do
              setFontSize (fi $ cdssFontSize st)
              selectFontFace (cdssFontName st) (cdssFontSlant st) (cdssFontWeight st)
              ext <- Cairo.textExtents text
              return $ round $ textExtentsWidth ext
    let s = shrinkIt shrinker
    D.shrinkWhile s (\n -> do
                           size <- calcWidth n
                           return $ size > fromIntegral wh) name

  placeWidgets = defaultPlaceWidgets

  paintDecoration dstyle win windowWidth windowHeight shrinker dd = do
      dpy <- asks display
      let widgets = ddLabels dd
          allWidgets = widgetLayout widgets
          style = ddStyle dd
          borders = csDecorationBorders style
          borderWidth = fi $ csDecoBorderWidth style
          scr = defaultScreenOfDisplay dpy
          visual = defaultVisualOfScreen scr
          widthD = fi windowWidth :: Double
          heightD = fi windowHeight :: Double
      surface <- io $ createXlibSurface dpy win visual (fi windowWidth) (fi windowHeight)

      paintBackground surface dd (csBackground style) 0 0 widthD heightD

      let (mbLeftBg, mbCenterBg, mbRightBg) = csWidgetsBackground style
      leftPad <- case mbLeftBg of
        Just leftBg -> do
          let leftRect = joinPlaces (wlLeft $ ddWidgetPlaces dd)
          paintBackground surface dd leftBg (fi $ rect_x leftRect) (fi $ rect_y leftRect)
                                            (fi $ rect_width leftRect) (fi $ rect_height leftRect)
          return $ rect_width leftRect
        Nothing -> return 0

      whenJust mbCenterBg $ \centerBg -> do
        let centerRect = joinPlaces (wlCenter $ ddWidgetPlaces dd)
        paintBackground surface dd centerBg (fi $ rect_x centerRect) (fi $ rect_y centerRect)
                                          (fi $ rect_width centerRect) (fi $ rect_height centerRect)
      rightPad <- case mbRightBg of
        Just rightBg -> do
          let rightRect = joinPlaces (wlRight $ ddWidgetPlaces dd)
          paintBackground surface dd rightBg (fi $ rect_x rightRect) (fi $ rect_y rightRect)
                                            (fi $ rect_width rightRect) (fi $ rect_height rightRect)
          return $ rect_width rightRect
        Nothing -> return 0

      paintCentralPanel surface dd (csCentralPanelBackground style) leftPad rightPad

      io $ renderWith surface $ do
        when (borderWidth > 0) $ do
          drawLineWith 0 0 widthD borderWidth (bxTop borders)
          drawLineWith 0 0 borderWidth widthD (bxLeft borders)
          drawLineWith 0 (heightD - borderWidth) widthD borderWidth (bxBottom borders)
          drawLineWith (widthD - borderWidth) 0 borderWidth heightD (bxRight borders)
      forM_ (zip allWidgets $ widgetLayout $ ddWidgetPlaces dd) $ \(widget, place) ->
        paintWidget dstyle surface place shrinker dd widget
      io $ surfaceFinish surface
      io $ Internal.surfaceDestroy surface
    where
      drawLineWith x y w h color = do
        let (r,g,b) = stringToColor color
        setSourceRGB r g b
        rectangle x y w h
        fill

      joinPlaces places =
        let x0 = minimum $ map (rect_x . wpRectangle) places
            y0 = minimum $ map (rect_y . wpRectangle) places
            w = sum $ map (rect_width . wpRectangle) places
            h = maximum $ map (rect_height . wpRectangle) places
        in  Rectangle x0 y0 w h

  calcWidgetPlace dstyle dd widget = do
    let decoRect = ddDecoRect dd
        decoWidth = fi $ rect_width decoRect
        decoHeight = fi $ rect_height decoRect
    res <- getWidgetImage dd widget
    case res of
      Left str -> do
        io $ withImageSurface FormatARGB32 decoWidth decoHeight $ \surface ->
          renderWith surface $ do
            let st = ddStyleState dd
            setFontSize (fi $ cdssFontSize st)
            selectFontFace (cdssFontName st) (cdssFontSlant st) (cdssFontWeight st)
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
        st = ddStyleState dd
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
                  then getShrinkedWindowName engine shrinker st str (rect_width rect) (rect_height rect) 
                  else return str
        renderWith surface $ do
          setSourceRGB textR textG textB
          setFontSize (fi $ cdssFontSize st)
          selectFontFace (cdssFontName st) (cdssFontSlant st) (cdssFontWeight st)
          moveTo (fi x) (fi y)
          showText str'
      Right image -> do
        renderWith surface $ do
          setSourceSurface image (fi $ rect_x rect) (fi $ rect_y rect)
          rectangle (fi $ rect_x rect) (fi $ rect_y rect) (fi $ rect_width rect) (fi $ rect_height rect)
          fill

paintBackground :: Surface -> DrawData CairoDecoration -> Fill -> Double -> Double -> Double -> Double -> X ()
paintBackground surface _ (Flat bgColor) x y width height = io $ renderWith surface $ do
  let (bgR, bgG, bgB) = stringToColor bgColor
  setSourceRGB bgR bgG bgB
  rectangle x y width height
  fill
paintBackground surface _ (Gradient gradType stops) x y width height = io $ renderWith surface $ do
  let (x0, y0, x1, y1) =
        case gradType of
          Vertical -> (0, y, 0, height)
          Horizontal -> (x, 0, width, 0)
  withLinearPattern x0 y0 x1 y1 $ \pattern -> do
    forM_ stops $ \(offset, color) -> do
      let (r,g,b) = stringToColor color
      patternAddColorStopRGB pattern offset r g b
    setSource pattern
    rectangle x y width height
    fill
paintBackground surface dd (Image usage imageName) x y width height = do
  image <- getImageSurface' dd imageName
  io $ renderWith surface $ do
    case usage of
      TileImage -> do
        withPatternForSurface image $ \pattern -> do
          patternSetExtend pattern ExtendRepeat
          setSource pattern
          rectangle x y width height
          fill
      ScaleImage -> do
        imgWidth <- io $ imageSurfaceGetWidth image
        imgHeight <- io $ imageSurfaceGetHeight image
        let scaleX = width / fi imgWidth
            scaleY = height / fi imgHeight
        translate x y
        scale scaleX scaleY
        setSourceSurface image 0 0
        rectangle 0 0 (fi imgWidth) (fi imgHeight)
        fill
        identityMatrix

paintImage :: Surface -> DrawData CairoDecoration -> FilePath -> Position -> Position -> Bool -> X (Int, Int)
paintImage surface dd imageName x y alignRight = do
  image <- getImageSurface' dd imageName
  imgWidth <- io $ imageSurfaceGetWidth image
  imgHeight <- io $ imageSurfaceGetHeight image
  let x' = if alignRight
             then x - fi imgWidth
             else x
  renderWith surface $ do
    setSourceSurface image (fi x') (fi y)
    rectangle (fi x') (fi y) (fi imgWidth) (fi imgHeight)
    fill
  return (imgWidth, imgHeight)

paintCentralPanel :: Surface -> DrawData CairoDecoration -> CentralPanelBackground -> Dimension -> Dimension -> X ()
paintCentralPanel surface dd bg leftPad rightPad = do
    let rect = if cpPadForWidgets bg
                 then padW False leftPad rightPad (ddDecoRect dd)
                 else (ddDecoRect dd) {rect_x = 0, rect_y = 0}
    leftPad' <-
      case cpLeftImage bg of
        Just imageName -> do
          sz <- paintImage surface dd imageName (rect_x rect) 0 False
          return $ snd sz
        Nothing -> return 0
    rightPad' <-
      case cpRightImage bg of
        Just imageName -> do
          sz <- paintImage surface dd imageName (rect_x rect + fi (rect_width rect)) 0 True
          return $ snd sz
        Nothing -> return 0

    whenJust (cpMiddle bg) $ \middle -> do
      let rect' = padW True (fi leftPad') (fi rightPad') rect
      paintBackground surface dd middle (fi $ rect_x rect') 0
                                        (fi $ rect_width rect') (fi $ rect_height rect')
  where
    padW keepX left right (Rectangle x y w h) =
      let x' = if keepX then  x + fi left else fi left
          w' = w - left - right
      in  Rectangle x' y w' h

getWidgetImage :: DrawData CairoDecoration -> StandardWidget -> X (Either String Surface)
getWidgetImage dd TitleWidget = return $ Left $ ddWindowTitle dd
getWidgetImage dd widget = do
  checked <- isWidgetChecked widget (ddOrigWindow dd)
  let imageName = if checked
                    then swCheckedText widget
                    else swUncheckedText widget
  Right <$> getImageSurface' dd imageName

getImageSurface' :: DrawData CairoDecoration -> String -> X Surface
getImageSurface' dd imageName = do
  let path = cdssIconsPath (ddStyleState dd) </> imageName
  getImageSurface path

getImageSurface :: String -> X Surface
getImageSurface path = do
  cache <- getImagesCache
  surface <-
    case M.lookup path cache of
      Just surface -> return surface
      Nothing -> do
        surface <- io $ loadImageSurface path
        let cache' = M.insert path surface cache
        return surface
  XS.put $ ImagesCache $ M.insert path surface cache
  return surface

loadImageSurface :: FilePath -> IO Surface
loadImageSurface path =
  case map toLower (takeExtension path) of
    ".png" -> do
      putStrLn $ "Loading PNG: " ++ path
      imageSurfaceCreateFromPNG path
    {-
    ".svg" -> do
      Just svgHandle <- Rsvg.handleNewFromFile (T.pack path)
      putStrLn $ "Loading SVG: " ++ path
      w <- Rsvg.getHandleWidth svgHandle
      h <- Rsvg.getHandleHeight svgHandle
      surface <- createImageSurface FormatARGB32 (fi w) (fi h)
      renderWith surface $ do
        ctx <- Connector.getContext
        rect <- Rsvg.newZeroRectangle
        Rsvg.setRectangleX rect 0
        Rsvg.setRectangleY rect 0
        Rsvg.setRectangleWidth rect (fi w)
        Rsvg.setRectangleHeight rect (fi h)
        Rsvg.handleRenderDocument svgHandle ctx rect
      return surface
      -}
    _ -> createImageSurface FormatARGB32 32 32

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
minimizeC = StandardWidget "png/minimize.png" "png/minimize.png" Minimize
maximizeC = StandardWidget "png/maximize.png" "png/maximize.png" ToggleMaximize
closeC = StandardWidget "png/close.png" "png/close.png" CloseWindow
dwmpromoteC = StandardWidget "png/demote.png" "png/promote.png" DwmPromote
moveToNextGroupC = StandardWidget "" "png/right.png" MoveToNextGroup
moveToPrevGroupC = StandardWidget "" "png/left.png" MoveToPrevGroup

