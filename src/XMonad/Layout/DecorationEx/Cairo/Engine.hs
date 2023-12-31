{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module XMonad.Layout.DecorationEx.Cairo.Engine (
    CairoDecoration (..),
    CairoWidget (..),
    cairoDecoration,
    cairoTabDecoration,
    cairoDwmDecoration,
    cairoDwmDecorationEx,
    toggleStickyC, minimizeC, maximizeC,
    closeC, dwmpromoteC,
    moveToNextGroupC, moveToPrevGroupC,
    windowIconC
  ) where

import qualified Data.Map as M
-- import qualified Data.Text as T
import Graphics.Rendering.Cairo as Cairo
-- import qualified Graphics.Rendering.Cairo.Internal as Internal
-- import qualified GI.Cairo.Render.Connector as Connector
-- import qualified GI.Rsvg as Rsvg
import System.FilePath
import Control.Exception

import XMonad
import XMonad.Prelude
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.Decoration as D
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Layout.DecorationEx
import XMonad.Layout.DecorationEx.Common
import XMonad.Layout.DecorationEx.Engine
import XMonad.Layout.DecorationEx.Widgets

import Graphics.X11.Cairo.CairoSurface
import XMonad.Layout.DecorationEx.Cairo.Theme
import XMonad.Layout.DecorationEx.Cairo.WindowIcon

data CairoDecoration widget a = CairoDecoration
  deriving (Show, Read)

type WidgetSurfaces = M.Map String Surface
type WindowIconSurfaces = M.Map (Window, Int) Surface

data CairoDecorationCache = CairoDecorationCache {
      cdcWidgets :: !WidgetSurfaces
    , cdcWindowIcons :: !WindowIconSurfaces
    , cdcDecorations :: !(M.Map (Window, Dimension, Dimension) Surface)
  }
  deriving Typeable

instance ExtensionClass CairoDecorationCache where
  initialValue = CairoDecorationCache M.empty M.empty M.empty

data RectangleD = RectangleD {
    rectX :: !Double
  , rectY :: !Double
  , rectWidth :: !Double
  , rectHeight :: !Double
  }
  deriving (Eq, Show, Read)

rectangleD :: Rectangle -> RectangleD
rectangleD (Rectangle x y w h) = RectangleD (fi x) (fi y) (fi w) (fi h)

getImagesCache :: X WidgetSurfaces
getImagesCache = do
  cache <- XS.get
  return $ cdcWidgets cache

data CairoEngineState = CairoEngineState {
    cdssFontName :: String
  , cdssFontSize :: Int
  , cdssFontWeight :: FontWeight
  , cdssFontSlant :: FontSlant
  , cdssIconsPath :: FilePath
  }

instance (CairoWidget widget, ClickHandler CairoTheme widget) => DecorationEngine CairoDecoration widget Window where
  type Theme CairoDecoration = CairoTheme
  type DecorationPaintingContext CairoDecoration = Surface
  type DecorationEngineState CairoDecoration = CairoEngineState

  describeEngine _ = "CairoDecoration"

  initializeState _ _ theme = do
    XS.put $ CairoDecorationCache M.empty M.empty M.empty
    return $ CairoEngineState {
      cdssFontName = ctFontName theme,
      cdssFontSize = ctFontSize theme,
      cdssFontWeight = ctFontWeight theme,
      cdssFontSlant = ctFontSlant theme,
      cdssIconsPath = ctIconsPath theme
    }

  releaseStateResources _ st = do
    cache <- XS.get
    forM_ (M.assocs $ cdcWidgets cache) $ \(path, surface) -> do
      st <- renderWith surface status
      when (st == StatusSuccess) $ do
        io $ surfaceFinish surface
    forM_ (M.elems $ cdcWindowIcons cache) $ \surface -> do
        io $ surfaceFinish surface
    forM_ (M.elems $ cdcDecorations cache) $ \surface -> do
        io $ surfaceFinish surface
    XS.put $ CairoDecorationCache M.empty M.empty M.empty
      -- io $ Internal.surfaceDestroy surface

  getShrinkedWindowName engine shrinker st name wh ht = do
    let calcWidth text = do
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

  paintDecoration engine win windowWidth windowHeight shrinker dd isExpose = do
      dpy <- asks display
      let scr = defaultScreenOfDisplay dpy
          visual = defaultVisualOfScreen scr
      buffer <- io $ createImageSurface FormatARGB32 (fi windowWidth) (fi windowHeight)
      paintDecorationImpl buffer engine win windowWidth windowHeight shrinker dd isExpose
      surface <- io $ createXlibSurface dpy win visual (fi windowWidth) (fi windowHeight)
      renderWith surface $ do
        setSourceSurface buffer 0 0
        rectangle 0 0 (fi windowWidth) (fi windowHeight)
        fill
      io $ surfaceFinish buffer
      io $ surfaceFinish surface
      -- io $ Internal.surfaceDestroy surface

  calcWidgetPlace engine dd widget = do
    let decoRect = ddDecoRect dd
        decoWidth = fi $ rect_width decoRect
        decoHeight = fi $ rect_height decoRect
    res <- getWidgetImage dd widget
    case res of
      Left str -> do
        if decoWidth > 32767
          then return $ WidgetPlace 0 $ Rectangle 0 0 0 0
          else do
            io $ withImageSurface FormatARGB32 decoWidth decoHeight $ \surface ->
              renderWith surface $ do
                let st = ddEngineState dd
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

  paintWidget engine surface place shrinker dd widget isExpose = do
    dpy <- asks display
    let style = ddStyle dd
        st = ddEngineState dd
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
        io $ renderWith surface $ do
          setFontSize (fi $ cdssFontSize st)
          selectFontFace (cdssFontName st) (cdssFontSlant st) (cdssFontWeight st)

          whenJust (csTextShadow style) $ \shadow -> do
            moveTo (fi x + tsDeltaX shadow) (fi y + tsDeltaY shadow)
            let (shadowR, shadowG, shadowB) = stringToColor (tsColor shadow)
            setSourceRGB shadowR shadowG shadowB
            showText str'

          setSourceRGB textR textG textB
          moveTo (fi x) (fi y)
          showText str'
      Right image -> do
        io $ renderWith surface $ do
          paintImageScaled image (fi $ rect_x rect) (fi $ rect_y rect) (fi $ rect_width rect) (fi $ rect_height rect)

paintDecorationImpl :: (Shrinker shrinker, CairoWidget widget, ClickHandler CairoTheme widget)
                    => Surface 
                    -> CairoDecoration widget Window
                    -> Window
                    -> Dimension
                    -> Dimension
                    -> shrinker
                    -> DrawData CairoDecoration widget
                    -> Bool
                    -> X ()
paintDecorationImpl surface engine win windowWidth windowHeight shrinker dd isExpose = do
      let widgets = ddWidgets dd
          allWidgets = widgetLayout widgets
          style = ddStyle dd
          st = ddEngineState dd
          borders = csDecorationBorders style
          borderWidth = fi $ csDecoBorderWidth style
          widthD = fi windowWidth :: Double
          heightD = fi windowHeight :: Double
      paintBackground surface st (csBackground style) 0 0 widthD heightD

      let (mbLeftBg, mbCenterBg, mbRightBg) = csWidgetsBackground style
      leftPad <- case mbLeftBg of
        Just leftBg -> do
          let leftRect = joinPlaces (wlLeft $ ddWidgetPlaces dd)
          if rect_width leftRect > 0
            then paintPanel surface leftRect st leftBg Nothing
            else return 0
        Nothing -> return 0

      whenJust mbCenterBg $ \centerBg -> do
        let centerRect = joinPlaces (wlCenter $ ddWidgetPlaces dd)
        when (rect_width centerRect > 0) $
          void $ paintPanel surface centerRect st centerBg Nothing

      rightPad <- case mbRightBg of
        Just rightBg -> do
          let rightRect = joinPlaces (wlRight $ ddWidgetPlaces dd)
          if rect_width rightRect > 0
            then paintPanel surface rightRect st rightBg Nothing
            else return 0
        Nothing -> return 0

      paintPanel surface ((ddDecoRect dd) {rect_x = 0, rect_y = 0}) st (csCentralPanelBackground style) $ mkPads style leftPad rightPad

      io $ renderWith surface $ do
        when (borderWidth > 0) $ do
          drawLineWith 0 0 widthD borderWidth (bxTop borders)
          drawLineWith 0 0 borderWidth widthD (bxLeft borders)
          drawLineWith 0 (heightD - borderWidth) widthD borderWidth (bxBottom borders)
          drawLineWith (widthD - borderWidth) 0 borderWidth heightD (bxRight borders)
      forM_ (zip allWidgets $ widgetLayout $ ddWidgetPlaces dd) $ \(widget, place) ->
        paintWidget engine surface place shrinker dd widget isExpose
    where
      drawLineWith x y w h color = do
        let (r,g,b) = stringToColor color
        setSourceRGB r g b
        rectangle x y w h
        fill

      joinPlaces places =
        let x0 = if null places
                   then 0
                   else minimum $ map (rect_x . wpRectangle) places
            y0 = 0
            w = sum $ map (rect_width . wpRectangle) places
            h = windowHeight
        in  Rectangle x0 y0 w h

paintBackground :: Surface -> DecorationEngineState CairoDecoration -> Fill -> Double -> Double -> Double -> Double -> X ()
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
paintBackground surface st (Image usage imageName) x y width height = do
  image <- getImageSurface' st imageName
  io $ renderWith surface $ do
    case usage of
      TileImage -> do
        withPatternForSurface image $ \pattern -> do
          patternSetExtend pattern ExtendRepeat
          setSource pattern
          rectangle x y width height
          fill
      ScaleImage -> do
        paintImageScaled image x y width height

paintImageScaled :: Surface -> Double -> Double -> Double -> Double -> Render ()
paintImageScaled image x y width height = do
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

paintImageFitHeight :: Surface -> DecorationEngineState CairoDecoration -> FilePath -> Double -> Double -> Double -> Bool -> X (Double, Double)
paintImageFitHeight surface st imageName x y height alignRight = do
  image <- getImageSurface' st imageName
  imgWidth <- io $ imageSurfaceGetWidth image
  imgHeight <- io $ imageSurfaceGetHeight image
  let scaleY = height / fi imgHeight :: Double
      scaledImgWidth = scaleY * fi imgWidth
      x' = if alignRight
             then x - scaledImgWidth
             else x
  renderWith surface $ do
    translate x' y
    scale scaleY scaleY
    setSourceSurface image 0 0
    rectangle 0 0 (fi imgWidth) (fi imgHeight)
    fill
    identityMatrix
  return (scaledImgWidth, height)

paintImage :: Surface -> DecorationEngineState CairoDecoration -> FilePath -> Position -> Position -> Bool -> X (Int, Int)
paintImage surface st imageName x y alignRight = do
  image <- getImageSurface' st imageName
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

mkPads :: CairoStyle -> Double -> Double -> Maybe (Double, Double)
mkPads style leftPad rightPad =
  if csPadCentralPanelForWidgets style
    then Just (leftPad, rightPad)
    else Nothing

paintPanel :: Surface -> Rectangle -> DecorationEngineState CairoDecoration -> PanelBackground -> Maybe (Double, Double) -> X Double
paintPanel surface panelRect st bg mbPads = do
    let rect = case mbPads of 
                 Just (leftPad, rightPad) -> padW True leftPad rightPad $ rectangleD panelRect
                 Nothing -> rectangleD panelRect
    if cpExpandMiddleToPads bg
      then do
        paintMiddle 0 0 rect
        whenJust (cpLeftImage bg) $ \imageName ->
          void $ paintImageFitHeight surface st imageName (rectX rect) 0 (rectHeight rect) False
        whenJust (cpRightImage bg) $ \imageName ->
          void $ paintImageFitHeight surface st imageName (rectX rect + rectWidth rect) 0 (rectHeight rect) True
        return $ fi $ rect_width panelRect
      else do
        leftPad' <-
          case cpLeftImage bg of
            Just imageName -> do
              sz <- paintImageFitHeight surface st imageName (rectX rect) 0 (rectHeight rect) False
              return $ fst sz
            Nothing -> return 0
        rightPad' <-
          case cpRightImage bg of
            Just imageName -> do
              sz <- paintImageFitHeight surface st imageName (rectX rect + rectWidth rect) 0 (rectHeight rect) True
              return $ fst sz
            Nothing -> return 0
        paintMiddle leftPad' rightPad' rect
        return $ fi $ rect_width panelRect
  where
    padW :: Bool -> Double -> Double -> RectangleD -> RectangleD
    padW keepX left right (RectangleD x y w h) =
      let x' = if keepX then  x + left else left
          w' = max 1 $ w - left - right
      in  RectangleD x' y w' h

    paintMiddle :: Double -> Double -> RectangleD -> X ()
    paintMiddle leftPad' rightPad' rect =
      whenJust (cpMiddle bg) $ \middle -> do
        let rect' = padW True leftPad' rightPad' rect
        paintBackground surface st middle (rectX rect') (rectY rect')
                                          (rectWidth rect') (rectHeight rect')

class DecorationWidget widget => CairoWidget widget where
  getWidgetImage :: DrawData CairoDecoration widget -> widget -> X (Either String Surface)

instance DecorationWidget (GenericWidget cmd) => CairoWidget (GenericWidget cmd) where
  getWidgetImage dd TitleWidget = return $ Left $ ddWindowTitle dd
  getWidgetImage dd (WindowIcon {}) = do
    Right <$> getWindowIconCached (ddDecoRect dd) (ddOrigWindow dd)
  getWidgetImage dd widget = do
    checked <- isWidgetChecked widget (ddOrigWindow dd)
    let imageName = if checked
                      then swCheckedText widget
                      else swUncheckedText widget
    Right <$> getImageSurface' (ddEngineState dd) imageName

getWindowIconCached :: Rectangle -> Window -> X Surface
getWindowIconCached rect win = do
  cache <- XS.get
  let icons = cdcWindowIcons cache
      height = fromIntegral $ rect_height rect
  surface <-
    case M.lookup (win,height) icons of
      Just surface -> return surface
      Nothing -> getWindowIcon height win
  let icons' = M.insert (win,height) surface icons
  XS.put $ cache {cdcWindowIcons = icons'}
  return surface

getImageSurface' :: DecorationEngineState CairoDecoration -> String -> X Surface
getImageSurface' st imageName = do
  let path = cdssIconsPath st </> imageName
  getImageSurface path

getImageSurface :: String -> X Surface
getImageSurface path = do
  cache <- XS.get
  let icons = cdcWidgets cache
  surface <-
    case M.lookup path icons of
      Just surface -> return surface
      Nothing -> io $ loadImageSurface path
  let icons' = M.insert path surface icons
  XS.put $ cache {cdcWidgets = icons'}
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
             -> ModifiedLayout (DecorationEx CairoDecoration StandardWidget DefaultGeometry shrinker) l Window
cairoDecoration s theme = decorationEx s theme CairoDecoration def

cairoTabDecoration :: (Shrinker shrinker) => shrinker -> CairoTheme StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx CairoDecoration StandardWidget TabbedGeometry shrinker) l Window
cairoTabDecoration s theme = decorationEx s theme CairoDecoration def

cairoDwmDecoration :: (Shrinker shrinker) => shrinker -> CairoTheme StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx CairoDecoration StandardWidget DwmGeometry shrinker) l Window
cairoDwmDecoration shrinker theme = decorationEx shrinker theme CairoDecoration def

cairoDwmDecorationEx :: (Shrinker shrinker) => shrinker -> DwmGeometry Window -> CairoTheme StandardWidget -> l Window
             -> ModifiedLayout (DecorationEx CairoDecoration StandardWidget DwmGeometry shrinker) l Window
cairoDwmDecorationEx shrinker geom theme = decorationEx shrinker theme CairoDecoration geom

toggleStickyC = GenericWidget "sticky.png" "sticky.png" ToggleSticky
minimizeC = GenericWidget "minimize.png" "minimize.png" Minimize
maximizeC = GenericWidget "maximize.png" "maximize.png" ToggleMaximize
closeC = GenericWidget "close.png" "close.png" CloseWindow
dwmpromoteC = GenericWidget "demote.png" "promote.png" DwmPromote
moveToNextGroupC = GenericWidget "" "right.png" MoveToNextGroup
moveToPrevGroupC = GenericWidget "" "left.png" MoveToPrevGroup
windowIconC = WindowIcon GridWindowMenu

