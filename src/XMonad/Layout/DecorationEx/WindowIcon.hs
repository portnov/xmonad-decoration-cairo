
module XMonad.Layout.DecorationEx.WindowIcon (
    getWindowIcon
  ) where

import Data.Maybe
import Data.Bits
import Graphics.X11.Xlib
import Graphics.Rendering.Cairo as Cairo
import Foreign.C.Types

import XMonad
import XMonad.Prelude

getIconDataRaw :: Display -> Window -> IO (Maybe [CLong])
getIconDataRaw dpy win = do
  iconAtom <- internAtom dpy "_NET_WM_ICON" False 
  getWindowProperty32 dpy iconAtom win

getIconData :: Display -> Window -> IO [IconData]
getIconData dpy win = do
  raw <- getIconDataRaw dpy win
  return $ parseIconData $ fromMaybe [] raw

data IconData = IconData {
    idWidth :: !CLong
  , idHeight :: !CLong
  , idData :: ![CLong]
  }
  deriving (Eq, Show)

parseIconData :: [CLong] -> [IconData]
parseIconData [] = []
parseIconData (w:h:rest) =
  let (image, rest') = splitAt (fromIntegral $ w*h) rest
      icon = IconData w h image
  in  icon : parseIconData rest'

selectBestIcon :: CLong -> [IconData] -> Maybe IconData
selectBestIcon _ [] = Nothing
selectBestIcon h icons = Just $ head $ sortOn (\i -> abs (idHeight i - h)) icons

makeIconSurface :: IconData -> IO Surface
makeIconSurface icon = do
    surface <- createImageSurface FormatARGB32 (fromIntegral $ idWidth icon) (fromIntegral $ idHeight icon)
    renderWith surface $ paint 0 (idData icon)
    return surface
  where
    width = fromIntegral $ idWidth icon
    height = fromIntegral $ idHeight icon

    paint _ [] = return ()
    paint rowIdx iconData = do
      let (row, rest) = splitAt width iconData
      paintRow rowIdx row
      paint (rowIdx+1) rest

    paintRow rowIdx row = do
      forM_ (zip [0..] row) $ \(colIdx, pixel) -> paintPixel rowIdx colIdx pixel

    paintPixel rowIdx colIdx pixel = do
      rectangle colIdx rowIdx 1 1
      let (r,g,b,a) = pixelRgba pixel
      setSourceRGBA r g b a
      fill

    pixelRgba pixel =
      let b = pixel .&. 0xff
          g = (pixel `shiftR` 8) .&. 0xff
          r = (pixel `shiftR` 16) .&. 0xff
          a = (pixel `shiftR` 24) .&. 0xff
      in  (fromIntegral r / 255.0, fromIntegral g / 255.0, fromIntegral b / 255.0, fromIntegral a / 255.0)

makeEmptySurface :: Int -> IO Surface
makeEmptySurface size = do
  surface <- createImageSurface FormatARGB32 size size
  renderWith surface $ do
    rectangle 0 0 (fromIntegral size - 1) (fromIntegral size - 1)
    setSourceRGBA 0 0 0 0
    fill
  return surface

getWindowIconIO :: Display -> Int -> Window -> IO Surface
getWindowIconIO dpy size win = do
  icons <- getIconData dpy win
  if null icons
    then makeEmptySurface size
    else makeIconSurface $ fromJust $ selectBestIcon (fromIntegral size) icons

getWindowIcon :: Int -> Window -> X Surface
getWindowIcon size win = do
  dpy <- asks display
  io $ getWindowIconIO dpy size win

