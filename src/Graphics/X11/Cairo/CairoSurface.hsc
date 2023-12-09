{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.Cairo
-- Copyright: (c) 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: unportable
-- Created: Thu Sep 08, 2022 01:25
--
--
-- Xlib Cairo Surface creation
--
------------------------------------------------------------------------------

module Graphics.X11.Cairo.CairoSurface (createXlibSurface) where

import Graphics.X11.Xlib.Types
import Graphics.X11.Types
import GI.Cairo.Render
import qualified GI.Cairo.Render.Internal as Internal

import Foreign
import Foreign.C

#include <cairo/cairo-xlib.h>

foreign import ccall "cairo_xlib_surface_create"
   cSurfaceCreate :: Display -> Drawable -> Visual -> CInt -> CInt -> Ptr Surface

foreign import ccall "cairo_xlib_surface_create_for_bitmap"
   cBitmapCreate :: Display -> Pixmap -> Screen -> CInt -> CInt -> Ptr Surface

foreign import ccall "cairo_xlib_surface_set_drawable"
   cSetDrawable :: Ptr Surface -> Drawable -> CInt -> CInt -> ()

createXlibSurface :: Display -> Drawable -> Visual -> Int -> Int -> IO Surface
createXlibSurface d dr v w h =
  Internal.mkSurface $ cSurfaceCreate d dr v (fromIntegral w) (fromIntegral h)

