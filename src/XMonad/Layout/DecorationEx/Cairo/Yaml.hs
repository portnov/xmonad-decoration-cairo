{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module XMonad.Layout.DecorationEx.Cairo.Yaml (
    loadTheme
  ) where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Aeson.Types
import Data.Yaml
import System.FilePath

import XMonad
import XMonad.Layout.DecorationEx
import XMonad.Layout.DecorationEx.Common
import XMonad.Layout.DecorationEx.Cairo.Theme

instance (FromJSON widget, FromJSON (WidgetCommand widget))
    => FromJSON (CairoTheme widget) where
  parseJSON = withObject "Theme" $ \v -> CairoTheme
      <$> v .: "active"
      <*> v .: "inactive"
      <*> v .: "urgent"
      <*> v .: "padding"
      <*> v .: "font_name"
      <*> v .:? "font_size" .!= 12
      <*> v .:? "font_weight" .!= FontWeightNormal
      <*> v .:? "font_slant" .!= FontSlantNormal
      <*> v .:? "on_click" .!= M.empty
      <*> v .:? "drag_buttons" .!= [1]
      <*> v .:? "images_path" .!= ""
      <*> v .: "widgets_left"
      <*> v .: "widgets_center"
      <*> v .: "widgets_right"

instance FromJSON a => FromJSON (BoxBorders a) where
  parseJSON = withObject "Box" $ \v -> BoxBorders
    <$> v .: "top"
    <*> v .: "right"
    <*> v .: "bottom"
    <*> v .: "left"

instance FromJSON FontWeight where
  parseJSON (String "normal") = pure FontWeightNormal
  parseJSON (String "bold") = pure FontWeightBold
  
instance FromJSON FontSlant where
  parseJSON (String "normal") = pure FontSlantNormal
  parseJSON (String "italic") = pure FontSlantItalic
  parseJSON (String "oblique") = pure FontSlantOblique

instance FromJSON GradientType where
  parseJSON (String "vertical") = pure Vertical
  parseJSON (String "horizontal") = pure Horizontal
  parseJSON invalid = prependFailure "parsing GradientType failed: " (typeMismatch "string" invalid)

instance FromJSON ImageUsage where
  parseJSON (String "tile") = pure TileImage
  parseJSON (String "scale") = pure ScaleImage
  parseJSON invalid = prependFailure "parsing ImageUsage failed: " (typeMismatch "string" invalid)


instance FromJSON Fill where
  parseJSON (String color) = pure (Flat $ T.unpack color)
  parseJSON o =
      (withObject "Gradient" $ \v -> Gradient
        <$> v .: "gradient"
        <*> v .: "stops") o
    <|> (withObject "Image" $ \v -> Image
        <$> v .:? "usage" .!= TileImage
        <*> v .: "image") o

instance FromJSON PanelBackground where
  parseJSON = withObject "PanelBackground" $ \v -> PanelBackground
    <$> v .:? "left_image"
    <*> v .:? "middle"
    <*> v .:? "right_image"

parseWidgetsBg :: Maybe Value -> Parser (Maybe PanelBackground, Maybe PanelBackground, Maybe PanelBackground)
parseWidgetsBg Nothing = pure (Nothing, Nothing, Nothing)
parseWidgetsBg (Just o) =
  (withObject "WidgetsBackground" $ \v -> do
    left <- v .:? "left"
    center <- v .:? "center"
    right <- v .:? "right"
    return (left, center, right)) o

instance FromJSON CairoStyle where
  parseJSON = withObject "Style" $ \v -> CairoStyle
    <$> v .: "background"
    <*> (parseWidgetsBg =<< v .:? "widgets_background")
    <*> v .:? "pad_central_panel_for_widgets" .!= True
    <*> v .:? "central_panel_background" .!= def
    <*> v .:? "text_color" .!= "#000000"
    <*> v .:? "border_width" .!= 1
    <*> v .:? "border_colors" .!= shadowBorder "#dddddd" "#333333"

instance FromJSON StandardCommand where
  parseJSON (String "focus") = pure FocusWindow
  parseJSON (String "focus_up") = pure FocusUp
  parseJSON (String "focus_down") = pure FocusUp
  parseJSON (String "move_to_next_group") = pure MoveToNextGroup
  parseJSON (String "move_to_prev_group") = pure MoveToPrevGroup
  parseJSON (String "dwmpromote") = pure DwmPromote
  parseJSON (String "sticky") = pure ToggleSticky
  parseJSON (String "maximize") = pure ToggleMaximize
  parseJSON (String "minimize") = pure Minimize
  parseJSON (String "close") = pure CloseWindow
  parseJSON (String "grid_menu") = pure GridWindowMenu

parseIconWidget :: Value -> Parser (GenericWidget StandardCommand)
parseIconWidget = withObject "Icon" $ \v -> WindowIcon
  <$> v .: "command"

instance FromJSON (GenericWidget StandardCommand) where
  parseJSON (String "title") = pure TitleWidget
  parseJSON o =
      (withObject "Icon" $ \v ->
        parseIconWidget =<< v .: "icon") o
    <|> (withObject "Other" $ \v -> GenericWidget
          <$> v .: "checked" 
          <*> v .: "unchecked"
          <*> v .: "command"
        ) o

loadTheme :: (FromJSON widget, FromJSON (WidgetCommand widget), Default (CairoTheme widget))
          => FilePath
          -> IO (CairoTheme widget)
loadTheme name = do
  dirs <- XMonad.getDirectories
  let path = dataDir dirs </> "themes" </> name
      yamlPath = path </> "theme.yaml"
  r <- Data.Yaml.decodeFileEither yamlPath
  case r of
    Right theme -> do
      return $ if ctIconsPath theme == ""
                 then theme {ctIconsPath = path}
                 else theme
    Left err -> do 
      xmessage $ prettyPrintParseException err
      return def

