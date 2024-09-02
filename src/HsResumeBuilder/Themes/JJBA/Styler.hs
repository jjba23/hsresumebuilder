module HsResumeBuilder.Themes.JJBA.Styler where

import RIO
import RIO.Text qualified as T
import Text.Blaze as Blaze
import Text.Blaze.Html5.Attributes as BlazeAttributes (class_, style)

type StyleAttribute = Text

type StyleAttributeValue = Text

type Style = (StyleAttribute, StyleAttributeValue)

type Styles = [Style]

type Class = Text

type Classes = [Text]

showStyles :: Styles -> Text
showStyles styles = toText
  where
    toText = T.concat $ map showStyle styles

showStyle :: (Semigroup a, IsString a) => (a, a) -> a
showStyle (k, v) = k <> ": " <> v <> ";"

showClasses :: Classes -> Text
showClasses classes = toText
  where
    toText = T.concat $ map (<> T.pack " ") classes

applyStyles :: Styles -> Attribute
applyStyles = BlazeAttributes.style . Blaze.toValue . showStyles

applyClasses :: Classes -> Attribute
applyClasses = BlazeAttributes.class_ . Blaze.toValue . showClasses
