module HsResumeBuilder.Themes.JJBA.Components where

import HsResumeBuilder.Model
import HsResumeBuilder.Themes.JJBA.Styler (Classes, applyClasses, applyStyles)
import RIO
import Text.Blaze as Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as BlazeAttributes

jHeader1 :: (ToMarkup a) => Text -> Text -> Text -> a -> Html
jHeader1 color fontFamily fontSize = (h1 ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("font-family", fontFamily),
        ("font-size", fontSize)
      ]

jHeader2 :: (ToMarkup a) => Text -> Text -> Text -> a -> Html
jHeader2 color fontFamily fontSize = (h2 ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("font-family", fontFamily),
        ("font-size", fontSize)
      ]

{-# INLINEABLE jText #-}
jText :: (ToMarkup a) => Text -> Text -> a -> Html
jText color fontSize = (p ! applyStyles css) . toHtml
  where
    css = [("color", color), ("font-size", fontSize)]

jIcon :: Text -> Classes -> Html
jIcon color classes = (i ! applyClasses classes ! applyStyles [("color", color)]) ""

jSmall :: (ToMarkup a) => Text -> a -> Html
jSmall color = (H.small ! applyStyles css) . toHtml
  where
    css = [("color", color)]

{-# INLINEABLE jJustified #-}
jJustified :: (ToMarkup a) => Text -> Text -> a -> Html
jJustified color fontSize = (p ! applyStyles css) . toHtml
  where
    css =
      [ ("color", color),
        ("text-align", "justify"),
        ("font-size", fontSize)
      ]

jLink :: Text -> Text -> Text -> Text -> Html
jLink color fontSize url = (a ! applyStyles css ! (BlazeAttributes.href . Blaze.toValue) url) . toHtml
  where
    css =
      [ ("color", color),
        ("padding", "1em"),
        ("font-size", fontSize)
      ]

-- For a prettier output when printing to pdf, prevent the body from being
-- broken in two by a page break.
--
-- Debugging tip: add "border: 1px solid black;" to visualize the sections and
-- make sure they are indeed small enough that you wouldn't mind wasting that
-- amount of space in order to bump it to the next page.
jShortSection :: Html -> Html
jShortSection = H.div ! BlazeAttributes.style "break-inside: avoid;"

-- Similar to @map jShortSection@, but also prevents a page break between the
-- header and the first item.
jHeaderAndShortSections :: Html -> [Html] -> Html
jHeaderAndShortSections headerData (firstItem : items) = do
  jShortSection $ do
    headerData
    firstItem
  forM_ items $ \itemData -> do
    jShortSection $ do
      itemData
jHeaderAndShortSections _ _ = ""

jTitleAndShortSections :: Html -> [Html] -> Html
jTitleAndShortSections headerData items = do
  jShortSection $ do
    headerData
    sequence_ items

{-# INLINEABLE jListItem #-}
jListItem :: (ToMarkup a) => Text -> Text -> a -> Html
jListItem color _ = (li ! applyStyles css) . toHtml
  where
    css = [("color", color)]

jLanguageSection :: Languages -> Html
jLanguageSection languages =
  if simpleMode languages
    then p . toHtml $ simpleModeContent languages
    else H.div $ jLanguageTable languages

jLanguageTable :: Languages -> Html
jLanguageTable languages = table $ do
  tr $ do
    th ""
    th "SPEAKING"
    th "WRITING"
    th "READING"
  forM_
    (complexModeContent languages)
    (tr . jLanguageLevelRow)

jExperienceItem :: JJBASettings -> ExperienceItem -> Html
jExperienceItem themeSettings itemData = H.div $ do
  let bodyColor' = bodyColor themeSettings
  let bodyFontSize = fontSize3 themeSettings
  let entityNameColor' = entityNameColor themeSettings
  let positionNameColor' = positionNameColor themeSettings
  let timeWorkedColor' = timeWorkedColor themeSettings

  H.div ! BlazeAttributes.style "display: flex;" $ do
    (strong ! applyStyles [("color", positionNameColor')]) . toHtml . positionName $ itemData
    ( H.span
        ! applyStyles
          [ ("margin-left", "0.4em"),
            ("margin-right", "0.4em"),
            ("color", bodyColor')
          ]
      )
      " - "
    (strong ! applyStyles [("color", entityNameColor')]) . toHtml . entityName $ itemData
  jSmall timeWorkedColor' $ timeWorked itemData
  ul $ forM_ (experiencePoints itemData) (jListItem bodyColor' bodyFontSize)

jLanguageLevelRow :: LanguageLevel -> Html
jLanguageLevelRow itemData = do
  td . strong . toHtml . languageName $ itemData
  td . toHtml . showLanguageLevel . speakingProficiency $ itemData
  td . toHtml . showLanguageLevel . writingProficiency $ itemData
  td . toHtml . showLanguageLevel . readingProficiency $ itemData

showLanguageLevel :: Int -> Text
showLanguageLevel rating =
  case rating of
    0 -> "N/A"
    1 -> "limited proficiency"
    2 -> "good proficiency"
    3 -> "very good"
    4 -> "native"
    _ -> ""

jFlexContainer :: Html -> Html
jFlexContainer =
  H.div
    ! applyStyles
      [ ("display", "flex"),
        ("align-items", "center"),
        ("gap", "0.5em")
      ]

jFlexContainer' :: Html -> Html
jFlexContainer' = do
  H.div
    ! applyStyles
      [ ("display", "flex"),
        ("align-items", "center"),
        ("gap", "1em")
      ]

jIconWithText :: (ToMarkup a) => Text -> Text -> Text -> Classes -> a -> Html
jIconWithText bodyColor iconColor fontSize classes itemData = do
  jFlexContainer $ do
    jIcon iconColor classes
    jText bodyColor fontSize itemData

loadStylesheet :: Text -> Html
loadStylesheet x = H.link ! rel "stylesheet" ! href (Blaze.toValue x)

internalThemeStylesheets :: [Text]
internalThemeStylesheets =
  [ "https://cdn.jsdelivr.net/npm/water.css@2/out/light.css",
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.1.1/css/all.min.css",
    "https://fonts.googleapis.com",
    "https://fonts.gstatic.com"
  ]

{-# INLINEABLE jSectionHeader #-}
jSectionHeader :: (ToMarkup a) => Text -> Text -> Text -> Bool -> a -> Html
jSectionHeader color fontFamily fontSize enableBorder =
  do
    let css =
          [ ("color", color),
            ("font-family", fontFamily),
            ("font-size", fontSize),
            if enableBorder
              then ("border-bottom", "0.8px solid" <> color)
              else ("border-bottom", "none")
          ]
    (h3 ! applyStyles css) . toHtml
