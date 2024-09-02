{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module HsResumeBuilder.Model
  ( LanguageLevel (..),
    Languages (..),
    PersonalInfoWebsites (..),
    PersonalInfoContactInfo (..),
    PersonalInfo (..),
    JJBASettings (..),
    AppearancePreferences (..),
    ExperienceItem (..),
    DocumentTitles (..),
    HsResumeBuilderConfig (..),
    hsResumeBuilderConfigCodec,
  )
where

import Data.Kind (Type)
import Optics
import RIO hiding ((^.))
import Toml

type LanguageLevel :: Type
data LanguageLevel = LanguageLevel
  { languageName :: Text,
    speakingProficiency :: Int,
    writingProficiency :: Int,
    readingProficiency :: Int
  }
  deriving (Generic, Show, Eq)

languageLevelCodec :: TomlCodec LanguageLevel
languageLevelCodec =
  LanguageLevel
    <$> Toml.text "language-name"
      .= (^. #languageName)
    <*> Toml.int "speaking-proficiency"
      .= (^. #speakingProficiency)
    <*> Toml.int "writing-proficiency"
      .= (^. #writingProficiency)
    <*> Toml.int "reading-proficiency"
      .= (^. #readingProficiency)

type Languages :: Type
data Languages = Languages
  { complexModeContent :: [LanguageLevel],
    simpleMode :: Bool,
    simpleModeContent :: Text
  }
  deriving (Generic, Show, Eq)

languagesCodec :: TomlCodec Languages
languagesCodec =
  Languages
    <$> Toml.list languageLevelCodec "complex-mode-content"
      .= (^. #complexModeContent)
    <*> Toml.bool "simple-mode"
      .= (^. #simpleMode)
    <*> Toml.text "simple-mode-content"
      .= (^. #simpleModeContent)

type PersonalInfoWebsites :: Type
data PersonalInfoWebsites = PersonalInfoWebsites
  { blogs :: [Text],
    codee :: [Text],
    linkedIn :: [Text]
  }
  deriving (Generic, Show, Eq)

personalInfoWebsitesCodec :: TomlCodec PersonalInfoWebsites
personalInfoWebsitesCodec =
  PersonalInfoWebsites
    <$> Toml.arrayOf Toml._Text "blogs"
      .= (^. #blogs)
    <*> Toml.arrayOf Toml._Text "code"
      .= (^. #codee)
    <*> Toml.arrayOf Toml._Text "linkedin"
      .= (^. #linkedIn)

type PersonalInfoContactInfo :: Type
data PersonalInfoContactInfo = PersonalInfoContactInfo
  { phoneNumbers :: [Text],
    emails :: [Text],
    websites :: PersonalInfoWebsites
  }
  deriving (Generic, Show, Eq)

personalInfoContactInfoCodec :: TomlCodec PersonalInfoContactInfo
personalInfoContactInfoCodec =
  PersonalInfoContactInfo
    <$> Toml.arrayOf Toml._Text "phone-numbers"
      .= (^. #phoneNumbers)
    <*> Toml.arrayOf Toml._Text "emails"
      .= (^. #emails)
    <*> Toml.table personalInfoWebsitesCodec "websites"
      .= (^. #websites)

type PersonalInfo :: Type
data PersonalInfo = PersonalInfo
  { displayName :: Text,
    jobTitle :: Text,
    addressLines :: [Text],
    contact :: PersonalInfoContactInfo,
    shortIntro :: [Text]
  }
  deriving (Generic, Show, Eq)

personalInfoCodec :: TomlCodec PersonalInfo
personalInfoCodec =
  PersonalInfo
    <$> Toml.text "display-name"
      .= (^. #displayName)
    <*> Toml.text "job-title"
      .= (^. #jobTitle)
    <*> Toml.arrayOf Toml._Text "address-lines"
      .= (^. #addressLines)
    <*> Toml.table personalInfoContactInfoCodec "contact"
      .= (^. #contact)
    <*> Toml.arrayOf Toml._Text "short-intro"
      .= (^. #shortIntro)

type JJBASettings :: Type
data JJBASettings = JJBASettings
  { bodyColor :: Text,
    jobTitleColor :: Text,
    nameColor :: Text,
    sectionTitlesColor :: Text,
    sectionTitleBorderEnabled :: Bool,
    entityNameColor :: Text,
    positionNameColor :: Text,
    timeWorkedColor :: Text,
    linkColor :: Text,
    bodyFontFamily :: Text,
    titleFontFamily :: Text,
    fontSize1 :: Text,
    fontSize2 :: Text,
    fontSize3 :: Text,
    customStylesheetsToLoad :: [Text]
  }
  deriving (Generic, Show, Eq)

jjbaSettingsCodec :: TomlCodec JJBASettings
jjbaSettingsCodec =
  JJBASettings
    <$> Toml.text "body-color"
      .= (^. #bodyColor)
    <*> Toml.text "job-title-color"
      .= (^. #jobTitleColor)
    <*> Toml.text "name-color"
      .= (^. #nameColor)
    <*> Toml.text "section-titles-color"
      .= (^. #sectionTitlesColor)
    <*> Toml.bool "section-title-border-enabled"
      .= (^. #sectionTitleBorderEnabled)
    <*> Toml.text "entity-name-color"
      .= (^. #entityNameColor)
    <*> Toml.text "position-name-color"
      .= (^. #positionNameColor)
    <*> Toml.text "time-worked-color"
      .= (^. #timeWorkedColor)
    <*> Toml.text "link-color"
      .= (^. #linkColor)
    <*> Toml.text "body-font-family"
      .= (^. #bodyFontFamily)
    <*> Toml.text "title-font-family"
      .= (^. #titleFontFamily)
    <*> Toml.text "font-size-1"
      .= (^. #fontSize1)
    <*> Toml.text "font-size-2"
      .= (^. #fontSize2)
    <*> Toml.text "font-size-3"
      .= (^. #fontSize3)
    <*> Toml.arrayOf Toml._Text "custom-stylesheets-to-load"
      .= (^. #customStylesheetsToLoad)

type AppearancePreferences :: Type
data AppearancePreferences = AppearancePreferences
  { theme :: Text,
    documentTitles :: DocumentTitles,
    themeSettings :: JJBASettings
  }
  deriving (Generic, Show, Eq)

appearancePreferencesCodec :: TomlCodec AppearancePreferences
appearancePreferencesCodec =
  AppearancePreferences
    <$> Toml.text "theme"
      .= (^. #theme)
    <*> Toml.table documentTitlesCodec "document-titles"
      .= (^. #documentTitles)
    <*> Toml.table jjbaSettingsCodec "theme-settings"
      .= (^. #themeSettings)

type ExperienceItem :: Type
data ExperienceItem = ExperienceItem
  { entityName :: Text,
    experiencePoints :: [Text],
    positionName :: Text,
    timeWorked :: Text
  }
  deriving (Generic, Show, Eq)

experienceItemCodec :: TomlCodec ExperienceItem
experienceItemCodec =
  ExperienceItem
    <$> Toml.text "entity-name"
      .= (^. #entityName)
    <*> Toml.arrayOf Toml._Text "experience-points"
      .= (^. #experiencePoints)
    <*> Toml.text "position-name"
      .= (^. #positionName)
    <*> Toml.text "time-worked"
      .= (^. #timeWorked)

type DocumentTitles :: Type
data DocumentTitles = DocumentTitles
  { shortIntroTitle :: Text,
    workExperienceTitle :: Text,
    educationTitle :: Text,
    interestsHobbiesTitle :: Text,
    driverLicenseTitle :: Text,
    languagesTitle :: Text,
    seeMyWebsitesTitle :: Text
  }
  deriving (Generic, Show, Eq)

documentTitlesCodec :: TomlCodec DocumentTitles
documentTitlesCodec =
  DocumentTitles
    <$> Toml.text "short-intro-title"
      .= (^. #shortIntroTitle)
    <*> Toml.text "work-experience-title"
      .= (^. #workExperienceTitle)
    <*> Toml.text "education-title"
      .= (^. #educationTitle)
    <*> Toml.text "interests-hobbies-title"
      .= (^. #interestsHobbiesTitle)
    <*> Toml.text "driver-license-title"
      .= (^. #driverLicenseTitle)
    <*> Toml.text "languages-title"
      .= (^. #languagesTitle)
    <*> Toml.text "see-my-websites-title"
      .= (^. #seeMyWebsitesTitle)

type HsResumeBuilderConfig :: Type
data HsResumeBuilderConfig = HsResumeBuilderConfig
  { personal :: PersonalInfo,
    appearance :: AppearancePreferences,
    experience :: [ExperienceItem],
    education :: [ExperienceItem],
    interestsHobbies :: [Text],
    driverLicense :: [Text],
    languages :: Languages
  }
  deriving (Generic, Show, Eq)

hsResumeBuilderConfigCodec :: TomlCodec HsResumeBuilderConfig
hsResumeBuilderConfigCodec =
  HsResumeBuilderConfig
    <$> Toml.table personalInfoCodec "personal"
      .= (^. #personal)
    <*> Toml.table appearancePreferencesCodec "appearance"
      .= (^. #appearance)
    <*> Toml.list experienceItemCodec "experience"
      .= (^. #experience)
    <*> Toml.list experienceItemCodec "education"
      .= (^. #education)
    <*> Toml.arrayOf Toml._Text "interests-hobbies"
      .= (^. #interestsHobbies)
    <*> Toml.arrayOf Toml._Text "driver-license"
      .= (^. #driverLicense)
    <*> Toml.table languagesCodec "languages"
      .= (^. #languages)

makeFieldLabelsNoPrefix ''Languages
makeFieldLabelsNoPrefix ''LanguageLevel
makeFieldLabelsNoPrefix ''PersonalInfo
makeFieldLabelsNoPrefix ''PersonalInfoContactInfo
makeFieldLabelsNoPrefix ''PersonalInfoWebsites
makeFieldLabelsNoPrefix ''AppearancePreferences
makeFieldLabelsNoPrefix ''JJBASettings
makeFieldLabelsNoPrefix ''ExperienceItem
makeFieldLabelsNoPrefix ''DocumentTitles
makeFieldLabelsNoPrefix ''HsResumeBuilderConfig
