module HsResumeBuilder.Themes.JJBA.Template where

import HsResumeBuilder.Model
import HsResumeBuilder.Themes.JJBA.Components
import HsResumeBuilder.Themes.JJBA.Styler (applyStyles)
import RIO
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

renderResume :: HsResumeBuilderConfig -> Html
renderResume config = docTypeHtml $ do
  H.head $ do
    H.title . toHtml $ "Resume of " <> (displayName . personal $ config)
    -- load internal theme stylesheets
    forM_ internalThemeStylesheets loadStylesheet
    -- load custom user stylesheets
    forM_ (customStylesheetsToLoad theme') loadStylesheet
    -- meta
    meta ! charset "UTF-8"

  H.body
    $ main
    ! applyStyles [("font-family", bodyFontFamily')]
    $ do
      H.div
        ! applyStyles
          [ ("display", "flex"),
            ("flex-direction", "row")
          ]
        $ do
          -- Name and job title section
          H.div ! applyStyles [("flex-grow", "1")] $ do
            renderPersonNameHeader config . displayName $ personal'
            renderJobTitleHeader config . jobTitle $ personal'

          -- Contact details section
          H.div ! applyStyles [("flex-grow", "0.6"), ("display", "flex")] $ do
            H.div
              ! applyStyles [("padding", "0.8em")]
              $ forM_
                (addressLines personal')
                (jText bodyColor' (fontSize3 theme'))
            H.div ! applyStyles [("padding", "0.8em")] $ do
              forM_
                (phoneNumbers . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    fontSize3'
                    ["fa-solid", "fa-phone"]
                )
              forM_
                (emails . contact $ personal')
                ( jIconWithText
                    bodyColor'
                    (nameColor theme')
                    fontSize3'
                    ["fa-solid", "fa-envelope"]
                )

      -- Short introduction section
      renderShortSection config (shortIntroTitle documentTitles')
        $ forM_ (shortIntro personal') (jJustified bodyColor' (fontSize3 theme'))

      -- Work experience section
      renderLongSection
        config
        (workExperienceTitle documentTitles')
        (fmap (jExperienceItem theme') (experience config))

      br

      -- Education section
      renderLongSection
        config
        (educationTitle documentTitles')
        (fmap (jExperienceItem theme') (education config))

      -- Languages section
      renderShortSection
        config
        (languagesTitle documentTitles')
        (jLanguageSection $ languages config)

      -- Driver license section
      renderShortSection
        config
        (driverLicenseTitle documentTitles')
        ( ul
            $ forM_ (driverLicense config) (jListItem bodyColor' (fontSize3 theme'))
        )

      -- Interests hobbies section
      renderLongSection
        config
        (interestsHobbiesTitle documentTitles')
        (fmap (jJustified bodyColor' (fontSize3 theme')) (interestsHobbies config))

      -- Websites section
      renderShortSection config (seeMyWebsitesTitle documentTitles')
        $ jFlexContainer'
        $ do
          mapM_
            ( jIconWithText
                bodyColor'
                bodyColor'
                (fontSize3 theme')
                ["fa-solid", "fa-rss"]
            )
            (blogs . websites . contact $ personal')

          mapM_
            ( jIconWithText
                bodyColor'
                bodyColor'
                (fontSize3 theme')
                ["fa-brands", "fa-github"]
            )
            (codee . websites . contact $ personal')

          mapM_
            ( jIconWithText
                bodyColor'
                bodyColor'
                (fontSize3 theme')
                ["fa-brands", "fa-linkedin"]
            )
            (linkedIn . websites . contact $ personal')

      -- Credits to hsResumeBuilder
      jShortSection
        $ H.div
        ! applyStyles [("margin-top", "16px"), ("text-align", "center")]
        $ small
        $ jLink
          linkColor'
          fontSize3'
          "https://gitlab.com/jjba-projects/hsresumebuilder"
          "This document has been proudly auto-generated with Haskell code"
  where
    documentTitles' = documentTitles . appearance $ config
    theme' = themeSettings . appearance $ config
    personal' = personal config
    bodyColor' = bodyColor theme'
    linkColor' = linkColor theme'
    bodyFontFamily' = bodyFontFamily theme'
    fontSize3' = fontSize3 theme'

renderSectionHeader :: (ToMarkup a) => HsResumeBuilderConfig -> a -> Html
renderSectionHeader config =
  jSectionHeader
    sectionHeaderColor
    (bodyFontFamily theme')
    (fontSize2 theme')
    (sectionTitleBorderEnabled theme')
  where
    theme' = themeSettings . appearance $ config
    sectionHeaderColor = sectionTitlesColor theme'

renderShortSection :: (ToMarkup a) => HsResumeBuilderConfig -> a -> Html -> Html
renderShortSection config headerText bodyData = do
  jShortSection $ do
    renderSectionHeader config headerText
    bodyData

renderLongSection :: (ToMarkup a) => HsResumeBuilderConfig -> a -> [Html] -> Html
renderLongSection config headerText = do
  jHeaderAndShortSections (renderSectionHeader config headerText)

renderPersonNameHeader :: (ToMarkup a) => HsResumeBuilderConfig -> a -> Html
renderPersonNameHeader config =
  jHeader1
    (nameColor theme')
    (titleFontFamily theme')
    (fontSize1 theme')
  where
    theme' = themeSettings . appearance $ config

renderJobTitleHeader :: (ToMarkup a) => HsResumeBuilderConfig -> a -> Html
renderJobTitleHeader config =
  jHeader2
    (jobTitleColor theme')
    (titleFontFamily theme')
    (fontSize2 theme')
  where
    theme' = themeSettings . appearance $ config
