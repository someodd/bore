-- FIXME: add constants
-- FIXME: make it so there's a nicer way to handle arguments for lambdas
{- | Moustache templating engine with a few extra features.

BUG: i tested if wrapped in lamba function but lambda doesn't exist, it'll just output nothing...
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Bore.Template (parseTemplate) where

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as H
import Text.Mustache.Compile
import Text.Mustache
import Text.Mustache.Parser
--import Text.Mustache (compileTemplate, overText, automaticCompile, Template)
import qualified Text.Mustache.Types as Mtype

import System.FilePath ((</>))
import qualified Data.Map as Map

import Bore.FrontMatter (FrontMatter(..), parent)
import Data.Maybe (fromMaybe)

import Bore.Text.Containers (applyContainer, ContainerCache)
import Bore.Library (Library(..))
import Bore.Text.Figlet

-- FIXME: will get removed
-- | This is where Mustache will look for files, especially for partials.
--searchSpace :: [FilePath]
--searchSpace = ["./templates", "."]


-- FIXME: take searchspace?
-- | This is like `automaticCompile`, except we use `Text`.
--
-- Based of the documentation in Text.Mustache.Compile:
--
--  "The same can be done manually using getFile, mustacheParser and
--  getPartials."
--    { name     :: String
--  , ast      :: STree
--  , partials :: TemplateCache
--
--  Used for the main document parsing, I guess?
automaticCompileText :: FilePath -> Text.Text -> IO Template
automaticCompileText projectRoot templateToRenderText = do
  let searchSpace = [projectRoot </> "templates", projectRoot]  -- FIXME: duplicated
  let mainPartialAST =
        case parse "partial" templateToRenderText of
          Left err -> error $ show err
          Right ast' -> ast'
      partials' = getPartials mainPartialAST
  partialTemplates <- traverse (getCompiledTemplate searchSpace) partials'
  let templateCache = H.fromList (zip partials' partialTemplates)
  pure $ Template "partial" mainPartialAST templateCache


-- FIXME: what is this even?!
getCompiledTemplate :: [FilePath] -> FilePath -> IO Template
getCompiledTemplate searchSpace' templateToRenderPath = do
  compiled <- automaticCompile searchSpace' templateToRenderPath
  case compiled of
    Left err -> error $ show err
    Right template -> pure template

{- | Find-replace list for Mustache find/replace.

Corresponds to the `Map.Map T.Text Mtype.Value` type which the Mustache library's
`susbstitute` function expects.

Example usage:

@@
dataForMustache ::  MustacheSubstitutions
dataForMustache =
    [ ("hello", overText (const "Hello, world!"))
    ]
@@

-}
type MustacheSubstitutions = [(Text.Text, Mtype.Value)]

-- | Convert a Mustache object to a list of key-value pairs.
--
-- Will just give back a blank list if the Value is not an object.
toSubstitutions :: Mtype.Value -> MustacheSubstitutions
toSubstitutions (Mtype.Object obj) = H.toList obj
toSubstitutions _ = []

-- Global variables which can be access by a Mustache file being parsed.
--
-- These are the "substitutions" for Mustache. These are functions (lambdas),
-- and regular string values.
--
-- These substitutions are later modified to include a Mustache partial for
-- the template/partial system.
defaultTemplateSubstitutions :: MustacheSubstitutions
defaultTemplateSubstitutions =
    [ ("hello", overText (const "Hello, world!"))
    ]

{- | Take the first line from a lambda's input Text, separate it by spaces and feed it to
the associated functions as arguments.

Returns the text without the arguments (first line) and then the arguments as a list of
Text.
-}
lambdaArgs :: Text.Text -> Maybe (Text.Text, [Text.Text])
lambdaArgs innerText =
  case Text.lines innerText of
    [] -> Nothing
    firstLine:rest -> Just (Text.unlines rest, Text.words firstLine)

{- | The template lambda version of `containerizer`. 

-}
lambdaContainerize :: ContainerCache -> Text.Text -> Text.Text
lambdaContainerize containers innerText =
  case lambdaArgs innerText of
    Nothing -> "ERROR: incorrect arguments for containerize lambda"
    Just (rest, [containerName]) ->
      let
        container = lookup (Text.unpack containerName) containers
      in
        case container of
          Nothing -> "ERROR: container " <> containerName <> " not found: " <> (Text.pack . show $ containers)
          Just container' -> applyContainer container' rest
    Just _ -> "ERROR: incorrect arguments for containerize lambda"

{- | The template lambda version of `figlet`.

-}
lambdaFiglet :: FigletCache -> Text.Text -> Text.Text
lambdaFiglet fonts innerText =
  case lambdaArgs innerText of
    Nothing -> "ERROR: incorrect arguments for figlet lambda"
    Just (rest, [font]) ->
      let
        figletFont = lookup (Text.unpack font) fonts
      in
        case figletFont of
          Nothing -> "ERROR: font " <> font <> " not found"
          Just figletFont' -> renderText figletFont' rest
    Just _ -> "ERROR: incorrect arguments for figlet lambda"

{- | Create substitutions for templating, dynamically.

The main/default/initial substitution list.

Helps build lambdas too, which use the library.

-}
initialSubstitutions :: Library -> Maybe FrontMatter -> MustacheSubstitutions
initialSubstitutions library maybeFrontMatter =
  let
    -- FIXME: why is there this doubling-up on the effort of translating the frontmatter, seemingly? just because it can't handle lists?
    postFm = substitutionsFromFrontMatter maybeFrontMatter (toSubstitutions . toMustache <$> maybeFrontMatter)
  in
    postFm ++ [
      ("containerize", overText (lambdaContainerize library.containers)),
      ("figlet", overText (lambdaFiglet library.fonts))
    ]
 where
  -- FIXME; if i want to add tagging support i'd add it here for hardcoding
  -- | Add substitutions based off of FrontMatter.
  substitutionsFromFrontMatter ::  Maybe FrontMatter -> Maybe MustacheSubstitutions -> MustacheSubstitutions
  substitutionsFromFrontMatter maybeFrontMatter' possibleHardcodedSubstitutions =
    let hardcodedFrontMatterVars = fromMaybe [] possibleHardcodedSubstitutions ++ defaultTemplateSubstitutions
        -- specially handle the "variables" field in the frontmatter
        userFrontMatterVars = fromMaybe [] ( maybeFrontMatter' >>= \fm -> fmap (Map.toList . Map.map (Mtype.String)) $ variables fm ) :: [(Text.Text, Mtype.Value)]
    in hardcodedFrontMatterVars ++ userFrontMatterVars

-- contentType argument should be renamed to defaultContentType FIXME
{- | Main template parsing entrypoint.

Prepares the arguments for `parseTemplate'` and then calls it.

The rationale is that how `parseTemplate'` is called depends on if a parent (FrontMatter)
is being used.

Some additional work with preparing the substitutions is performed.
-}
parseTemplate :: Library -> FilePath -> Maybe FrontMatter -> Text.Text -> IO Text.Text
parseTemplate library projectRoot maybeFrontMatter sourceText = do
  let
    parentToUse = maybeFrontMatter >>= parent
    substitutions' = initialSubstitutions library maybeFrontMatter
  case parentToUse of
    Nothing ->
      parseTemplate' sourceText projectRoot Nothing substitutions'
    Just parentTemplateName ->
      parseTemplate'
        sourceText
        projectRoot
        -- The partial to use named below.
        (Just $ "templates" </> Text.unpack parentTemplateName)
        (("partial", Mtype.String sourceText):substitutions')  -- FIXME: partial is done somewhere else too

-- FIXME
-- | ...
--
-- maybeIncludePartial: If a partial is being used, the main file to be parsed is instead specified here,
-- so it may be loaded as a partial named "partial" in the template/.
--
-- The source of this function is a bit confusing due to the fact that if we
-- are using a parent template then we are using the main file as a partial
-- inserted as a substitution for Mustache named "partial," where the parent
-- template is the main that gets rendered.
parseTemplate' :: Text.Text -> FilePath -> Maybe String -> MustacheSubstitutions -> IO Text.Text
parseTemplate' mainText projectRoot maybeIncludePartial mustacheSubstitutions = do
  -- The main template will be the main file in question which the recipe is
  -- for if there's no use of parent template, but if the parent template is
  -- used then the mainTemplate will be the parent template, with the file the
  -- recipe is for being inserted as a Mustache substitution named "partial,"
  -- which the main template can call as "partial."
  mainTemplate <-
    case maybeIncludePartial of
      Just parentTemplatePath -> newPrepareTemplateUsingParent projectRoot parentTemplatePath
      Nothing -> prepareTemplate projectRoot
  pure $ substitute mainTemplate (Map.fromList $ if null mustacheSubstitutions then defaultTemplateSubstitutions else mustacheSubstitutions)
 where
  -- | Prepare a template which will insert itself inside a parent template.
  --
  -- Performs a substitution operation for "partial" (in order to put the file
  -- the recipe is for inside of the specified parent template).
  newPrepareTemplateUsingParent :: FilePath -> FilePath -> IO Template
  newPrepareTemplateUsingParent projectRoot' parentTemplatePath = do
    -- we are going to put mainTemplate in the template cache as a partial as "partial" for the parent template!
    mainTemplate <- automaticCompileText projectRoot' mainText
    let
      mainTemplateCache = H.fromList [("partial", mainTemplate)] :: H.HashMap String Template
      newSearchSpace = [projectRoot' </> "templates", projectRoot'] -- FIXME
    parentTemplate <- compileTemplateWithCache newSearchSpace mainTemplateCache (projectRoot </> parentTemplatePath)
    case parentTemplate of
      Left err -> error $ "Error with file " ++ (projectRoot </> parentTemplatePath) ++ ": " ++ show err
      Right template -> pure template

  -- | Prepare a template normally.
  prepareTemplate :: FilePath -> IO Template
  prepareTemplate projectRoot' = do
    mainTemplate <- automaticCompileText projectRoot' mainText
    pure mainTemplate