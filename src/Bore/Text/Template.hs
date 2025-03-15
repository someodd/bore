{- | Moustache templating engine with a few extra features.

Notes:
    Currently has some extreme efficiency issues. I keep using something that does IO when
    not needed, over-and-over, just because this library is difficult for me to use.

    I think it'll help to have two different functions for compiling a child template vs.
    normal template.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Bore.Text.Template (renderTemplate) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as H
import Text.Mustache.Compile
import Text.Mustache
import Text.Mustache.Parser
import Text.Mustache.Types (STree, Node(..))

import System.FilePath ((</>))
import qualified Data.Map as Map

import Bore.FrontMatter (FrontMatter(..), parent, updateFrontMatter, parseFrontMatter, splitFrontmatter)

import Bore.Text.Template.Replacements
import Bore.Library (Library(..))
import Bore.FileLayout

-- | Function to disable HTML escaping in an AST
disableEscaping :: STree -> STree
disableEscaping = map transformNode
  where
    transformNode :: Node Text.Text -> Node Text.Text
    transformNode (Variable _ name) = Variable False name  -- Set escaped to False
    transformNode (Section name tree) = Section name (disableEscaping tree)
    transformNode (InvertedSection name tree) = InvertedSection name (disableEscaping tree)
    transformNode node = node

-- | Helper to disable escaping in a template
disableEscapingTemplate :: Template -> Template
disableEscapingTemplate template = 
  template { 
    ast = disableEscaping (ast template),
    -- Also process partial templates recursively
    partials = H.map disableEscapingTemplate (partials template)
  }

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
automaticCompileText :: AbsolutePath -> Text.Text -> IO Template
automaticCompileText projectRoot templateToRenderText = do
  let searchSpace = [projectRoot </> templateDirectory, projectRoot]  -- FIXME: duplicated
  let mainPartialAST =
        case parse "partial" templateToRenderText of
          Left err -> error $ show err
          Right ast' -> disableEscaping ast'  -- Apply transformation here
      partials' = getPartials mainPartialAST
  partialTemplates <- traverse (getCompiledTemplate searchSpace) partials'
  -- Apply transformation to partials too
  let templateCache = H.fromList (zip partials' partialTemplates)
  pure $ Template "partial" mainPartialAST templateCache

getCompiledTemplate :: [FilePath] -> FilePath -> IO Template
getCompiledTemplate searchSpace' templateToRenderPath = do
  compiled <- automaticCompile searchSpace' templateToRenderPath
  case compiled of
    Left err -> error $ show err
    Right template -> pure $ disableEscapingTemplate template

-- FIXME: could use some work! also i'm thinking about if frontmatter inheritence is a
-- good or bad thing is... like it won't show inherited titles for phlog posts, so...
renderParentTemplate :: Library -> [Char] -> FrontMatter -> Text.Text -> Text.Text -> IO Text.Text
renderParentTemplate library projectRoot frontMatter sourceText parentTemplateName = do
    let
        parentPath =  projectRoot </> templateDirectory </> Text.unpack parentTemplateName
    eitherParentFrontMatterOrException <- parseFrontMatter <$> TIO.readFile parentPath
    let
        -- ugly logic FIXME
        mergedFrontMatter = case (eitherParentFrontMatterOrException, frontMatter) of
            (Right (parentFrontMatter, _), mfm) -> updateFrontMatter parentFrontMatter mfm
            (Left _, mfm) -> mfm
        substitutions' = initialSubstitutions library (Just mergedFrontMatter)
    -- new stuff
    mainTemplatePending <- automaticCompileText projectRoot sourceText
    let
        mainTemplateCache = H.fromList [("partial", mainTemplatePending)] :: H.HashMap String Template
        newSearchSpace = [projectRoot </> templateDirectory, projectRoot] -- FIXME
    -- fixme: need to trim out the frontmatter...
    -- FIXME: why is this compiled twice?
    mainTemplate <- compileTemplateWithCache newSearchSpace mainTemplateCache parentPath >>= \errorOrTemplate ->
        case errorOrTemplate of
            Left err -> error $ "Error with file " ++ parentPath ++ ": " ++ show err
            Right template -> pure $ disableEscapingTemplate template  -- Apply transformation here

    -- this is weird and repeats...
    let templateText = substitute mainTemplate $ Map.fromList substitutions'
    pure templateText

-- | Parse an orphan template (doesn't have a parent).
renderOrphanTemplate :: Library -> FilePath -> Maybe FrontMatter -> Text.Text -> IO Text.Text
renderOrphanTemplate library projectRoot maybeFrontMatter sourceText = do
    let substitutions' = initialSubstitutions library maybeFrontMatter
    --parseTemplate' sourceText projectRoot Nothing substitutions'
    mainTemplate <- automaticCompileText projectRoot sourceText
    -- Piling on terrible hacks. also repeat.
    -- Honestly default substitutions like this is kind of stupid. Can add later if it's such a good idea.
    let templateText = substitute mainTemplate $ Map.fromList substitutions'
    case splitFrontmatter templateText of
        Nothing -> pure templateText
        Just (_, restOfText) -> pure restOfText

{- | Render a Mustache template to `Text`.

Main template rendering entrypoint.

-}
renderTemplate :: Library -> FilePath -> Maybe FrontMatter -> Text.Text -> IO Text.Text
renderTemplate library projectRoot maybeFrontMatter sourceText = do
  -- Determine if we are parsing an orphan template or a template that has a parent.
  case (maybeFrontMatter >>= parent, maybeFrontMatter) of
    (Just parentTemplateName, Just frontMatter) -> do
      renderParentTemplate library projectRoot frontMatter sourceText parentTemplateName
    _ ->
      renderOrphanTemplate library projectRoot maybeFrontMatter sourceText