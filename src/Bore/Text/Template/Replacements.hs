module Bore.Text.Template.Replacements where


import qualified Data.Text as Text
import qualified Data.HashMap.Strict as H

import Text.Mustache
--import Text.Mustache (compileTemplate, overText, automaticCompile, Template)
import qualified Text.Mustache.Types as Mtype

import qualified Data.Map as Map

import Bore.FrontMatter
import Data.Maybe (fromMaybe)

import Bore.Text.Containers (applyContainer, ContainerCache)
import Bore.Library (Library(..))
import Bore.Text.Figlet
import Bore.Text.Wrap (wrapParagraphs)

-- FIXME: should be deleted and just added to initialsubsitutions, i mean if we don't assume any changes to be appended later or something
-- | Convert MustacheSubstitutions to a Value.
toValue :: MustacheSubstitutions -> Mtype.Value
toValue substitutions = Mtype.Object . H.fromList $ substitutions

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
      ("figlet", overText (lambdaFiglet library.fonts)),
      ("wrapVt320WideMode", overText (wrapParagraphs 132)),
      ("wrapVt320StandardMode", overText (wrapParagraphs 80))
    ]
 where
  -- FIXME; if i want to add tagging support i'd add it here for hardcoding
  -- | Add substitutions based off of FrontMatter.
  substitutionsFromFrontMatter ::  Maybe FrontMatter -> Maybe MustacheSubstitutions -> MustacheSubstitutions
  substitutionsFromFrontMatter maybeFrontMatter' possibleHardcodedSubstitutions =
    let hardcodedFrontMatterVars = fromMaybe [] possibleHardcodedSubstitutions
        -- specially handle the "variables" field in the frontmatter
        userFrontMatterVars = fromMaybe [] ( maybeFrontMatter' >>= \fm -> fmap (Map.toList . Map.map (Mtype.String)) $ variables fm ) :: [(Text.Text, Mtype.Value)]
    in hardcodedFrontMatterVars ++ userFrontMatterVars