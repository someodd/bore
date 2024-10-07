{- | Bore library system.

Cache of assets, but also constants for expected locations for said assets and such.

-}

module Bore.Library
    ( Library(..)
    , loadOnce
    , figletFontDirectory
    , containersDirectory
    ) where

import Bore.Text.Containers (Container, loadContainers)
import Bore.Text.Figlet (FigletCache, loadFigletFonts)
import Bore.FileLayout
import Bore.Config 

import System.FilePath ((</>))


{- | Assets for parser to use when parsing/rendering content.

The hope is to load all the library just once and hopefully pass it around to avoid
frequently re-reading from disk. Keeps it all in memory.

I need to move templates to here, which currently get loaded from disk each time.

Asset management that also cuts down on file path confusion.
-}
data Library = Library
    { containers :: [(RelativePath, Container)]
    , fonts :: FigletCache
    , config :: Config
    }

{- | Load the library.

-}
loadOnce :: FilePath -> IO Library
loadOnce sourceDirectory = do
    -- FIXME: should define containersDirectory here
    containers <- loadContainers (sourceDirectory </> containersDirectory)
    fonts <- loadFigletFonts (sourceDirectory </> figletFontDirectory)
    config <- getConfig (sourceDirectory </> projectConfigFile)
    pure $ Library { containers = containers, fonts = fonts, config = config }