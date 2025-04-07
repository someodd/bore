{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bore.Text.GopherExtension
 ( gophermenuSpec
 ) where

import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Blocks
import qualified Data.Text as T
import Text.Parsec
import Commonmark.TokParsers
import Data.Tree (Tree(..))
import Control.Monad (void)

-- Helper function to check if a token contains a tab
hasTabInToken :: Tok -> Bool
hasTabInToken tok = '\t' `T.elem` tokContents tok

-- | A syntax specification that ensures lines with tabs are their own blocks
gophermenuSpec :: (Monad m, IsBlock il bl)
               => SyntaxSpec m il bl
gophermenuSpec = mempty
 { syntaxBlockSpecs = [gophermenuBlockSpec]
 }

-- Add this function to your module
peekNextLineHasTab :: Monad m => BlockParser m il bl Bool
peekNextLineHasTab = do
  -- Save current parser state
  pst <- getParserState
  
  -- Try to look at the next line
  result <- option False $ try $ do
    -- Skip to the end of current line
    skipWhile (not . hasType LineEnd)
    void lineEnd
    
    -- Check if we're at the end of input
    notFollowedBy eof
    
    -- Get the next line's content up to its end
    nextLine <- many (satisfyTok (not . hasType LineEnd))
    
    -- Check if any token contains a tab
    return $ any hasTabInToken nextLine
  
  -- Restore the parser state
  _ <- setParserState pst
  
  -- Return whether the next line has a tab
  return result

-- Block specification for lines with tabs (original)
gophermenuBlockSpec :: (Monad m, IsBlock il bl)
                    => BlockSpec m il bl
gophermenuBlockSpec = BlockSpec
 { blockType = "GophermenuItem"
 , blockStart = try $ do
     pos <- getPosition
     
     -- Collect tokens in the current line (up to a LineEnd)
     toks <- many1 (satisfyTok (not . hasType LineEnd))
     
     -- Check if any token contains a tab
     let hasTab = any hasTabInToken toks

     -- check if the NEXT line has a tab
    -- This is a bit tricky, as we need to look ahead
  -- Look ahead to see if the next line has a tab
     nextHasTab <- peekNextLineHasTab
     
     if hasTab
       then do
         eol <- option [] $ (:[]) <$> lookAhead lineEnd
         addNodeToStack $
           Node (defBlockData gophermenuBlockSpec)
                { blockStartPos = [pos]
                , blockLines = [toks ++ if nextHasTab then eol else eol ++ eol]
                } []
         return BlockStartMatch
       else return $ BlockStartNoMatchBefore pos
 
 , blockCanContain = \_ -> False
 , blockContainsLines = False
 , blockParagraph = False
 , blockContinue = \node -> do
     pos <- getPosition
     return (pos, node)
 
 , blockConstructor = \node -> do
     let content = getBlockText node
     return $ plain $ str $ untokenize content
 
 , blockFinalize = defaultFinalizer
 }
