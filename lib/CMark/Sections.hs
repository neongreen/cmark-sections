{-# LANGUAGE
TemplateHaskell,
RecordWildCards,
DeriveFunctor,
DeriveTraversable,
OverloadedStrings,
NoImplicitPrelude
  #-}


{- |
__This module is designed to be imported qualified (for instance, as “MD”).__

This library lets you parse Markdown into a hierarchical structure (delimited by headings). For instance, let's say your document looks like this:

@
This is the preface.

First chapter
========================================

This chapter doesn't have sections.

Second chapter
========================================

First section
--------------------

Here's some text.

Second section
--------------------

And more text.
@

It can be represented as a tree:

@
'preface': "This is the preface."
'sections':
    * 'heading': __"First chapter"__
      'content': "This chapter doesn't have sections."
      'sections': []

    * 'heading': __"Second chapter"__
      'sections':
          * 'heading': __"First section"__
            'content': "Here's some text."
            'sections': []

          * 'heading': __"Second section"__
            'content': "And more text."
            'sections': []
@

That's what this library does. Moreover, it lets you access the Markdown source of every node of the tree.

In most cases the only thing you need to do is something like this:

@
'toDocument' . 'parse' ['optSafe', 'optNormalize']
@

You can preprocess parsed Markdown after doing 'parse' as long as you don't add or remove any top-level nodes.
-}
module CMark.Sections
(
  parse,
  toDocument,
  fromDocument,
  Annotated(..),
  Section(..),
  Document(..),
)
where


import BasePrelude
-- Lenses
import Lens.Micro hiding ((&))
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- Markdown
import CMark
-- Containers
import qualified Data.Tree as Tree
-- Lists
import Data.List.Split


{- |
A data type for annotating things with their source. In this library we only use @Annotated [Node]@, which stands for “some Markdown nodes + source”.
-}
data Annotated a = Ann {
  source :: Text,
  value  :: a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

{- |
A section in the Markdown tree. Does not contain subsections (the tree is built using 'Tree.Forest' from "Data.Tree").
-}
data Section = Section {
  -- | Level (from 1 to 6).
  level   :: Int,
  heading :: Annotated [Node],
  -- | Text between the heading and the first subsection. Can be empty.
  content :: Annotated [Node] }
  deriving (Eq, Show)

mkHeadingNode :: Section -> Node
mkHeadingNode s = Node Nothing (HEADING (level s)) (value (heading s))

{- |
The whole parsed Markdown tree.
-}
data Document = Document {
  -- | Text before the first section. Can be empty.
  preface  :: Annotated [Node],
  sections :: Tree.Forest Section }
  deriving (Eq, Show)

{- |
'parse' parses Markdown with the given options and extracts nodes from the initial 'DOCUMENT' node.
-}
parse :: [CMarkOption] -> Text -> Annotated [Node]
parse opts s = Ann s ns
  where
    Node _ DOCUMENT ns = commonmarkToNode opts s

{- | Break Markdown into pieces:

@
    blah blah blah               }
                                 }----> init
    blah blah blah               }

    # foo                        }
                                 }
    blah blah                    }----> (heading, blocks after)
                                 }
    blah blah                    }

    ## bar                       }
                                 }----> (heading, blocks after)
    blah blah                    }

    ...
@
-}
breakAtHeadings
  :: [Node]
  -> ([Node], [(Node, [Node])])     -- ^ (blocks before the first heading,
                                    --    headings + blocks after)
breakAtHeadings nodes =
  let (init':rest') = split (keepDelimsL (whenElt isHeading)) nodes
  in  (init', map (fromJust . uncons) rest')
  where
    isHeading (Node _ (HEADING _) _) = True
    isHeading _ = False

-- | Get start line of a node.
start :: Node -> Int
start (Node (Just p) _ _) = startLine p
start (Node Nothing  _ _) =
  error "CMark.Sections.start: node doesn't have a position"

-- We assume here that two top-level blocks can't possibly be on the same line.
cut
  :: Node      -- ^ First node to include
  -> Node      -- ^ First node to exclude
  -> Text
  -> Text
cut a b = T.unlines . take (start b - start a) . drop (start a - 1) . T.lines

cutTo
  :: Node
  -> Text
  -> Text
cutTo b = T.unlines . take (start b - 1) . T.lines

cutFrom
  :: Node
  -> Text
  -> Text
cutFrom a = T.unlines . drop (start a - 1) . T.lines

{- |
Turn a list of Markdown nodes into a tree.
-}
toDocument :: Annotated [Node] -> Document
toDocument (Ann src nodes) = do
  -- Break at headings
  let prefaceNodes :: [Node]
      restNodes :: [(Node, [Node])]
      (prefaceNodes, restNodes) = breakAtHeadings nodes
  -- Annotate the first block with the source. If there are no headings at
  -- all, we just copy everything; otherwise we cut until the first heading.
  let prefaceAnnotated :: Annotated [Node]
      prefaceAnnotated = case restNodes of
        []    -> Ann src prefaceNodes
        (x:_) -> Ann (cutTo (fst x) src) prefaceNodes
  -- Annotate other blocks with their sources by cutting until the position
  -- of the next block
  let blocks :: [((Int, Annotated [Node]), Annotated [Node])]
      blocks = do
        ((heading, afterBlocks), mbNext) <-
            zip restNodes (tail (map Just restNodes ++ [Nothing]))
        let Node _ (HEADING hLevel) hNodes = heading
        let hSrc = case (afterBlocks, mbNext) of
              (x:_, _)          -> cut heading x src
              ([], Just (x, _)) -> cut heading x src
              ([], Nothing)     -> cutFrom heading src
        let afterBlocksSrc = case (afterBlocks, mbNext) of
              ([], _)            -> ""
              (x:_, Just (y, _)) -> cut x y src
              (x:_, Nothing)     -> cutFrom x src
        return ((hLevel, Ann hSrc hNodes),
                Ann afterBlocksSrc afterBlocks)
  -- A function for turning blocks into a tree
  let makeTree [] = []
      makeTree (((level, heading), rest) : xs) =
        let (nested, others) = span (\x -> x^._1._1 > level) xs
        in  Tree.Node (Section level heading rest) (makeTree nested) :
            makeTree others
  -- Return the result
  Document {
    preface = prefaceAnnotated,
    sections = makeTree blocks }

fromDocument :: Document -> Annotated [Node]
fromDocument Document{..} = Ann src nodes
  where
    flattenForest = concatMap Tree.flatten
    src = source preface <>
          mconcat [source (heading s) <> source (content s) |
                   s <- flattenForest sections]
    nodes = value preface ++
            concat [mkHeadingNode s : value (content s) |
                    s <- flattenForest sections]
