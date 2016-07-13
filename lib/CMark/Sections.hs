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
  -- * Basic API
  parse,
  toDocument,
  Annotated(..),
  Section(..),
  Document(..),

  -- * Internal functions
  cut,
  fromDocument,
)
where


import BasePrelude
-- Lenses
import Lens.Micro hiding ((&))
import Lens.Micro.TH
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- Markdown
import CMark
-- Containers
import qualified Data.Tree as Tree
-- Lists
import Data.List.Split


flip makeLensesFor ''PosInfo [
  ("startColumn", "_x1"),
  ("endColumn"  , "_x2"),
  ("startLine"  , "_y1"),
  ("endLine"    , "_y2") ]

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
'parse' parses Markdown with the given options and extracts nodes from the initial 'DOCUMENT' node. It also fixes source annotations of some nodes (see 'fixPosition'), which is needed for the library to work properly.
-}
parse :: [CMarkOption] -> Text -> Annotated [Node]
parse opts s = Ann s (map fixPosition ns)
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

cut
  :: PosInfo      -- ^ Position of the first node which should be included
  -> PosInfo      -- ^ Position of the node at which to stop
  -> Text
  -> Text
cut pa pb s =
  case take (endY-startY+1) . drop (startY-1) $ ls of
    []  -> ""
    -- We only ever cut blocks so the block will always have a newline, and
    -- that's why we use 'unlines' here instead of just 'take' +
    -- 'drop'. Otherwise, let's say, you're cutting something like this:
    --
    --    # header A
    --    # header B
    --
    -- Header B starts at (2, 0), and hence (endX, endY) will be (maxBound,
    -- 1) and we'll end up cutting header A from (1, 1) to (maxBound, 1),
    -- which is only one line. We're cutting a single line. If we didn't use
    -- 'unlines', we'd return "# header A", which would be wrong.
    [x] -> T.unlines [T.take (endX-startX+1) . T.drop (startX-1) $ x]
    xs  -> T.unlines $ xs & over _head (T.drop (startX-1))
                          & over _last (T.take endX)
  where
    ls = T.lines s
    (startX, startY) = (pa^._x1, pa^._y1)
    (endX, endY) | pb^._x1 > 1 = (pb^._x1 - 1, pb^._y1)
                 | otherwise   = (maxBound, pb^._y1 - 1)

cutFromStart
  :: PosInfo
  -> Text
  -> Text
cutFromStart pb s = cut (PosInfo 1 1 1 1) pb s

cutUntilEnd
  :: PosInfo
  -> Text
  -> Text
cutUntilEnd pa s =
  case drop (startY-1) ls of
    []  -> ""
    xs  -> T.unlines (xs & _head %~ T.drop (startX-1))
  where
    ls = T.lines s
    (startX, startY) = (pa^._x1, pa^._y1)

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
        (x:_) -> Ann (cutFromStart (x^._1._pos) src) prefaceNodes
  -- Annotate other blocks with their sources by cutting until the position
  -- of the next block
  let blocks :: [((Int, Annotated [Node]), Annotated [Node])]
      blocks = do
        ((heading, afterBlocks), mbNext) <-
            zip restNodes (tail (map Just restNodes ++ [Nothing]))
        let Node (Just hPos) (HEADING hLevel) hNodes = heading
        let hSrc = case (afterBlocks, mbNext) of
              (x:_, _)          -> cut hPos (x^._pos) src
              ([], Just (x, _)) -> cut hPos (x^._pos) src
              ([], Nothing)     -> cutUntilEnd hPos src
        let afterBlocksSrc = case (afterBlocks, mbNext) of
              ([], _)            -> ""
              (x:_, Just (y, _)) -> cut (x^._pos) (y^._pos) src
              (x:_, Nothing)     -> cutUntilEnd (x^._pos) src
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

{- |
cmark sets some source positions incorrectly and we have to fix them. For instance, with ###-headers it doesn't consider “#” a part of the header (see https://github.com/jgm/cmark/issues/141).

Since we only apply 'fixPosition' to top-level blocks, we simply /always/ set the initial position to the beginning of the line.
-}
fixPosition :: Node -> Node
fixPosition n = n & _pos %~ fixBeginning
  where
    fixBeginning pos =
      pos & _x1 .~ 1

_pos :: Lens' Node PosInfo
_pos f (Node (Just p) x y) = (\p' -> Node (Just p') x y) <$> f p
_pos _ (Node Nothing  x y) =
  error "CMark.Sections._pos: node doesn't have a position"
