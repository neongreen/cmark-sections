{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | This library lets you parse Markdown into a hierarchical structure
(delimited by headings). For instance, let's say your document looks like
this:

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

That's what this library does. Moreover, it lets you access the Markdown
source of every node of the tree.

In most cases the only thing you need to do is something like this:

@
'nodesToDocument' . 'commonmarkToNodesWithSource' ['optSafe', 'optNormalize']
@

You can preprocess parsed Markdown after doing 'commonmarkToNodesWithSource'
as long as you don't add or remove any top-level nodes.
-}
module CMark.Sections
(
  -- * Parse Markdown to trees
  commonmarkToNodesWithSource,
  nodesToDocument,
  WithSource(..),
    getSource,
    stripSource,
  Section(..),
  Document(..),

  -- * Work with parsed trees
  -- $monoid-note
  flattenDocument,
  flattenSection,
  flattenTree,
  flattenForest,
)
where


#if !(MIN_VERSION_base(4,11,0))
import BasePrelude hiding ((<>))
import Data.Semigroup
#else
import BasePrelude
#endif
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


{- | A data type for annotating things with their source. In this library we
only use @WithSource [Node]@, which stands for “some Markdown nodes + source”.
-}
data WithSource a = WithSource Text a
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, Data)

-- | Extract source from 'WithSource' (it's stored there in a field).
getSource :: WithSource a -> Text
getSource (WithSource src _) = src

-- | Extract data from 'WithSource'.
stripSource :: WithSource a -> a
stripSource (WithSource _ x) = x

instance Semigroup a => Semigroup (WithSource a) where
  WithSource s1 v1 <> WithSource s2 v2 =
    WithSource (s1 <> s2) (v1 <> v2)

instance (Monoid a, Semigroup a) => Monoid (WithSource a) where
  mempty = WithSource "" mempty
  mappend = (<>)

{- | A section in the Markdown tree.

Sections do not contain subsections; i.e. `Section` isn't recursive and the
tree structure is provided by "Data.Tree".

In a @Section a b@, the heading is coupled with a value of type @a@, and
content – with a value of type @b@. This is occasionally useful.
-}
data Section a b = Section {
  -- | Level (from 1 to 6).
  level      :: Int,
  -- | Heading
  heading    :: WithSource [Node],
  -- | Annotation for the heading
  headingAnn :: a,
  -- | Text between the heading and the first subsection. Can be empty.
  content    :: WithSource [Node],
  -- | Annotation for the content
  contentAnn :: b
  }
  deriving (Eq, Show, Generic, Data)

{- | The whole parsed Markdown tree. In a @Document a b@, headings are
annotated with @a@ and content blocks – with @b@.
-}
data Document a b = Document {
  -- | Text before the first section. Can be empty.
  preface    :: WithSource [Node],
  -- | Annotation for the preface
  prefaceAnn :: b,
  -- | A tree with the sections comprising the rest of the document
  sections   :: Tree.Forest (Section a b) }
  deriving (Eq, Show, Generic, Data)

{- | 'commonmarkToNodesWithSource' parses Markdown with the given options and
extracts nodes from the initial 'DOCUMENT' node.
-}
commonmarkToNodesWithSource :: [CMarkOption] -> Text -> WithSource [Node]
commonmarkToNodesWithSource opts src = WithSource src ns
  where
    Node _ DOCUMENT ns = commonmarkToNode opts src

{- | Break Markdown into pieces:

@
    blah blah blah               }---- preface
                                 }----
    blah blah blah               }----

    # foo                        }---- heading
                                 }
    blah blah                    }---- blocks after
                                 }----
    blah blah                    }----

    ## bar                       }---- heading
                                 }
    blah blah                    }---- blocks after
                                 }----
    ...                          }----
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
  -> Text      -- ^ Source that was parsed
  -> Text
cut a b = T.unlines . take (start b - start a) . drop (start a - 1) . T.lines

cutTo
  :: Node      -- ^ First node to exclude
  -> Text      -- ^ Source that was parsed
  -> Text
cutTo b = T.unlines . take (start b - 1) . T.lines

cutFrom
  :: Node      -- ^ First node to include
  -> Text      -- ^ Source that was parsed
  -> Text
cutFrom a = T.unlines . drop (start a - 1) . T.lines

{- | Turn a list of Markdown nodes into a tree.
-}
nodesToDocument :: WithSource [Node] -> Document () ()
nodesToDocument (WithSource src nodes) = do
  -- Break at headings
  let prefaceNodes :: [Node]
      restNodes :: [(Node, [Node])]
      (prefaceNodes, restNodes) = breakAtHeadings nodes
  -- Annotate the first block with the source. If there are no headings at
  -- all, we just copy everything; otherwise we cut until the first heading.
  let prefaceAnnotated :: WithSource [Node]
      prefaceAnnotated = case restNodes of
        []    -> WithSource src prefaceNodes
        (x:_) -> WithSource (cutTo (fst x) src) prefaceNodes
  -- Annotate other blocks with their sources by cutting until the position
  -- of the next block
  let blocks :: [((Int, WithSource [Node]), WithSource [Node])]
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
        return ((hLevel, WithSource hSrc hNodes),
                WithSource afterBlocksSrc afterBlocks)
  -- A function for turning blocks into a tree
  let makeTree [] = []
      makeTree (((level, heading), content) : xs) =
        let (nested, others) = span (\x -> x^._1._1 > level) xs
            section = Section {
              level      = level,
              heading    = heading,
              headingAnn = (),
              content    = content,
              contentAnn = ()
              }
        in  Tree.Node section (makeTree nested) : makeTree others
  -- Return the result
  Document {
    preface    = prefaceAnnotated,
    prefaceAnn = (),
    sections   = makeTree blocks
    }

{- $monoid-note

Note that you can use ('<>') to combine 'WithSource' nodes together. It will
concatenate sources and parsed Markdown.

I'm not sure how valid this operation is for Markdown, but probably
more-or-less valid (when you exclude corner cases like missing newlines at
the end and duplicate links). Maybe cmark doesn't even allow duplicate links,
I don't know.
-}

-- | Turn the whole parsed-and-broken-down 'Document' into a list of nodes.
flattenDocument :: Document a b -> WithSource [Node]
flattenDocument Document{..} = preface <> flattenForest sections

-- | Turn a section into a list of nodes.
flattenSection :: Section a b -> WithSource [Node]
flattenSection Section{..} =
  WithSource (getSource heading <> getSource content)
             (headingNode : stripSource content)
  where
    headingNode = Node Nothing (HEADING level) (stripSource heading)

-- | Turn a "Data.Tree" 'Tree.Tree' into a list of nodes.
flattenTree :: Tree.Tree (Section a b) -> WithSource [Node]
flattenTree (Tree.Node r f) = flattenSection r <> flattenForest f

-- | Turn a "Data.Tree" 'Tree.Forest' into a list of nodes.
flattenForest :: Tree.Forest (Section a b) -> WithSource [Node]
flattenForest = mconcat . map flattenSection . concatMap Tree.flatten

----------------------------------------------------------------------------
-- Instances for GHC 7.8
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ == 708
deriving instance Typeable WithSource
deriving instance Typeable Section
deriving instance Typeable Document
#endif
