{-# LANGUAGE
OverloadedStrings,
RecordWildCards,
ViewPatterns,
NoImplicitPrelude
  #-}


module Main where


import BasePrelude
-- Trees
import qualified Data.Tree as Tree
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- Tests
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
-- Markdown
import CMark
import CMark.Sections


main :: IO ()
main = hspec $ do
  describe "converting:" $ do
    it "empty document" $ do
      let src = ""
          preface = mempty
          sections = []
      nodesToDocument (commonmarkToAnnotatedNodes [] src)
        `shouldBe` Document{..}
    it "spaces" $ do
      let src = "  \n\n  \n"
          preface = Ann "  \n\n  \n" []
          sections = []
      nodesToDocument (commonmarkToAnnotatedNodes [] src)
        `shouldBe` Document{..}
    it "paragraph" $ do
      let src = "x"
          preface = Ann "x" [
            Node (Just (PosInfo 1 1 1 1)) PARAGRAPH [text "x"] ]
          sections = []
      nodesToDocument (commonmarkToAnnotatedNodes [] src)
        `shouldBe` Document{..}
    it "3 paragraphs" $ do
      let src = T.unlines ["","x","","","y","","z",""]
          preface = Ann "\nx\n\n\ny\n\nz\n\n" [
            Node (Just (PosInfo 2 1 2 1)) PARAGRAPH [text "x"],
            Node (Just (PosInfo 5 1 5 1)) PARAGRAPH [text "y"],
            Node (Just (PosInfo 7 1 7 1)) PARAGRAPH [text "z"] ]
          sections = []
      nodesToDocument (commonmarkToAnnotatedNodes [] src)
        `shouldBe` Document{..}
    it "headers" $ do
      let src = T.unlines ["# 1", "", "## 2", "", "## 3"]
          preface = mempty
          sections = [
            Tree.Node (Section () 1 (Ann "# 1\n\n" [text "1"]) mempty) [
              Tree.Node (Section () 2 (Ann "## 2\n\n" [text "2"]) mempty) [],
              Tree.Node (Section () 2 (Ann "## 3\n" [text "3"]) mempty) [] ] ]
      nodesToDocument (commonmarkToAnnotatedNodes [] src)
        `shouldBe` Document{..}
    it "headers+content" $ do
      let src = T.unlines ["# 1", "", "## 2", "test", "## 3"]
          preface = mempty
          sections = [
            Tree.Node (Section () 1 (Ann "# 1\n\n" [text "1"]) mempty) [
              Tree.Node (Section () 2 (Ann "## 2\n" [text "2"])
                (Ann "test\n" [Node (Just (PosInfo 4 1 4 4)) PARAGRAPH
                               [text "test"]])) [],
              Tree.Node (Section () 2 (Ann "## 3\n" [text "3"]) mempty) [] ] ]
      nodesToDocument (commonmarkToAnnotatedNodes [] src)
        `shouldBe` Document{..}
    it "preface+headers" $ do
      let src = T.unlines ["blah", "# 1", "", "## 2", "", "## 3"]
          preface = commonmarkToAnnotatedNodes [] "blah\n"
          sections = [
            Tree.Node (Section () 1 (Ann "# 1\n\n" [text "1"]) mempty) [
              Tree.Node (Section () 2 (Ann "## 2\n\n" [text "2"]) mempty) [],
              Tree.Node (Section () 2 (Ann "## 3\n" [text "3"]) mempty) [] ] ]
      nodesToDocument (commonmarkToAnnotatedNodes [] src)
        `shouldBe` Document{..}

  describe "reconstruction:" $ do
    it "paragraph + ###-header" $
      fromToDoc "foo\n\n# bar\n"
    it "paragraph + ===-header" $
      fromToDoc "foo\n\nbar\n===\n"
    it "no blank line after header" $
      fromToDoc "# header\n# header\n"
    it "header + blockquote" $
      fromToDoc "# header\n\n> a blockquote\n"
    it "header + list" $
      fromToDoc "# header\n * item\n"
    it "blanks + header" $
      fromToDoc "\n\n\n# header\n"
    modifyMaxSize (*20) $ modifyMaxSuccess (*10) $
      prop "QuickCheck" $
        forAllShrink mdGen shrinkMD $ \(T.concat -> src) ->
          let md1 = commonmarkToAnnotatedNodes [] src
              md2 = flattenDocument . nodesToDocument $ md1
              err = printf "%s: %s /= %s" (show src) (show md1) (show md2)
          in  counterexample err (compareMD md1 md2)

text :: Text -> Node
text t = Node Nothing (TEXT t) []

fromToDoc :: Text -> Expectation
fromToDoc src =
  flattenDocument (nodesToDocument (commonmarkToAnnotatedNodes [] src))
    `shouldBeMD` commonmarkToAnnotatedNodes [] src

shouldBeMD :: Annotated [Node] -> Annotated [Node] -> Expectation
shouldBeMD x y = x `shouldSatisfy` (compareMD y)

-- | Check that pieces of Markdown are equivalent (modulo trailing newline
-- and position info).
compareMD :: Annotated [Node] -> Annotated [Node] -> Bool
compareMD x y =
  map (\(Node _ a b) -> Node Nothing a b) (annValue x) ==
  map (\(Node _ a b) -> Node Nothing a b) (annValue y)
  &&
  or [annSource x == annSource y,
      and [not (T.isSuffixOf "\n" (annSource x)),
           T.isSuffixOf "\n" (annSource y),
           annSource x == T.init (annSource y)],
      and [not (T.isSuffixOf "\n" (annSource y)),
           T.isSuffixOf "\n" (annSource x),
           annSource y == T.init (annSource x)] ]

-- | Try to shrink Markdown.
shrinkMD :: [Text] -> [[Text]]
shrinkMD = shrinkList shrinkNothing

-- | Generate random Markdown.
mdGen :: Gen [Text]
mdGen = do
  ls <- listOf $ elements [
    -- ###-headers
    "# header 1\n", "# header 1 #\n",
    "## header 2\n",
    "### header 3\n",
    "#### header 4\n",
    "##### header 5\n",
    "###### header 6\n",
    " # header 1\n",   "  # header 1\n",
    " ## header 2\n",  "  ## header 2\n",
    -- ===-headers
    "header 1\n======\n", "header 2\n------\n",
    "multiline\nheader 1\n======\n", "multiline\nheader 2\n------\n",
    -- blocks with headers inside
    "> # header\n", "* # header\n",
    -- lists
    "* item\n", " * item\n",
    "+ item 1\n+ item 2\n",
    -- links and link references
    "[link][link]\n",
    "[link]: http://google.com\n",
    "> [link]: http://google.com\n",
    -- blockquotes
    "> blockquote\n> 1\n", ">> blockquote\n>> 2\n",
    -- other blocks
    "  a *paragraph*\n",
    "*multiline*\nparagraph\n",
    "~~~\ncode block\n~~~\n", "    code\n",
    "---\n", "* * *\n", " * * * \n",
    -- other things
    "", " ", "    ", "\n", "\n\n",
    "`", "``", "```"]
  let randomNL = T.replicate <$> choose (0, 3) <*> pure "\n"
  concat <$> mapM (\x -> do nl <- randomNL; return [x, nl]) ls
