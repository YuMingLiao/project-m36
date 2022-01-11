{-# lANGUAGE OverloadedStrings #-}
--writes Relation to a String suitable for terminal output
module ProjectM36.Relation.Show.Term where
import ProjectM36.Base 
import ProjectM36.Atom
import ProjectM36.AtomType
import ProjectM36.Tuple
import ProjectM36.Relation
import ProjectM36.Attribute hiding (null)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Text.Lazy.Builder (Builder)
import Control.Arrow hiding (left)
import Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import ProjectM36.WCWidth --guess the width that the character will appear as in the terminal

boxV :: Builder
boxV = "│"
boxH :: Builder
boxH = "─"

boxTL :: Builder
boxTL = "┌"
boxTR :: Builder
boxTR = "┐"
boxBL :: Builder
boxBL = "└"
boxBR :: Builder
boxBR = "┘"

boxLB :: Builder
boxLB = "├"
boxRB :: Builder
boxRB = "┤"
boxTB :: Builder
boxTB = "┬"
boxBB :: Builder
boxBB = "┴"

boxC :: Builder
boxC = "┼"

--represent a relation as a table similar to those drawn by Date
type Cell = Builder 
type Table = ([Cell], [[Cell]]) --header, body

addRow :: [Cell] -> Table -> Table
addRow cells (header,body) = (header, body ++ [cells])

--calculate maximum per-row and per-column sizes

cellLocations :: Table -> ([Int],[Int]) --column size, row size
cellLocations tab@(header, _) = (maxWidths, maxHeights)
  where
    cellSizeMatrix = cellSizes tab
    maxWidths = foldl mergeMax (baseSize (length header)) (map fst cellSizeMatrix)
    baseSize num = replicate num 0
    rowHeights = map snd cellSizeMatrix
    maxHeights = map (\l -> if null l then 0 else L.maximum l) rowHeights
    mergeMax = zipWith max

--the normal "lines" function returns an empty list for an empty string which is not what we want
breakLines :: Builder -> [Builder]
breakLines "" = [""]
breakLines x = map TLB.fromLazyText (TL.lines (TLB.toLazyText x))

cellSizes :: Table -> [([Int], [Int])]
cellSizes (header, body) = map (map maxRowWidth &&& map (length . breakLines)) allRows
  where
    maxRowWidth row = if null (lengths row) then
                         0
                      else
                        L.maximum (lengths row)
    lengths row = map stringDisplayLength (breakLines row)
    allRows = header : body
    
relationAsTable :: Relation -> Table
relationAsTable rel@(Relation _ tupleSet) = (header, body)
  where
    oAttrs = orderedAttributes (attributes rel)
    oAttrNames = orderedAttributeNames (attributes rel)
    header = map TLB.fromText $ map (prettyAttribute) oAttrs
    body :: [[Cell]]
    body = L.foldr tupleFolder [] (asList tupleSet)
    tupleFolder tuple acc = map (TLB.fromText . (\attrName -> case atomForAttributeName attrName tuple of
                                                                   Left _ -> "?"
                                                                   Right atom -> showAtom 0 atom
                                                )) oAttrNames : acc

showParens :: Bool -> StringType -> StringType
showParens predicate f = if predicate then
                      "(" <> f <> ")"
                    else
                      f

showAtom :: Int -> Atom -> StringType
showAtom _ (RelationAtom rel) = TL.toStrict $ TLB.toLazyText $ renderTable $ relationAsTable rel
showAtom level (ConstructedAtom dConsName _ atoms) = showParens (level >= 1 && not (null atoms)) $ T.concat (L.intersperse " " (dConsName : map (showAtom 1) atoms))
showAtom _ (TextAtom t) = "\"" <> t <> "\""
showAtom _ (ByteStringAtom bs) = TE.decodeUtf8 (B64.encode bs)
showAtom _ atom = atomToText atom

renderTable :: Table -> Builder 
renderTable table = renderHeader table (fst cellLocs) <> renderBody (snd table) cellLocs
  where
    cellLocs = cellLocations table

renderHeader :: Table -> [Int] -> Builder 
renderHeader (header, body) columnLocations = renderTopBar <> renderHeaderNames <> renderBottomBar
  where
    renderTopBar = boxTL <> concatTLB (L.intersperse boxTB (map (`repeatString` boxH) columnLocations)) <> boxTR <> "\n"
    renderHeaderNames = renderRow header columnLocations 1 boxV
    renderBottomBar = if null body then ""
                      else renderHBar boxLB boxC boxRB columnLocations <> "\n"

renderHBar :: Builder -> Builder -> Builder -> [Int] -> Builder
renderHBar left middle end columnLocations = left <> concatTLB (L.intersperse middle (map (`repeatString` boxH) columnLocations)) <> end

--pad a block of potentially multi-lined text
leftPaddedString :: Int -> Int -> Builder -> Builder 
leftPaddedString lineNum size str = if lineNum > length paddedLines -1 then
                                      repeatString size " "
                                    else
                                      paddedLines !! lineNum
  where
    paddedLines = map (\line -> line <> repeatString (size - stringDisplayLength line) " ") (breakLines str)

renderRow :: [Cell] -> [Int] -> Int -> Builder -> Builder 
renderRow cells columnLocations rowHeight interspersed = TLB.fromLazyText $ TL.unlines $ map TLB.toLazyText $ map renderOneLine [0..rowHeight-1]
  where
    renderOneLine lineNum = boxV <> concatTLB (L.intersperse interspersed (zipWith (leftPaddedString lineNum) columnLocations cells)) <> boxV

renderBody :: [[Cell]] -> ([Int],[Int]) -> Builder 
renderBody cellMatrix cellLocs = renderRows <> renderBottomBar
  where
    columnLocations = fst cellLocs
    rowLocations = snd cellLocs
    renderRows = concatTLB (map (\(row, rowHeight)-> renderRow row columnLocations rowHeight boxV) rowHeightMatrix)
    rowHeightMatrix = zip cellMatrix (tail rowLocations)
    renderBottomBar = renderHBar boxBL boxBB boxBR columnLocations

repeatString :: Int -> Builder -> Builder
repeatString c s = concatTLB (replicate c s)

showRelation :: Relation -> T.Text 
showRelation rel = TL.toStrict $ TLB.toLazyText (renderTable (relationAsTable rel))

--use wcwidth to guess the string width in the terminal- many CJK characters can take multiple columns in a fixed width font
stringDisplayLength :: Builder -> Int
stringDisplayLength str = TL.foldr charSize 0 (TLB.toLazyText str)
  where
    charSize char accum = let w = wcwidth char in
      accum + if w < 0 then
        1 
      else
        w 

concatTLB :: [Builder] -> Builder
concatTLB = foldr (<>) mempty
