 import Text.PrettyPrint.Boxes
 import Data.List

 pad width x = x ++ replicate k ' '
   where k = width - length x

 fmt_column :: [String] -> Box
 fmt_column items = hsep // vcat left (intersperse hsep (map (text.pad width) items)) // hsep
   where width = maximum $ map length items
         hsep = text ( replicate width '-' )

 table :: [[String]] -> Box
 table rows = vsep <> hcat top (intersperse vsep (map fmt_column columns)) <> vsep
   where
     columns = transpose rows
     nrows = length rows
     vsep =  vcat left $ map char ("+" ++ (concat $ replicate nrows "|+"))