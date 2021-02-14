
module PTables
      (columna,
      table
      ) where

import Text.PrettyPrint.Boxes as P
import Data.List


-- Sacado de : https://codereview.stackexchange.com/questions/171992/pretty-printed-tables-in-haskell

columna :: [String] -> Box
columna xs = separacion // vcat left (intersperse separacion (map (text.pad width) xs)) // separacion
   -- (//) -> Une 2 Boxes de manera vertical
   -- intersperse elem xs -> inserta un separador (elem) entre los elementos de la lista (xs)
   where width = maximum $ map length xs
         separacion = text (take width $ repeat '-')
         -- (text) -> convierte nuestro str en un Box
         pad width x = x ++ (take k $ repeat ' ')
               where k = width - length x

table :: [[String]] -> Box
table filas = separacion P.<> hcat top (intersperse separacion (map columna columnas)) P.<> separacion
   -- (<>) -> Une 2 Box de manera horizontal
   -- (hcat) -> Une una lista de Boxes juntos horizontalmente, con la alineación dada (top). 
   where columnas = transpose filas -- transpose = transpone las filas y columnas
         nfilas = length filas
         separacion =  vcat left $ map char ("+" ++ (concat $ take nfilas $ repeat "|+")) -- crea la separación 