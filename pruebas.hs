-- ["Photo Editor & Candy Camera & Grid & ScrapBook","ART_AND_DESIGN","4.1","159","19M","10,000+","Free","0","Everyone","Art & Design","January 7, 2018","1.0.0","4.0.3 and up"]
-- 2 : ["Coloring book moana","ART_AND_DESIGN","3.9","967","14M","500,000+","Free","0","Everyone","Art & Design;Pretend Play","January 15, 2018","2.0.0","4.0.3 and up"]
-- 3 : ["U Launcher Lite \8211 FREE Live Cool Themes, Hide Apps","ART_AND_DESIGN","4.7","87510","8.7M","5,000,000+","Free","0","Everyone","Art & Design","August 1, 2018","1.2.4","4.0.3 and up"]

import Data.Char

-- let app1 = def {app = (traduce xs "app"), category = (traduce xs "Category"), rating = traduceRating $ traduce xs "rating" }

-- traduce :: [(String,String)] -> String -> String
-- traduce xs str = [b | (a,b)<-xs, a==str]


-- traduceRating :: String -> Maybe Float
-- traduceRating str
--     | str=="NaN" = Nothing
--     | otherwise = Just $ read str :: Maybe Float
-- module Aplication
--     (Aplication,
--     Aplications,
--     imprimeCabecera,
--     traduccionRecords,
--     masPorcentaje5Categorias,
--     porcentajeCategorias,
--     listarCategorias,
--     obtieneLCategorias,
--     imprMasPorcentaje5Categorias,
--     listarPrecios,
--     appsMasInstalls,
--     imprMasInstalls,
--     appMasReviews,
--     listarRating,
--     obtieneLCatPRating,
--     imprimirTablaRting,
--     listarMediaCat,
--     imprimeMediaCat
--     ) where



data Foo2 = Bar2 | Baz2 {bazNumber::Int, bazName::String} deriving (Show)

h :: Foo2 -> Int
h Baz2 {bazName=name} = length name
h Bar2 {} = 0

x = Baz2 1 "Haskell"     -- construct by declaration order, try ":t Baz2" in GHCi
y = Baz2 {bazName = "Curry", bazNumber = 2} -- construct by name

-- ["app","category","rating","reviews", "size","installs","typeprice","price","contentrating","genres","lastupdated","currentversion","androidver"]


-- let f =  obtieneLCategorias aplications
        -- putStrLn $ show f
        -- let f1 = listarCategorias f
        -- putStrLn $ show f1
        -- let f2 =  porcentajeCategorias f1
        -- putStrLn $ show f2
        -- let f3 =  masPorcentaje5Categorias f2



esNumero :: String -> Bool
esNumero xs = case xs of
        "" -> False
        _ -> all isDigit xs
        
