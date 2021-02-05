-- Main
-- Kenny Jesús Flores Huamán
-- Jesús Pineda Marquez
-- Universidad de Sevilla
-- Sevilla, 1 febrero de 2021
-- =====================================================================

{- BORRAR AL FINAL
-- • Dos usos de cada concepto básico de programación funcional visto en la asignatura. Es
-- decir: al menos usar 2 funciones básicas de prelude y Data.List, definir 2 funciones
-- recursivas, definir 2 funciones por patrones, 2 usos de guardas, 2 usos de case of, 2 usos
-- de listas por comprensión, 2 usos de orden superior, declaraciones de tipos para todas las
-- funciones definidas, 2 usos de evaluación perezosa, etc.
-- • Creación de un módulo
-- • Creación de dos tipos de datos nuevos y usos de éstos.
-- • Uso de al menos dos de tipos de datos abstractos o librerías vistos en la asignatura (por
-- ejemplo, pilas, colas, map, matrix, array).
-}
-- INVESTIGACIÓN DEL MERCADO UTILIZANDO ANALISIS ESTADÍSTICOS EN UN DATASET DE APLICACIONES DE GOOGLE PLAY

--- METODOS QUE PODRIAMOS IMPLEMENTAR
-- 1) Numero de atributos que tiene nuestro dataset
-- 2) Mostrar las categorías que nos encontramos en el dataset


-- =====================================================================
-- MODULOS UTILIZADAS
import Text.CSV -- Implementación de csv en Haskell
import Data.Default -- Librería que nos permite instanciar la clase Default
-- import GHC.Generics
-- =====================================================================


-- =====================================================================
-- TIPOS DE DATOS UTILIZADOS

-- Creación de un nuevo tipo de datos, que corresponderá con cada fila de atributos
-- que contiene nuestro dataset

data Aplication = App {
    app::String, category::String, rating::Maybe Float, reviews::Int, size::String, installs::Int,
    typeprice::String, price::Float, contentrating::String, genres::String, lastupdated::String,
    currentversion::String, androidver:: String

} deriving (Show, Eq)

-- Creación de una instancia para tener valores por defecto en el caso de que falten campos 
-- en nuestra aplicación
instance Default Aplication where
    def =  App {
    app=def, category=def, rating=def, reviews=def, size=def, installs=def,
    typeprice=def, price=def, contentrating=def, genres=def, lastupdated=def,
    currentversion=def, androidver=def
    }

type Aplications = [Aplication] -- Lista de Aplicaciones
-- =====================================================================

-- =====================================================================
-- MAIN
main :: IO()
main =  do
    -- Guardamos el nombre de nuestro dataset
    let fileName = "googleplaystore.csv"
    -- Analizamos nuestro fichero de entrada para ver si se trata
    -- de un csv o no, dependiendo de ello lo trataremos de una manera u otra.
    csv <- parseCSVFromFile fileName
    -- La función parseCSVFromFile es de tipo Either Text.Parsec.Error.ParseError CSV
    -- Por lo que si no hay error, devuelve (Right x), siendo x de tipo csv.

    let filas = case csv of
            -- En caso de que no hubiera ningún error, se nos devolvería todas las líneas
            -- de nuestro fichero csv en una lista.
            (Right lineas) -> lineas -- [Record]
            -- En caso contrario, devolvemos []
            _ -> []

    if null filas then 
        putStrLn "El fichero CSV no es válido o carece de contenido"
    else do -- Significa que es un fichero valido 
        let cabecera = head filas -- Cabecera del fichero CSV
        let cuerpo = tail filas -- Filas con información del CSV
        putStrLn "Atributos de cada aplicación \n"
        imprimeCabecera cabecera  -- Imprime la cabecera 
        let aplications = traduccionRecords cabecera cuerpo
        -- putStrLn $ show aplications -- Muestra por consola la lista aplicaciones
        putStrLn " "
-- =====================================================================


-- FUNCIONES AUXILIARES
-- =====================================================================
-- (imprimeCabecera xs) procesa la cabecera, imprimiendo por cada campo de la misma su número y la línea.
-- Por ejemplo:
-- *Main> let ej = ["app","category","rating","reviews"]::Record
-- *Main> imprimeCabecera ej
-- 1 : app
-- 2 : category
-- 3 : rating
-- 4 : reviews
imprimeCabecera :: Record -> IO()
imprimeCabecera xs = sequence_ $ map (\ (x,y) -> putStrLn $ concat $ [show x," : ",y]) (zip [1..] xs)
-- =====================================================================


-- =====================================================================
-- (traduccionRecords cabecera xs) Dada una lista de Strings con la cabecera del csv "cabecera" y 
-- una lista de Records "xs", obtiene un tipo Aplications

traduccionRecords :: [String] -> [Record] -> Aplications
traduccionRecords cabecera xs = [traduccionRecord cabecera x | x<-xs]
-- =====================================================================

-- =====================================================================
-- (traduccionRecord cabecera xs) dada una lista de Strings con la cabecera del csv y un Record,
-- obtiene un tipo Aplication. Por ejemplo:

-- *Main> let cabecera = ["app","category","rating","reviews", "size","installs","typeprice","price"
-- ,"contentrating","genres","lastupdated","currentversion","androidver"]
--
-- *Main> let xs = ["Photo Editor & Candy Camera & Grid & ScrapBook","ART_AND_DESIGN","4.1","159","19M",
-- "10,000+","Free","0","Everyone","Art & Design","January 7, 2018","1.0.0","4.0.3 and up"]::Record
--
-- *Main> traduccionRecord cabecera xs
--
-- App {app = "Photo Editor & Candy Camera & Grid & ScrapBook", category = "ART_AND_DESIGN", 
-- rating = Just 4.1, reviews = 159, size = "19M", installs = 10000, typeprice = "Free", 
-- price = 0.0, contentrating = "Everyone", genres = "Art & Design", lastupdated = "January 7, 2018",
--  currentversion = "1.0.0", androidver = "4.0.3 and up"}

traduccionRecord :: [String] -> Record -> Aplication -- [Photo Editor & Candy Camera & Grid & ScrapBook,ART_AND_DESIGN,4.1,159,19M,"10,000+",Free,0,Everyone,Art & Design,"January 7, 2018",1.0.0,4.0.3 and up]
traduccionRecord cabecera xs = variable
    where ts =  [(a,b) | (a,b) <- (zip cabecera xs)] -- [(app,photoeditor...), (Category,ART_AND_DESING).....]
          variable = def {
          app=(traduce ts "app"), category=(traduce ts "category"), rating=(traduceRating (traduce ts "rating")), reviews=(traduceReview (traduce ts "reviews")),
          size=(traduce ts "size"), installs=(traduceInstall (traduce ts "installs")),
          typeprice=(traduce ts "typeprice"), price=(traducePrice (traduce ts "price")), contentrating=(traduce ts "contentrating"), genres=(traduce ts "genres"), lastupdated=(traduce ts "lastupdated"),
          currentversion=(traduce ts "currentversion"), androidver=(traduce ts "androidver")
          }
-- =====================================================================

-- =====================================================================
-- (traduce xs str) dada una listas de tuplas "xs" y un string "str",
-- va a devolver el segundo elemento de la tupla si el primer elemento de la tupla es igual al "str".

traduce :: [(String,String)] -> String -> String
traduce xs str = head [b | (a,b)<-xs, a==str]
-- =====================================================================

-- =====================================================================
-- (traduceRating str) es la devolución de un Maybe Float dependiendo de si se conoce el rating
-- o el dato es desconocido. Por ejemplo:
--    traduceRating "NaN" == Nothing
--    traduceRating "2.3" == Just 2.3

traduceRating :: String -> Maybe Float
traduceRating str
    | str=="NaN" = Nothing
    | otherwise = Just $ read str :: Maybe Float
-- =====================================================================

-- =====================================================================
-- (traduceReview str) es la devolución de un Integer dado un String. Por ejemplo:
--   traduceReview "159" == 159     

traduceReview :: String -> Int 
traduceReview str = read str :: Int
-- =====================================================================

-- =====================================================================
-- (traduceInstall str) es la devolución de un Integer dado un String, habiéndo tratado el str
-- para que pueda ser usado como Integer. Por ejemplo:
--   traduceInstall "10,000+" == 10000

traduceInstall :: String -> Int
traduceInstall str = read (eliminarcoma $ init str) :: Int
    where eliminarcoma str = filter (\x -> x/=',') str
-- =====================================================================

-- =====================================================================
-- (traducePrice str) es la devolución de un valor Float dado un String, habiendo tratado el str para
-- que pueda ser usado como Float. Por ejemplo:
--   traducePrice "$4.99" == 4.99

traducePrice :: String -> Float
traducePrice str 
        | (str == "0") = read str :: Float
        | otherwise = read (tail str) :: Float
-- =====================================================================


