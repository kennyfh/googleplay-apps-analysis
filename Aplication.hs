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

-- - METODOS QUE PODRIAMOS IMPLEMENTAR
-- 1) Numero de atributos que tiene nuestro dataset (hecho)
-- 2) Mostrar las categorías que nos encontramos en el dataset (hecho)
-- 3) Calcular la media de las instalaciones por categoría (hecho)
-- 4) Calcular las 5 aplicaciones con mejor valoración por categoría
-- 5) Porcentaje de cuantas aplicaciones son gratuita o de pago en google play (hecho)
-- 8) Aplicaciones con más descargas (Hecho, tarda casi 1,10 min)
-- 7) Aplicación con más Installs(No es concreto debido a que hay varias con más de 1 millón) (Hecho)
-- 6) Aplicación con más rewiews (Hecho)
-- 5) Porcentaje de cuantas aplicaciones son gratuita o de pago en google play (Hecho)
-- 4) Calcular la media de del rating por categoría 
-- 3) Calcular la media de las instalaciones por categoría



-- =====================================================================
-- MODULOS UTILIZADAS
import Text.CSV -- Implementación de csv en Haskell
import Data.Default -- Librería que nos permite instanciar la clase Default
import Data.List as L
import Data.Ord (comparing)
import Data.Maybe
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
    let fileName = "ejemplos/googleplaystore.csv"
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
        imprimeCabecera cabecera  -- Imprime la cabecera = ["app","category","rating","reviews", "size","installs","typeprice","price","contentrating","genres","lastupdated","currentversion","androidver"]
        let aplications = traduccionRecords cabecera cuerpo
        -- putStrLn $ show aplications -- Muestra por consola la lista aplicaciones
        ------------------------------------------------
        putStrLn "\n"
        putStrLn "Las 5 categorías con mayor número de instalaciones de todas:"
        let porcentajeCat = masPorcentaje5Categorias $ porcentajeCategorias $ listarCategorias $ obtieneLCategorias aplications
        imprMasPorcentaje5Categorias porcentajeCat
        ------------------------------------
        -- putStrLn "\n"
        -- let f1 = [(app x, show(installs x)++"+") | x<-appsMasInstalls aplications]
        -- putStrLn $ show f1
        ------------------------------------
        ------------------------------------
        ------------------------------------
        let x = appMasReviews aplications
        putStrLn $ show x

        ---
        -- let lcat = obtieneLCatPRating aplications
        -- putStrLn $ show lcat
        -- listarRating lcat
        -- putStrLn $ show prueba
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

-- *Main> let cabecera = ["app","category","rating","reviews", "size","installs","typeprice","price","contentrating","genres","lastupdated","currentversion","androidver"]
--
-- *Main> let xs = ["Photo Editor & Candy Camera & Grid & ScrapBook","ART_AND_DESIGN","4.1","159","19M","10,000+","Free","0","Everyone","Art & Design","January 7, 2018","1.0.0","4.0.3 and up"]::Record
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

-- Calcular la media de las instalaciones por categoría
-- =====================================================================
-- (obtieneLCategorias apps) Dada una lista de aplicaciones devuelve una lista de tuplas
-- donde la primera componente es la categoría donde se encuentra la aplicación y la segunda componente
-- es el numero de instalaciones de dicha aplicación 
obtieneLCategorias :: Aplications -> [(String,Int)]
obtieneLCategorias (app:apps)
    | null apps = [(category app, installs app)]
    | otherwise = [(category app, installs app)] ++ (obtieneLCategorias apps)
-- obtieneLCategorias apps = foldl (\acc x -> acc ++ [(category x, installs x)]) [] apps -- + lento

-- Esto devolvería algo así: [("ART_AND_DESIGN",80),("AUTO_AND_VEHICLES",90),("BUSINESS",99),("AUTO_AND_VEHICLES",110),("COMICS",10)]
-- =====================================================================

-- (obtenerInsPCategoria c tuplas) Dada una categoría y una lista de tuplas, nos devuelve el número de instalaciones
-- de la categoría que le hemos dado como parámetro. Por ejemplo:
--  let c =  "ART_AND_DESIGN"
--  let tuplas = [("ART_AND_DESIGN",80),("AUTO_AND_VEHICLES",90),("BUSINESS",99),("AUTO_AND_VEHICLES",110),("COMICS",10)]
--  
--              obtenerInsPCategoria c tuplas == 80

obtenerInsPCategoria :: String -> [(String,Int)] -> Int
obtenerInsPCategoria c ((a,b):tuplas)
    | null tuplas = x
    | otherwise = x + (obtenerInsPCategoria c tuplas)
    where x = if (c==a) then b else 0 -- Si pertenece a la categoría devolvemos su valor, si no 0
-- =====================================================================
-- =====================================================================

-- (listarCategorias) Dada una lista de tuplas, lo que hace es devolverte una lista con el numero 
-- total de instalaciones por categoría. Por ejemplo:
-- let tuplas = [("ART_AND_DESIGN",80),("AUTO_AND_VEHICLES",90),("BUSINESS",99),("AUTO_AND_VEHICLES",110),("COMICS",10)]

--      listarCategorias tuplas == [("ART_AND_DESIGN",80),("AUTO_AND_VEHICLES",200),("BUSINESS",99),("COMICS",10)]
listarCategorias :: [(String,Int)] -> [(String,Int)]
listarCategorias tuplas = [(c,obtenerInsPCategoria c tuplas) | c<-categorias]
    where categorias = L.nub [a | (a,b)<-tuplas]
-- =====================================================================
-- =====================================================================

-- (porcentajeCategorias tuplas) función que haga los porcentajes las categorías respecto a las instalaciones totales. Por ejemplo:
-- let tuplas = [("ART_AND_DESIGN",80),("AUTO_AND_VEHICLES",90),("BUSINESS",99),("AUTO_AND_VEHICLES",110),("COMICS",10)]

        -- porcentajeCategorias tuplas == [("ART_AND_DESIGN",20.565552),("AUTO_AND_VEHICLES",23.136248),("BUSINESS",25.449871),("AUTO_AND_VEHICLES",28.277636),("COMICS",2.570694)]

porcentajeCategorias :: [(String,Int)] -> [(String, Float)]
porcentajeCategorias tuplas = map (\(a,b) -> (a, (fromIntegral (b*100) / fromIntegral total))) tuplas
    where total = sum [b | (a,b) <- tuplas]
-- =====================================================================
-- =====================================================================

-- (masPorcentaje5Categorias) Funcion que haga que nos devuelva las 5 categorias con más porcentaje
masPorcentaje5Categorias :: [(String, Float)] -> [(String, Float)]
masPorcentaje5Categorias tuplas= take 5 $ reverse $ sortBy (comparing (\(x,y) -> y)) tuplas
-- =====================================================================
-- =====================================================================

-- (imprMasPorcentaje5Categorias tuplas) Muestra por pantalla la categoría con su porcentaje

imprMasPorcentaje5Categorias :: [(String, Float)] -> IO()
imprMasPorcentaje5Categorias tuplas= sequence_ $ map (\ (a,b) -> putStrLn $ concat $ [show a, " : ", show b, "%"]) tuplas

-- =====================================================================

-- Porcentaje de cuantas aplicaciones son gratuita o de pago en google play

--Calcula la cantidad de aplicaciones que son de pago.
-- Devuelve una [Float] con los precios del DataSet
listarPrecios :: Aplications -> [Float]
listarPrecios (app:apps)
    | null apps = [price app]
    | otherwise = [price app] ++ listarPrecios (apps)

--Calcula la cantidad de aplicaciones que son de pago.
cantAppsPago :: [Float] -> Int
cantAppsPago (x:xs)
    | null xs = res
    | otherwise = res + (cantAppsPago xs)
    where res = if x/=0.0 then 1 else 0

--Calcula el porcentaje de aplicaciones de pago respecto al total
porcAppsPago :: [Float]-> Int -> String
porcAppsPago xs a = "El "++res++"% de la aplicaciones del DataSet son de pago."
        where res =show ((a * 100) `div`length xs)

-- Precio Medio de las aplicaciones de pago en el DataSet
precMedioAplicaciones :: [Float] -> [Char]
precMedioAplicaciones xs = "El Precio medio de las aplicaciones de pago es: "++show(res)
        where res = sumPrecioAppsPago xs / fromIntegral (cantAppsPago xs)

-- Suma de todos los precios de aplicaciones de pago
sumPrecioAppsPago :: [Float] -> Float
sumPrecioAppsPago (x:xs)
    | null xs = res
    | otherwise = res + (sumPrecioAppsPago xs)
    where res = if x/=0.0 then x else 0

-- sumPrecioAppsPago' :: [Float] -> Float
-- sumPrecioAppsPago' xs = sum $ filter (/=0) xs   
-- =====================================================================
-- =====================================================================
-- Aplicación con más rewiews

appMasReviews :: Aplications -> Aplication
appMasReviews (app:apps)
    | null apps = app
    | otherwise = res
    where aux = appMasReviews apps
          res = if reviews app > reviews aux then app else aux

-- Aplicación con más intalls (Aunque hay varias)

appMasinstalls :: Aplications -> Aplication
appMasinstalls (app:apps)
    | null apps = app
    | otherwise = res
    where aux = appMasinstalls apps
          res = if installs app > installs aux then app else aux


-- [Aplicaciones] con más installs (Se puede hacer con menos cálculos añadiendo en la cabecera las líneas de calculo anteriores)
appsMasInstalls :: Aplications -> Aplications
appsMasInstalls (app:apps)
    | null apps = res
    | otherwise = res ++ appsMasInstalls apps
    where   xs = app : apps
            aux = appMasinstalls xs
            aux1 = installs aux
            res = [app | installs app ==  aux1]

-- appsMasInstalls2 :: Aplications -> []
-- appsMasInstalls2 acc (app:apps)
--     | null apps = acc
--     | otherwise = 

-- appsMasInstalls' :: Aplications -> Aplication -> Aplications
-- appsMasInstalls' (app:apps) appAux
--     | null apps = [app]
--     | otherwise = res ++ appsMasInstalls apps
--     where  res = [app | installs app == installs appAux]

-- =====================================================================
-- =====================================================================


-- Calcular las 5 aplicaciones con mejor valoración por categoría

-- Funcion que obtiene una lista de tuplas compuestas por el nombre de la aplicación, su categoría y 
-- el rating que tiene
obtieneLCatPRating :: Aplications -> [(String,String,Maybe Float)]
obtieneLCatPRating apps = foldl (\acc x -> acc ++ [(app x, category x,rating x)]) [] apps


-- Funcion que dada una categoría y una lista de tuplas, te devuelve una lista con los 5 valores
obtieneRating :: String -> [(String,String,Maybe Float)] -> [(String,Maybe Float)]
obtieneRating cat tuplas =  take 5 $ reverse $ sortBy (comparing (\(x,y) -> y)) $ map (\(a,b,c) -> (a,c)) $ filter (\(a,b,c) -> isJust c && cat==b) tuplas


-- -- listarRating :: [(String,String,Maybe Float)] -> IO()
-- listarRating tuplas = [imprimeprueba cat (obtieneRating cat tuplas) | cat<-categorias]
--     where categorias = L.nub [b | (a,b,c)<-tuplas] -- nos devuelve las categorías

-- impr :: [(String,Maybe Float)] -> IO()
-- impr tuplas= sequence_ $ map (\ (a,b) -> putStrLn $ concat $ [show a, " : ", show b]) tuplas

-- imprimeprueba :: String -> [(String,Maybe Float)] -> IO()
-- imprimeprueba cat tuplas =  do
--     putStrLn cat
--     impr tuplas
--     putStrLn "-----------------------"