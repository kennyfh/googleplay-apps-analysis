-- Aplication
-- Kenny Jesús Flores Huamán
-- Jesús Pineda Marquez
-- Universidad de Sevilla
-- Sevilla, 1 febrero de 2021
-- =====================================================================


-- INVESTIGACIÓN DEL MERCADO UTILIZANDO ANALISIS ESTADÍSTICOS EN UN DATASET DE APLICACIONES DE GOOGLE PLAY

-- - METODOS QUE PODRIAMOS IMPLEMENTAR
-- 1) Numero de atributos que tiene nuestro dataset (hecho)
-- 2) Mostrar las categorías que nos encontramos en el dataset (hecho)
-- 3) Calcular la media de las instalaciones por categoría (hecho)
-- 4) Calcular las 5 aplicaciones con mejor valoración por categoría
-- 5) Porcentaje de cuantas aplicaciones son gratuita o de pago en google play (hecho)
-- 6) Aplicaciones con más descargas (Hecho, tarda casi 1,10 min)
-- 7) Aplicación con más Installs (No es concreto debido a que hay varias con más de 1 millón) (Hecho)
-- 8) Aplicación con más rewiews (Hecho)
-- 9) Porcentaje de cuantas aplicaciones son gratuita o de pago en google play (Hecho)
-- 10) Calcular la media de del rating por categoría. (Hecho)
-- 11) Calcular la media de las instalaciones por categoría (hecho)

module Aplication
    (Aplication,
    Aplications,
    imprimeCabecera,
    traduccionRecords,
    masPorcentaje5Categorias,
    porcentajeCategorias,
    listarCategorias,
    obtieneLCategorias,
    imprMasPorcentaje5Categorias,
    listarPrecios,
    appsMasInstalls,
    imprMasInstalls,
    appMasReviews,
    listarRating,
    obtieneLCatPRating,
    imprimirTablaRting,
    listarMediaCat,
    imprimeMediaCat,
    cantAppsPago,
    imprAPP,
    porcAppsPago,
    precMedioAplicaciones,
    obtieneLPCategorias,
    medInsPCat,
    imprInsPCategorias
    ) where

-- =====================================================================
-- MODULOS UTILIZADAS
import Text.CSV -- Implementación de csv en Haskell
import Data.Default -- Librería que nos permite instanciar la clase Default
import Data.List as L
import Data.Ord (comparing)
import Data.Maybe
import Text.PrettyPrint.Boxes
import PTables as P

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

-- FUNCIONES
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
-- =====================================================================
-- (listarPrecios apps) Calcula la cantidad de aplicaciones que son de pago o gratis.
--  Devuelve una [Float] con los precios del DataSet
listarPrecios :: Aplications -> [Float]
listarPrecios (app:apps)
    | null apps = [price app]
    | otherwise = [price app] ++ listarPrecios (apps)
-- =====================================================================
-- =====================================================================
-- (cantAppsPago xs) Calcula la cantidad de aplicaciones que son de pago.
-- Por ejemplo:
-- let xs = [2.3,1.2,0.0,1.2]::[Float]

         -- cantAppsPago xs == 3
cantAppsPago :: [Float] -> Int
cantAppsPago (x:xs)
    | null xs = res
    | otherwise = res + (cantAppsPago xs)
    where res = if x/=0.0 then 1 else 0
-- =====================================================================
-- =====================================================================

-- (porcAppsPago xs a) Calcula el porcentaje de aplicaciones de pago respecto al total.
porcAppsPago :: [Float]-> Int -> IO()
porcAppsPago xs a = putStrLn ("El "++res++"% de la aplicaciones del DataSet son de pago.")
        where res =show ((a * 100) `div` length xs)
-- =====================================================================
-- =====================================================================

-- (precMedioAplicaciones xs) Precio Medio de las aplicaciones de pago en el DataSet
precMedioAplicaciones :: [Float] -> IO()
precMedioAplicaciones xs = putStrLn ("El Precio medio de las aplicaciones de pago es: $"++show(res))
        where res = sumPrecioAppsPago xs / fromIntegral (cantAppsPago xs)
-- =====================================================================
-- =====================================================================

-- (sumPrecioAppsPago xs) Suma de todos los precios de aplicaciones de pago
sumPrecioAppsPago :: [Float] -> Float
sumPrecioAppsPago (x:xs)
    | null xs = res
    | otherwise = res + (sumPrecioAppsPago xs)
    where res = if x/=0.0 then x else 0

-- sumPrecioAppsPago' :: [Float] -> Float
-- sumPrecioAppsPago' xs = sum $ filter (/=0) xs   
-- =====================================================================
-- =====================================================================

-- (appMasReviews apps) Aplicación con más rewiews dada una lista de aplicaciones
appMasReviews :: Aplications -> Aplication
appMasReviews (app:apps)
    | null apps = app
    | otherwise = res
    where aux = appMasReviews apps
          res = if reviews app > reviews aux then app else aux
-- =====================================================================
-- =====================================================================

-- (appMasinstalls apps) Aplicación con más intalls (Aunque hay varias)

appMasinstalls :: Aplications -> Aplication
appMasinstalls (app:apps)
    | null apps = app
    | otherwise = res
    where aux = appMasinstalls apps
          res = if installs app > installs aux then app else aux

-- =====================================================================
-- =====================================================================

-- (appsMasInstalls apps) [Aplicaciones] con más installs (Se puede hacer con menos cálculos añadiendo en la cabecera las líneas de calculo anteriores)
appsMasInstalls :: Aplications -> [(String,String)]
appsMasInstalls (apu:apps)
    | null apps = res
    | otherwise = res ++ appsMasInstalls apps
    where   xs = apu : apps
            aux = appMasinstalls xs
            aux1 = installs aux
            res = [ (app apu , show (installs apu) ++ "+") | installs apu ==  aux1]
-- =====================================================================
-- =====================================================================
   
-- (imprMasInstalls) Formato de impresión de la lista de aplicaciones con más descargas
imprMasInstalls :: [(String, String)] -> IO()
imprMasInstalls = mapM_ (\ (a,b) -> putStrLn $ concat $ [a, " : ", b])

-- =====================================================================
-- =====================================================================

-- (imprAPP app) Formato de impresión de una APP
imprAPP :: Aplication -> IO()
imprAPP x = putStrLn res
    where res = if typeprice x == "Free" then
                        concat ["Nombre: ",app x, "\n", "Categoría: ", category x,"\n",
                        "Rating: ",show(fromJust (rating x)), "\n", "Reviews: ",show(reviews x), "\n",
                        "Installs: ",show(installs x), "+ \n", "Precio: ",typeprice x]
                        else
                         concat ["Nombre: ",app x, "\n", "Categoría: ", category x,"\n",
                        "Rating: ", show(fromJust (rating x)) , "\n", "Reviews: ",show(reviews x), "\n",
                        "Installs: ",show(installs x), "+ \n", "Precio: ",show(price x)]

-- =====================================================================
-- =====================================================================
-- Calcular la aplicación con mejor valoración por categoría

-- Funcion que obtiene una lista de tuplas compuestas por el nombre de la aplicación, su categoría y 
-- el rating que tiene
obtieneLCatPRating :: Aplications -> [(String,String,Maybe Float)]
obtieneLCatPRating apps = foldl (\acc x -> acc ++ [(app x, category x,rating x)]) [] apps

-- =====================================================================
-- =====================================================================

-- (obtieneRating cat tuplas) Dada una categoría y una lista de tuplas, te devuelve la aplicación con 
-- mejor rating de cat.
obtieneRating :: String -> [(String,String,Maybe Float)] -> (String,String,Maybe Float)
obtieneRating cat tuplas =  head $ reverse $ sortBy (comparing (\(x,y,z) -> y)) $ filter (\(a,b,c) -> isJust c && cat==b) tuplas
-- obtieneRating :: String -> [(String,String,Maybe Float)] -> [(String,Maybe Float)]
-- obtieneRating cat tuplas =  take 5 $ reverse $ sortBy (comparing (\(x,y) -> y)) $ map (\(a,b,c) -> (a,c)) $ filter (\(a,b,c) -> isJust c && cat==b) tuplas

-- =====================================================================
-- =====================================================================
-- (listarRating tuplas) Nos devuelve una lista con la aplicación con mejor rating por cada categoría.

listarRating :: [(String,String,Maybe Float)] -> [(String,String,Maybe Float)] 
listarRating tuplas =  [(obtieneRating cat tuplas) | cat<-categorias] -- Si en obtieneRating devolvemos una [(String,String,Maybe Float)] tenemos que añadir concat [(obtieneRating.....]
    where categorias = L.nub [b | (a,b,c)<-tuplas] -- nos devuelve las categorías

-- =====================================================================
-- =====================================================================
-- (imprimirTablaRting xs) Dada una lista de tuplas de (app,categoria,rating), imprime una tabla utilizando 
-- el módulo boxes. Por ejemplo:
-- let xs = [("app1",  "cat1", Just 1.9),("app2",  "cat2", Just 2.9)]    

 -- (imprimirTablaRting xs) ==   +----------+---------+------+
                            --   |Aplicacion|Categoria|Rating|
                            --   +----------+---------+------+
                            --   |app1      |cat1     |1.9   |
                            --   +----------+---------+------+
                            --   |app2      |cat2     |2.9   |
                            --   +----------+---------+------+

imprimirTablaRting :: [(String,String,Maybe Float)] -> IO()
-- imprimirTablaRting lista = mapM_ print lista
-- imprimirTablaRting lista = mapM_ (\ (a,b,c) -> putStrLn $ concat $ ["app: ", a,"      " , "Categoria: ", b,"   " , "Puntuacion: ",show $ fromJust c]) lista
imprimirTablaRting lista = putStrLn $ render $ P.table tabla
    where tabla = ["Aplicacion", "Categoria", "Rating"]:[[a,b,show $ fromJust c] | (a,b,c)<-lista]
-- =====================================================================
-- =====================================================================

-- Calcular la media de del rating por categoría.

-- (filtrarRatingCat) Dada una categoría y una lista de tuplas, nos devuelve una lista con todas las aplicaciones
-- que tienen esa categoria
filtrarRatingCat :: String -> [(String,String,Maybe Float)] -> [(String, String, Maybe Float)]
filtrarRatingCat cat tuplas = filter (\(a,b,c) -> isJust c && cat==b) tuplas

-- (obtenerMediaCat cat tuplas) Dada una categoría y una lista de aplicaciones con esa categoría,
-- devuelves una categoría con la media de todos los rating
obtenerMediaCat :: String -> [(String, String, Maybe Float)] -> (String,Float)
obtenerMediaCat cat tuplas = (cat,media)
        where ls = filtrarRatingCat cat tuplas
              xs = [fromJust c | (a,b,c)<-ls]
              media = sum xs / (fromIntegral $ length xs)

-- (listarMediaCat tuplas) Devuelve la media de todas las aplicaciones de cada categoría
listarMediaCat :: [(String,String,Maybe Float)] -> [(String, Float)] 
listarMediaCat tuplas =  [(obtenerMediaCat cat tuplas) | cat<-categorias] -- Si en obtieneRating devolvemos una [(String,String,Maybe Float)] tenemos que añadir concat [(obtieneRating.....]
    where categorias = L.nub [b | (a,b,c)<-tuplas] -- nos devuelve las categorías

-- (imprimeMediaCat tuplas) Muestra por pantalla cada categoría con la media del rating en forma de tabla
imprimeMediaCat :: [(String, Float)] -> IO()
imprimeMediaCat tuplas = putStrLn $ render $ P.table tabla
    where tabla = ["Categoria","Media Rating"]:[[a,show b] | (a,b)<-tuplas]

--------
---------------------11) Calcular la media de las instalaciones por categoría 
-- Obtiene las tuplas [(Categoría, 1)]
obtieneLPCategorias :: Aplications -> [(String,Int)]
obtieneLPCategorias (app:apps)
    | null apps = [(category app, 1)]
    | otherwise = [(category app, 1)] ++ (obtieneLPCategorias apps)

-- Función que calcula la media con las dos listas de tuplas, devido a que vienen en el mismo orden simplemente comparamos
-- si son iguales o no las categorías y trabajamos haciendo el cálculo, es importante el orden de las listas de entrada.
medInsPCat :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
medInsPCat ((a,b):t) ((c,d):h)
        | (length t /= length h) = [("Error en la ejecución", 0)]
        | null t = x
        | otherwise = x ++ medInsPCat t h
        where x = [(a,b`div`d) | a == c]

imprInsPCategorias :: [(String, Int)] -> IO()
-- imprInsPCategorias tuplas= sequence_ $ map (\ (a,b) -> putStrLn $ concat $ [show a, " : ", show b]) tuplas
imprInsPCategorias tuplas = putStrLn $ render $ P.table tabla
    where tabla = ["Categoria","Media Instalaciones"]:[[a,show b] | (a,b)<-tuplas]
