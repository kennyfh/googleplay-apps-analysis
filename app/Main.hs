-- Main
-- Kenny Jesús Flores Huamán
-- Jesús Pineda Marquez
-- Universidad de Sevilla
-- Sevilla, 1 febrero de 2021
-- =====================================================================

module Main where

import Review as R
import Aplication as A
import Text.CSV
import Data.Char


-- MAIN
main :: IO()
main = do
    putStrLn "Bienvenido:"
    mapM_ putStrLn ["1 : Muestra dataset (Aplications) ", "2 : Muestra dataset (Reviews)","3 : Muestra ambos dataset","4 : Salir del menú"]
    putStrLn "Seleccione una opción:"
    x<-getLine
    opciones x
    

-- (opciones op) Según el String (op) nos ejecuta las funciones que correspondan
opciones :: String -> IO()
opciones "1" = do
    putStrLn "La carga completa del dataset puede tardar unos minutos"
    appsDataset
opciones "2" = do 
    putStrLn "La carga completa del dataset puede tardar unos minutos"
    reviewsDataset
opciones "3" = do 
    putStrLn "La carga completa de los datasets puede tardar unos minutos"
    appsDataset
    reviewsDataset
opciones "4" = do
    putStrLn "Saliendo del menú...."
opciones _  = do
    putStrLn "Lo siento, la opción no es correcta, vuelve a intentarlo"
    main

appsDataset :: IO()
appsDataset =  do
    -- Guardamos el nombre de nuestro dataset
    let fileName = "examples/googleplaystore.csv"
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
        A.imprimeCabecera cabecera  -- Imprime la cabecera = ["app","category","rating","reviews", "size","installs","typeprice","price","contentrating","genres","lastupdated","currentversion","androidver"]
        let aplications = A.traduccionRecords cabecera cuerpo
        -- putStrLn $ show aplications -- Muestra por consola la lista aplicaciones
        ------------------------------------------------
        putStrLn "\n"
        putStrLn "Las 5 categorías con mayor número de instalaciones de todas:"
        let porcentajeCat = A.masPorcentaje5Categorias $ A.porcentajeCategorias $ A.listarCategorias $ A.obtieneLCategorias aplications
        A.imprMasPorcentaje5Categorias porcentajeCat
        ------------------------------------
        putStrLn "\n"
        let lPrecios = A.listarPrecios aplications --[Float]
        A.porcAppsPago lPrecios (A.cantAppsPago lPrecios)
        A.precMedioAplicaciones lPrecios
        ------------------------------------
        putStrLn "\n"
        putStrLn "5 de las aplicaciones con más descargas: \n"
        let f1 = take 5 $ A.appsMasInstalls aplications
        A.imprMasInstalls f1
        ------------------------------------
        ------------------------------------
        putStrLn "\n"
        putStrLn "Aplicación con más Reviews: \n"
        let x = A.appMasReviews aplications
        A.imprAPP x
        ------------------------------------
        ------------------------------------
        putStrLn "\n"
        putStrLn "La aplicación con mayor rating por categoría: "
        let lcat = A.listarRating $ A.obtieneLCatPRating aplications
        A.imprimirTablaRting lcat
        ------------------------------------
        putStrLn "\n Calcular la media de del rating por categoría:"
        let lcatR = A.obtieneLCatPRating aplications -- obtieneLCatPRating
        let mediasCat = A.listarMediaCat lcatR
        A.imprimeMediaCat mediasCat
        ------------------------------------
        putStrLn "\n"
        putStrLn "Tests:"
        let x1 = A.listarCategorias $ A.obtieneLPCategorias aplications
        let x2 = A.listarCategorias $ A.obtieneLCategorias aplications
        let x3 = A.medInsPCat x2 x1
        putStrLn "\n"
        A.imprInsPCategorias x3
        putStrLn " "



reviewsDataset :: IO()
reviewsDataset =  do
    -- Guardamos el nombre de nuestro dataset
    let fileName = "examples/googleplaystore_user_reviews_fixed.csv"
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
        --1)
        R.imprimeCabecera cabecera  -- Imprime la cabecera = ["app","translated_Review","sentiment","sentiment_Polarity","sentiment_Subjectivity"]
        let reviews = R.traduccionRecords cabecera cuerpo
        -- putStrLn $ show reviews
        ---------------------
        -- 2)
        putStrLn "\n "
        putStrLn "Cantidad de review segun una evaluación:" 
        R.porcVal reviews "Positive"
        R.porcVal reviews "Neutral"
        R.porcVal reviews "Negative"
        -- R.porcVal [] "Negative" --test
        -- R.porcVal  reviews "" -- test
        --------------------------------------------
        -- 3)
        putStrLn "\n "
        -- Housing-Real Estate & Property --> Es el mejor para hacer pruebas por el número bajo de elementos que tiene (21)
        putStrLn "Por favor, introduzca un nombre de una aplicación (Por ejemplo: 11st, Adobe Acrobat Reader, Agar.io, etc)"
        
        nom <- getLine
        let xs = R.obtenerReviews nom reviews
        if (length xs ==0) then do
            error "La aplicación no existe y/o no hay reviews sobre ella"
        else do
            putStrLn ("Lista de las reviews de la aplicación " ++ (show nom))
            imprimirRTraduccion xs
        ---------------------
        -- 4) 
        putStrLn "\n "
        putStrLn "Por favor, seleccione un comentario poniendo el número correspondiente"
        numero<-getLine
        if (all isDigit numero) then do
            let n =  read numero::Int
            -- putStrLn $ show n
            let coment = R.seleccionarComentario n xs -- comentario sin limpiar
            putStrLn "\n"
            putStrLn "Árbol con las 5 letras más usadas en la review \n"
            putStrLn $ show $ arbolP coment
            imprimeLetras coment
        else do
            error "No se puede seleccionar un comentario con un número inválido"
        -------------------------------------------
        -- 5)
        putStrLn "\n "
        putStrLn "Dada las valoraciones anteriores podemos calcular la media del sentimiento subjetivo calculado en el CSV:"
        putStrLn "\n "
        polAverage reviews "Positive"
        polAverage reviews "Neutral"
        polAverage reviews "Negative"

        putStrLn "\n"    
        putStrLn $ "***********************************************************************************************************************\n" ++
                 "* Las medias más cercanas a 0 indican que la media de comentarios no ha sido en cada caso muy positiva o muy negativa. *\n"
                 ++"* Los valores neutros en el dataset se clasifican como 0 de ahí la media Neutral de 0.                                *\n"
                 ++"***********************************************************************************************************************" 
        putStrLn "\n"