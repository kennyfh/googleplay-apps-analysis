import Review as R
import Text.CSV 

-- MAIN
main :: IO()
main =  do
    -- Guardamos el nombre de nuestro dataset
    let fileName = "ejemplos/googleplaystore_user_reviews_fixed.csv"
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
        R.imprimeCabecera cabecera  -- Imprime la cabecera = ["app","translated_Review","sentiment","sentiment_Polarity","sentiment_Subjectivity"]
        let reviews = R.traduccionRecords cabecera cuerpo
        -- putStrLn $ show reviews -- Muestra por consola la lista aplicaciones
        ------------------------------------------------
        --let f1 = [(app x, show(installs x)++"+") | x<-appsMasInstalls aplications]
        --putStrLn $ show f1
        putStrLn " "