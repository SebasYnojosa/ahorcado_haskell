import Data.Char (isAlpha, toLower)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time (diffUTCTime)
import Data.List (nub)
import System.FilePath
import System.IO (hFlush, stdout, openFile, hClose, hGetContents, hPutStr, IOMode(ReadMode, WriteMode))
import System.Directory (doesFileExist)
import Control.Monad (unless)
import Distribution.SPDX (LicenseId(DOC))

archivoEstadisticas :: FilePath
archivoEstadisticas = "estadisticas.txt"
archivoPalabras :: FilePath
archivoPalabras = "palabras.txt"

numIntentos :: Int
numIntentos = 6

limpiarYLlevarAMinusculas :: String -> String
limpiarYLlevarAMinusculas = map toLower

procesarPalabrasSimuladas :: [String] -> [String]
procesarPalabrasSimuladas lineas =
    let palabrasProcesadas = map limpiarYLlevarAMinusculas lineas
    in filter (\p -> not (null p) && all isAlpha p) palabrasProcesadas

escogerPalabraPseudoAleatoria :: [String] -> IO String
escogerPalabraPseudoAleatoria palabrasPosibles = do
    tiempoActual <- getCurrentTime
    let semilla = floor (toRational (diffUTCTime tiempoActual (read "2000-01-01 00:00:00 UTC" :: UTCTime)))
    let numPalabras = length palabrasPosibles
    if numPalabras == 0
        then return ""
        else do
            let indiceAleatorio = fromIntegral (semilla `mod` numPalabras)
            return (palabrasPosibles !! indiceAleatorio)

representarPalabraOculta :: String -> String
representarPalabraOculta palabra = do
    map (const '_') palabra

obtenerEntradaValida :: IO Char
obtenerEntradaValida = do
    putStr "Ingresa una letra: "
    hFlush stdout
    entrada <- getLine

    case entrada of
        [c] | isAlpha c -> return (toLower c)
        _               -> do
            putStrLn "Entrada inválida. Por favor, ingresa una única letra."
            obtenerEntradaValida

letraEstaEnPalabra :: Char -> String -> Bool
letraEstaEnPalabra letra palabra = letra `elem` palabra

data GameState = GameState
    { palabraSecreta     :: String
    , palabraAdivinada   :: String
    , letrasIncorrectas  :: [Char]
    , intentosRestantes  :: Int
    , letrasYaIntentadas :: [Char]
    } deriving (Show, Eq)

estadoInicialJuego :: String -> GameState
estadoInicialJuego palabra = GameState
    { palabraSecreta     = palabra
    , palabraAdivinada   = representarPalabraOculta palabra
    , letrasIncorrectas  = []
    , intentosRestantes  = numIntentos
    , letrasYaIntentadas = []
    }

revelarLetras :: Char -> String -> String -> String
revelarLetras letraCorrecta secreta actual =
    [ if charSecreta == letraCorrecta then letraCorrecta else charActual
    | (charSecreta, charActual) <- zip secreta actual
    ]

jugarTurnoCompleto :: GameState -> Char -> (String, GameState)
jugarTurnoCompleto estado letra = do
    if letra `elem` letrasYaIntentadas estado
        then ("Ya ingresaste la letra '" ++ [letra] ++ "'. Prueba con otra.", estado)
        else if letra `elem` palabraSecreta estado
            then
                let nuevoEstado = estado
                        { palabraAdivinada   = revelarLetras letra (palabraSecreta estado) (palabraAdivinada estado)
                        , letrasYaIntentadas = nub $ letra : letrasYaIntentadas estado
                        }
                in ("¡Acertaste! La letra '" ++ [letra] ++ "' está en la palabra.", nuevoEstado)
            else
                let nuevoEstado = estado
                        { letrasIncorrectas  = nub $ letra : letrasIncorrectas estado
                        , intentosRestantes  = intentosRestantes estado - 1
                        , letrasYaIntentadas = nub $ letra : letrasYaIntentadas estado
                        }
                in ("Lo siento, la letra '" ++ [letra] ++ "' NO está en la palabra.", nuevoEstado)

mostrarEstadoJuego :: GameState -> IO ()
mostrarEstadoJuego estado = do
    putStrLn "\n------------------------------------"
    putStrLn $ "Palabra: " ++ palabraAdivinada estado
    putStrLn $ "Letras incorrectas: " ++ show (letrasIncorrectas estado)
    putStrLn $ "Intentos restantes: " ++ show (intentosRestantes estado) ++ "/6"
    putStrLn $ "Letras ya intentadas: " ++ show (letrasYaIntentadas estado)
    putStrLn "------------------------------------"

bucleJuegoPrincipal :: GameState -> Estadisticas -> IO Estadisticas
bucleJuegoPrincipal estadoActual estadisticas = do
    mostrarEstadoJuego estadoActual

    if palabraAdivinada estadoActual == palabraSecreta estadoActual
        then do
            let nEstadisticas = estadisticas {partidasGanadas = partidasGanadas estadisticas + 1}
            putStrLn $ "¡Felicidades! ¡Ganaste! La palabra era \"" ++ palabraSecreta estadoActual ++ "\"."
            return nEstadisticas
        else if intentosRestantes estadoActual <= 0
            then do
                let nEstadisticas = estadisticas {partidasPerdidas = partidasPerdidas estadisticas + 1}
                putStrLn $ "¡Perdiste! Agotaste tus intentos. La palabra era \"" ++ palabraSecreta estadoActual ++ "\"."
                return nEstadisticas
            else do
                letraUsuario <- obtenerEntradaValida
                let (mensaje, nuevoEstado) = jugarTurnoCompleto estadoActual letraUsuario
                putStrLn mensaje

                bucleJuegoPrincipal nuevoEstado estadisticas

data Estadisticas = Estadisticas
    { partidasGanadas   :: Int
    , partidasPerdidas  :: Int
    , partidasAbandonadas :: Int
    } deriving (Show, Read)

cargarPalabrasFichero :: FilePath -> IO [String]
cargarPalabrasFichero fp = do
    cs <- readFile fp
    let lineas = lines cs
    let palabras = procesarPalabrasSimuladas lineas
    return palabras

cargarEstadisticas :: FilePath -> IO Estadisticas
cargarEstadisticas nombreArchivo = do
    existe <- doesFileExist nombreArchivo
    unless existe $ writeFile nombreArchivo "0\n0\n0"
    h <- openFile nombreArchivo ReadMode
    cs <- hGetContents h
    let lineas = lines cs
        [g, p, a] = take 3 (lineas ++ repeat "0")
        estadisticas = Estadisticas (read g) (read p) (read a)
    length cs `seq` hClose h
    return estadisticas

guardarEstadisticas :: FilePath -> Estadisticas -> IO ()
guardarEstadisticas nombreArchivo estadisticas = do
    h <- openFile nombreArchivo WriteMode
    hPutStr h $
        show (partidasGanadas estadisticas) ++ "\n" ++
        show (partidasPerdidas estadisticas) ++ "\n" ++
        show (partidasAbandonadas estadisticas) ++ "\n"
    hClose h
    

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n--- MENÚ PRINCIPAL ---"
    putStrLn "1. Jugar Partida"
    putStrLn "2. Visualizar Estadísticas"
    putStrLn "3. Salir"
    putStr "Selecciona una opción: "
    hFlush stdout

obtenerOpcionMenu :: IO Int
obtenerOpcionMenu = do
    opcionStr <- getLine
    case reads opcionStr of
        [(n, "")] | n >= 1 && n <= 3 -> return n
        _ -> do
            putStrLn "Opción inválida. Por favor, ingresa 1, 2 o 3."
            obtenerOpcionMenu

manejarOpcion :: Int -> [String] -> Estadisticas -> IO ()
manejarOpcion 1 palabrasDisponibles estadisticas = do
    putStrLn "\n--- Nueva Partida ---"
    print palabrasDisponibles
    if null palabrasDisponibles
        then do 
            putStrLn "No hay palabras válidas para jugar. Revisa tu lista."
            menuPrincipal palabrasDisponibles estadisticas
        else do
            palabraInicial <- escogerPalabraPseudoAleatoria palabrasDisponibles
            putStrLn $ "DEBUG: Palabra secreta para esta partida: " ++ palabraInicial

            let estadoInicial = estadoInicialJuego palabraInicial
            nEstadisticas <- bucleJuegoPrincipal estadoInicial estadisticas
            putStrLn "La partida ha terminado."
            menuPrincipal palabrasDisponibles nEstadisticas

manejarOpcion 2 palabras estadisticas = do
    putStrLn "\n--- ESTADÍSTICAS ---"
    putStrLn ("Partidas Ganadas: \t" ++ show (partidasGanadas estadisticas))
    putStrLn ("Partidas Perdidas: \t" ++ show (partidasPerdidas estadisticas))
    putStrLn ("Partidas Abandonadas: \t" ++ show (partidasAbandonadas estadisticas))
    menuPrincipal palabras estadisticas

manejarOpcion 3 _ estadisticas = do
    putStrLn "¡Gracias por jugar! Saliendo del programa."
    guardarEstadisticas archivoEstadisticas estadisticas


manejarOpcion _ palabras estadisticas = do
    putStrLn "Opción no reconocida. Volviendo al menú."
    menuPrincipal palabras estadisticas

menuPrincipal :: [String] -> Estadisticas -> IO ()
menuPrincipal palabrasFiltradas estadisticas = do
    mostrarMenu
    opcion <- obtenerOpcionMenu
    manejarOpcion opcion palabrasFiltradas estadisticas

main :: IO ()
main = do
    palabras <- cargarPalabrasFichero archivoPalabras
    estadisticas <- cargarEstadisticas archivoEstadisticas

    if null palabras
        then putStrLn "No hay palabras válidas disponibles. El juego no puede iniciar."
        else do
            putStrLn "Palabras cargadas exitosamente. ¡Bienvenido al Ahorcado!"
            menuPrincipal palabras estadisticas