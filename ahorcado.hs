-- ============================================================================
-- Juego del Ahorcado en Haskell
-- ----------------------------------------------------------------------------
-- Este programa implementa el clásico juego del ahorcado en consola.
-- Permite jugar partidas, lleva estadísticas de partidas ganadas, perdidas y
-- abandonadas, y reproduce sonidos de acierto/error usando PowerShell en Windows.
-- ============================================================================

import Data.Char (isAlpha, toLower)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time (diffUTCTime)
import Data.List (nub, isPrefixOf)
import System.FilePath
import System.IO (hFlush, stdout, openFile, hClose, hGetContents, hPutStr, IOMode(ReadMode, WriteMode))
import System.Directory (doesFileExist)
import Control.Monad (unless, when)
import System.Process (callCommand)

-- ============================================================================
-- Constantes de archivos y sonidos
-- ============================================================================

-- Archivo donde se guardan las estadísticas del jugador
archivoEstadisticas :: FilePath
archivoEstadisticas = "estadisticas.txt"

-- Archivo con la lista de palabras válidas (una por línea)
archivoPalabras :: FilePath
archivoPalabras = "palabras.txt"

-- Rutas de los sonidos para acierto y error
sonidoError :: FilePath 
sonidoError = "./sonido/error.wav"

sonidoExito :: FilePath 
sonidoExito = "./sonido/exito.wav"

-- Número máximo de intentos permitidos por partida
numIntentos :: Int
numIntentos = 6

-- ============================================================================
-- Utilidades de procesamiento de palabras
-- ============================================================================

-- Convierte una cadena a minúsculas
limpiarYLlevarAMinusculas :: String -> String
limpiarYLlevarAMinusculas = map toLower

-- Procesa las líneas del archivo de palabras, filtrando solo palabras válidas
-- (alfabéticas y no vacías)
procesarPalabrasSimuladas :: [String] -> [String]
procesarPalabrasSimuladas lineas =
    let palabrasProcesadas = map limpiarYLlevarAMinusculas lineas
    in filter (\p -> not (null p) && all isAlpha p) palabrasProcesadas

-- ============================================================================
-- Selección pseudoaleatoria de palabras
-- ============================================================================

-- Escoge una palabra pseudoaleatoria de la lista usando la hora actual como semilla
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

-- Representa la palabra oculta con guiones bajos
representarPalabraOculta :: String -> String
representarPalabraOculta palabra = map (const '_') palabra

-- ============================================================================
-- Sonido (solo Windows, usando PowerShell)
-- ============================================================================

-- Reproduce un sonido .wav usando PowerShell (Windows)
reproducirSonido :: FilePath -> IO ()
reproducirSonido archivoSonido = 
    callCommand $ "powershell -c (New-Object Media.SoundPlayer \"" ++ archivoSonido ++ "\").PlaySync();"

-- ============================================================================
-- Entrada y validación del usuario
-- ============================================================================

-- Solicita al usuario una letra válida (solo una letra alfabética)
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

-- Verifica si una letra está en la palabra secreta
letraEstaEnPalabra :: Char -> String -> Bool
letraEstaEnPalabra letra palabra = letra `elem` palabra

-- ============================================================================
-- Tipos de datos principales
-- ============================================================================

-- Estado del juego en cada momento
data GameState = GameState
    { palabraSecreta     :: String      -- Palabra a adivinar
    , palabraAdivinada   :: String      -- Estado actual de la palabra adivinada (con guiones bajos)
    , letrasIncorrectas  :: [Char]      -- Letras incorrectas ya intentadas
    , intentosRestantes  :: Int         -- Intentos restantes
    , letrasYaIntentadas :: [Char]      -- Todas las letras ya intentadas
    } deriving (Show, Eq)

-- Crea el estado inicial del juego para una palabra dada
estadoInicialJuego :: String -> GameState
estadoInicialJuego palabra = GameState
    { palabraSecreta     = palabra
    , palabraAdivinada   = representarPalabraOculta palabra
    , letrasIncorrectas  = []
    , intentosRestantes  = numIntentos
    , letrasYaIntentadas = []
    }

-- Revela las letras acertadas en la palabra adivinada
revelarLetras :: Char -> String -> String -> String
revelarLetras letraCorrecta secreta actual =
    [ if charSecreta == letraCorrecta then letraCorrecta else charActual
    | (charSecreta, charActual) <- zip secreta actual
    ]

-- ============================================================================
-- Lógica de un turno de juego
-- ============================================================================

-- Lógica de un turno: actualiza el estado y da un mensaje según la letra jugada
jugarTurnoCompleto :: GameState -> Char -> (String, GameState)
jugarTurnoCompleto estado letra
    | letra `elem` letrasYaIntentadas estado =
        ("Ya ingresaste la letra '" ++ [letra] ++ "'. Prueba con otra.", estado)
    | letra `elem` palabraSecreta estado =
        let nuevoEstado = estado
                { palabraAdivinada   = revelarLetras letra (palabraSecreta estado) (palabraAdivinada estado)
                , letrasYaIntentadas = nub $ letra : letrasYaIntentadas estado
                }
        in ("¡Acertaste! La letra '" ++ [letra] ++ "' está en la palabra.", nuevoEstado)
    | otherwise =
        let nuevoEstado = estado
                { letrasIncorrectas  = nub $ letra : letrasIncorrectas estado
                , intentosRestantes  = intentosRestantes estado - 1
                , letrasYaIntentadas = nub $ letra : letrasYaIntentadas estado
                }
        in ("Lo siento, la letra '" ++ [letra] ++ "' NO está en la palabra.", nuevoEstado)

-- Muestra el estado actual del juego al usuario
mostrarEstadoJuego :: GameState -> IO ()
mostrarEstadoJuego estado = do
    putStrLn "\n------------------------------------"
    putStrLn $ "Palabra: " ++ palabraAdivinada estado
    putStrLn $ "Letras incorrectas: " ++ show (letrasIncorrectas estado)
    putStrLn $ "Intentos restantes: " ++ show (intentosRestantes estado) ++ "/6"
    putStrLn $ "Letras ya intentadas: " ++ show (letrasYaIntentadas estado)
    putStrLn "------------------------------------"

-- ============================================================================
-- Bucle principal del juego
-- ============================================================================

-- Bucle principal del juego: gestiona los turnos y el final de la partida
bucleJuegoPrincipal :: GameState -> Estadisticas -> IO Estadisticas
bucleJuegoPrincipal estadoActual estadisticas = do
    mostrarEstadoJuego estadoActual

    if palabraAdivinada estadoActual == palabraSecreta estadoActual
        then do
            -- El jugador ha ganado
            let nEstadisticas = estadisticas {partidasGanadas = partidasGanadas estadisticas + 1}
            putStrLn $ "¡Felicidades! ¡Ganaste! La palabra era \"" ++ palabraSecreta estadoActual ++ "\"."
            return nEstadisticas
        else if intentosRestantes estadoActual <= 0
            then do
                -- El jugador ha perdido
                let nEstadisticas = estadisticas {partidasPerdidas = partidasPerdidas estadisticas + 1}
                putStrLn $ "¡Perdiste! Agotaste tus intentos. La palabra era \"" ++ palabraSecreta estadoActual ++ "\"."
                return nEstadisticas
            else do
                -- Turno normal
                letraUsuario <- obtenerEntradaValida
                let (mensaje, nuevoEstado) = jugarTurnoCompleto estadoActual letraUsuario
                putStrLn mensaje
                -- Reproduce sonidos según el resultado del turno
                when ("¡Acertaste!" `isPrefixOf` mensaje) $ reproducirSonido sonidoExito
                when ("Lo siento," `isPrefixOf` mensaje) $ reproducirSonido sonidoError
                bucleJuegoPrincipal nuevoEstado estadisticas

-- ============================================================================
-- Estadísticas del jugador
-- ============================================================================

-- Estructura para las estadísticas del jugador
data Estadisticas = Estadisticas
    { partidasGanadas   :: Int
    , partidasPerdidas  :: Int
    , partidasAbandonadas :: Int
    } deriving (Show, Read)

-- Carga la lista de palabras desde un archivo
cargarPalabrasFichero :: FilePath -> IO [String]
cargarPalabrasFichero fp = do
    cs <- readFile fp
    let lineas = lines cs
    let palabras = procesarPalabrasSimuladas lineas
    return palabras

-- Carga las estadísticas desde un archivo, creándolo si no existe
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

-- Guarda las estadísticas en un archivo
guardarEstadisticas :: FilePath -> Estadisticas -> IO ()
guardarEstadisticas nombreArchivo estadisticas = do
    h <- openFile nombreArchivo WriteMode
    hPutStr h $
        show (partidasGanadas estadisticas) ++ "\n" ++
        show (partidasPerdidas estadisticas) ++ "\n" ++
        show (partidasAbandonadas estadisticas) ++ "\n"
    hClose h

-- ============================================================================
-- Menú principal y opciones
-- ============================================================================

-- Muestra el menú principal
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n--- MENÚ PRINCIPAL ---"
    putStrLn "1. Jugar Partida"
    putStrLn "2. Visualizar Estadísticas"
    putStrLn "3. Salir"
    putStr "Selecciona una opción: "
    hFlush stdout

-- Solicita y valida la opción del menú principal
obtenerOpcionMenu :: IO Int
obtenerOpcionMenu = do
    opcionStr <- getLine
    case reads opcionStr of
        [(n, "")] | n >= 1 && n <= 3 -> return n
        _ -> do
            putStrLn "Opción inválida. Por favor, ingresa 1, 2 o 3."
            obtenerOpcionMenu

-- Gestiona la opción seleccionada en el menú principal
manejarOpcion :: Int -> [String] -> Estadisticas -> IO ()
-- Opción 1: Jugar partida
manejarOpcion 1 palabrasDisponibles estadisticas = do
    putStrLn "\n--- Nueva Partida ---"
    if null palabrasDisponibles
        then do 
            putStrLn "No hay palabras válidas para jugar. Revisa tu lista."
            menuPrincipal palabrasDisponibles estadisticas
        else do
            palabraInicial <- escogerPalabraPseudoAleatoria palabrasDisponibles
            putStrLn "Si aprietas Ctrl+C, abandonarás la partida"
            let abandonoEstadisticas = estadisticas {partidasAbandonadas = partidasAbandonadas estadisticas + 1}
            guardarEstadisticas archivoEstadisticas abandonoEstadisticas
            let estadoInicial = estadoInicialJuego palabraInicial
            abandonoEstadisticas <- bucleJuegoPrincipal estadoInicial abandonoEstadisticas
            let nEstadisticas = abandonoEstadisticas {partidasAbandonadas = partidasAbandonadas abandonoEstadisticas - 1}
            guardarEstadisticas archivoEstadisticas nEstadisticas
            putStrLn "La partida ha terminado."
            menuPrincipal palabrasDisponibles nEstadisticas
-- Opción 2: Mostrar estadísticas
manejarOpcion 2 palabras estadisticas = do
    putStrLn "\n--- ESTADÍSTICAS ---"
    putStrLn ("Partidas Ganadas: \t" ++ show (partidasGanadas estadisticas))
    putStrLn ("Partidas Perdidas: \t" ++ show (partidasPerdidas estadisticas))
    putStrLn ("Partidas Abandonadas: \t" ++ show (partidasAbandonadas estadisticas))
    menuPrincipal palabras estadisticas
-- Opción 3: Salir del programa
manejarOpcion 3 _ estadisticas = do
    putStrLn "¡Gracias por jugar! Saliendo del programa."
    guardarEstadisticas archivoEstadisticas estadisticas
-- Opción no reconocida
manejarOpcion _ palabras estadisticas = do
    putStrLn "Opción no reconocida. Volviendo al menú."
    menuPrincipal palabras estadisticas

-- Bucle principal del menú
menuPrincipal :: [String] -> Estadisticas -> IO ()
menuPrincipal palabrasFiltradas estadisticas = do
    mostrarMenu
    opcion <- obtenerOpcionMenu
    manejarOpcion opcion palabrasFiltradas estadisticas

-- ============================================================================
-- Punto de entrada principal del programa
-- ============================================================================

main :: IO ()
main = do
    palabras <- cargarPalabrasFichero archivoPalabras
    estadisticas <- cargarEstadisticas archivoEstadisticas
    if null palabras
        then putStrLn "No hay palabras válidas disponibles. El juego no puede iniciar."
        else do
            putStrLn "Palabras cargadas exitosamente. ¡Bienvenido al Ahorcado!"
            menuPrincipal palabras estadisticas