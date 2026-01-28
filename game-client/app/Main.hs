module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- 1. MODELO DE DATOS (ESTADO)
--------------------------------------------------------------------------------
-- Definimos las pantallas posibles (agregamos StartScreen)
data Screen = StartScreen | Menu | Pokedex | Multiplayer | PlayingAI
    deriving (Show, Eq)

data GameState = GameState
    { currentScreen  :: Screen      -- En qué pantalla estamos
    , selectedOption :: Int         -- Indice del menú (0, 1, 2)
    , startBgImage   :: Picture     -- Imagen de fondo para pantalla de inicio
    , menuBgImage    :: Picture     -- Imagen de fondo para menú principal
    }

-- Las opciones del menú en texto
menuOptions :: [String]
menuOptions = 
    [ "1. Ver Pokedex"
    , "2. Conectar P2P"
    , "3. Jugar vs AI"
    ]

-- Estado inicial: Pantalla de inicio, opción 0, y las imágenes cargadas
initialState :: Picture -> Picture -> GameState
initialState startBg menuBg = GameState
    { currentScreen = StartScreen
    , selectedOption = 0
    , startBgImage = startBg
    , menuBgImage = menuBg
    }

-- 2. VISTA (RENDER)
-- Esta función toma el Estado y devuelve una "Picture" (dibujo)
--------------------------------------------------------------------------------
draw :: GameState -> Picture
draw state = case currentScreen state of
    StartScreen -> startBgImage state  -- Solo la pantalla de inicio
    Menu -> pictures [ menuBgImage state  -- 1. Dibujamos el fondo del menú
                     , drawMenu (selectedOption state) -- 2. Dibujamos el menú encima
                     ]
    Pokedex -> pictures [ blank, color white $ text "Pantalla Pokedex (ESC para volver)" ]
    Multiplayer -> pictures [ blank, color white $ text "Pantalla P2P (ESC para volver)" ]
    PlayingAI -> pictures [ blank, color white $ text "Pantalla AI (ESC para volver)" ]

-- Función auxiliar para dibujar el menú centrado
drawMenu :: Int -> Picture
drawMenu selection = pictures (title : options)
  where
    -- Título del juego
    title = translate (-200) 200 
          $ scale 0.5 0.5 
          $ color yellow 
          $ text "POKEMONAD HASKELL"

    -- Generar la lista de opciones visuales
    options = zipWith (drawOption selection) [0..] menuOptions

-- Dibuja una opción individual. Si está seleccionada, la pone roja y grande.
drawOption :: Int -> Int -> String -> Picture
drawOption currentSelection index label = 
    let 
        isSelected = currentSelection == index
        yPosition  = fromIntegral (50 - (index * 60)) -- Separación vertical
        col        = if isSelected then red else white
        scl        = if isSelected then 0.3 else 0.2
        -- Agregamos una "flecha" o pokebola si está seleccionado
        prefix     = if isSelected then ">> " else "" 
    in
        translate (-150) yPosition 
        $ scale scl scl 
        $ color col 
        $ text (prefix ++ label)

-- 3. CONTROLADOR (INPUTS)
-- Reacciona a eventos de teclado
--------------------------------------------------------------------------------
handleInput :: Event -> GameState -> GameState
-- Teclas específicas primero
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { selectedOption = max 0 (selectedOption state - 1) }
        _    -> state

handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { selectedOption = min 2 (selectedOption state + 1) }
        _    -> state

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state = 
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        Menu -> state { currentScreen = chooseScreen (selectedOption state) }
        _    -> state

handleInput (EventKey (SpecialKey KeyEsc) Down _ _) state = 
    -- ESC vuelve al menú (excepto desde StartScreen que pasa al menú también)
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        _           -> state { currentScreen = Menu }

-- Cualquier otra tecla en StartScreen pasa al menú principal
handleInput (EventKey _ Down _ _) state =
    case currentScreen state of
        StartScreen -> state { currentScreen = Menu }
        _           -> state

-- Ignoramos cualquier otro evento (mouse, soltar teclas, etc.)
handleInput _ state = state

-- Helper para convertir índice a pantalla
chooseScreen :: Int -> Screen
chooseScreen 0 = Pokedex
chooseScreen 1 = Multiplayer
chooseScreen 2 = PlayingAI
chooseScreen _ = Menu

-- 4. LOGICA DE TIEMPO (STEP)
-- Se ejecuta X veces por segundo. Por ahora no hace nada.
--------------------------------------------------------------------------------
update :: Float -> GameState -> GameState
update _ state = state

-- 5. MAIN
--------------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "Cargando recursos..."
    
    -- Cargamos ambas imágenes
    -- IMPORTANTE: Debes tener 'background.bmp' y 'main_screen.bmp' en la carpeta donde ejecutas el juego
    startBg <- loadBMP "background.bmp"     -- Fondo de pantalla de inicio
    menuBg  <- loadBMP "main_screen.bmp"    -- Fondo del menú principal
    
    putStrLn "Iniciando Ventana..."
    
    -- Configuración de la ventana
    let window = InWindow "Pokemonad P2P" (1280, 720) (100, 100)
    
    -- Iniciamos el loop del juego
    play 
        window                      -- Configuración ventana
        black                       -- Color de fondo base (detrás de la imagen)
        30                          -- FPS (cuadros por segundo)
        (initialState startBg menuBg)  -- Estado inicial con ambas imágenes
        draw                        -- Función de dibujo
        handleInput                 -- Función de eventos
        update                      -- Función de tiempo