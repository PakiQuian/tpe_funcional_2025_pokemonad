module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- 1. MODELO DE DATOS (ESTADO)
--------------------------------------------------------------------------------
-- Definimos las 3 pantallas posibles
data Screen = Menu | Pokedex | Multiplayer | PlayingAI
    deriving (Show, Eq)

data GameState = GameState
    { currentScreen  :: Screen      -- En qué pantalla estamos
    , selectedOption :: Int         -- Indice del menú (0, 1, 2)
    , bgImage        :: Picture     -- La imagen de fondo cargada en memoria
    }

-- Las opciones del menú en texto
menuOptions :: [String]
menuOptions = 
    [ "1. Ver Pokedex"
    , "2. Conectar P2P"
    , "3. Jugar vs AI"
    ]

-- Estado inicial: Menú, opción 0, y la imagen que pasaremos al iniciar
initialState :: Picture -> GameState
initialState loadedBg = GameState
    { currentScreen = Menu
    , selectedOption = 0
    , bgImage = loadedBg
    }

-- 2. VISTA (RENDER)
-- Esta función toma el Estado y devuelve una "Picture" (dibujo)
--------------------------------------------------------------------------------
draw :: GameState -> Picture
draw state = case currentScreen state of
    Menu -> pictures [ bgImage state  -- 1. Dibujamos el fondo primero
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
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = 
    case currentScreen state of
        Menu -> state { selectedOption = max 0 (selectedOption state - 1) }
        _    -> state

handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = 
    case currentScreen state of
        Menu -> state { selectedOption = min 2 (selectedOption state + 1) }
        _    -> state

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state = 
    case currentScreen state of
        Menu -> state { currentScreen = chooseScreen (selectedOption state) }
        _    -> state

handleInput (EventKey (SpecialKey KeyEsc) Down _ _) state = 
    -- ESC siempre vuelve al menú
    state { currentScreen = Menu }

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
    
    -- Intentamos cargar el fondo. Si falla, el programa crashea (para simplificar ahora)
    -- IMPORTANTE: Debes tener 'background.bmp' en la carpeta donde ejecutas el juego
    bg <- loadBMP "background.bmp"
    
    putStrLn "Iniciando Ventana..."
    
    -- Configuración de la ventana
    let window = InWindow "Pokemonad P2P" (800, 600) (100, 100)
    
    -- Iniciamos el loop del juego
    play 
        window              -- Configuración ventana
        black               -- Color de fondo base (detrás de la imagen)
        30                  -- FPS (cuadros por segundo)
        (initialState bg)   -- Estado inicial
        draw                -- Función de dibujo
        handleInput         -- Función de eventos
        update              -- Función de tiempo