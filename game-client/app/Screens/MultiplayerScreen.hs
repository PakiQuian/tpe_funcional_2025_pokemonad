module Screens.MultiplayerScreen (drawMultiplayerScreen) where

import Engine.Common (drawLogo, drawTextWithShadow, pokemonBlue, pokemonYellow)
import Engine.GameState (GameState (..))
import Engine.World (NetSubState (..))
import Graphics.Gloss
  ( Picture,
    blank,
    color,
    makeColorI,
    pictures,
    polygon,
    rectangleSolid,
    scale,
    text,
    translate,
    white,
  )

-- | Pantalla P2P alineada con Menu / TeamSelect: fondo, logo, caja y tipografía.
drawMultiplayerScreen :: Picture -> Picture -> GameState -> NetSubState -> Picture
drawMultiplayerScreen menuBgImage logoImage gs netSt =
  pictures
    [ menuBgImage,
      drawLogo logoImage,
      translate 0 (-35) $ drawMainPanel gs netSt,
      drawFooter,
      translate 0 (-278) $ maybe blank drawErrorBanner (multiplayerError gs)
    ]

-- Caja principal (mismo patrón borde blanco + interior azul que el menú).
drawMainPanel :: GameState -> NetSubState -> Picture
drawMainPanel gs netSt =
  let row = multiplayerRow gs
      hostStr = multiplayerHost gs
      portStr = multiplayerPort gs
      netLine = netStatusLine netSt
   in pictures
        [ color white $ rectangleSolid 720 400,
          color pokemonBlue $ rectangleSolid 700 380,
          drawTextWithShadow "MULTIJUGADOR P2P" 0.2 155 white,
          drawTextWithShadow "TCP — Host o cliente" 0.12 115 (makeColorI 200 220 255 255),
          drawFieldRow row 0 45 "HOST" hostStr,
          drawFieldRow row 1 (-25) "PUERTO" portStr,
          drawActionRow row 2 (-95) "ESCUCHAR EN ESTE PUERTO (SER HOST)",
          drawActionRow row 3 (-165) "CONECTAR AL HOST INDICADO",
          if null netLine
            then blank
            else drawTextWithShadow netLine 0.11 (-228) (makeColorI 180 230 255 255)
        ]

netStatusLine :: NetSubState -> String
netStatusLine netSt = case netSt of
  NetDisconnected -> ""
  NetListening p -> "Estado: escuchando en puerto " ++ show p ++ " (esperando peer)…"
  NetConnecting h p -> "Estado: conectando a " ++ h ++ ":" ++ show p ++ "…"
  NetInLobby -> "Estado: conectado (lobby)."
  NetInBattle -> "Estado: batalla P2P."

-- | Fila con etiqueta + valor (host / puerto).
drawFieldRow :: Int -> Int -> Float -> String -> String -> Picture
drawFieldRow currentRow idx yBase label val =
  let sel = currentRow == idx
      valColor = if sel then pokemonYellow else makeColorI 195 195 195 255
      line =
        pictures
          [ translate (-260) yBase $
              scale 0.14 0.14 $
                color (makeColorI 160 200 255 255) $
                  text (label ++ ":"),
            translate (-120) yBase $
              scale 0.14 0.14 $
                color valColor $
                  text val
          ]
      cursor =
        if sel
          then translate (-295) (yBase + 5) $ color pokemonYellow $ polygon [(0, 0), (0, 14), (11, 7)]
          else blank
   in pictures [cursor, line]

-- | Fila de acción (solo texto descriptivo).
drawActionRow :: Int -> Int -> Float -> String -> Picture
drawActionRow currentRow idx yBase label =
  let sel = currentRow == idx
      txtColor = if sel then white else makeColorI 165 165 165 255
      cursor =
        if sel
          then translate (-295) (yBase + 5) $ color pokemonYellow $ polygon [(0, 0), (0, 14), (11, 7)]
          else blank
      txt =
        translate (-260) yBase $
          scale 0.13 0.13 $
            color txtColor $
              text label
   in pictures [cursor, txt]

-- | Barra inferior como TeamSelect (fondo oscuro semitransparente + texto con sombra).
drawFooter :: Picture
drawFooter =
  pictures
    [ translate 0 (-320) $
        color (makeColorI 0 0 0 200) $
          rectangleSolid 1280 52,
      drawTextWithShadow
        "ENTER: confirmar fila  |  ARRIBA/ABAJO: mover  |  ESC: volver al menu"
        0.13
        (-327)
        white
    ]

-- | Banner de error debajo de la caja principal (borde claro + interior rojo oscuro).
drawErrorBanner :: String -> Picture
drawErrorBanner err =
  let msg = if length err > 72 then take 69 err ++ "…" else err
   in pictures
        [ color white $ rectangleSolid 620 58,
          color (makeColorI 40 12 12 255) $ rectangleSolid 602 44,
          translate 0 24 $
            color (makeColorI 255 100 100 255) $
              rectangleSolid 598 4,
          drawTextWithShadow ("!  " ++ msg) 0.12 0 (makeColorI 255 210 210 255)
        ]
