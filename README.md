# Pokemonad

- **Materia:** 72.60 - Programación Funcional
- **Profesor:** Pablo Ernesto Martinez Lopez
- **Integrantes:** Francisco Quian Blanco (63006), Theo Stanfield (63403)

Pokemonad es un juego de combate Pokémon por turnos escrito en Haskell, con un
oponente controlado por una IA de **Aprendizaje por Refuerzo** (Q-Learning con
aproximación lineal) y un modo **multijugador peer-to-peer** sobre TCP.

El proyecto se organiza en tres paquetes Cabal, siguiendo el patrón
*Pure Core + Thin IO Layer*:

- **`game-engine`** — núcleo puro: lógica de combate, cálculo de daño y el
  sistema de IA (modelo, entrenamiento por auto-juego y persistencia).
- **`p2p-net`** — biblioteca de red peer-to-peer genérica (sockets TCP,
  protocolo de mensajes y serialización).
- **`game-client`** — ejecutable del juego: interfaz gráfica (`gloss`),
  manejo de eventos y orquestación de los dos paquetes anteriores.

## Requisitos

- **GHC** y **Cabal** (recomendado instalarlos vía [`ghcup`](https://www.haskell.org/ghcup/)).
  El proyecto se compila con GHC 9.6.
- **Dependencias de sistema de OpenGL/GLUT**, requeridas por `gloss` para la
  ventana gráfica. Este es el motivo más común de fallo en una compilación
  desde cero, así que conviene instalarlas antes:

  - **Fedora / RHEL:**
    ```sh
    sudo dnf install freeglut-devel mesa-libGL-devel mesa-libGLU-devel
    ```
  - **Debian / Ubuntu:**
    ```sh
    sudo apt install freeglut3-dev libgl1-mesa-dev libglu1-mesa-dev
    ```

## Compilación

```sh
cabal build all
```

## Ejecución

> [!IMPORTANT]
> El juego debe ejecutarse **desde la raíz del repositorio**. Las imágenes y
> sprites se cargan mediante rutas relativas a `game-client/assets/`, por lo
> que lanzarlo desde otro directorio provocará un error al no encontrar los
> recursos.

```sh
cabal run pokemonad
```

## Cómo jugar

Desde el menú principal se accede a los distintos modos:

- **Un jugador (vs. IA):** se elige un equipo y un entrenador rival. Cada
  entrenador tiene una **dificultad** que controla la tasa de exploración de la
  IA, desde `Easy` (35 % de exploración) hasta `Extreme` (juego greedy, 0 %).
- **Entrenamiento de la IA:** el simulador de IA ejecuta el ciclo de auto-juego
  (*self-play*) de Q-Learning en un hilo aparte y, al finalizar, guarda los
  pesos aprendidos en `game-engine/data/ai_checkpoint.txt`. Si ese archivo no
  existe, la IA usa los pesos por defecto.
- **Multijugador P2P:** un jugador **hospeda** la partida escuchando en un
  puerto; el otro se **conecta** indicando `host` y `puerto`.

### Sobre el multijugador

La arquitectura es **peer-to-peer pura: no hay servidor central**. El jugador
que se conecta lo hace **directamente** contra el que hospeda, por lo que el
host debe ser **alcanzable en la red** desde el otro peer:

- En una **misma LAN** suele funcionar directamente.
- Fuera de la LAN se necesita una **VPN** entre ambos equipos o **redirección
  de puertos** (*port forwarding*) en el router del host.
- Verificar además que el **firewall** del host permita conexiones entrantes en
  el puerto elegido.

## Tests

Para validar que el núcleo puro y la serialización de red funcionan
correctamente:

```sh
cabal test all
```

La suite (basada en `tasty`, con `HUnit` y `QuickCheck`) cubre invariantes de
combate, efectividad de tipos, la ida y vuelta de la serialización P2P y, en
particular, el **determinismo del entrenamiento de la IA**: dado un mismo
generador pseudoaleatorio inicial, el entrenamiento produce exactamente los
mismos pesos, evidenciando la pureza del modelo de Aprendizaje por Refuerzo.
