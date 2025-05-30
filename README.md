# Ahorcado Haskell

Juego clásico del ahorcado implementado en Haskell para consola.  
Incluye estadísticas de partidas y sonidos de acierto/error (requiere Windows y PowerShell).

---

## Requisitos

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) instalado.
- PowerShell (incluido en Windows).
- Archivos de sonido `.wav` en la carpeta `sonido/`:
  - `sonido/error.wav`
  - `sonido/exito.wav`
- Un archivo de palabras llamado `palabras.txt` (una palabra por línea, solo letras).

---

## Compilar el ejecutable

Abre una terminal en la carpeta del proyecto y ejecuta:

```sh
ghc [ahorcado.hs](http://_vscodecontentref_/1)