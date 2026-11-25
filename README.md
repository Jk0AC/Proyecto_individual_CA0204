# Proyecto_individual_CA0204

# Simulación de carteras de inversión mediante Monte Carlo

Este proyecto analiza el comportamiento de distintas carteras de acciones del **S&P 500** 
mediante simulaciones de Monte Carlo basadas en el **Movimiento Browniano Geométrico (MBG)**, 
utilizando datos históricos y herramientas de R.

## Estructura del repositorio

| Carpeta | Contenido |
|---|---|
| `cod/` | Scripts de R para el procesamiento, análisis y simulación de datos. |
| `config/` | Archivos de configuración del proyecto. |
| `data/` | Base de datos histórica de precios de acciones. |
| `info/` | Archivos informativos y documentación complementaria. |
| `res/` | Informe final en PDF generado mediante Quarto. |

## Ejecución

El proyecto se desarrolla en **R y Quarto**. Los scripts de `cod/` se ejecutan 
desde el documento principal `.qmd` mediante la función `source()`.

Para reproducir el análisis, se recomienda abrir el proyecto `proyecto individual.Rproj` 
en RStudio y renderizar el documento `.qmd`.

## Información adicional

Cada carpeta contiene su propia documentación cuando corresponde. Los datos de 
mayor tamaño se gestionan mediante **Git LFS** para facilitar su almacenamiento y descarga.
