# Código del proyecto

Esta carpeta contiene los scripts de **R** utilizados para el procesamiento, análisis y simulación de 
carteras de inversión mediante el **Movimiento Browniano Geométrico (MBG)** y el método de **Monte Carlo**.

## Estructura de archivos

| Archivo | Descripción |
|---|---|
| `01_carga_librerias.R` | Carga de las bibliotecas necesarias para el desarrollo del proyecto. |
| `02_funciones_auxiliares.R` | Definición de funciones auxiliares utilizadas en los análisis. |
| `03_Revisión_y_depuración_data.R` | Revisión, limpieza y preparación de los datos. |
| `04_Información_descriptiva_data.R` | Obtención de estadísticas e información descriptiva de los datos. |
| `05_Correlaciones_empresas.R` | Análisis de las correlaciones históricas entre las empresas. |
| `06_Modelo_estocástico_y_Monte_Carlo.R` | Implementación del modelo estocástico y las simulaciones de Monte Carlo. |
| `07_Construcción_carteras.R` | Construcción y evaluación de las carteras de inversión. |
| `08_Gráficos.R` | Generación de las visualizaciones utilizadas en el informe. |

## Ejecución

Los scripts se ejecutan desde el documento principal **Quarto (`.qmd`)**, mediante la función `source()` de R, por lo que no es 
necesario ejecutarlos individualmente.Los archivos están numerados para facilitar su identificación y organización dentro 
del flujo de trabajo.

## Documentación

Cada script incluye un encabezado descriptivo que detalla su contenido y propósito específico. Para conocer las operaciones, 
funciones y procedimientos implementados, se recomienda consultar directamente dichos encabezados.

## Reproducibilidad

El documento `.qmd` integra la ejecución del código, los resultados y las visualizaciones, permitiendo generar el informe 
a partir de los scripts contenidos en esta carpeta.