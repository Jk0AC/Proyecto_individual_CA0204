# Datos del proyecto

Esta carpeta contiene la base de datos histórica **S&P 500 US Stock Market Data 2026**, 
obtenida de Kaggle y almacenada en formato CSV (`.csv`).

## Estructura

El archivo contiene las siguientes variables:

| Variable | Descripción |
|---|---|
| `Date` | Fecha de negociación. |
| `Ticker` | Símbolo bursátil de la empresa. |
| `Open`, `High`, `Low`, `Close` | Precios de apertura, máximo, mínimo y cierre. |
| `Adj Close` | Precio de cierre ajustado. |
| `Volume` | Volumen de acciones negociadas. |

## Uso

Los datos se utilizan para la selección de empresas, el análisis de rendimientos, 
volatilidades y correlaciones, así como para la construcción y simulación de carteras
mediante el **Movimiento Browniano Geométrico (MBG)**.

El procesamiento se realiza mediante los scripts de R de la carpeta `cod/`.

## Almacenamiento

Debido a su tamaño, el archivo CSV se gestiona mediante **Git LFS**. Para descargarlo 
después de clonar el repositorio, puede ejecutarse:

```bash
git lfs pull
```

Para más información sobre el conjunto de datos, consúltese su publicación 
original en Kaggle.