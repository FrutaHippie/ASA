# Código del Proyecto ASA DataQuest 2026 (WPP 2024)

Para correr todo, pon la carpeta `codigo/` dentro de `ASA/` (junto a los datos) y ejecuta `99_correr_todo.R` en R ≥ 4.2. Sin video tarda unos 3 minutos; con el video completo (3 animaciones, 1950–2100), entre 20 y 40. Al inicio de `99` están las opciones: `CORRER_VIDEO`, `HACER`, `COLUMNA_ETAPA`, `ANIOS` y `SEGUIR_SI_FALLA`. Si un script falla, los demás siguen y al final aparece un resumen; el registro queda en `salidas/registro_corrida.txt`.

Si la carpeta está en otra ruta, cambia `RUTA_BASE` en `00_configuracion.R` o define la variable de entorno `ASA_RUTA`.

| Script | Qué hace | Salidas principales |
| --- | --- | --- |
| `00_configuracion.R` | Define rutas, paquetes, parámetros y colores | — |
| `01_limpieza.R` | Arma el panel de 237 países, la jerarquía regional y la región ODS; une LE60 y la clasificación de ingreso del Banco Mundial (`OGHIST_*.xlsx`); verifica identidades; filtra OtherVariants | `salidas/datos/paises_anual.rds`, `variantes_paises.rds`, `salidas/tablas/reporte_calidad.txt` |
| `02_etapas_transicion.R` | Asigna la etapa de transición de cada país en cada año (con detalle 4a/4b y 5a/5b/5c), las crisis en dos niveles, la tipología de Webb adaptada, el año de la transición migratoria y las clasificaciones alternativas (WPP y Reher); hace la sensibilidad y los contrastes | `salidas/datos/etapas_transicion.rds/.csv` y tablas `etapas_*` |
| `03_timelapse_video.R` | Genera los videos MP4 del mapa por etapas, del plano CBR–CDR y del plano esperanza de vida–TFR | `salidas/video/*.mp4` |
| `04_figuras_video.R` | Figuras listas para el video (1920 × 1080, en español) de las escenas 2, 3, 4 y 6 | `salidas/figuras/video_*.png` |
| `P1` … `P8` | Código base para cada pregunta | `salidas/tablas/P*_*.csv`, `salidas/figuras/P*_*.png` |

## Paquetes

data.table, ggplot2, sf, rnaturalearthdata, av, mclust, cluster, scales, readxl y patchwork. El script de configuración instala los que falten.

## Decisiones que hay que justificar en el informe

- **Unidad de análisis:** países con 1 millón de habitantes o más en 2023 (`UMBRAL_POB_PEQUENO`).
- **Etapas:** la clasificación usa umbrales de TFR (4,5 y 2,5) y crecimiento natural, con suavizado de 5 años y duración mínima de 3 años. Las crisis se marcan aparte. Su robustez se mide con 81 combinaciones de parámetros (columna `certeza`).
- **Etapa 5:** por defecto es crecimiento natural negativo. La columna `etapa_criterio_total` usa en cambio el decrecimiento de la población total.
- **MFPCA (P5):** se calcula sobre 1950–2100. Hay que repetirlo con 1950–2023, porque el tramo proyectado converge por diseño del modelo de la ONU.
- **Crisis en dos niveles:** "catástrofe" (caída de e0 ≥ 5 años; se pinta como Crisis) y "choque" (≥ 2 años; se registra en `nivel_crisis` y se interpola, pero no se pinta). La columna `covid` marca 2020–2022 (Heuveline, 2022).
- **Inicio del descenso de la fecundidad:** 10 % bajo el máximo previo y sin volver a esa meseta (Coale y Treadway, 1986). `inicio_censurado` marca los países cuya meseta no se observa (ya bajaban en 1950); P6 los excluye.
- **Detalle de etapas:** 4a/4b según la TNR (Blacker, 1947) y 5a/5b/5c según la migración neta (Blacker, 1947, p. 97). La etapa 1–5 principal no cambia.
- **Webb (1963):** se usa como *adaptación a escala nacional* (promedios de 5 o 10 años, banda de tolerancia de 1 ‰, F y G fusionados, estimación y proyección por separado). Las reglas están en `00_configuracion.R` y la versión por periodos en P4.
- **Bongaarts (P3):** orden de Bongaarts y Bulatao (1999): migración = Medium/Zero migration; fecundidad = Zero migration/IRZM.
- **Grupo de ingreso (Banco Mundial, OGHIST 1987–2025):** el archivo `OGHIST_*.xlsx` debe estar en `codigo/` o en `externos/`. Cada clasificación se asigna al año del INB en que se basa (la de FY(t+2) al año t). Hay tres columnas: `grupo_ingreso_anual` (la del año), `grupo_ingreso_inicial` (la primera disponible) y `grupo_ingreso` (la vigente, FY27). P7 las usa para cruzar etapas, componentes y migración con el ingreso. Verificar que las reglas de la competencia permiten datos externos.

## Columnas nuevas de `etapas_transicion`

| Columna | Qué es |
| --- | --- |
| `nivel_crisis`, `caida_LEx`, `covid` | Nivel de crisis (Ninguna / Choque / Catástrofe), caída de e0 frente al máximo de 5 años previos y marca COVID |
| `etapa_detalle` | Etapa con 4a/4b y 5a/5b/5c |
| `anio_inicio_descenso`, `anio_inicio_descenso_simple`, `inicio_censurado` | Inicio irreversible, inicio sin irreversibilidad y marca de censura |
| `webb_tipo`, `webb_grupo`, `webb_dinamico` | Tipo de Webb adaptado (5 años), crece/decrece y tipo de Długosz frente a 10 años antes |
| `regimen_migratorio`, `anio_transicion_migratoria_2023/_2100`, `anio_transicion_inversa_2100` | Emisor / equilibrio / receptor y año del paso sostenido de emisor a receptor |
| `fase_wpp`, `inicio_fase2_wpp`, `inicio_fase3_wpp`, `anio_inicio_reher` | Clasificaciones alternativas para la sensibilidad |

Para el video con el detalle de etapas: `COLUMNA_ETAPA <- "etapa_detalle"` antes de correr `03_timelapse_video.R`. Para hacer solo una animación: `HACER <- "tfr_e0"`.
