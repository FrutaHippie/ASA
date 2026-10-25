# =============================================================================
# 00_configuracion.R
# Proyecto ASA DataQuest 2026 - WPP 2024
# Rutas, paquetes y parámetros comunes. Todos los demás scripts lo cargan.
# =============================================================================


# 1. Ruta base del proyecto
#    Si alguien del equipo tiene la carpeta en otro lugar, basta con definir la
#    variable de entorno ASA_RUTA o cambiar la ruta por defecto de abajo.

if (!exists("RUTA_BASE")) {
  RUTA_BASE <- Sys.getenv("ASA_RUTA", unset = "C:/Users/Asus/Documents/2026/Vértice/ASA")
}

# Archivos suministrados (nótese que los dos archivos grandes NO tienen
# extensión .csv: terminan en "_csv" y están dentro de una carpeta del mismo nombre)
RUTA_MEDIUM <- file.path(RUTA_BASE,
                         "WPP2024_Demographic_Indicators_Medium_csv",
                         "WPP2024_Demographic_Indicators_Medium_csv")
RUTA_VARIANTES <- file.path(RUTA_BASE,
                            "WPP2024_Demographic_Indicators_OtherVariants_csv",
                            "WPP2024_Demographic_Indicators_OtherVariants_csv")
RUTA_SUPLEMENTO <- file.path(RUTA_BASE, "WPP2024_Supplement_Births_and_LE60.csv")
RUTA_NOTAS      <- file.path(RUTA_BASE, "WPP2024_Demographic_Indicators_notes.csv")

# Clasificación de ingreso del Banco Mundial (OGHIST_*.xlsx): 01_limpieza.R la
# busca sola en codigo/ o en externos/ y toma la más reciente.

# Carpetas de salida
DIR_DATOS    <- file.path(RUTA_BASE, "salidas", "datos")
DIR_TABLAS   <- file.path(RUTA_BASE, "salidas", "tablas")
DIR_FIGURAS  <- file.path(RUTA_BASE, "salidas", "figuras")
DIR_VIDEO    <- file.path(RUTA_BASE, "salidas", "video")
for (d in c(DIR_DATOS, DIR_TABLAS, DIR_FIGURAS, DIR_VIDEO)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


# 2. Paquetes
#    data.table : lectura y manipulación rápida (los archivos son grandes)
#    ggplot2    : gráficos
#    sf, rnaturalearthdata : mapas del mundo
#    av         : exportar video MP4 a partir de cuadros PNG
#    mclust     : mezclas gaussianas (contraste de la clasificación de etapas)
#    cluster    : silueta para validar clusters
#    readxl     : leer la clasificación de ingreso del Banco Mundial (Excel)

paquetes <- c("data.table", "ggplot2", "sf", "rnaturalearthdata",
              "av", "mclust", "cluster", "scales", "readxl")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) {
  install.packages(faltantes)
}
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})


# 3. Parámetros comunes

ANIO_ULTIMA_ESTIMACION <- 2023   # 1950-2023 = estimaciones; 2024+ = proyección
UMBRAL_POB_PEQUENO     <- 1000   # en miles: países con < 1 millón en 2023
SEMILLA                <- 20261010

# Paleta de etapas de transición (apta para daltonismo; tonos de azul a ocre
# como en el mapa de PRB, más un color aparte para crisis)
COLORES_ETAPA <- c(
  "Etapa 1" = "#08306B",
  "Etapa 2" = "#2171B5",
  "Etapa 3" = "#6BAED6",
  "Etapa 4" = "#A1D99B",
  "Etapa 5" = "#D4A017",
  "Crisis"  = "#B2182B"
)

# Grupos de ingreso del Banco Mundial (de menor a mayor)
COLORES_INGRESO <- c(
  "Ingreso bajo"         = "#8C510A",
  "Ingreso mediano bajo" = "#D8B365",
  "Ingreso mediano alto" = "#5AB4AC",
  "Ingreso alto"         = "#01665E"
)

# Paleta de la etapa con detalle (02_etapas_transicion.R):
#   4a = fecundidad baja con TNR >= 1; 4b = TNR < 1 (bajo el reemplazo)
#   5a = decrecimiento natural compensado por inmigración (la población crece)
#   5b = decrecimiento natural no compensado por la inmigración (decrece)
#   5c = decrecimiento natural sin inmigración neta (migración despreciable o negativa)
COLORES_ETAPA_DETALLE <- c(
  "Etapa 1"  = "#08306B",
  "Etapa 2"  = "#2171B5",
  "Etapa 3"  = "#6BAED6",
  "Etapa 4a" = "#C7E9C0",
  "Etapa 4b" = "#74C476",
  "Etapa 5a" = "#F6D47A",
  "Etapa 5b" = "#D4A017",
  "Etapa 5c" = "#8C6510",
  "Crisis"   = "#B2182B"
)


# 4. Tipología de Webb (1963), adaptada a escala nacional
#    Webb clasificó los distritos de Inglaterra y Gales (1921-1931) según el
#    signo y el peso relativo del crecimiento natural (CN) y de la migración neta
#    (MN). Tipos A-D: la población crece; E-H: decrece. (Definiciones tomadas de
#    Kurek et al., 2015, pp. 61-62; el original de Webb no se pudo consultar.)
#      A: CN > 0, MN < 0, CN >= |MN|      E: CN < 0, MN > 0, |CN| >= MN
#      B: CN > 0, MN > 0, CN >= MN        F: CN < 0, MN < 0, |CN| >= |MN|
#      C: CN > 0, MN > 0, MN >  CN        G: CN < 0, MN < 0, |MN| >  |CN|
#      D: CN < 0, MN > 0, MN >  |CN|      H: CN > 0, MN < 0, |MN| >  CN
#    ADAPTACIÓN (decisiones propias del equipo, no de Webb):
#      - F y G se fusionan en "FG" porque Kurek (2015) los define de forma
#        contradictoria en la p. 61 y la p. 62, y ambos significan "decrece por
#        los dos componentes".
#      - Banda de tolerancia: si |MN| < tol (por mil) la migración se considera
#        despreciable y el tipo depende solo del signo de CN: "N+" o "N-".
#        Sin esta banda, un país con MN = -0,01 por mil saltaría entre A y B.
#        En WPP, ~30 % de los país-años 1950-2023 y ~59 % de 2024-2100 tienen
#        |MN| < 1 por mil (muchas proyecciones fijan la migración en ~0).
#      - Empates y ceros: CN = 0 cuenta como positivo; en un empate de
#        magnitudes gana el crecimiento natural (A, B, E, F), como en Kurek.
#      - El crecimiento usado es CN + MN (no la tasa de crecimiento publicada),
#        para que el tipo y el signo del crecimiento sean coherentes.
#    Grupo: "Crece" (A, B, C, D, N+) o "Decrece" (E, FG, H, N-).

TIPOS_WEBB <- c(
  "A"  = "A. Crece: CN+ supera a la emigración",
  "B"  = "B. Crece: CN+ mayor que la inmigración",
  "C"  = "C. Crece: inmigración mayor que CN+",
  "D"  = "D. Crece: inmigración compensa CN-",
  "N+" = "N+. Crece: solo CN (migración despreciable)",
  "E"  = "E. Decrece: inmigración no compensa CN-",
  "FG" = "FG. Decrece: CN- y emigración",
  "H"  = "H. Decrece: emigración supera a CN+",
  "N-" = "N-. Decrece: solo CN (migración despreciable)"
)
COLORES_WEBB <- c(
  "A" = "#08519C", "B" = "#3182BD", "C" = "#6BAED6", "D" = "#9ECAE1", "N+" = "#C6DBEF",
  "E" = "#FCBBA1", "FG" = "#A50F15", "H" = "#EF3B2C", "N-" = "#FEE0D2"
)

tipo_webb <- function(cn, mn, tol = 1) {
  fcase(
    is.na(cn) | is.na(mn),           NA_character_,
    abs(mn) < tol & cn >= 0,         "N+",
    abs(mn) < tol & cn <  0,         "N-",
    cn >= 0 & mn < 0 & cn >= -mn,    "A",
    cn >= 0 & mn > 0 & cn >=  mn,    "B",
    cn >= 0 & mn > 0,                "C",
    cn <  0 & mn > 0 & mn >  -cn,    "D",
    cn <  0 & mn > 0,                "E",
    cn <  0 & mn < 0,                "FG",
    cn >= 0 & mn < 0,                "H"
  )
}
grupo_webb <- function(tipo) {
  fifelse(is.na(tipo), NA_character_,
          fifelse(tipo %in% c("A", "B", "C", "D", "N+"), "Crece", "Decrece"))
}

# Versión dinámica (Długosz, 2001; en Kurek et al., 2015, pp. 61-62): cruza el
# grupo de Webb en dos periodos.
#   I   Poblamiento permanente  : crece   -> crece
#   II  Repoblamiento           : decrece -> crece
#   III Despoblamiento          : crece   -> decrece
#   IV  Despoblamiento permanente: decrece -> decrece
#   (Los subtipos o/x/a/b de Długosz no se implementan.)
webb_dinamico <- function(grupo_antes, grupo_despues) {
  fcase(
    is.na(grupo_antes) | is.na(grupo_despues),                NA_character_,
    grupo_antes == "Crece"   & grupo_despues == "Crece",      "I. Poblamiento permanente",
    grupo_antes == "Decrece" & grupo_despues == "Crece",      "II. Repoblamiento",
    grupo_antes == "Crece"   & grupo_despues == "Decrece",    "III. Despoblamiento",
    grupo_antes == "Decrece" & grupo_despues == "Decrece",    "IV. Despoblamiento permanente"
  )
}

# Fondo blanco explícito: sin él, algunos PNG salen transparentes y se ven negros
theme_set(theme_minimal(base_size = 12) +
            theme(plot.background = element_rect(fill = "white", colour = NA)))
