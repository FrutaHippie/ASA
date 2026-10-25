# =============================================================================
# 02_etapas_transicion.R
# Etapa de la transición demográfica de cada país en cada año (1950-2100).
#
# IDEA
#   El modelo clásico (Thompson, Notestein; figura de PRB) define las etapas por
#   el nivel de la natalidad, de la mortalidad y del crecimiento natural:
#     Etapa 1: natalidad alta,     mortalidad alta,  crecimiento bajo
#     Etapa 2: natalidad alta,     mortalidad cae,   crecimiento alto
#     Etapa 3: natalidad cae,      mortalidad baja,  crecimiento moderado
#     Etapa 4: natalidad baja,     mortalidad baja,  crecimiento bajo
#     Etapa 5: natalidad muy baja, crecimiento natural negativo
#
# DECISIONES (todas son parámetros en la sección 1, para poder justificarlas
# y someterlas a sensibilidad):
#   a) La fecundidad se mide con la TFR, no con la tasa bruta de natalidad (CBR),
#      porque la CBR depende de la estructura por edad: un país envejecido tiene
#      CBR baja aunque su TFR no haya cambiado. La CBR y la CDR se usan para el
#      crecimiento natural.
#   b) Niveles de fecundidad por umbrales de TFR: "alta" (>= 4.5), "en descenso"
#      (entre 2.5 y 4.5) y "baja" (<= 2.5). Se probó también exigir que la TFR
#      hubiera caído 10 % desde su máximo (inicio de la transición según el
#      Princeton European Fertility Project), pero eso manda a la etapa 3 a países
#      con TFR de 6-7 que apenas empiezan a bajar (Níger, Malí en 2013), en contra
#      de la clasificación de PRB. Ese año de inicio se guarda como variable
#      aparte (anio_inicio_descenso), útil para alinear curvas en P5 y P6.
#      Se exige IRREVERSIBILIDAD, como en Coale y Treadway (1986, p. 37 y
#      Apéndice D, p. 178): el inicio es el primer año en que la TFR queda 10 %
#      por debajo de su máximo previo "in a descent ... that never again returned
#      to the plateau". (La versión sin esa condición queda en
#      anio_inicio_descenso_simple.) Nota: Coale y Treadway lo midieron sobre Ig
#      (fecundidad marital), no sobre la TFR; aquí es una adaptación.
#   c) Las series se suavizan con una media móvil centrada para que un año
#      atípico no cambie la etapa.
#   d) CRISIS EN DOS NIVELES, según la caída de la esperanza de vida (e0) frente
#      a su máximo de los 5 años previos:
#        - "Catástrofe": caída >= 5 años, o crecimiento natural negativo con
#          fecundidad alta (guerras, genocidios, hambrunas: Camboya 1975-79,
#          Ruanda 1994). Se pinta como "Crisis" y NO es la etapa 5.
#        - "Choque": caída >= 2 años (y < 5). Heuveline (2022, PDR 48(1)) muestra
#          que desde 1950 las caídas anuales de e0 rara vez superan 2 años salvo
#          en hambrunas y violencia, y que con el COVID-19 hubo caídas de más de
#          4 años en algunos países. Con un solo umbral de 5 años, el COVID se
#          marcaba en Ecuador, Bolivia y Paraguay pero no en Perú (-4,7) ni en
#          México (-4,8). El choque se registra (nivel_crisis) pero no se pinta.
#        - Marca aparte para el COVID-19 (covid): choque o catástrofe en 2020-2022.
#      En ambos niveles las series se interpolan antes de suavizar, para que la
#      crisis no contamine la etapa de los años vecinos.
#   e) Se exige una duración mínima: un cambio de etapa que dura menos de N años
#      se revierte a la etapa previa (salvo las crisis, que se dejan visibles).
#   f) DETALLE de las etapas 4 y 5 (columna etapa_detalle):
#        4a: TNR >= 1; 4b: TNR < 1. Blacker (1947, pp. 95-96) recomienda guiarse
#            por la tasa neta de reproducción en la fase "estacionaria baja"; la
#            TNR < 1 es el reemplazo por debajo (Espenshade et al., 2003).
#        5a: CN < 0, migración neta > 0 y la población crece (tipo D de Webb).
#        5b: CN < 0, migración neta > 0 y la población decrece (tipo E).
#        5c: CN < 0 y migración neta despreciable o negativa (tipos F/G, N-).
#        Blacker (1947, p. 97): la fase declinante tiene más defunciones que
#        nacimientos y "unless compensated by immigration" la población cae.
#      La etapa 1-5 principal no cambia: el detalle es una capa adicional.
#   g) Tipología de Webb (1963) adaptada a escala nacional (ver 00_configuracion.R)
#      y su versión dinámica (Długosz, 2001), con promedios móviles de 5 años.
#   h) Clasificaciones ALTERNATIVAS para la sensibilidad del inicio y de las
#      fases: fases de WPP 2024 (Metodología, p. 28) e inicio según Reher (2004,
#      p. 21: caída >= 8 % en dos quinquenios sin regresar al nivel de partida).
#   i) Año de la TRANSICIÓN MIGRATORIA: paso sostenido de emisor neto a receptor
#      neto (de Haas, 2021, p. 13; Zelinsky, 1971; Skeldon, 1997).
#
# SALIDAS
#   salidas/datos/etapas_transicion.rds y .csv   (país-año con la etapa)
#   salidas/tablas/etapas_*.csv                  (resúmenes, sensibilidad)
#   salidas/figuras/etapas_*.png
# =============================================================================

if (!exists("RUTA_BASE")) {
  RUTA_BASE <- Sys.getenv("ASA_RUTA", unset = "C:/Users/Asus/Documents/2026/Vértice/ASA")
}
source(file.path(RUTA_BASE, "codigo", "00_configuracion.R"), encoding = "UTF-8")

paises <- readRDS(file.path(DIR_DATOS, "paises_anual.rds"))
setkey(paises, LocID, Time)


# 1. Parámetros base

PARAM_BASE <- list(
  k_suavizado    = 5,     # años de la media móvil centrada (impar)
  inicio_descenso = 0.10, # solo informativo: año en que la TFR cae 10 % desde su máximo
  tfr_alta       = 4.5,   # TFR mínima para hablar de natalidad "alta"
  tfr_baja       = 2.5,   # TFR máxima para hablar de natalidad "baja"
  cdr_alta       = 20,    # CDR (por mil) mínima para hablar de mortalidad "alta"
  crec_bajo      = 10,    # crecimiento natural (por mil) bajo la cual es "bajo" (etapa 1)
  caida_lex      = 5,     # años de caída de LEx frente al máximo de los 5 años previos => catástrofe
  choque_lex     = 2,     # años de caída de LEx => choque (Heuveline, 2022)
  interpolar_choques = TRUE, # TRUE: también se interpolan los años de choque antes de suavizar
  duracion_min   = 3,     # años mínimos que debe durar una etapa
  nrr_reemplazo  = 1,     # TNR que separa 4a (>=) de 4b (<)
  tol_migracion  = 1,     # |migración neta| (por mil) por debajo de la cual se considera despreciable
  criterio_etapa5 = "natural"  # "natural": nacimientos < defunciones (definición demográfica usual)
                               # "total":   la población total decrece (incluye migración)
)

# 2. Funciones auxiliares y de clasificación
#    clasificar_etapas() recibe el panel y una lista de parámetros; devuelve una
#    data.table con las columnas intermedias y la etapa. Está escrita paso a paso
#    a propósito.

# Máximo de los w valores anteriores (sin incluir el actual)
max_previo <- function(x, w) {
  sapply(seq_along(x), function(i) if (i == 1) NA_real_ else max(x[max(1, i - w):(i - 1)]))
}
# Interpolación lineal sobre los años marcados (rule = 2: extremos constantes)
interpolar_sin_crisis <- function(x, t, en_crisis) {
  if (!any(en_crisis) || sum(!en_crisis) < 2) return(x)
  approx(t[!en_crisis], x[!en_crisis], xout = t, rule = 2)$y
}
# Media móvil centrada con ventana parcial en los extremos (1950 y 2100)
media_movil <- function(x, h) {
  n <- length(x)
  sapply(seq_len(n), function(i) mean(x[max(1, i - h):min(n, i + h)]))
}
# Duración mínima: las rachas de menos de dmin años toman el valor de la racha
# anterior (o de la siguiente si es la primera)
filtrar_rachas <- function(x, dmin) {
  r <- rle(x)
  if (dmin > 1 && length(r$lengths) > 1) {
    for (j in seq_along(r$lengths)) {
      if (r$lengths[j] < dmin) r$values[j] <- if (j > 1) r$values[j - 1] else r$values[j + 1]
    }
  }
  inverse.rle(r)
}
# Inicio IRREVERSIBLE del descenso (Coale y Treadway, 1986): primer año en que
# la serie está bajo (1 - p) veces su máximo previo y nunca más vuelve a ese máximo
inicio_irreversible <- function(t, x, p) {
  xmax <- cummax(x)
  max_posterior <- c(rev(cummax(rev(x)))[-1], -Inf)   # máximo de los años siguientes
  ok <- x <= (1 - p) * xmax & max_posterior < xmax
  if (any(ok)) as.integer(t[which(ok)[1]]) else NA_integer_
}

clasificar_etapas <- function(datos, par) {

  d <- datos[, .(LocID, Time, TFR, NRR, CBR, CDR, NatChangeRT, CNMR, LEx, PopGrowthRate)]
  setkey(d, LocID, Time)
  h <- (par$k_suavizado - 1) / 2

  # (d) Crisis en dos niveles: se detectan PRIMERO y con datos SIN suavizar,
  #     porque duran pocos años. LEx_ref = máximo de e0 en los 5 años anteriores.
  d[, LEx_ref := max_previo(LEx, 5), by = LocID]
  d[, caida_LEx := fifelse(is.na(LEx_ref), 0, pmax(LEx_ref - LEx, 0))]
  d[, catastrofe := caida_LEx >= par$caida_lex | (NatChangeRT < 0 & TFR >= par$tfr_alta)]
  d[, nivel_crisis := fifelse(catastrofe, "Catástrofe",
                      fifelse(caida_LEx >= par$choque_lex, "Choque", "Ninguna"))]
  d[, covid := nivel_crisis != "Ninguna" & Time %between% c(2020, 2022)]
  d[, crisis := catastrofe]          # la que se pinta como "Crisis"

  # Para que la crisis no contamine la etapa de los años vecinos, en los años
  # marcados las series se reemplazan por una interpolación lineal entre los
  # años sin crisis ANTES de suavizar. (La etapa "de fondo" queda en etapa_estable.)
  d[, interpolar := if (isTRUE(par$interpolar_choques)) nivel_crisis != "Ninguna" else catastrofe]
  d[, `:=`(TFR_i = interpolar_sin_crisis(TFR, Time, interpolar),
           NRR_i = interpolar_sin_crisis(NRR, Time, interpolar),
           CBR_i = interpolar_sin_crisis(CBR, Time, interpolar),
           CDR_i = interpolar_sin_crisis(CDR, Time, interpolar)), by = LocID]

  # (c) Media móvil centrada. La migración (m_s) NO se interpola: los flujos de
  #     refugiados de una crisis son reales y se quieren ver.
  d[, `:=`(TFR_s = media_movil(TFR_i, h),
           NRR_s = media_movil(NRR_i, h),
           CBR_s = media_movil(CBR_i, h),
           CDR_s = media_movil(CDR_i, h),
           m_s   = media_movil(CNMR, h),
           r_s   = media_movil(PopGrowthRate, h)), by = LocID]
  d[, CN_s := CBR_s - CDR_s]                    # crecimiento natural suavizado (por mil)
  d[, c("TFR_i", "NRR_i", "CBR_i", "CDR_i", "interpolar") := NULL]

  # (b) Máximo de la TFR hasta el año t y año de inicio del descenso (informativo)
  d[, TFR_max := cummax(TFR_s), by = LocID]
  d[, descenso := TFR_s <= (1 - par$inicio_descenso) * TFR_max]
  d[, anio_inicio_descenso_simple := if (any(descenso)) as.integer(min(Time[descenso])) else NA_integer_,
    by = LocID]
  d[, anio_inicio_descenso := inicio_irreversible(Time, TFR_s, par$inicio_descenso), by = LocID]
  # Censurado: el máximo previo al inicio es el valor de 1950, es decir, la
  # meseta no se observa y el descenso pudo empezar antes de 1950.
  d[, inicio_censurado := {
    k <- match(anio_inicio_descenso[1], Time)
    if (is.na(k)) NA else TFR_max[k] == TFR_s[1]
  }, by = LocID]

  # Nivel de fecundidad y de mortalidad
  d[, fecundidad := fifelse(TFR_s <= par$tfr_baja, "baja",
                    fifelse(TFR_s >= par$tfr_alta, "alta", "en descenso"))]
  d[, mortalidad := fifelse(CDR_s >= par$cdr_alta, "alta", "baja")]

  # Reglas de etapa (el orden importa)
  d[, etapa_num := NA_integer_]
  d[fecundidad == "alta",        etapa_num := 2L]
  d[fecundidad == "alta" & mortalidad == "alta" & CN_s < par$crec_bajo, etapa_num := 1L]
  d[fecundidad == "en descenso", etapa_num := 3L]
  d[fecundidad == "baja",        etapa_num := 4L]
  # Etapa 5: decrecimiento sin fecundidad alta (población envejecida).
  #   criterio "natural": CBR < CDR. Ej.: EE. UU. entra hacia 2040 aunque su
  #   población siga creciendo por inmigración.
  #   criterio "total": la población total decrece.
  if (par$criterio_etapa5 == "natural") {
    d[CN_s < 0 & fecundidad != "alta", etapa_num := 5L]
  } else {
    d[r_s < 0 & fecundidad != "alta", etapa_num := 5L]
  }

  # (e) Duración mínima: los tramos (rachas) de menos de duracion_min años se
  #     reemplazan por la etapa del tramo anterior (o del siguiente si es el primero).
  d[, etapa_estable := filtrar_rachas(etapa_num, par$duracion_min), by = LocID]

  # Etapa final: la crisis (catástrofe) se superpone a la etapa estable
  d[, etapa := fifelse(crisis, "Crisis", paste("Etapa", etapa_estable))]

  # (f) Detalle de las etapas 4 y 5. La duración mínima se aplica dentro de
  #     cada tramo de etapa, para que el detalle nunca contradiga a la etapa.
  d[, sub := fcase(
    etapa_estable == 4L & NRR_s <  par$nrr_reemplazo,                       "4b",
    etapa_estable == 4L,                                                     "4a",
    etapa_estable == 5L & m_s >= par$tol_migracion & CN_s + m_s >= 0,        "5a",
    etapa_estable == 5L & m_s >= par$tol_migracion,                          "5b",
    etapa_estable == 5L,                                                     "5c",
    rep(TRUE, .N),                                                           as.character(etapa_estable))]
  d[, tramo_etapa := rleid(etapa_estable), by = LocID]
  d[, sub := filtrar_rachas(sub, par$duracion_min), by = .(LocID, tramo_etapa)]
  d[, etapa_detalle := fifelse(crisis, "Crisis", paste("Etapa", sub))]
  d[, c("sub", "tramo_etapa") := NULL]

  # Versión monótona (la transición "no retrocede"): máximo acumulado de la etapa
  d[, etapa_monotona := paste("Etapa", cummax(etapa_estable)), by = LocID]
  d[]
}


# 3. Clasificación base

etapas <- clasificar_etapas(paises, PARAM_BASE)
etapas <- merge(paises[, .(LocID, Time, ISO3_code, Location, region, region_ods, subregion,
                           TPopulation1July, pais_pequeno, periodo,
                           grupo_ingreso, grupo_ingreso_anual, grupo_ingreso_inicial)],
                etapas, by = c("LocID", "Time"))
setkey(etapas, LocID, Time)

cat("Distribución de país-años por etapa (base):\n")
print(etapas[, .N, by = etapa][order(etapa)])

cat("\nCuántas veces una etapa se 'devuelve' (retrocesos fuera de crisis):\n")
etapas[, retroceso := etapa_estable < shift(etapa_estable), by = LocID]
print(etapas[retroceso == TRUE, .(Location, Time, de = shift(etapa_estable, type = "lag"))][, .N])
print(etapas[retroceso == TRUE & !pais_pequeno, .(Location, Time, etapa)][1:20])

cat("\nAños de catástrofe detectados (países >= 1 millón):\n")
print(etapas[crisis == TRUE & !pais_pequeno,
             .(anios = paste(range(Time), collapse = "-"), n = .N), by = Location][order(-n)][1:25])
cat("\nPaís-años por nivel de crisis (todos los países):\n")
print(etapas[, .N, by = nivel_crisis])
cat("\nCOVID-19 (2020-2022): países >= 1 millón con choque o catástrofe y su mayor caída de e0:\n")
covid_tab <- etapas[covid == TRUE & !pais_pequeno,
                    .(anios = paste(sort(unique(Time)), collapse = ","), caida_max = round(max(caida_LEx), 2),
                      nivel = fifelse(any(nivel_crisis == "Catástrofe"), "Catástrofe", "Choque")),
                    by = .(Location, region_ods)][order(-caida_max)]
print(covid_tab[1:min(.N, 30)])
cat("Total:", nrow(covid_tab), "países\n")
fwrite(covid_tab, file.path(DIR_TABLAS, "etapas_crisis_covid.csv"))

# Alternativa: etapa 5 por decrecimiento TOTAL (con migración). Se guarda aparte.
par_total <- PARAM_BASE; par_total$criterio_etapa5 <- "total"
alt <- clasificar_etapas(paises, par_total)[, .(LocID, Time, etapa_criterio_total = etapa)]
etapas <- merge(etapas, alt, by = c("LocID", "Time"))
setkey(etapas, LocID, Time)
cat("\nPaíses en etapa 5 según criterio (años clave):\n")
print(etapas[Time %in% c(2023, 2050, 2100),
             .(natural = sum(etapa == "Etapa 5"), total = sum(etapa_criterio_total == "Etapa 5")), by = Time])

cat("\nDistribución de país-años por etapa con detalle (base):\n")
print(etapas[, .N, by = etapa_detalle][order(etapa_detalle)])


# 3b. Tipología de Webb (1963) adaptada a escala nacional, año por año.
#     Se usan promedios móviles centrados de 5 años de las tasas SIN interpolar
#     (la tipología describe lo que pasó, crisis incluidas). La versión por
#     periodos (5 y 10 años) y su sensibilidad a la tolerancia están en P4.
#     Dinámica (Długosz): se compara el grupo de Webb del año t con el de t - 10.

TOL_WEBB <- PARAM_BASE$tol_migracion
webb <- paises[, .(Time, cn_5 = media_movil(NatChangeRT, 2), mn_5 = media_movil(CNMR, 2)), by = LocID]
webb[, webb_tipo := tipo_webb(cn_5, mn_5, tol = TOL_WEBB)]
webb[, webb_grupo := grupo_webb(webb_tipo)]
webb[, webb_dinamico := webb_dinamico(shift(webb_grupo, 10), webb_grupo), by = LocID]
etapas <- merge(etapas, webb, by = c("LocID", "Time"))
setkey(etapas, LocID, Time)

cat("\nTipo de Webb (adaptado) por periodo, % de país-años (países >= 1 millón):\n")
print(dcast(etapas[pais_pequeno == FALSE, .N, by = .(periodo, webb_tipo)][, p := round(100 * N / sum(N), 1), by = periodo],
            webb_tipo ~ periodo, value.var = "p", fill = 0))
cat("\nCruce etapa con detalle x tipo de Webb (país-años 1950-2100, países >= 1 millón):\n")
print(dcast(etapas[pais_pequeno == FALSE, .N, by = .(etapa_detalle, webb_tipo)],
            etapa_detalle ~ webb_tipo, value.var = "N", fill = 0))


# 3c. Año de la transición migratoria: paso SOSTENIDO de emisor neto a receptor
#     neto (de Haas, 2021, p. 13). Pasos:
#       1. régimen anual con la migración neta suavizada (5 años) y la misma
#          banda de tolerancia: emisor (< -tol), equilibrio, receptor (> tol);
#       2. duración mínima de DUR_MIGRA años para cada régimen;
#       3. el año de la transición es el inicio del último tramo receptor,
#          siempre que antes haya habido un tramo emisor y que el país siga
#          siendo receptor al final del horizonte (los tramos de equilibrio no
#          interrumpen). Se calcula hasta 2023 (observado) y hasta 2100 (con
#          la migración proyectada, que en WPP es en buena parte un supuesto
#          experto: Metodología WPP 2024, pp. 47-51).
#     También se registra la transición inversa (de receptor a emisor).

DUR_MIGRA <- 10
etapas[, regimen_bruto := fcase(mn_5 < -TOL_WEBB, -1L, mn_5 > TOL_WEBB, 1L, default = 0L)]
etapas[, regimen_migratorio := filtrar_rachas(regimen_bruto, DUR_MIGRA), by = LocID]

# La duración mínima se aplica DENTRO del horizonte: con horizonte 2023, un
# tramo receptor que empezó en 2018 dura 6 años y no cuenta como sostenido.
anio_cambio_regimen <- function(t, reg_bruto, hasta, de, a) {
  sel <- t <= hasta
  r <- rle(filtrar_rachas(reg_bruto[sel], DUR_MIGRA))
  inicio <- t[sel][cumsum(c(1, head(r$lengths, -1)))]
  v <- r$values[r$values != 0]; ini <- inicio[r$values != 0]
  if (length(v) < 2 || tail(v, 1) != a) return(NA_integer_)
  k <- length(v)
  while (k > 1 && v[k - 1] == a) k <- k - 1
  if (k == 1 || v[k - 1] != de) return(NA_integer_)
  as.integer(ini[k])
}
trans_mig <- etapas[, .(
  anio_transicion_migratoria_2023 = anio_cambio_regimen(Time, regimen_bruto, 2023, -1L, 1L),
  anio_transicion_migratoria_2100 = anio_cambio_regimen(Time, regimen_bruto, 2100, -1L, 1L),
  anio_transicion_inversa_2100    = anio_cambio_regimen(Time, regimen_bruto, 2100, 1L, -1L)
), by = .(LocID, Location, region_ods, pais_pequeno)]
etapas <- merge(etapas, trans_mig[, !c("Location", "region_ods", "pais_pequeno")], by = "LocID")
setkey(etapas, LocID, Time)
etapas[, regimen_bruto := NULL]
etapas[, regimen_migratorio := factor(regimen_migratorio, levels = c(-1L, 0L, 1L),
                                      labels = c("Emisor", "Equilibrio", "Receptor"))]
fwrite(trans_mig[order(anio_transicion_migratoria_2100)], file.path(DIR_TABLAS, "etapas_transicion_migratoria.csv"))
cat("\nTransición migratoria (emisor -> receptor), países >= 1 millón:\n")
cat("  observada hasta 2023:", trans_mig[pais_pequeno == FALSE, sum(!is.na(anio_transicion_migratoria_2023))],
    " | incluida la proyección hasta 2100:", trans_mig[pais_pequeno == FALSE, sum(!is.na(anio_transicion_migratoria_2100))],
    " | inversa (receptor -> emisor) hasta 2100:", trans_mig[pais_pequeno == FALSE, sum(!is.na(anio_transicion_inversa_2100))], "\n")
print(trans_mig[!pais_pequeno & !is.na(anio_transicion_migratoria_2023),
                .(Location, region_ods, anio_transicion_migratoria_2023)][order(anio_transicion_migratoria_2023)])


# 3d. Clasificaciones ALTERNATIVAS (sensibilidad del inicio y de las fases).
#     Ambas trabajan con la TFR promedio por quinquenio (1950-54, ..., 2095-99;
#     2100 se asigna al último quinquenio), sin interpolar crisis, como sus autores.
#   (i) Fases de WPP 2024 (Metodología, p. 28):
#       - fase II empieza en el quinquenio del máximo de la TFR en 1950-2023;
#         si ese máximo es < 5,5, el país ya estaba en fase II antes de 1950;
#       - fase III empieza en el quinquenio del segundo de dos aumentos
#         sucesivos de la TFR después de haber caído bajo 2 (aquí: los dos
#         quinquenios de partida bajo 2). Se calcula también con la proyección,
#         marcando si la fase III solo aparece después de 2023.
#   (ii) Reher (2004, p. 21): inicio = comienzo del primer quinquenio después de
#        un máximo desde el cual la fecundidad cae al menos 8 % en dos
#        quinquenios y nunca vuelve a niveles cercanos al punto de partida
#        (aquí "cercano" = 95 % o más; Reher reconoce que es arbitrario).

TFR_MAX_WPP <- 5.5; TFR_PISO_WPP <- 2
REHER_CAIDA <- 0.08; REHER_RETORNO <- 0.95

quinq <- paises[, .(TFR = mean(TFR)), by = .(LocID, q = pmin(1950 + 5 * ((Time - 1950) %/% 5), 2095))]
setkey(quinq, LocID, q)

fases_wpp <- function(q, x) {
  obs <- q <= ANIO_ULTIMA_ESTIMACION
  im <- which.max(x[obs])
  ini2 <- if (max(x[obs]) < TFR_MAX_WPP) q[1] else q[im]
  censurado <- max(x[obs]) < TFR_MAX_WPP
  ini3 <- NA_real_
  for (p in seq_along(x)[-(1:2)]) {
    if (q[p - 2] > ini2 && x[p - 2] < TFR_PISO_WPP && x[p - 1] < TFR_PISO_WPP &&
        x[p - 1] > x[p - 2] && x[p] > x[p - 1]) { ini3 <- q[p]; break }
  }
  list(inicio_fase2_wpp = as.integer(ini2), fase2_antes_1950 = censurado, inicio_fase3_wpp = as.integer(ini3))
}
inicio_reher <- function(q, x) {
  n <- length(x)
  for (p in seq_len(n - 2)) {
    es_maximo <- p == 1 || x[p] >= x[p - 1]
    cae <- x[p + 2] <= (1 - REHER_CAIDA) * x[p]
    sin_retorno <- all(x[(p + 1):n] < REHER_RETORNO * x[p])
    if (es_maximo && cae && sin_retorno) return(list(anio_inicio_reher = as.integer(q[p + 1]),
                                                     reher_censurado = p == 1))
  }
  list(anio_inicio_reher = NA_integer_, reher_censurado = NA)
}
alternativas <- merge(quinq[, fases_wpp(q, TFR), by = LocID], quinq[, inicio_reher(q, TFR), by = LocID], by = "LocID")
etapas <- merge(etapas, alternativas, by = "LocID")
setkey(etapas, LocID, Time)
etapas[, fase_wpp := fcase(Time < inicio_fase2_wpp, "Fase I",
                           !is.na(inicio_fase3_wpp) & Time >= inicio_fase3_wpp, "Fase III",
                           default = "Fase II")]

# Comparación de los años de inicio del descenso (países >= 1 millón, sin censura)
comp_inicio <- unique(etapas[pais_pequeno == FALSE, .(LocID, Location, region_ods, anio_inicio_descenso,
                                              anio_inicio_descenso_simple, inicio_censurado,
                                              anio_inicio_reher, reher_censurado,
                                              inicio_fase2_wpp, fase2_antes_1950, inicio_fase3_wpp)])
fwrite(comp_inicio, file.path(DIR_TABLAS, "etapas_inicio_descenso_comparacion.csv"))
ok <- comp_inicio[inicio_censurado == FALSE & reher_censurado == FALSE & fase2_antes_1950 == FALSE]
cat("\nInicio del descenso de la fecundidad: comparación entre criterios (",
    nrow(ok), "países >= 1 millón con los tres sin censura)\n")
print(ok[, .(
  dif_mediana_irrev_vs_simple = median(anio_inicio_descenso - anio_inicio_descenso_simple, na.rm = TRUE),
  cambia_por_irreversibilidad = sum(anio_inicio_descenso != anio_inicio_descenso_simple, na.rm = TRUE),
  dif_mediana_reher_vs_10pct  = median(anio_inicio_reher - anio_inicio_descenso, na.rm = TRUE),
  cor_reher_vs_10pct          = round(cor(anio_inicio_reher, anio_inicio_descenso, use = "complete.obs"), 3),
  dif_mediana_10pct_vs_maxWPP = median(anio_inicio_descenso - inicio_fase2_wpp, na.rm = TRUE))])
cat("Países donde la irreversibilidad cambia el año de inicio:\n")
print(comp_inicio[anio_inicio_descenso != anio_inicio_descenso_simple | xor(is.na(anio_inicio_descenso), is.na(anio_inicio_descenso_simple)),
                  .(Location, simple = anio_inicio_descenso_simple, irreversible = anio_inicio_descenso)])
cat("\nFases WPP x etapa (país-años 1950-2023, países >= 1 millón):\n")
print(dcast(etapas[!pais_pequeno & periodo == "Estimación", .N, by = .(etapa, fase_wpp)],
            etapa ~ fase_wpp, value.var = "N", fill = 0))
cat("Países en fase III de WPP en 2023 (todos):", etapas[Time == 2023, sum(fase_wpp == "Fase III")],
    "| solo con quinquenios completos hasta 2015-2019:",
    etapas[Time == 2023, sum(!is.na(inicio_fase3_wpp) & inicio_fase3_wpp <= 2015)],
    "\n(WPP 2024, p. 30, reporta 39. La diferencia NO se reconcilia aquí: la regla de la",
    "p. 28 se aplica sobre la TFR anual promediada por quinquenios y WPP puede usar",
    "detalles adicionales que la metodología no describe. Usar esta fase solo como contraste.)\n")


# 4. Sensibilidad: se repite la clasificación con una rejilla de parámetros.
#    Para cada país-año se calcula la "certeza": la proporción de combinaciones
#    que coinciden con la etapa modal. Sirve para el video (transparencia) y
#    para el informe (qué tan robusta es la etapa asignada).

rejilla <- CJ(k_suavizado = c(3, 5, 7),
              tfr_alta = c(4.0, 4.5, 5.0),
              tfr_baja = c(2.1, 2.5, 2.8),
              cdr_alta = c(15, 20, 25))
cat("\nCombinaciones de sensibilidad:", nrow(rejilla), "\n")

resultados <- vector("list", nrow(rejilla))
for (i in seq_len(nrow(rejilla))) {
  par_i <- PARAM_BASE
  for (nm in names(rejilla)) par_i[[nm]] <- rejilla[[nm]][i]
  r <- clasificar_etapas(paises, par_i)
  resultados[[i]] <- r[, .(LocID, Time, etapa, comb = i)]
}
sens <- rbindlist(resultados)

# Etapa modal y certeza por país-año
modal <- sens[, .N, by = .(LocID, Time, etapa)]
modal <- modal[order(LocID, Time, -N)][, .SD[1], by = .(LocID, Time)]
modal[, certeza := N / nrow(rejilla)]
etapas <- merge(etapas, modal[, .(LocID, Time, etapa_modal = etapa, certeza)], by = c("LocID", "Time"))
setkey(etapas, LocID, Time)

# Acuerdo de cada combinación con la clasificación base
base_ref <- etapas[, .(LocID, Time, etapa_base = etapa)]
acuerdo <- merge(sens, base_ref, by = c("LocID", "Time"))[, .(acuerdo = mean(etapa == etapa_base)), by = comb]
acuerdo <- cbind(rejilla, acuerdo[order(comb), .(acuerdo)])
fwrite(acuerdo[order(acuerdo)], file.path(DIR_TABLAS, "etapas_sensibilidad_parametros.csv"))

cat("Acuerdo con la clasificación base, resumen sobre las", nrow(rejilla), "combinaciones:\n")
print(summary(acuerdo$acuerdo))
cat("Proporción de país-años con certeza >= 0.8:", round(mean(etapas$certeza >= 0.8), 3), "\n")
cat("Qué parámetro mueve más la clasificación (acuerdo medio por valor):\n")
for (nm in names(rejilla)) {
  print(acuerdo[, .(acuerdo_medio = round(mean(acuerdo), 3)), by = nm])
}


# 5. Contraste con un método sin reglas: mezclas gaussianas (mclust) sobre
#    TFR, CBR, CDR y crecimiento natural suavizados, con 5 grupos, en los años
#    de estimación. Si las etapas por reglas capturan algo real, el índice de
#    Rand ajustado (ARI) entre ambas clasificaciones debería ser alto.

library(mclust)
set.seed(SEMILLA)
datos_gmm <- etapas[periodo == "Estimación" & crisis == FALSE & !pais_pequeno]
X <- scale(datos_gmm[, .(TFR_s, CBR_s, CDR_s, CN_s)])
gmm <- Mclust(X, G = 5, verbose = FALSE)
datos_gmm[, grupo_gmm := gmm$classification]
ari <- adjustedRandIndex(datos_gmm$etapa, datos_gmm$grupo_gmm)
cat("\nARI etapas por reglas vs. mezcla gaussiana (5 grupos):", round(ari, 3), "\n")
tabla_cruce <- dcast(datos_gmm[, .N, by = .(etapa, grupo_gmm)], etapa ~ grupo_gmm, value.var = "N", fill = 0)
print(tabla_cruce)
fwrite(tabla_cruce, file.path(DIR_TABLAS, "etapas_vs_mezcla_gaussiana.csv"))

# Contraste con el mapa de PRB 2013 (Haub y Kaneda, 2013 World Population Data
# Sheet). OJO: es una lectura visual del mapa para países grandes y claros; PRB
# usó datos de 2013, no WPP 2024, así que no se espera coincidencia total.
prb_2013 <- data.table(
  Location = c("Niger", "Mali", "Chad", "Democratic Republic of the Congo", "Somalia",
               "Afghanistan", "Uganda", "Angola", "Mozambique", "Zambia",
               "Pakistan", "Kenya", "Egypt", "Saudi Arabia", "Bolivia (Plurinational State of)",
               "Philippines", "Kazakhstan", "Algeria", "Namibia", "Papua New Guinea",
               "United States of America", "Canada", "Brazil", "China", "India", "Mexico",
               "Australia", "Argentina", "Colombia", "Türkiye", "France", "Iran (Islamic Republic of)",
               "Russian Federation", "Japan", "Germany", "Ukraine", "Italy", "Hungary", "Bulgaria"),
  etapa_prb = c(rep("Etapa 2", 10), rep("Etapa 3", 10), rep("Etapa 4", 12), rep("Etapa 5", 7))
)
comp_prb <- merge(prb_2013, etapas[Time == 2013, .(Location, etapa, etapa_estable)], by = "Location")
cat("\nCoincidencia con el mapa PRB 2013 (", nrow(comp_prb), "países):",
    round(mean(comp_prb$etapa == comp_prb$etapa_prb), 3), "\n")
print(comp_prb[etapa != etapa_prb])
fwrite(comp_prb, file.path(DIR_TABLAS, "etapas_contraste_prb_2013.csv"))


# 6. Tablas de resumen

# (a) Países y población mundial en cada etapa, por año
resumen_anual <- etapas[, .(paises = .N, poblacion_miles = sum(TPopulation1July)), by = .(Time, etapa)]
# Completar con ceros las combinaciones año-etapa sin países (si no, el área apilada se deforma)
resumen_anual <- merge(CJ(Time = unique(etapas$Time), etapa = names(COLORES_ETAPA)),
                       resumen_anual, by = c("Time", "etapa"), all.x = TRUE)
resumen_anual[is.na(paises), `:=`(paises = 0L, poblacion_miles = 0)]
resumen_anual[, prop_poblacion := poblacion_miles / sum(poblacion_miles), by = Time]
fwrite(resumen_anual, file.path(DIR_TABLAS, "etapas_resumen_anual.csv"))

# (b) Año de entrada a cada etapa (primera vez, sin contar crisis)
entradas <- etapas[crisis == FALSE, .(anio_entrada = min(Time)), by = .(LocID, Location, region_ods, etapa_estable)]
entradas <- dcast(entradas, LocID + Location + region_ods ~ paste0("entra_etapa_", etapa_estable),
                  value.var = "anio_entrada")
fwrite(entradas, file.path(DIR_TABLAS, "etapas_anio_entrada.csv"))

# (c) Foto de años clave
foto <- dcast(etapas[Time %in% c(1950, 1975, 2000, 2013, 2023, 2050, 2075, 2100), .N, by = .(Time, etapa)],
              etapa ~ Time, value.var = "N", fill = 0)
cat("\nNúmero de países por etapa en años clave:\n"); print(foto)
fwrite(foto, file.path(DIR_TABLAS, "etapas_paises_anios_clave.csv"))

# (d) Lo mismo con el detalle de las etapas 4 y 5
resumen_detalle <- etapas[, .(paises = .N, poblacion_miles = sum(TPopulation1July)), by = .(Time, etapa_detalle)]
resumen_detalle <- merge(CJ(Time = unique(etapas$Time), etapa_detalle = names(COLORES_ETAPA_DETALLE)),
                         resumen_detalle, by = c("Time", "etapa_detalle"), all.x = TRUE)
resumen_detalle[is.na(paises), `:=`(paises = 0L, poblacion_miles = 0)]
resumen_detalle[, prop_poblacion := poblacion_miles / sum(poblacion_miles), by = Time]
fwrite(resumen_detalle, file.path(DIR_TABLAS, "etapas_detalle_resumen_anual.csv"))
foto_det <- dcast(etapas[Time %in% c(1950, 1975, 2000, 2023, 2050, 2075, 2100), .N, by = .(Time, etapa_detalle)],
                  etapa_detalle ~ Time, value.var = "N", fill = 0)
cat("\nNúmero de países por etapa con detalle en años clave:\n"); print(foto_det)
fwrite(foto_det, file.path(DIR_TABLAS, "etapas_detalle_paises_anios_clave.csv"))
cat("\nPaíses en etapa 5a (crecen solo por la inmigración) en 2023, 2050 y 2100 (>= 1 millón):\n")
for (a in c(2023, 2050, 2100)) cat(a, ":", paste(etapas[Time == a & etapa_detalle == "Etapa 5a" & !pais_pequeno, Location],
                                              collapse = ", "), "\n")


# 7. Figuras de control

# (a) Proporción de la población mundial en cada etapa
niveles_etapa <- names(COLORES_ETAPA)
resumen_anual[, etapa := factor(etapa, levels = niveles_etapa)]
g1 <- ggplot(resumen_anual, aes(Time, prop_poblacion, fill = etapa)) +
  geom_area(colour = "white", linewidth = 0.1) +
  geom_vline(xintercept = ANIO_ULTIMA_ESTIMACION + 0.5, linetype = 2) +
  annotate("text", x = 2026, y = 1.03, label = "proyección →", hjust = 0, size = 3.5) +
  scale_fill_manual(values = COLORES_ETAPA, drop = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Población mundial", fill = NULL,
       title = "Población mundial según la etapa de transición de su país")
ggsave(file.path(DIR_FIGURAS, "etapas_poblacion_mundial.png"), g1, width = 10, height = 5.5, dpi = 150)

# (b) Trayectorias de casos ilustrativos con la etapa de fondo
casos <- c("Colombia", "Japan", "Niger", "Cambodia", "Republic of Korea", "Nigeria")
dc <- etapas[Location %in% casos]
dc[, etapa := factor(etapa, levels = niveles_etapa)]
dl <- melt(dc, id.vars = c("Location", "Time"), measure.vars = c("CBR", "CDR"),
           variable.name = "tasa", value.name = "por_mil")
g2 <- ggplot() +
  geom_tile(data = dc, aes(x = Time, y = -4, fill = etapa), height = 6) +
  geom_line(data = dl, aes(Time, por_mil, linetype = tasa)) +
  geom_vline(xintercept = ANIO_ULTIMA_ESTIMACION + 0.5, colour = "grey50", linewidth = 0.3) +
  facet_wrap(~ Location, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = COLORES_ETAPA, drop = FALSE) +
  scale_linetype_manual(values = c(CBR = 1, CDR = 2),
                        labels = c("Natalidad (CBR)", "Mortalidad (CDR)")) +
  labs(x = NULL, y = "Por mil habitantes", fill = "Etapa", linetype = NULL,
       title = "Natalidad, mortalidad y etapa asignada")
ggsave(file.path(DIR_FIGURAS, "etapas_casos.png"), g2, width = 11, height = 9, dpi = 150)

# (c) Población mundial por etapa con detalle
resumen_detalle[, etapa_detalle := factor(etapa_detalle, levels = names(COLORES_ETAPA_DETALLE))]
g3 <- ggplot(resumen_detalle, aes(Time, prop_poblacion, fill = etapa_detalle)) +
  geom_area(colour = "white", linewidth = 0.1) +
  geom_vline(xintercept = ANIO_ULTIMA_ESTIMACION + 0.5, linetype = 2) +
  scale_fill_manual(values = COLORES_ETAPA_DETALLE, drop = FALSE) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Población mundial", fill = NULL,
       title = "Población mundial según la etapa de su país (con detalle de las etapas 4 y 5)",
       subtitle = paste("4a: TNR >= 1 · 4b: TNR < 1 · 5a: la inmigración compensa el decrecimiento natural ·",
                        "5b: no lo compensa · 5c: sin inmigración neta"))
ggsave(file.path(DIR_FIGURAS, "etapas_detalle_poblacion_mundial.png"), g3, width = 11, height = 5.5, dpi = 150)

# (d) Tipología de Webb adaptada: % de países (>= 1 millón) por tipo y año
tw <- etapas[!pais_pequeno & !is.na(webb_tipo), .N, by = .(Time, webb_tipo)]
tw <- merge(CJ(Time = unique(tw$Time), webb_tipo = names(TIPOS_WEBB)), tw, by = c("Time", "webb_tipo"), all.x = TRUE)
tw[is.na(N), N := 0L]
tw[, prop := N / sum(N), by = Time]
tw[, webb_tipo := factor(webb_tipo, levels = names(TIPOS_WEBB), labels = TIPOS_WEBB)]
g4 <- ggplot(tw, aes(Time, prop, fill = webb_tipo)) +
  geom_area(colour = "white", linewidth = 0.1) +
  geom_vline(xintercept = ANIO_ULTIMA_ESTIMACION + 0.5, linetype = 2) +
  scale_fill_manual(values = setNames(COLORES_WEBB, TIPOS_WEBB)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Países (>= 1 millón)", fill = NULL,
       title = "Tipología de Webb (1963) adaptada a escala nacional",
       subtitle = paste0("Promedios móviles de 5 años; migración despreciable si |MN| < ", TOL_WEBB, " por mil")) +
  theme(legend.position = "right")
ggsave(file.path(DIR_FIGURAS, "etapas_webb_tipos.png"), g4, width = 12, height = 5.5, dpi = 150)


# 8. Guardar

etapas[, retroceso := NULL]
saveRDS(etapas, file.path(DIR_DATOS, "etapas_transicion.rds"))
fwrite(etapas, file.path(DIR_DATOS, "etapas_transicion.csv"))
cat("\nGuardado etapas_transicion.rds y .csv\n")
