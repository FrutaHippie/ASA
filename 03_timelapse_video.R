# =============================================================================
# 03_timelapse_video.R
# Animaciones para el video: dónde está cada país en la transición demográfica,
# año por año, de 1950 a 2100.
#
#   A) Mapa del mundo coloreado por etapa + barra con el % de la población
#      mundial en cada etapa.                      -> salidas/video/mapa_etapas.mp4
#   B) Plano natalidad (CBR) vs. mortalidad (CDR): cada país es una burbuja
#      (tamaño = población, color = etapa) que se desplaza en el tiempo.
#                                                  -> salidas/video/plano_cbr_cdr.mp4
#   C) Plano esperanza de vida (e0) vs. fecundidad (TFR), como la figura 3 de
#      Lee (2003, J. Econ. Perspectives 17(4), p. 176). A diferencia de B, no
#      depende de la estructura por edad.        -> salidas/video/plano_tfr_e0.mp4
#
# Método: se dibuja un PNG por cuadro con ggplot2 y luego se unen en un MP4 con
# el paquete av. Es más lento que gganimate pero más fácil de controlar.
# Requiere haber corrido 01_limpieza.R y 02_etapas_transicion.R.
# =============================================================================

if (!exists("RUTA_BASE")) {
  RUTA_BASE <- Sys.getenv("ASA_RUTA", unset = "C:/Users/Asus/Documents/2026/Vértice/ASA")
}
source(file.path(RUTA_BASE, "codigo", "00_configuracion.R"), encoding = "UTF-8")
for (p in c("patchwork")) if (!p %in% rownames(installed.packages())) install.packages(p)
suppressPackageStartupMessages({
  library(sf)
  library(patchwork)
})

etapas <- readRDS(file.path(DIR_DATOS, "etapas_transicion.rds"))
# Qué clasificación mostrar: "etapa" (etapa 5 = crecimiento natural negativo)
# o "etapa_criterio_total" (etapa 5 = la población total decrece). Ver 02_etapas_transicion.R
# o "etapa_detalle" (etapas 4a/4b y 5a/5b/5c, con la migración).
if (!exists("COLUMNA_ETAPA")) COLUMNA_ETAPA <- "etapa"
COLORES_VIDEO <- if (COLUMNA_ETAPA == "etapa_detalle") COLORES_ETAPA_DETALLE else COLORES_ETAPA
niveles_etapa <- names(COLORES_VIDEO)
etapas[, etapa := factor(get(COLUMNA_ETAPA), levels = niveles_etapa)]
# Qué animaciones hacer (para pruebas se puede dejar solo una)
if (!exists("HACER")) HACER <- c("mapa", "cbr_cdr", "tfr_e0")

# -----------------------------------------------------------------------------
# Parámetros del video
# -----------------------------------------------------------------------------
if (!exists("ANIOS")) ANIOS <- 1950:2100   # para una prueba rápida: ANIOS <- c(1950, 1980, 2023, 2100)
CUADROS_X_SEG  <- 8                # velocidad del video
PAUSA_FINAL    <- 3                # segundos congelados al final (y al inicio)
ANCHO_PX       <- 1920
ALTO_PX        <- 1080
DPI            <- 150
USAR_CERTEZA   <- FALSE            # TRUE = los países con etapa poco robusta se ven más tenues
SUBCUADROS     <- 3                # B) cuadros interpolados entre años (movimiento suave)
PAISES_ETIQUETA <- c("Colombia", "Nigeria", "India", "China", "Japan", "Niger")

dir_cuadros <- file.path(DIR_VIDEO, "cuadros_tmp")

if ("mapa" %in% HACER) {
# =============================================================================
# A) MAPA POR ETAPAS
# =============================================================================
mundo <- rnaturalearthdata::countries50
mundo <- mundo[mundo$adm0_a3 != "ATA", ]                      # sin Antártida
mundo$ISO3_code <- ifelse(mundo$iso_a3 == "-99", mundo$adm0_a3, mundo$iso_a3)
# Ajustes de códigos entre Natural Earth y la ONU
mundo$ISO3_code[mundo$adm0_a3 == "KOS"] <- "XKX"             # Kosovo
mundo$ISO3_code[mundo$adm0_a3 == "SOL"] <- "SOM"             # Somalilandia -> Somalia
mundo$ISO3_code[mundo$adm0_a3 == "CYN"] <- "CYP"             # Chipre del Norte -> Chipre
mundo <- st_transform(mundo[, "ISO3_code"], "+proj=robin")   # proyección Robinson

sin_mapa <- setdiff(unique(etapas[pais_pequeno == FALSE, ISO3_code]), mundo$ISO3_code)
cat("Países >= 1 millón sin polígono en el mapa:", paste(sin_mapa, collapse = ", "), "\n")

# % de la población mundial por etapa y año (para la barra inferior)
barra <- etapas[, .(pob = sum(TPopulation1July)), by = .(Time, etapa)]
barra[, prop := pob / sum(pob), by = Time]

unlink(dir_cuadros, recursive = TRUE); dir.create(dir_cuadros, recursive = TRUE)
archivos <- character(0)

for (anio in ANIOS) {
  datos_anio <- etapas[Time == anio, .(ISO3_code, etapa, certeza)]
  mapa_anio <- merge(mundo, datos_anio, by = "ISO3_code", all.x = TRUE)

  g_mapa <- ggplot(mapa_anio) +
    geom_sf(aes(fill = etapa, alpha = if (USAR_CERTEZA) certeza else 1),
            colour = "white", linewidth = 0.08) +
    scale_fill_manual(values = COLORES_VIDEO, drop = FALSE, na.value = "grey85",
                      na.translate = FALSE, name = NULL) +
    scale_alpha_identity() +
    coord_sf(datum = NA) +
    annotate("text", x = -1.55e7, y = -5.2e6, label = anio, size = 22,
             fontface = "bold", colour = "grey25", hjust = 0) +
    annotate("text", x = -1.55e7, y = -7.4e6,
             label = if (anio <= ANIO_ULTIMA_ESTIMACION) "estimación" else "proyección (variante media)",
             size = 5.5, colour = "grey40", hjust = 0) +
    labs(title = "¿En qué etapa de la transición demográfica está cada país?") +
    theme_void(base_size = 16) +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 8)),
          plot.background = element_rect(fill = "white", colour = NA))

  b <- barra[Time == anio]
  # Barra horizontal apilada, en el orden de las etapas (1 a la izquierda)
  b <- b[order(etapa)]
  b[, fin := cumsum(prop)]
  b[, inicio := fin - prop]
  g_barra <- ggplot(b) +
    geom_rect(aes(xmin = inicio, xmax = fin, ymin = 0, ymax = 1, fill = etapa),
              colour = "white", linewidth = 0.3) +
    geom_text(data = b[prop >= 0.04],
              aes(x = (inicio + fin) / 2, y = 0.5, label = scales::percent(prop, accuracy = 1)),
              colour = "white", fontface = "bold", size = 5) +
    scale_fill_manual(values = COLORES_VIDEO, drop = FALSE, guide = "none") +
    scale_x_continuous(expand = c(0, 0)) +
    labs(caption = "Porcentaje de la población mundial en cada etapa  ·  Fuente: ONU, World Population Prospects 2024") +
    theme_void(base_size = 14) +
    theme(plot.caption = element_text(hjust = 0.5, colour = "grey40"),
          plot.background = element_rect(fill = "white", colour = NA))

  g <- g_mapa / g_barra + plot_layout(heights = c(10, 1))

  f <- file.path(dir_cuadros, sprintf("mapa_%04d.png", anio))
  ggsave(f, g, width = ANCHO_PX / DPI, height = ALTO_PX / DPI, dpi = DPI)
  archivos <- c(archivos, f)
  if (anio %% 10 == 0) cat("mapa", anio, "\n")
}

# Pausas: repetir el primer y el último cuadro
pausa <- PAUSA_FINAL * CUADROS_X_SEG
secuencia <- c(rep(archivos[1], pausa), archivos, rep(archivos[length(archivos)], pausa))
av::av_encode_video(secuencia, output = file.path(DIR_VIDEO, "mapa_etapas.mp4"),
                    framerate = CUADROS_X_SEG, verbose = FALSE)
# Copias fijas de algunos años para el informe
for (a in intersect(c(1950, 1975, 2000, 2023, 2050, 2100), ANIOS)) {
  file.copy(file.path(dir_cuadros, sprintf("mapa_%04d.png", a)),
            file.path(DIR_FIGURAS, sprintf("mapa_etapas_%d.png", a)), overwrite = TRUE)
}
cat("Listo: mapa_etapas.mp4\n")

}

if ("cbr_cdr" %in% HACER) {
# =============================================================================
# B) PLANO NATALIDAD vs. MORTALIDAD
#    Las diagonales son líneas de igual crecimiento natural (CBR - CDR).
#    Los países recorren el plano de arriba-derecha (etapas 1-2) hacia
#    abajo-izquierda (etapa 4) y cruzan la diagonal 0 al entrar a la etapa 5.
# =============================================================================
plano <- etapas[pais_pequeno == FALSE,
                .(LocID, Location, Time, CBR, CDR, pob = TPopulation1July, etapa)]

# Interpolación lineal entre años para un movimiento suave (SUBCUADROS por año).
# La etapa (discreta) se toma del año inicial de cada tramo.
tiempos <- seq(min(ANIOS), max(ANIOS), by = 1 / SUBCUADROS)
if (length(ANIOS) < 10) tiempos <- ANIOS   # modo de prueba: solo los años pedidos
plano_interp <- plano[, {
  ii <- findInterval(tiempos, Time)
  .(t = tiempos,
    CBR = approx(Time, CBR, xout = tiempos)$y,
    CDR = approx(Time, CDR, xout = tiempos)$y,
    pob = approx(Time, pob, xout = tiempos)$y,
    etapa = etapa[ii])
}, by = .(LocID, Location)]

lim_x <- c(0, 40)    # CDR (se recortan las crisis extremas para no perder escala)
lim_y <- c(0, 60)    # CBR
diagonales <- data.table(cn = c(0, 10, 20, 30, 40))
# Posición de la etiqueta de cada diagonal: donde la línea sale del gráfico
diagonales[, x_et := pmin(lim_x[2] - 0.5, lim_y[2] - 3 - cn)]
# Ángulo del texto: pendiente 1 en datos convertida a pantalla (aprox. 16° a 1920x1080)
ANGULO_DIAG <- atan((ALTO_PX * 0.70 / diff(lim_y)) / (ANCHO_PX * 0.92 / diff(lim_x))) * 180 / pi
diagonales[, y_et := x_et + cn]

unlink(dir_cuadros, recursive = TRUE); dir.create(dir_cuadros, recursive = TRUE)
archivos <- character(0)
k <- 0
for (tt in tiempos) {
  k <- k + 1
  d <- plano_interp[abs(t - tt) < 1e-9]
  d[, CDR_v := pmin(CDR, lim_x[2])]
  anio <- floor(tt + 1e-9)
  # Estela de los últimos 15 años para los países destacados
  estela <- plano_interp[Location %in% PAISES_ETIQUETA & t <= tt + 1e-9 & t >= tt - 15]

  g <- ggplot() +
    geom_abline(data = diagonales, aes(intercept = cn, slope = 1),
                colour = "grey75", linetype = 2) +
    geom_text(data = diagonales, aes(x_et, y_et, label = paste0("crec. natural ", cn, "‰")),
              angle = ANGULO_DIAG, hjust = 1, vjust = -0.4, size = 3.4, colour = "grey55") +
    geom_path(data = estela, aes(CDR, CBR, group = Location), colour = "grey40",
              linewidth = 0.5, alpha = 0.6) +
    geom_point(data = d, aes(CDR_v, CBR, size = pob, fill = etapa),
               shape = 21, colour = "grey20", stroke = 0.2, alpha = 0.85) +
    geom_label(data = d[Location %in% PAISES_ETIQUETA],
               aes(CDR_v, CBR, label = Location), size = 3.6, fontface = "bold",
               nudge_x = 0.6, hjust = 0, fill = alpha("white", 0.75), label.size = 0,
               label.padding = unit(0.12, "lines")) +
    annotate("text", x = lim_x[1] + 1, y = lim_y[2] - 2, label = anio,
             size = 20, fontface = "bold", colour = "grey30", hjust = 0, vjust = 1) +
    annotate("text", x = lim_x[1] + 1, y = lim_y[2] - 11,
             label = if (anio <= ANIO_ULTIMA_ESTIMACION) "estimación" else "proyección",
             size = 5, colour = "grey45", hjust = 0, vjust = 1) +
    scale_fill_manual(values = COLORES_VIDEO, drop = FALSE, name = NULL) +
    scale_size_area(max_size = 28, guide = "none") +
    coord_cartesian(xlim = lim_x, ylim = lim_y, expand = FALSE) +
    labs(x = "Tasa bruta de mortalidad (por mil)", y = "Tasa bruta de natalidad (por mil)",
         title = "Cada país recorre la transición: de natalidad y mortalidad altas a bajas",
         caption = "Tamaño = población. Países con 1 millón de habitantes o más. Fuente: ONU, WPP 2024") +
    guides(fill = guide_legend(override.aes = list(size = 6))) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", colour = NA))

  f <- file.path(dir_cuadros, sprintf("plano_%05d.png", k))
  ggsave(f, g, width = ANCHO_PX / DPI, height = ALTO_PX / DPI, dpi = DPI)
  archivos <- c(archivos, f)
  if (abs(tt - round(tt)) < 1e-9 && round(tt) %% 10 == 0) cat("plano", round(tt), "\n")
}

pausa <- PAUSA_FINAL * CUADROS_X_SEG * SUBCUADROS
secuencia <- c(rep(archivos[1], pausa), archivos, rep(archivos[length(archivos)], pausa))
av::av_encode_video(secuencia, output = file.path(DIR_VIDEO, "plano_cbr_cdr.mp4"),
                    framerate = CUADROS_X_SEG * SUBCUADROS, verbose = FALSE)
file.copy(archivos[length(archivos)], file.path(DIR_FIGURAS, "plano_cbr_cdr_final.png"), overwrite = TRUE)
unlink(dir_cuadros, recursive = TRUE)
cat("Listo: plano_cbr_cdr.mp4\n")
}

# =============================================================================
# C) PLANO ESPERANZA DE VIDA vs. FECUNDIDAD (Lee, 2003, fig. 3)
#    Los países van de arriba-izquierda (e0 baja, TFR alta) a abajo-derecha.
#    Líneas de referencia: TFR 4,5 y 2,5 (umbrales de las etapas) y 2,1
#    (reemplazo aproximado). Las crisis aparecen como saltos hacia la izquierda.
# =============================================================================
if ("tfr_e0" %in% HACER) {
plano2 <- etapas[pais_pequeno == FALSE,
                 .(LocID, Location, Time, LEx, TFR, pob = TPopulation1July, etapa)]
tiempos <- seq(min(ANIOS), max(ANIOS), by = 1 / SUBCUADROS)
if (length(ANIOS) < 10) tiempos <- ANIOS
plano2_interp <- plano2[, {
  ii <- findInterval(tiempos, Time)
  .(t = tiempos,
    LEx = approx(Time, LEx, xout = tiempos)$y,
    TFR = approx(Time, TFR, xout = tiempos)$y,
    pob = approx(Time, pob, xout = tiempos)$y,
    etapa = etapa[ii])
}, by = .(LocID, Location)]

lim_e0  <- c(20, 95)
lim_tfr <- c(0.5, 8.5)
refs <- data.table(tfr = c(4.5, 2.5, 2.1),
                   texto = c("TFR 4,5: fecundidad alta", "TFR 2,5: fecundidad baja", "2,1: reemplazo aprox."))

unlink(dir_cuadros, recursive = TRUE); dir.create(dir_cuadros, recursive = TRUE)
archivos <- character(0)
k <- 0
for (tt in tiempos) {
  k <- k + 1
  d <- plano2_interp[abs(t - tt) < 1e-9]
  d[, LEx_v := pmax(LEx, lim_e0[1])]
  anio <- floor(tt + 1e-9)
  estela <- plano2_interp[Location %in% PAISES_ETIQUETA & t <= tt + 1e-9 & t >= tt - 15]

  g <- ggplot() +
    geom_hline(data = refs, aes(yintercept = tfr), colour = "grey75", linetype = 2) +
    geom_text(data = refs, aes(x = lim_e0[2] - 0.5, y = tfr, label = texto),
              hjust = 1, vjust = -0.4, size = 3.4, colour = "grey55") +
    geom_path(data = estela, aes(LEx, TFR, group = Location), colour = "grey40",
              linewidth = 0.5, alpha = 0.6) +
    geom_point(data = d, aes(LEx_v, TFR, size = pob, fill = etapa),
               shape = 21, colour = "grey20", stroke = 0.2, alpha = 0.85) +
    geom_label(data = d[Location %in% PAISES_ETIQUETA],
               aes(LEx_v, TFR, label = Location), size = 3.6, fontface = "bold",
               nudge_x = 0.8, hjust = 0, fill = alpha("white", 0.75), label.size = 0,
               label.padding = unit(0.12, "lines")) +
    annotate("text", x = lim_e0[1] + 1, y = lim_tfr[1] + 1.6, label = anio,
             size = 20, fontface = "bold", colour = "grey30", hjust = 0, vjust = 0) +
    annotate("text", x = lim_e0[1] + 1, y = lim_tfr[1] + 0.9,
             label = if (anio <= ANIO_ULTIMA_ESTIMACION) "estimación" else "proyección",
             size = 5, colour = "grey45", hjust = 0, vjust = 0) +
    scale_fill_manual(values = COLORES_VIDEO, drop = FALSE, name = NULL) +
    scale_size_area(max_size = 28, guide = "none") +
    coord_cartesian(xlim = lim_e0, ylim = lim_tfr, expand = FALSE) +
    labs(x = "Esperanza de vida al nacer (años)", y = "Hijos por mujer (TFR)",
         title = "Vivir más y tener menos hijos: la transición en el plano esperanza de vida - fecundidad",
         caption = "Tamaño = población. Países con 1 millón de habitantes o más. Idea: Lee (2003), fig. 3. Fuente: ONU, WPP 2024") +
    guides(fill = guide_legend(override.aes = list(size = 6), nrow = 1)) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "top", plot.title = element_text(face = "bold", size = 17),
          plot.background = element_rect(fill = "white", colour = NA))

  f <- file.path(dir_cuadros, sprintf("tfr_e0_%05d.png", k))
  ggsave(f, g, width = ANCHO_PX / DPI, height = ALTO_PX / DPI, dpi = DPI)
  archivos <- c(archivos, f)
  if (abs(tt - round(tt)) < 1e-9 && round(tt) %% 10 == 0) cat("tfr_e0", round(tt), "\n")
}

pausa <- PAUSA_FINAL * CUADROS_X_SEG * SUBCUADROS
secuencia <- c(rep(archivos[1], pausa), archivos, rep(archivos[length(archivos)], pausa))
av::av_encode_video(secuencia, output = file.path(DIR_VIDEO, "plano_tfr_e0.mp4"),
                    framerate = CUADROS_X_SEG * SUBCUADROS, verbose = FALSE)
for (a in intersect(c(1950, 2023, 2100), ANIOS)) {
  i_a <- which(abs(tiempos - a) < 1e-9)
  if (length(i_a)) file.copy(archivos[i_a], file.path(DIR_FIGURAS, sprintf("plano_tfr_e0_%d.png", a)), overwrite = TRUE)
}
unlink(dir_cuadros, recursive = TRUE)
cat("Listo: plano_tfr_e0.mp4\n")
}
