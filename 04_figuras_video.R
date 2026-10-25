# =============================================================================
# 04_figuras_video.R
# Figuras listas para el video (1920 x 1080, letra grande, etiquetas en
# español) del bloque "transición demográfica + preguntas 5, 6 y 7".
# Ver el documento "Propuesta de video" para el guion de cada escena.
#
# Requiere haber corrido 02, P5, P6 y P7. Salidas en salidas/figuras/video_*.png
#   video_escena2_casos.png    natalidad y mortalidad de 2 países con su etapa
#   video_escena3_indice.png   índice compuesto (P5) de 5 países
#   video_escena4_grupos.png   fecundidad y esperanza de vida por grupo de trayectoria (P6)
#   video_escena6_cierre.png   los dos ejes: avance de la transición y migración (2023)
# =============================================================================

if (!exists("RUTA_BASE")) {
  RUTA_BASE <- Sys.getenv("ASA_RUTA", unset = "C:/Users/Asus/Documents/2026/Vértice/ASA")
}
source(file.path(RUTA_BASE, "codigo", "00_configuracion.R"), encoding = "UTF-8")
if (!"ggrepel" %in% rownames(installed.packages())) install.packages("ggrepel")   # etiquetas sin traslape

ANCHO <- 1920 / 150; ALTO <- 1080 / 150          # pulgadas a 150 dpi = 1920 x 1080
tema_video <- theme_minimal(base_size = 20) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(face = "bold", size = 26),
        plot.subtitle = element_text(colour = "grey35", size = 17),
        plot.caption = element_text(colour = "grey45", size = 12),
        plot.title.position = "plot", legend.position = "top",
        panel.grid.minor = element_blank())
fuente <- "Fuente: ONU, World Population Prospects 2024"
guardar <- function(g, nombre) ggsave(file.path(DIR_FIGURAS, nombre), g, width = ANCHO, height = ALTO, dpi = 150)

etapas <- readRDS(file.path(DIR_DATOS, "etapas_transicion.rds"))
paises <- readRDS(file.path(DIR_DATOS, "paises_anual.rds"))
nombres_es <- c("Colombia" = "Colombia", "Cambodia" = "Camboya", "Japan" = "Japón", "Niger" = "Níger",
                "Republic of Korea" = "Corea del Sur", "Nigeria" = "Nigeria", "India" = "India",
                "Germany" = "Alemania", "Spain" = "España", "United Arab Emirates" = "Emiratos Árabes",
                "Qatar" = "Catar", "China" = "China", "United States of America" = "Estados Unidos",
                "Venezuela (Bolivarian Republic of)" = "Venezuela", "Italy" = "Italia",
                "Canada" = "Canadá", "Brazil" = "Brasil", "Mexico" = "México")


# Escena 2: cómo se asigna la etapa (Colombia y Camboya)
casos <- c("Colombia", "Cambodia")
dc <- etapas[Location %in% casos]
dc[, pais := factor(nombres_es[Location], levels = nombres_es[casos])]
dc[, etapa := factor(etapa, levels = names(COLORES_ETAPA))]
dl <- melt(dc, id.vars = c("pais", "Time"), measure.vars = c("CBR", "CDR"), variable.name = "tasa", value.name = "v")
g2 <- ggplot() +
  geom_tile(data = dc, aes(Time, -5, fill = etapa), height = 7) +
  geom_line(data = dl, aes(Time, v, linetype = tasa), linewidth = 1.2) +
  geom_vline(xintercept = ANIO_ULTIMA_ESTIMACION + 0.5, colour = "grey50") +
  annotate("text", x = 2026, y = Inf, label = "proyección", hjust = 0, vjust = 1.5, colour = "grey45", size = 5) +
  facet_wrap(~ pais, ncol = 2) +
  scale_fill_manual(values = COLORES_ETAPA, drop = TRUE, name = NULL) +
  scale_linetype_manual(values = c(CBR = 1, CDR = 2), labels = c("Natalidad", "Mortalidad"), name = NULL) +
  coord_cartesian(ylim = c(-9, 60)) +
  labs(x = NULL, y = "Por cada mil habitantes",
       title = "Cada país, cada año, en una etapa",
       subtitle = "La barra de abajo es la etapa asignada. En rojo, la crisis de Camboya (1975-79), separada a propósito",
       caption = fuente) +
  tema_video + theme(strip.text = element_text(face = "bold", size = 22))
guardar(g2, "video_escena2_casos.png")


# Escena 3: índice compuesto (P5)
ind <- readRDS(file.path(DIR_DATOS, "P5_indice_transversal.rds"))
casos3 <- c("Japan", "Republic of Korea", "Colombia", "Nigeria", "Niger")
d3 <- ind[Location %in% casos3]
d3[, pais := factor(nombres_es[Location], levels = nombres_es[casos3])]
fin <- d3[Time == 2100]
g3 <- ggplot(d3, aes(Time, indice_acp, colour = pais)) +
  geom_vline(xintercept = ANIO_ULTIMA_ESTIMACION + 0.5, colour = "grey60", linetype = 2) +
  geom_line(linewidth = 1.6) +
  geom_text(data = fin, aes(label = pais), hjust = 0, nudge_x = 2, size = 6.5, fontface = "bold") +
  scale_colour_manual(values = c("#01665E", "#5AB4AC", "#D4A017", "#B2182B", "#8C510A"), guide = "none") +
  scale_x_continuous(breaks = c(1950, 2000, 2023, 2050, 2100), limits = c(1950, 2125)) +
  labs(x = NULL, y = "Índice del perfil demográfico",
       title = "Seis indicadores, un solo número",
       subtitle = "Más alto = transición más avanzada. Combina fecundidad, mortalidad, edad, crecimiento y migración",
       caption = fuente) +
  tema_video
guardar(g3, "video_escena3_indice.png")


# Escena 4: grupos de trayectoria (P6) en unidades reales
cl <- fread(file.path(DIR_TABLAS, "P6_clusters_paises.csv"))
# Nombres por regla (los números de k-means pueden cambiar entre corridas):
#   el grupo más pequeño = Golfo; de los demás, mayor edad mediana en 1990 =
#   temprana, menor esperanza de vida en 1990 = tardía, el restante = intermedia.
caract <- merge(cl[, .(LocID, cluster_tray)], paises[Time == 1990, .(LocID, MedianAgePop, LEx)], by = "LocID")
caract <- caract[, .(n = .N, edad = mean(MedianAgePop), e0 = mean(LEx)), by = cluster_tray]
golfo <- caract[which.min(n), cluster_tray]
resto <- caract[cluster_tray != golfo]
temprana <- resto[which.max(edad), cluster_tray]
tardia   <- resto[cluster_tray != temprana][which.min(e0), cluster_tray]
nombre_grupo <- function(k) fcase(k == golfo, "Golfo (migración)", k == temprana, "Temprana",
                                  k == tardia, "Tardía", default = "Intermedia")
cl[, grupo := factor(nombre_grupo(cluster_tray), levels = c("Temprana", "Intermedia", "Tardía", "Golfo (migración)"))]
d4 <- merge(paises[, .(LocID, Time, TFR, LEx)], cl[, .(LocID, grupo)], by = "LocID")
d4 <- melt(d4, id.vars = c("LocID", "Time", "grupo"), measure.vars = c("TFR", "LEx"))
d4[, variable := factor(variable, levels = c("TFR", "LEx"),
                        labels = c("Fecundidad (hijos por mujer)", "Esperanza de vida (años)"))]
m4 <- d4[, .(v = median(value), p25 = quantile(value, 0.25), p75 = quantile(value, 0.75)), by = .(grupo, variable, Time)]
n_grupo <- cl[, .N, by = grupo]
etiq <- setNames(paste0(n_grupo$grupo, " (", n_grupo$N, ")"), n_grupo$grupo)
colores_grupo <- c("Temprana" = "#01665E", "Intermedia" = "#5AB4AC", "Tardía" = "#D4A017", "Golfo (migración)" = "#B2182B")
g4 <- ggplot(m4, aes(Time, v, colour = grupo, fill = grupo)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1.5) +
  geom_vline(xintercept = ANIO_ULTIMA_ESTIMACION + 0.5, colour = "grey60", linetype = 2) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_colour_manual(values = colores_grupo, labels = etiq, name = NULL) +
  scale_fill_manual(values = colores_grupo, labels = etiq, name = NULL) +
  labs(x = NULL, y = NULL,
       title = "Cuatro caminos de la transición",
       subtitle = "Mediana de cada grupo (sombra: percentiles 25-75). Entre paréntesis, número de países",
       caption = paste(fuente, "· Grupos: k-means sobre trayectorias 1950-2100")) +
  tema_video + theme(strip.text = element_text(face = "bold", size = 20))
guardar(g4, "video_escena4_grupos.png")


# Escena 6: los dos ejes (2023)
mig <- paises[Time %between% c(2014, 2023), .(migracion = mean(CNMR)), by = LocID]
d6 <- merge(ind[Time == 2023, .(LocID, Location, indice_acp)], mig, by = "LocID")
d6 <- merge(d6, etapas[Time == 2023, .(LocID, etapa, pob = TPopulation1July, pais_pequeno)], by = "LocID")
d6 <- d6[pais_pequeno == FALSE]
d6[, etapa := factor(etapa, levels = names(COLORES_ETAPA))]
d6[, nombre := nombres_es[Location]]
destacar <- c("Colombia", "Japan", "Niger", "Germany", "Qatar", "United Arab Emirates", "Venezuela (Bolivarian Republic of)",
              "India", "China", "United States of America", "Spain", "Nigeria")
g6 <- ggplot(d6, aes(indice_acp, migracion)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(aes(size = pob, fill = etapa), shape = 21, colour = "grey25", alpha = 0.85) +
  ggrepel::geom_text_repel(data = d6[Location %in% destacar], aes(label = nombre), size = 5.5, fontface = "bold",
                           box.padding = 0.6, min.segment.length = 0, seed = SEMILLA) +
  scale_fill_manual(values = COLORES_ETAPA, drop = TRUE, name = NULL) +
  scale_size_area(max_size = 22, guide = "none") +
  scale_y_continuous(limits = c(-25, 30)) +
  labs(x = "Avance de la transición (índice del perfil demográfico, 2023)",
       y = "Migración neta 2014-2023 (por mil)",
       title = "Dos ejes para entender a un país",
       subtitle = "El avance de la transición ordena a casi todos; la migración separa a países con el mismo avance",
       caption = paste(fuente, "· Países con 1 millón de habitantes o más; eje vertical recortado a ±25-30 por mil")) +
  guides(fill = guide_legend(override.aes = list(size = 7))) +
  tema_video
guardar(g6, "video_escena6_cierre.png")

cat("Figuras del video listas en", DIR_FIGURAS, "\n")
