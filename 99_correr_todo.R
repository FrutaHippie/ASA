# =============================================================================
# 99_correr_todo.R
# Corre todo el flujo en orden y deja un registro de lo que pasó.
#
# Tiempos aproximados en un portátil:
#   01 limpieza ................ ~0,5 min
#   02 etapas (81 combinaciones)  ~2 min
#   P1-P8 ...................... ~1 min en total
#   03 video (3 animaciones, 1950-2100) 20-40 min  <- el más lento
#
# Dependencias (por eso el orden):
#   01 -> todo lo demás (crea paises_anual.rds; necesita OGHIST_*.xlsx en
#         codigo/ o externos/ para el grupo de ingreso)
#   02 -> P6 (año de inicio del descenso), P7 (cruces con etapas) y 03 (video)
#   P5 -> P6 (puntajes del ACP funcional)
#   P2 y P3 usan OtherVariants (lo prepara 01)
# =============================================================================

RUTA_BASE <- Sys.getenv("ASA_RUTA", unset = "C:/Users/Asus/Documents/2026/Vértice/ASA")


# 1. Opciones

CORRER_VIDEO   <- TRUE        # FALSE: salta 03 (útil para revisar tablas y figuras rápido)
HACER          <- c("mapa", "cbr_cdr", "tfr_e0")   # qué animaciones de 03 hacer
COLUMNA_ETAPA  <- "etapa"     # "etapa", "etapa_detalle" o "etapa_criterio_total" (video)
ANIOS          <- 1950:2100   # prueba rápida del video: c(1950, 1980, 2023, 2100)
SEGUIR_SI_FALLA <- TRUE       # TRUE: si un script falla, sigue con el siguiente y lo reporta al final


# 2. Scripts en orden

scripts <- c("01_limpieza.R",
             "02_etapas_transicion.R",
             "P1_estructura_edad.R",
             "P2_high_low.R",
             "P3_sensibilidad_fecundidad.R",
             "P4_migracion_crecimiento.R",
             "P5_indice_compuesto.R",
             "P6_clusters_trayectorias.R",     # requiere P5 y 02
             "P7_componentes_conjuntos.R",     # requiere 02 (y el Excel de ingreso vía 01)
             "P8_un_solo_indicador.R",
             "04_figuras_video.R")             # figuras del video (requiere 02, P5, P6 y P7)
if (CORRER_VIDEO) scripts <- c(scripts, "03_timelapse_video.R")   # el más lento: va al final


# 3. Correr, medir tiempos y registrar errores

dir.create(file.path(RUTA_BASE, "salidas"), showWarnings = FALSE)
archivo_log <- file.path(RUTA_BASE, "salidas", "registro_corrida.txt")
cat("Corrida iniciada:", format(Sys.time()), "\n", file = archivo_log)

resumen <- data.frame(script = scripts, estado = NA_character_, minutos = NA_real_,
                      mensaje = "", stringsAsFactors = FALSE)
t_total <- Sys.time()

for (i in seq_along(scripts)) {
  s <- scripts[i]
  cat("\n==========", s, "==========\n")
  t0 <- Sys.time()
  res <- tryCatch({
    source(file.path(RUTA_BASE, "codigo", s), encoding = "UTF-8", local = new.env())
    "OK"
  }, error = function(e) {
    message("ERROR en ", s, ": ", conditionMessage(e))
    paste("ERROR:", conditionMessage(e))
  })
  resumen$minutos[i] <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  resumen$estado[i]  <- if (res == "OK") "OK" else "ERROR"
  resumen$mensaje[i] <- if (res == "OK") "" else res
  cat(s, "-", resumen$estado[i], "-", resumen$minutos[i], "min", resumen$mensaje[i], "\n",
      file = archivo_log, append = TRUE)
  cat("(", resumen$minutos[i], "min )\n")
  if (res != "OK" && !SEGUIR_SI_FALLA) break
}


# 4. Resumen final

cat("\n\n========== RESUMEN ==========\n")
print(resumen[, c("script", "estado", "minutos")], row.names = FALSE)
cat("Tiempo total:", round(as.numeric(difftime(Sys.time(), t_total, units = "mins")), 1), "min\n")
if (any(resumen$estado == "ERROR", na.rm = TRUE)) {
  cat("\nScripts con error (detalle en salidas/registro_corrida.txt):\n")
  for (j in which(resumen$estado == "ERROR")) cat(" -", resumen$script[j], ":", resumen$mensaje[j], "\n")
  cat("Ojo: si falló 01 o 02, los scripts siguientes pueden haber usado datos viejos.\n")
} else {
  cat("Todo corrió sin errores. Figuras en salidas/figuras, tablas en salidas/tablas.\n")
}
