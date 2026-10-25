# =============================================================================
# 01_limpieza.R
# Gestión y limpieza de los datos WPP 2024.
#
# Produce (en salidas/datos):
#   paises_anual.rds / .csv     panel país-año 1950-2100 (237 países), con
#                               región, subregión, región ODS, LE60, banderas
#   agregados_oficiales.rds     filas oficiales de Mundo, regiones e ingreso
#   variantes_paises.rds        OtherVariants solo para países (2024-2100)
#   ubicaciones.csv             tabla de ubicaciones con su jerarquía
# y en salidas/tablas:
#   reporte_calidad.txt         verificaciones de identidades, faltantes, etc.
# =============================================================================

if (!exists("RUTA_BASE")) {
  RUTA_BASE <- Sys.getenv("ASA_RUTA", unset = "C:/Users/Asus/Documents/2026/Vértice/ASA")
}
source(file.path(RUTA_BASE, "codigo", "00_configuracion.R"), encoding = "UTF-8")

# Todo lo que se imprime en este script queda también en el reporte de calidad
archivo_reporte <- file.path(DIR_TABLAS, "reporte_calidad.txt")
sink(archivo_reporte, split = TRUE)
cat("REPORTE DE CALIDAD - WPP 2024 -", format(Sys.time()), "\n\n")


# 1. Lectura del archivo Medium
#    fread lee el archivo aunque no tenga extensión .csv y maneja el BOM UTF-8.

medium <- fread(RUTA_MEDIUM, encoding = "UTF-8")
# Por si el BOM quedara pegado al nombre de la primera columna
setnames(medium, 1, sub("^\ufeff", "", names(medium)[1]))

cat("Medium:", nrow(medium), "filas,", ncol(medium), "columnas\n")
cat("Años:", min(medium$Time), "a", max(medium$Time), "\n")
cat("Variantes en Medium:", paste(unique(medium$Variant), collapse = ", "), "\n\n")

# Los ~234 agrupamientos ad hoc (BRICS, ECA, ADB, ...) vienen con LocTypeName vacío.
medium[LocTypeName == "" | is.na(LocTypeName), LocTypeName := "Ad hoc (sin tipo)"]
cat("Ubicaciones distintas por tipo:\n")
print(medium[, .(ubicaciones = uniqueN(LocID)), by = LocTypeName][order(-ubicaciones)])


# 2. Tabla de ubicaciones y jerarquía geográfica
#    País -> (ParentID) -> subregión -> (ParentID) -> región geográfica.
#    Excepción: los países de Norteamérica (ParentID 918) no tienen la fila de
#    su subregión en el archivo; se asignan a mano.

ubic <- unique(medium[, .(LocID, Location, ISO3_code, LocTypeName, ParentID)])
stopifnot(!anyDuplicated(ubic$LocID))

padres  <- ubic[, .(ParentID = LocID, padre_nombre = Location, padre_tipo = LocTypeName,
                    abuelo_ID = ParentID)]
abuelos <- ubic[, .(abuelo_ID = LocID, abuelo_nombre = Location)]

paises_ubic <- ubic[LocTypeName == "Country/Area"]
paises_ubic <- merge(paises_ubic, padres, by = "ParentID", all.x = TRUE)
paises_ubic <- merge(paises_ubic, abuelos, by = "abuelo_ID", all.x = TRUE)

paises_ubic[padre_tipo == "Subregion", `:=`(subregion = padre_nombre, region = abuelo_nombre)]
# Bermuda, Canadá, Groenlandia, San Pedro y Miquelón y EE. UU. tienen ParentID = 918
# (subregión "Northern America"), que no aparece como fila en el archivo.
paises_ubic[ParentID == 918, `:=`(subregion = "Northern America", region = "Northern America")]
cat("\nPaíses sin padre identificado:", paises_ubic[is.na(subregion), .N], "\n")

# Región ODS (8 regiones de la ONU) construida a partir de la subregión M49
mapa_ods <- data.table(
  subregion = c("Eastern Africa", "Middle Africa", "Southern Africa", "Western Africa",
                "Northern Africa", "Western Asia",
                "Central Asia", "Southern Asia",
                "Eastern Asia", "South-Eastern Asia",
                "Caribbean", "Central America", "South America",
                "Melanesia", "Micronesia", "Polynesia",
                "Australia/New Zealand",
                "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe",
                "Northern America"),
  region_ods = c(rep("África subsahariana", 4),
                 rep("África del Norte y Asia occidental", 2),
                 rep("Asia central y meridional", 2),
                 rep("Asia oriental y sudoriental", 2),
                 rep("América Latina y el Caribe", 3),
                 rep("Oceanía (sin Australia/NZ)", 3),
                 "Australia/Nueva Zelanda",
                 rep("Europa y Norteamérica", 5))
)
paises_ubic <- merge(paises_ubic, mapa_ods, by = "subregion", all.x = TRUE)

cat("\nPaíses:", nrow(paises_ubic), "\n")
cat("Países sin ISO3:", sum(is.na(paises_ubic$ISO3_code) | paises_ubic$ISO3_code == ""), "\n")
cat("Países sin subregión:", sum(is.na(paises_ubic$subregion)), "\n")
cat("Países sin región ODS:", sum(is.na(paises_ubic$region_ods)), "\n")
cat("\nPaíses por región geográfica:\n");  print(paises_ubic[, .N, by = region][order(-N)])
cat("\nPaíses por región ODS:\n");          print(paises_ubic[, .N, by = region_ods][order(-N)])
stopifnot(!any(is.na(paises_ubic$region_ods)))

paises_ubic <- paises_ubic[, .(LocID, ISO3_code, Location, subregion, region, region_ods)]
fwrite(paises_ubic, file.path(DIR_DATOS, "ubicaciones.csv"))


# 3. Panel país-año

indicadores <- setdiff(names(medium),
                       c("SortOrder", "LocID", "Notes", "ISO3_code", "ISO2_code", "SDMX_code",
                         "LocTypeID", "LocTypeName", "ParentID", "Location", "VarID",
                         "Variant", "Time"))

paises <- medium[LocTypeName == "Country/Area", c("LocID", "Time", indicadores), with = FALSE]
setkey(paises, LocID, Time)
stopifnot(!anyDuplicated(paises[, .(LocID, Time)]))

# La fila 2101 solo trae la población al 1 de enero. La guardamos como
# "población al 1 de enero del año siguiente" y luego quitamos 2101.
paises[, TPopulation1Jan_sig := shift(TPopulation1Jan, type = "lead"), by = LocID]
paises <- paises[Time <= 2100]

# Suplemento: LE60 y nacimientos por sexo (no trae Holy See)
supl <- fread(RUTA_SUPLEMENTO, encoding = "UTF-8")
setnames(supl, 1, sub("^\ufeff", "", names(supl)[1]))
paises <- merge(paises, supl[, .(LocID, Time, BirthsMale, BirthsFemale, LE60, LE60Male, LE60Female)],
                by = c("LocID", "Time"), all.x = TRUE)

paises <- merge(paises_ubic, paises, by = "LocID")
setkey(paises, LocID, Time)

# Banderas útiles
paises[, periodo := fifelse(Time <= ANIO_ULTIMA_ESTIMACION, "Estimación", "Proyección")]
pob_2023 <- paises[Time == ANIO_ULTIMA_ESTIMACION, .(LocID, pob_2023 = TPopulation1July)]
paises <- merge(paises, pob_2023, by = "LocID")
paises[, pais_pequeno := pob_2023 < UMBRAL_POB_PEQUENO]
setkey(paises, LocID, Time)

cat("\nPanel país-año:", nrow(paises), "filas;", uniqueN(paises$LocID), "países;",
    min(paises$Time), "-", max(paises$Time), "\n")
cat("Países con < 1 millón de habitantes en 2023:", uniqueN(paises[pais_pequeno == TRUE, LocID]), "\n")


# 4. Verificaciones de consistencia (identidades demográficas)

cat("\n--- Identidades demográficas (países, 1950-2100) ---\n")
# (a) Ecuación compensadora: P(1 ene, t+1) = P(1 ene, t) + cambio poblacional
dif_a <- paises[, abs(TPopulation1Jan_sig - (TPopulation1Jan + PopChange))]
# (b) Crecimiento natural = nacimientos - defunciones
dif_b <- paises[, abs(NatChange - (Births - Deaths))]
# (c) Cambio poblacional = crecimiento natural + migración neta
dif_c <- paises[, abs(PopChange - (NatChange + NetMigrations))]
# (d) Tasa de crecimiento = 100 * ln(P(t+1)/P(t))  (tasa exponencial)
dif_d <- paises[, abs(PopGrowthRate - 100 * log(TPopulation1Jan_sig / TPopulation1Jan))]
# (e) Tasa de crecimiento natural = CBR - CDR (por mil)
dif_e <- paises[, abs(NatChangeRT - (CBR - CDR))]

resumen_id <- data.table(
  identidad = c("P(t+1) = P(t) + PopChange", "NatChange = Births - Deaths",
                "PopChange = NatChange + NetMigrations", "PopGrowthRate = 100 ln(P(t+1)/P(t))",
                "NatChangeRT = CBR - CDR"),
  max_abs = sapply(list(dif_a, dif_b, dif_c, dif_d, dif_e), max, na.rm = TRUE),
  p99     = sapply(list(dif_a, dif_b, dif_c, dif_d, dif_e), quantile, probs = 0.99, na.rm = TRUE)
)
print(resumen_id)


# 5. Faltantes y rangos

cat("\n--- Faltantes por columna (solo columnas con alguno) ---\n")
falt <- paises[, lapply(.SD, function(x) sum(is.na(x)))]
falt <- melt(falt, measure.vars = names(falt), variable.name = "columna", value.name = "n_faltantes")
print(falt[n_faltantes > 0])

cat("\n--- Rangos de indicadores clave ---\n")
clave <- c("TFR", "LEx", "MedianAgePop", "CBR", "CDR", "CNMR", "PopGrowthRate", "IMR", "LE60", "LE65")
rangos <- paises[, lapply(.SD, function(x) c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))), .SDcols = clave]
rangos[, estadistico := c("min", "max")]
print(rangos)

cat("\nValores extremos de migración neta (|CNMR| > 100 por mil):\n")
print(paises[abs(CNMR) > 100, .(Location, Time, CNMR, pob_2023)][order(-abs(CNMR))][1:15])


# 6. Agregados oficiales (para comparar con los países, nunca para sumar con ellos)

agregados <- medium[LocTypeName %in% c("World", "Geographic region", "Subregion", "SDG region",
                                       "Income group", "Development group"),
                    c("LocID", "Location", "LocTypeName", "Time", indicadores), with = FALSE]
cat("\nAgregados oficiales guardados:", uniqueN(agregados$LocID), "ubicaciones\n")

# Verificación: la suma de los países reproduce la población mundial
chequeo_mundo <- merge(paises[, .(suma_paises = sum(TPopulation1July)), by = Time],
                       agregados[Location == "World", .(Time, mundo = TPopulation1July)], by = "Time")
cat("Máxima diferencia relativa suma de países vs. Mundo:",
    signif(chequeo_mundo[, max(abs(suma_paises / mundo - 1))], 3), "\n")


# 7. Grupo de ingreso del Banco Mundial (dato externo)
#    Archivo: OGHIST_*.xlsx (World Bank, "Historical classification by income"),
#    hoja "Country Analytical History", en la carpeta codigo/ o externos/.
#    Trae la clasificación de cada país en cada año fiscal FY89-FY27:
#      L = ingreso bajo, LM = mediano bajo, UM = mediano alto, H = alto.
#    DECISIONES
#    a) Año: cada columna del archivo dice "Data for calendar year" (1987-2025):
#       la clasificación del año fiscal FY(t+2) se basa en el INB per cápita
#       (método Atlas) del año calendario t. Se asigna al año t, es decir, al año
#       al que se refiere el ingreso. (Para usar el año en que la clasificación
#       estuvo vigente, cambiar DESFASE_INGRESO a 1 o 2.)
#    b) ".." = sin clasificar -> NA. "LM*" (Yemen unificado, 1987-1989) -> LM.
#    c) No se heredan clasificaciones de estados que ya no existen (URSS,
#       Checoslovaquia, Yugoslavia): sus sucesores quedan NA antes de su primera
#       clasificación propia.
#    d) Columnas que se crean en el panel:
#       grupo_ingreso_anual : clasificación del año (NA fuera de 1987-2025)
#       grupo_ingreso       : clasificación vigente (último año disponible del
#                             país), FIJA para todos los años; es la que se usa
#                             para agrupar trayectorias completas y proyecciones
#       grupo_ingreso_inicial: primera clasificación disponible (1987 en la
#                             mayoría): sirve para agrupar sin usar información
#                             posterior al periodo que se analiza
#       anio_grupo_vigente, anio_grupo_inicial
#    OJO: verificar que las reglas de la competencia permiten datos externos y
#    citar la fuente: World Bank (2026), World Bank Country and Lending Groups,
#    Historical classification by income (OGHIST, actualizado julio 2026).

DESFASE_INGRESO <- 0
NIVELES_INGRESO <- c("L" = "Ingreso bajo", "LM" = "Ingreso mediano bajo",
                     "UM" = "Ingreso mediano alto", "H" = "Ingreso alto")

archivos_ingreso <- list.files(c(file.path(RUTA_BASE, "codigo"), file.path(RUTA_BASE, "externos")),
                               pattern = "^OGHIST.*\\.xlsx$", full.names = TRUE)
if (length(archivos_ingreso) > 0) {
  if (!"readxl" %in% rownames(installed.packages())) install.packages("readxl")
  RUTA_INGRESO <- archivos_ingreso[which.max(file.mtime(archivos_ingreso))]
  cat("\nClasificación de ingreso:", basename(RUTA_INGRESO), "\n")
  bm <- as.data.table(readxl::read_excel(RUTA_INGRESO, sheet = "Country Analytical History",
                                         col_names = FALSE, col_types = "text",
                                         .name_repair = function(n) paste0("c", seq_along(n))))
  fila_anios <- which(bm$c2 == "Data for calendar year :")
  anios_bm <- as.integer(unlist(bm[fila_anios, -(1:2)]))
  bm <- bm[!is.na(c1) & nchar(c1) %in% 3:4]                     # filas de países
  setnames(bm, c("ISO3_code", "nombre_bm", paste0("a", anios_bm)))
  ingreso <- melt(bm, id.vars = c("ISO3_code", "nombre_bm"), variable.name = "anio", value.name = "cod")
  ingreso[, Time := as.integer(sub("a", "", anio)) + DESFASE_INGRESO]
  ingreso[, cod := sub("\\*", "", cod)]
  ingreso[cod == "..", cod := NA_character_]
  ingreso <- ingreso[!is.na(cod)]
  ingreso[, grupo_ingreso_anual := factor(NIVELES_INGRESO[cod], levels = NIVELES_INGRESO)]

  sin_wpp <- setdiff(unique(ingreso$ISO3_code), unique(paises$ISO3_code))
  cat("Códigos del Banco Mundial sin país en WPP (estados que ya no existen o agregados):",
      paste(sin_wpp, collapse = ", "), "\n")
  ingreso <- ingreso[ISO3_code %in% unique(paises$ISO3_code)]

  resumen_pais <- ingreso[order(Time), .(
    grupo_ingreso         = grupo_ingreso_anual[.N], anio_grupo_vigente = Time[.N],
    grupo_ingreso_inicial = grupo_ingreso_anual[1],  anio_grupo_inicial = Time[1]
  ), by = ISO3_code]

  paises <- merge(paises, ingreso[, .(ISO3_code, Time, grupo_ingreso_anual)],
                  by = c("ISO3_code", "Time"), all.x = TRUE)
  paises <- merge(paises, resumen_pais, by = "ISO3_code", all.x = TRUE)
  setkey(paises, LocID, Time)

  cat("Países >= 1 millón sin clasificación:",
      paste(unique(paises[pais_pequeno == FALSE & is.na(grupo_ingreso), Location]), collapse = ", "), "\n")
  cat("Países (todos) con clasificación:", uniqueN(paises[!is.na(grupo_ingreso), LocID]), "de", uniqueN(paises$LocID), "\n")
  cat("Clasificación vigente que no corresponde al último año del archivo (país sin clasificar después):\n")
  print(unique(paises[!is.na(anio_grupo_vigente) & anio_grupo_vigente < max(anios_bm) + DESFASE_INGRESO,
                      .(Location, grupo_ingreso, anio_grupo_vigente)]))
  cat("Países >= 1 millón por grupo vigente:\n")
  print(unique(paises[pais_pequeno == FALSE, .(LocID, grupo_ingreso)])[, .N, by = grupo_ingreso][order(grupo_ingreso)])
} else {
  paises[, `:=`(grupo_ingreso_anual = factor(NA, levels = NIVELES_INGRESO),
                grupo_ingreso = factor(NA, levels = NIVELES_INGRESO),
                anio_grupo_vigente = NA_integer_,
                grupo_ingreso_inicial = factor(NA, levels = NIVELES_INGRESO),
                anio_grupo_inicial = NA_integer_)]
  cat("\nNo se encontró OGHIST_*.xlsx en codigo/ ni en externos/ -> grupo de ingreso vacío.\n")
}


# 8. OtherVariants: solo países y columnas útiles (archivo de ~230 MB)

cols_var <- c("LocID", "LocTypeName", "Variant", "Time", "TPopulation1Jan", "TPopulation1July",
              "Births", "Deaths", "NetMigrations", "TFR", "LEx", "MedianAgePop",
              "PopGrowthRate", "CBR", "CDR", "CNMR", "NatChangeRT")
variantes <- fread(RUTA_VARIANTES, encoding = "UTF-8", select = cols_var)
variantes <- variantes[LocTypeName == "Country/Area"][, LocTypeName := NULL]
variantes <- merge(paises_ubic[, .(LocID, ISO3_code, Location, region, region_ods)], variantes, by = "LocID")
variantes <- variantes[Time <= 2100]

cat("\nOtherVariants (países):", nrow(variantes), "filas\n")
print(variantes[, .(filas = .N, faltantes_pob = sum(is.na(TPopulation1July))), by = Variant])
cat("Nota: 'Mean' no trae población; las bandas PI son cuantiles marginales por indicador.\n")


# 9. Guardar

saveRDS(paises,    file.path(DIR_DATOS, "paises_anual.rds"))
saveRDS(agregados, file.path(DIR_DATOS, "agregados_oficiales.rds"))
saveRDS(variantes, file.path(DIR_DATOS, "variantes_paises.rds"))
fwrite(paises,     file.path(DIR_DATOS, "paises_anual.csv"))

cat("\nArchivos guardados en", DIR_DATOS, "\n")
sink()
