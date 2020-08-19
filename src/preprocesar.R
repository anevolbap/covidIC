FECHA_ORIGEN <- "2020-01-26"

preprocesar <- function(input_filename) {
   
    ## Datos crudos
    sisa_estudiados <- read.csv(input_filename,
                                sep = ",",
                                header = TRUE,
                                encoding = "UTF-8",
                                stringsAsFactors = FALSE)

    ## Limpieza
    sisa_estudiados <- filter(
        sisa_estudiados,
        residencia_provincia_nombre != "SIN ESPECIFICAR", # con provincia
        fecha_diagnostico != "", # confirmados o descartados
        fecha_apertura != "" # con fecha de apertura
    )

    ## Renombro CABA para pegar los mapas  # TODO: revisar
    sisa_estudiados <- sisa_estudiados %>%
        mutate(provincia = replace(
                   residencia_provincia_nombre,
                   residencia_provincia_nombre == "CABA",
                   "Ciudad Autónoma de Buenos Aires")                
               )

    ## Fechas
    sisa_estudiados <- sisa_estudiados %>%
        mutate(labelsemana = floor_date(as.Date(fecha_apertura),
                                        "weeks", week_start = 1),
               semanaFA = isoweek(fecha_apertura) - isoweek(FECHA_ORIGEN)
               )
      
    ## Final output
    output <- select(sisa_estudiados,
                       "Clasificacion" = clasificacion_resumen,
                       "Fecha"         = fecha_apertura,
                       "SemanaNum"     = semanaFA,
                       "SemanaLab"     = labelsemana,
                       "Provincia"     = provincia,
                       "Departamento"  = residencia_departamento_nombre
                       )

    return(output)
}
