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

    ## Renombro CABA solo para pegar mas facil los mapas
    sisa_estudiados <- sisa_estudiados %>%
        mutate(provincia = replace(
                   residencia_provincia_nombre,
                   residencia_provincia_nombre == "CABA",
                   "Ciudad Autónoma de Buenos Aires"
               ))

    ##FIXME:lubridate
    ## Genero semanas para fecha de apertura  
    dia1 <- (sisa_estudiados$fecha_apertura)[1]
    fecha_apertura2 <- as.numeric(as.Date(sisa_estudiados$fecha_apertura, format = "%Y-%m-%d"))
    dia1n <- min(na.omit(fecha_apertura2, na.omit))
    ## resto 4 porque el primer dia fue un viernes, asi lo llevo a lunes (esto da la semana numerica)
    sisa_estudiados$semanaFA <- as.integer(trunc((fecha_apertura2 - (dia1n - 4)) / 7) + 1)
    ## genero el label de cada semana
    auxsemana <- (sisa_estudiados$semanaFA - 1) * 7 + 1
    sisa_estudiados$labelsemana <- as.integer(as.Date(auxsemana, origin = "2020-01-26"))
    
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
