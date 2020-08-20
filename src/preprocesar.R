FECHA_ORIGEN <- "2020-01-26"

preprocesar <- function(input_filename) {
   
    ## Datos crudos
    sisa_estudiados <- read.csv(input_filename,
                                sep = ",",
                                header = TRUE,
                                encoding = "UTF-8",
                                stringsAsFactors = FALSE)

    ## Proceso
    output <- sisa_estudiados %>%
        filter(
            residencia_provincia_nombre != "SIN ESPECIFICAR", # con provincia
            fecha_diagnostico != "", # confirmados o descartados
            fecha_apertura != "" # con fecha de apertura
        ) %>%
        mutate(
            provincia = replace(
                residencia_provincia_nombre,
                residencia_provincia_nombre == "CABA",
                "Ciudad Autónoma de Buenos Aires"),
            provincia = replace(
                residencia_provincia_nombre,
                residencia_provincia_nombre == "Tierra del Fuego",
                "Antártida e Islas del Atlántico Sur"),
            label_semana = floor_date(as.Date(fecha_apertura), 
                                         "weeks", week_start = 1)
        ) %>%
        select(
            "Clasificacion" = clasificacion_resumen,
            "Fecha"         = fecha_apertura,
            "semana"        = label_semana,
            "Provincia"     = provincia,
            "Departamento"  = residencia_departamento_nombre)
    
    return(output)
}
