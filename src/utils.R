load_processed_data <- function(input_filename, output_filename) {
    #' Si existe el archivo de datos procesados lo carga, si no los procesa,
    #' guarda el archivo y los devuelve
    #'
    #' @param input_filename ruta de los datos crudos
    #' @param output_filename ruta de los datos procesados
    #'
    #' return un dataframe con los datos preprocesados
    
    data <- data.frame()
    
    if (!file.exists(output_filename)){
        data <- preprocesar(input_filename)
        saveRDS(data, output_filename)
    } else {
        data <- readRDS(output_filename)
    }

    return(data)
}

datos_AMBA <- function(datos){

    ## TODO: improve
    datos <- filter(datos, Departamento != "SIN ESPECIFICAR")
    datosCABA <- filter(datos, Provincia %in% c("Ciudad Autónoma de Buenos Aires"))
    datosBA <- filter(datos, Provincia %in% c("Buenos Aires"))
    datosGBA <- filter(datos, Departamento %in% GBA)
    
    ## a todo caba le pongo el mismo departamento
    ncaba <- length(datosCABA$Departamento)
    datosCABA$Departamento <- rep("Ciudad Autónoma de Buenos Aires", ncaba)
    datosAMBA <- bind_rows(datosCABA, datosGBA)
    return(datosAMBA)
}

distrito_a_seccion <- function(distrito) {
    switch(distrito,
           AMBA = "Departamento",
           PAIS = "Provincia")
}

redondear_fecha <- function(fecha) {
    floor_date(as.Date(fecha), "weeks", week_start = 1)
}
