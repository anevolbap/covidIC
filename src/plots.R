library(tidyverse)
library(plotly)
library(sf)
library(viridis)        # FIXME: se usa?
library(sp)             # FIXME: se usa?
library(RColorBrewer)   # FIXME: se usa?

GBA = readLines("data/gba.dat")
AMBA = readLines("data/amba.dat")

## MAPAS
mapaAMBA <- st_read("data/departamento.shp")
argen <- st_read('data/provincia.json')
colnames(argen) <- c("gid", "entidad", "objeto", "fna", "gna", "Provincia",
                     "in1", "fdc", "sag", "geometry")

graficosemanas <- function(datos) {
    
    nsem <- max(as.numeric(datos$SemanaNum))
    TESTPS <- test_prov_aux(datos)
    salidaPS <- bind_rows(lapply(1:nsem, function(x) testsemanprov(x, TESTPS, datos)))
    auxsemana <- (as.numeric(salidaPS$semana) - 1) * 7 + 1
    salidaPS$labelsemana <- as.Date(auxsemana, origin = "2020-01-26")

    d <- ggplot(salidaPS, aes(x = labelsemana, y = PorcPos, group = Provincia,
                              text = paste("provincia:", Provincia, "\nsemana",
                                           labelsemana, "\ntest:", TestTot,
                                           "\npositivos:", TestPos,
                                           "\nPositividad", PorcPos, "%"))) +
        geom_line(size = 1, aes(colour = Provincia)) +
        xlab("Semana") +
        ylab("Porcentaje de positividad") +
        scale_x_date(date_labels = "%b %d")
    p <- ggplotly(d, tooltip = "text") %>% add_markers()
    
    return(p)
}

test_prov_aux <- function(datos){
    ConfPS <- datos %>%
        group_by(Provincia, SemanaNum) %>%
        summarise(sum(Clasificacion == "Confirmado"))
    TotalPS <- datos %>%
        group_by(Provincia, SemanaNum) %>%
        summarise(sum(Clasificacion == "Descartado") +
                  sum(Clasificacion == "Confirmado"))
    PorPosPS <- datos %>%
        group_by(Provincia, SemanaNum) %>%
        summarise(round(100 * sum(Clasificacion == "Confirmado") /
                        (sum(Clasificacion == "Descartado") +
                         sum(Clasificacion == "Confirmado"))))
    TESTPS <- bind_cols(TotalPS, ConfPS[, 3], PorPosPS[, 3])
    colnames(TESTPS) <- c("Provincia", "semana", "TestTot", "TestPos", "PorcPos")
    return(TESTPS)
}

testsemanprov <- function(LAsemana, TESTPS, datos) {
    ## calcula la positividad para LAsemana en todas las provincias
    TESTPSELEC <- filter(TESTPS, semana == LAsemana)
    todoelec <- TESTPSELEC
    
    ## esto lo hago por si hay provincias que no fue medido test para que ponga 0 y no NA
    if (length(unique(datos$Provincia)) != length(unique(TESTPSELEC$Provincia))) {
        numprov <- length(unique(datos$Provincia))
        PROVINCIASNA <- data.frame(unique(datos$Provincia))
        aux <- matrix(rep(0, numprov * 3), numprov, 3)
        PROVINCIASNA <- cbind(PROVINCIASNA, rep(LAsemana, numprov), aux)
        colnames(PROVINCIASNA) <- c("Provincia", "semana", "TestTot", "TestPos", "PorcPos")
        PROVSINTEST <- PROVINCIASNA %>% filter(!Provincia %in% unique(TESTPSELEC$Provincia))
        todoelec <- bind_rows(TESTPSELEC, PROVSINTEST)
    }
    reside <- todoelec$Provincia
    todoelec$Provincia <- ifelse(reside == "Tierra del Fuego",
                                 "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
                                 reside)
    return(todoelec)
}

graficosemanaPAIS <- function(LAsemana, datos, mapaarg) {
    ## Funcion que grafica en un mapa positividad para una semana dada
    todoelec <- testsemanprov(LAsemana, datos)

    mapaarg1 <- left_join(mapaarg, todoelec)
    posiblessemanas <- unique(datos$SemanaLab)

    semanaN1 <- (LAsemana - 1) * 7 + 1
    semanaN <- as.Date(semanaN1, origin = "2020-01-31")
    mp <- ggplot(mapaarg1) +
        geom_sf(aes(fill = PorcPos,
                    text = paste(Provincia, "\n", TestTot, "test </b>  \n",
                                 TestPos, "positivos\n", PorcPos,
                                 "% positividad"))) +
        ggtitle(paste("semana", semanaN)) +
        labs(fill = "%positividad", scale_fill_viridis_c(alpha = 0.8))
    mp <- mp + coord_sf(xlim = c(-80, -45), ylim = c(-55, -20), clip = "on")
    mp <- ggplotly(mp, tooltip = "text")
    return(mp)
}

graficosemanasAMBA <- function(datos) {
    nsem <- max(as.numeric(datos$SemanaNum))
    salidaPS <- bind_rows(lapply(1:nsem, function(x) testsemanaAMBA(x, datos)))
    auxsemana <- (as.numeric(salidaPS$semana) - 1) * 7 + 1
    salidaPS$labelsemana <- as.Date(auxsemana, origin = "2020-01-26")

    d <- ggplot(salidaPS,
                aes(x = labelsemana, y = PorcPos, group = Departamento,
                    text = paste("departamento:", Departamento, "\nsemana",
                                 labelsemana, "\ntest:", TestTot,
                                 "\npositivos:", TestPos, "\nPositividad",
                                 PorcPos, "%"))) +
        geom_line(size = 1) +
        xlab("Semana") +
        ylab("Porcentaje de positividad") +
        scale_x_date(date_labels = "%b %d")
    p <- ggplotly(d, tooltip = "text") %>% add_markers()
    return(p)
}

testsemanaAMBA <- function(LAsemana, datos) {
    datos <- filter(datos, Departamento != "SIN ESPECIFICAR")
    datosCABA <- filter(datos, Provincia %in% c("Ciudad Autónoma de Buenos Aires"))
    datosBA <- filter(datos, Provincia %in% c("Buenos Aires"))
    datosGBA <- filter(datos, Departamento %in% GBA)
    
    ## a todo caba le pongo el mismo departamento
    ncaba <- length(datosCABA$Departamento)
    datosCABA$Departamento <- rep("Ciudad Autónoma de Buenos Aires", ncaba)
    datosAMBA <- bind_rows(datosCABA, datosGBA)

    ConfDS <- datosAMBA %>%
        group_by(Departamento, SemanaNum) %>%
        summarise(sum(Clasificacion == "Confirmado"))
    TotalDS <- datosAMBA %>%
        group_by(Departamento, SemanaNum) %>%
        summarise(sum(Clasificacion == "Descartado") + sum(Clasificacion == "Confirmado"))
    PorPosDS <- datosAMBA %>%
        group_by(Departamento, SemanaNum) %>%
        summarise(round(100 * sum(Clasificacion == "Confirmado") / (sum(Clasificacion == "Descartado") + sum(Clasificacion == "Confirmado"))))
    TESTDS <- bind_cols(TotalDS, ConfDS[, 3], PorPosDS[, 3])
    ##          TESTDS <- data.frame(TESTDS)
    colnames(TESTDS) <- c("Departamento", "semana", "TestTot", "TestPos", "PorcPos")
    TESTDSELEC <- filter(TESTDS, semana == LAsemana)
    todoelec <- TESTDSELEC
    ## esto lo hago por si hay provincias que no fue medido test para que ponga 0
    if (length(unique(datosAMBA$Departamento)) != length(unique(TESTDSELEC$Departamento))) {
        numdep <- length(AMBA)
        DepartamentoNA <- data.frame(AMBA)
        aux <- matrix(rep(0, numdep * 3), numdep, 3)
        DepartamentoNA <- cbind(DepartamentoNA, rep(LAsemana, numdep), aux)
        colnames(DepartamentoNA) <- c("Departamento", "semana", "TestTot", "TestPos", "PorcPos")
        DepSINTEST <- DepartamentoNA %>% filter(!Departamento %in% unique(TESTDSELEC$Departamento))
        todoelec <- bind_rows(TESTDSELEC, DepSINTEST)
    }
    return(todoelec)
}

graficosemanaAMBA <- function(LAsemana, datos, mapaAMBA) {
    salida <- testsemanaAMBA(LAsemana, datos)
    mapaAMBA$Departamento <- mapaAMBA$nam
    mapaAMBA$Departamento[mapaAMBA$gna == "Comuna"] <- "Ciudad Autónoma de Buenos Aires"
    listado <- c(390, 391, 445, 9, 11, 57, 50, 62, 52, 53, 97, 74, 101, 122,
                 145, 146, 147, 179, 299, 199, 309, 191, 192, 197, 200, 205,
                 260, 354, 355, 273, 306, 350, 351, 375, 392, 393, 420, 419,
                 433, 439, 440, 452, 446, 449, 450, 451, 453, 458, 461, 520,
                 521, 60, 187, 202, 196)
    LosdeGBABA <- (mapaAMBA$gid %in% listado)
    GBABA <- mapaAMBA[LosdeGBABA, ]
    GBABA2 <- GBABA %>% left_join(salida, by = "Departamento")
    auxsemana <- (LAsemana - 1) * 7 + 1
    semanaN <- as.Date(auxsemana, origin = "2020-01-26")
    mp <- ggplot(data = GBABA2) +
        geom_sf(aes(fill = PorcPos, text = paste("En <b>", Departamento,
                                                 "</b> se hicieron \n", TestTot,
                                                 "test </b>  \n", TestPos,
                                                 "positivos\n", PorcPos,
                                                 "% de positividad"))) +
        ggtitle(paste("semana", semanaN)) +
        labs(fill = "%positividad", scale_fill_viridis_c(alpha = 0.8))
    mp <- ggplotly(mp, tooltip = "text")
    return(mp)
}

## esta funcion no la uso
##          testsemanaGBABA <- function(LAsemana, datos) {
##            datos <- filter(datos, Departamento != "SIN ESPECIFICAR")
##            datosCABA <- filter(datos, Provincia %in% c("Ciudad Autónoma de Buenos Aires"))
##            datosBA <- filter(datos, Provincia %in% c("Buenos Aires"))
##            ncaba <- length(datosCABA$Departamento)
##            datosCABA$labelDepto <- rep("Ciudad Autónoma de Buenos Aires",ncaba)
##            datosBA$labelDepto <- datosBA$Departamento
##            datosGBABA <- data.frame(rbind(datosCABA, datosBA))
##            ConfDS <- datosGBABA %>%
##              group_by(labelDepto, SemanaNum) %>%
##              summarise(sum(Clasificacion == "Confirmado"))
##            TotalDS <- datosGBABA %>%
##              group_by(labelDepto, SemanaNum) %>%
##              summarise(sum(Clasificacion == "Descartado") + sum(Clasificacion == "Confirmado"))
##            PorPosDS <- datosGBABA %>%
##              group_by(labelDepto, SemanaNum) %>%
##                summarise(round(100 * sum(Clasificacion == "Confirmado") / (sum(Clasificacion == "Descartado") + sum(Clasificacion == "Confirmado"))))
##            TESTDS <- cbind(TotalDS, ConfDS[, 3], PorPosDS[, 3])
##            TESTDS <- data.frame(TESTDS)
##            colnames(TESTDS) <- c("labelDepto", "semana", "TestTot", "TestPos", "PorcPos")
##            TESTDSELEC <- filter(TESTDS, semana == LAsemana)
##            todoelec <- TESTDSELEC
##            # esto lo hago por si hay provincias que no fue medido test para que ponga 0
##            if (length(unique(datosGBABA$labelDepto)) != length(unique(TESTDSELEC$labelDepto))) {
##              numdep <- length(unique(datosGBABA$labelDepto))
##              Departamentoceros <- data.frame(unique(datosGBABA$labelDepto))
##              Departamentoceros <- cbind(Departamentoceros, rep(LAsemana, numdep), matrix(rep(0, numdep * 3), numdep, 3))
##              colnames(Departamentoceros) <- c("labelDepto", "semana", "TestTot", "TestPos", "PorcPos")
##              DepSINTEST <- Departamentoceros %>% filter(!labelDepto %in% unique(TESTDSELEC$labelDepto))
##              todoelec <- rbind(TESTDSELEC, DepSINTEST)
##            }
##            todoelec
##          }
