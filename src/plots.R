library(tidyverse)
library(plotly)
library(sf)
library(sp)             # FIXME: se usa?
library(RColorBrewer)   # FIXME: se usa?

source("src/tests.R")
source("src/mapas.R")

plot_curva_positividad <- function(datos, distrito) {
    #' Grafica la positividad por semana para cada distrito
    #'
    #' @param datos
    #' @param distrito "A"
    if (distrito == "AMBA") {
        datos <- datos_AMBA(datos)
        group <- "Departamento"
        fun <- testsemanaAMBA
    }
    if (distrito == "PAIS") {
        group <- "Provincia"
        fun <- testsemanprov
    }
    
    testPS <- test_df(datos, group = group)
    salidaPS <- bind_rows(lapply(1:max(datos$SemanaNum),
                                 function(x) fun(x, testPS, datos)))

    d <- ggplot(salidaPS,
                aes(x = SemanaLab, y = PorcPos, group = get(group),
                    text = paste("provincia:", group,
                                 "\nsemana", SemanaLab,
                                 "\ntest:", TestTot,
                                 "\npositivos:", TestPos,
                                 "\nPositividad", PorcPos, "%"))) +
        geom_line(size = 1, aes(colour = get(group))) +
        xlab("Fecha") +
        ylab("Porcentaje de positividad") +
        scale_x_date(date_labels = "%b %d")
    p <- ggplotly(d, tooltip = "text") %>% add_markers()
    
    return(p)
}

grafico_por_semana <- function(semana, datos, distrito) {
    if (distrito == "AMBA"){
        ret = grafico_semana_AMBA(semana, datos, mapa_AMBA)
    }
    if (distrito == "PAIS"){
        ret = grafico_semana_PAIS(semana, datos, mapa_ARG)
    }
    ret 
}

grafico_semana_PAIS <- function(semana, datos, mapa_arg) {
    #' Funcion que grafica en un mapa positividad para una semana dada
    #'
    #' @param semana la semana a graficar
    #' @param datos dataframe con los datos
    #' @param mapa_arg datos para el mapa
    #'

    TESTPS <- test_df(datos, group = "Provincia")
    todoelec <- testsemanprov(semana, TESTPS, datos)

    mapa_arg1 <- left_join(mapa_arg, todoelec)
    posiblessemanas <- unique(datos$SemanaLab)
    semanaN1 <- (semana - 1) * 7 + 1
    semanaN <- as.Date(semanaN1, origin = "2020-01-31")

    ##
    mp <- ggplot(data = mapa_arg1) +
        geom_sf_interactive(aes(fill = PorcPos,
                                tooltip = paste(Provincia, "\n", TestTot, "test </b>  \n",
                                                TestPos, "positivos\n", PorcPos,
                                                "Positividad (%)")))  +
        coord_sf(xlim = c(-80, -45), ylim = c(-55, -20), clip = "on") +
        ggtitle(paste("semana", semanaN)) +
        labs(fill = "Positividad (%)", scale_fill_viridis_c(alpha = 0.8))
    mp <- girafe(ggobj = mp)
    mp
}

grafico_semana_AMBA <- function(LAsemana, datos, mapaAMBA) {
    salida <- testsemanaAMBA(LAsemana, test_df(datos,"Departamento"), mapaAMBA)
    mapaAMBA$Departamento <- mapaAMBA$nam
    mapaAMBA$Departamento[mapaAMBA$gna == "Comuna"] <- "Ciudad Autónoma de Buenos Aires"
    ## TODO sacar afuera
    listado <- c(390, 391, 445, 9, 11, 57, 50, 62, 52, 53, 97, 74, 101, 122,
                 145, 146, 147, 179, 299, 199, 309, 191, 192, 197, 200, 205,
                 260, 354, 355, 273, 306, 350, 351, 375, 392, 393, 420, 419,
                 433, 439, 440, 452, 446, 449, 450, 451, 453, 458, 461, 520,
                 521, 60, 187, 202, 196)
    LosdeGBABA <- (mapaAMBA$gid %in% listado)
    GBABA <- mapaAMBA[LosdeGBABA, ]
    GBABA2 <- left_join(GBABA, salida, by = "Departamento")
    auxsemana <- (LAsemana - 1) * 7 + 1
    semanaN <- as.Date(auxsemana, origin = "2020-01-26")
   
    mp <- ggplot(data = GBABA2) +
        geom_sf_interactive(aes(fill = PorcPos,
                                tooltip = paste(Departamento, "\n",
                                                TestTot, "test </b>  \n",
                                                TestPos, "positivos\n", PorcPos,
                                                "Positividad (%)"))) + 
        ggtitle(paste("semana", semanaN)) +
        labs(fill = "Positividad (%)", scale_fill_viridis_c(alpha = 0.8))
    mp <- girafe(ggobj = mp)
    mp
}
