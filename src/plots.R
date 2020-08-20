library(tidyverse)
library(ggthemes)
library(plotly)
library(sf)
library(sp)             # FIXME: se usa?
library(RColorBrewer)   # FIXME: se usa?

source("src/calcular_positividad.R")

plot_curva_positividad <- function(datos) {
    #' Grafica la positividad por semana para cada distrito
    #'
    #' @param datos
    #' @return

    group = colnames(datos)[1]
    p <- ggplot(datos,
                aes(
                    x = semana,
                    y = PorcPos,
                    group = get(group),
                    color = get(group),
                    text = paste(get(group),
                                 "\n semana:", semana,
                                 "\n test:", TestTot,
                                 "\n positivos:", TestPos,
                                 "\n Positividad:", PorcPos, "%"))) +
        geom_line(size = 1) +
        xlab("Fecha") +
        ylab("Positividad (%)") +
        scale_x_date(date_labels = "%b %d") +
        theme(legend.title = element_blank())
    p = ggplotly(p, tooltip = 'text') %>% add_markers()
    p
}

plot_positividad_por_semana <- function(semana, datos, distrito) {
      
    if (distrito == "AMBA"){
        ret = grafico_semana_AMBA(semana, datos, mapa_AMBA)
    }
    if (distrito == "PAIS"){
        ret = grafico_semana_PAIS(semana, datos, mapa_ARG)
    }
    ret 
}

grafico_semana_PAIS <- function(semana, datos, mapa) {
    #' Funcion que grafica en un mapa positividad para una semana dada
    #'
    #' @param semana la semana a graficar
    #' @param datos dataframe con los datos
    #' @param mapa_arg datos para el mapa
    #'

    group = "Provincia"
    mapa1 <- left_join(mapa, test_por_semana(semana, datos, group), by = group)

    ##
    mp <- ggplot(data = mapa1) +
        geom_sf_interactive(aes(fill = PorcPos,
                                tooltip = paste(Provincia, "\n",
                                                TestTot, "test \n",
                                                TestPos, "positivos\n",
                                                PorcPos, "Positividad (%)"))) +
        coord_sf(xlim = c(-80, -45), ylim = c(-55, -20), clip = "on") +
        labs(fill = "(%)", scale_fill_viridis_c(alpha = 0.8)) +
        theme_map()
    mp <- girafe(ggobj = mp)
    mp
}

grafico_semana_AMBA <- function(semana, datos, mapa) {
    
    mapa$Departamento <- mapa$nam
    mapa$Departamento[mapa$gna == "Comuna"] <- "Ciudad Autónoma de Buenos Aires"
    LosdeGBABA <- (mapa$gid %in% LISTADO)
    GBABA <- mapa[LosdeGBABA, ]

    group = "Departamento"
    GBABA2 <- left_join(GBABA, test_por_semana(semana, datos, group), by = group)
   
    mp <- ggplot(data = GBABA2) +
        geom_sf_interactive(aes(fill = PorcPos,
                                tooltip = paste(Departamento, "\n",
                                                TestTot, "test \n",
                                                TestPos, "positivos\n", PorcPos,
                                                "Positividad (%)"))) + 
        labs(fill = "(%)", scale_fill_viridis_c(alpha = 0.8)) +
        theme_map()
    mp <- girafe(ggobj = mp)
    mp
}
