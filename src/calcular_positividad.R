test_df <- function(datos, group) {
    #' Calcula el porcentaje de positividad.
    datos %>%
        group_by_at(.vars = c(distrito_a_seccion(group), "semana")) %>%
        summarise(
            TestPos =
                sum(Clasificacion == "Confirmado"),
            TestTot =
                sum(Clasificacion == "Descartado") +
                sum(Clasificacion == "Confirmado"),
            PorcPos =
                round(100 * sum(Clasificacion == "Confirmado") /
                      (sum(Clasificacion == "Descartado") +
                       sum(Clasificacion == "Confirmado")))
        ) 
}

test_por_semana <- function(la_semana, positividad_df, group) {
    #' Calcula la positividad para la_semana

    if (group == "Provincia") {
        group_df <- data.frame(PROVINCIAS)
        colnames(group_df) <- group
    }
    if (group == "Departamento") {
        group_df <- data.frame(AMBA)
        colnames(group_df) <- group
    }

    ## Completo missing con 0
    ret <- positividad_df %>%
        filter(semana == la_semana) %>%
        right_join(data.frame(group_df), by = group) %>%
        replace_na(list(
            semana = la_semana,
            TestPos = 0,
            TestTot = 0,
            PorcPos = 0
        ))
    
    return(ret)
}
