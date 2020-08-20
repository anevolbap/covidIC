test_df <- function(datos, group) {
    #' Devuelve summaries sobre tests.

    datos %>%
        ##  FIXME: revisar esta bizarreada
        group_by(!!!rlang::syms(group), semana, SemanaLab) %>%
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
        ) ##          %>%
        ##          rename(semana = SemanaNum)
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
