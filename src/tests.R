
test_df <- function(datos, group){
    ## Devuelve summaries sobre tests.
    salida <- datos %>% 
        group_by(!!!rlang::syms(group), SemanaNum, SemanaLab) %>%   # FIXME: revisar esta bizarreada
        summarise(TestPos =
                      sum(Clasificacion == "Confirmado"),
                  TestTot =
                      sum(Clasificacion == "Descartado") +
                      sum(Clasificacion == "Confirmado"),
                  PorcPos =
                      round(100 * sum(Clasificacion == "Confirmado") /
                                  (sum(Clasificacion == "Descartado") +
                                   sum(Clasificacion == "Confirmado")))) %>%
        rename(semana = SemanaNum)         
    return(salida)
}

test_por_semana <- function(LAsemana, test_df, group) {
    
    ## Calcula la positividad para LAsemana
    ret = test_df %>%
        filter(semana == LAsemana) %>%
        left_join(data.frame(group = PROVINCIAS),
                  by = group)

    print(head(ret))
    ## esto lo hago por si hay regiones que no fue medido test para que ponga 0 y no NA
    ##          if (length(unique(datos$Provincia)) != length(unique(test_df_semana$Provincia))) {
    ##              num <- length(unique(datos$Provincia))
    ##              df_NA <- data.frame(unique(datos$Provincia))
    ##              aux <- matrix(rep(0, numprov * 3), numprov, 3)
    ##              df_NA <- cbind(df_NA, rep(LAsemana, numprov), aux)
    ##              colnames(df_NA) <- c(group, "semana", "TestTot", "TestPos", "PorcPos")
    ##              sin_test <- df_NA %>% filter(!get(group) %in% unique(test_df_semana$Provincia))
    ##              todoelec <- bind_rows(test_df_semana, sin_test)
    ##          }

    ##          if (length(unique(datosAMBA$Departamento)) != length(unique(TESTDSELEC$Departamento))) {
    ##              num <- length(AMBA)
    ##              df_NA <- data.frame(AMBA)
    ##              aux <- matrix(rep(0, numdep * 3), num, 3)
    ##              df_NA <- cbind(df_NA, rep(LAsemana, num), aux)
    ##              colnames(df_NA) <- c(group, "semana", "TestTot", "TestPos", "PorcPos")
    ##              sin_test <- df_NA %>% filter(!Departamento %in% unique(TESTDSELEC$Departamento))
    ##              todoelec <- bind_rows(TESTDSELEC, sin_test)
    ##          }
    
    return(ret)
}
