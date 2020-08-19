
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

testsemanprov <- function(LAsemana, TESTPS, datos) {
    ## Calcula la positividad para LAsemana en todas las provincias
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

testsemanaAMBA <- function(LAsemana, TESTDS, datosAMBA) {
    
    TESTDSELEC <- filter(TESTDS, semana == LAsemana)
    todoelec <- TESTDSELEC

    ## TODO: refactorar con testsemanprov
    
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
