## Mapa AMBA
mapa_AMBA <- st_read("data/departamento.shp", stringsAsFactors = FALSE)

## Mapa Argentina
mapa_ARG <- st_read('data/provincia.json')
colnames(mapa_ARG) <- c("gid", "entidad", "objeto", "fna", "gna", "Provincia",
                        "in1", "fdc", "sag", "geometry")

## Datos distritos
GBA <- readLines("data/gba.dat")
AMBA <- readLines("data/amba.dat")
