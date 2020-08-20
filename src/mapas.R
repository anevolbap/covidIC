## Mapa AMBA
mapa_AMBA <- st_read("data/departamento.shp", stringsAsFactors = FALSE)

## Mapa Argentina
mapa_ARG <- st_read('data/provincia.json')
colnames(mapa_ARG) <- c("gid", "entidad", "objeto", "fna", "gna", "Provincia",
                        "in1", "fdc", "sag", "geometry")

## Datos distritos
GBA <- readLines("data/gba.dat")
AMBA <- readLines("data/amba.dat")
PROVINCIAS <- readLines("data/provincias.dat")

LISTADO <- c(390, 391, 445, 9, 11, 57, 50, 62, 52, 53, 97, 74, 101, 122,
             145, 146, 147, 179, 299, 199, 309, 191, 192, 197, 200, 205,
             260, 354, 355, 273, 306, 350, 351, 375, 392, 393, 420, 419,
             433, 439, 440, 452, 446, 449, 450, 451, 453, 458, 461, 520,
             521, 60, 187, 202, 196)
