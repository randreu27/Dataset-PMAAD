# ==============================================================================
# [IDEAI] -  GeoDescriptive Analysis in R
# 
# Author(s):    Dante Conti, Sergi RamÃ­rez y Miquel Umbert, IDEAI  (c)
# Date:         10th March 2023
# Description: 
#             Script que permite realizas una descriptiva de los datos de manera
#.            geospacial
# ==============================================================================
# Cargamos las librerias necesarias
list.of.packages = c("ggplot2", "dplyr", "sf", "tmap", "spData", "RColorBrewer", 
                     "osmdata", "gapminder", "leaflet", "spDataLarge", "mapSpain", 
                     "jsonlite", "tmaptools", "readxl", "tidygeocoder") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# ==============================================================================
# Datos Espaciales: Inicializandose
## Los datos geogrÃ¡ficos pueden venir en base de vecotores localizados en un sistema 
## de coordenadas de referencia. Un sitio puntual puede estar representado en longitud 
## y latitud por un par coordenado  (x, y). AdemÃ¡s, es posible encontrar datos que 
## tambiÃ©n posean informaciÃ³n sobre altitud. Los puntos pueden conectarse formando 
## polÃ­gonos (por ejemplo, los bordes de una regiÃ³n).
plot(world)
plot(world[4:5])

## Podemos seleccionar sÃ³lo una parte del mapa
americano <- world %>%
  filter(region_un=="Americas") %>%
  select("subregion", "lifeExp")

# Graficar
plot(americano)

## El mismo resultado puede obtenerse usando base R (figura omitida).
americano <- world[world$region_un=="Americas",
                   c("subregion", "lifeExp")]
# Graficar
plot(americano)

## CÃ³mo estÃ¡ organizado el dataset. Podemos ver que cada paÃ­s tiene un cÃ³digo iso_a2 
## (dos letras que identifican a cada paÃ­s), un nombre largo o name_long y un 
## continente (podemos chequear toda la informaciÃ³n en el dataset con names(world)). 
## Vemos tambiÃ©n que tenemos una columna geom con objetos MULTIPOLYGON de clase 
## sfc_MULTIPOLYGON, `sfc. Estos geoms constituyen los bordes de los paises.
world[1:5, 1:3]

# ==============================================================================
# Topologia geospacial
## Las relaciones de topologÃ­a espacial hacen referencia a cÃ³mo se encuentran en 
## el espacio distintos elementos geogrÃ¡ficos. Por ejemplo, podemos pensar en la 
## intersecciÃ³n entre dos regiones en el espacio o en la intersecciÃ³n entre una 
## regiÃ³n del espacio y un set de coordenadas. En efecto, muchas veces nuestro 
## interÃ©s estÃ¡ en entender las relaciones topolÃ³gicas entre un set de coordenadas e
## spaciales y un set de polÃ­gonos15. Existen funciones muy Ãºtiles en el paquete 
## sf que nos permiten resolver con facilidad estas cuestiones.

## Dataset XY
x <- seq(0, 20, 0.5) 
y <- x * sin(x) 

plot(x,y)

### Agreguemos polÃ­gonos a este set para explorar relaciones topolÃ³gicas. Para 
### definir polÃ­gonos con la funciÃ³n st_polygon() debemos tener en cuenta que:
### 1. Los polÃ­gonos deben ser cerrados. Si la primera coordenada de un triÃ¡ngulo
### es p1 = (1, 1) debemos proporcionar 4 coordenadas, siendo la Ãºltima p4 = p1 = 
### (1, 1)
### 2. Los puntos dentro de los polÃ­gonos se conectar segÃºn el orden especificado
### en la llamada a st_polygon()
# Creemos un rectÃ¡ngulo
r1 <- st_polygon(list(rbind(c(5, -5),
                            c(5, 10),
                            c(10, 10),
                            c(10, -5),
                            c(5, -5))))
# Lo pasamos a sf
r1 <- st_sfc(r1)

# QuerÃ­amos crear un rectÃ¡ngulo pero...
r2 <- st_polygon(list(rbind(c(15, -5),
                            c(15, 10),
                            c(20, -5),
                            c(20, 10),
                            c(15, -5))))
# Lo pasamos a sf
r2 <- st_sfc(r2)

plot(r1, xlim=c(2.5, 22.5))
plot(r2, add=TRUE, col="gray90")


### Incorporamos nuestro set de coordenadas para mostrar el problema 
### que pretendemos resolver
plot(x, y)
plot(r1, add=TRUE)
plot(r2, add=TRUE, col="gray90")

### Este ejemplo es relativamente sencillo y podemos resolverlo sin las herramientas 
### espaciales proporcionadas por sf. Sin embargo, utilizar funciones espaciales 
### es altamente recomendado para operaciones dentro de sistemas de referencia y 
### un mayor volumen de datos. Utilizaremos este ejemplo para demostrarlo.

#### Incorporamos las coordenadas a una matriz
#### Transformamos esa matriz a MULTIPOINT
coordenadas <- st_multipoint(matrix(c(x,y), ncol = 2))

#### Transformamos a puntos simples, o "POINT"
coordenadas <- st_cast(st_sfc(coordenadas), "POINT")

### Nos interesa saber cuales son los puntos contenidos dentro de cada polÃ­gono. 
### Por ejmplo, para r1 tenemos:
#### sparse=FALSE nos darÃ¡ un vector columna
inter_r1 <- st_intersects(coordenadas, r1, sparse = FALSE)

### El resultado de esta funciÃ³n y del resto que utilizaremos en esta secciÃ³n con 
### sparse=FALSE es un vector lÃ³gico que podemos utilizar para hacer subset de 
### nuestros coordenadas originales. Si hubieramos conservado el default sparse=TRUE 
### sÃ³lo los resultados positivos de la operaciÃ³n se hubieran conservado. 
### Recuerda que trabajar con bases de datos grandes (caracterÃ­sticas de datos espaciales) 
### implica requerimientos importantes de memoria. Por eso, las funciones de sf 
### tienden a optimizar recursos. Podemos visualizar grÃ¡ficamente los puntos de 
### interes con el siguiente llamado:
plot(x, y, pch=19)
plot(r1, add=TRUE)
plot(r2, add=TRUE)
points(x[inter_r1], y[inter_r1], pch=19, col="red")

### Debemos notar que st_intersects() nos da tanto puntos dentro como aquellos 
### sobre el borde. Estrictamente, podrÃ­amos usar st_within() para ver aquellos 
### que estÃ¡n dentro del polÃ­gono en cuestiÃ³n.
dentro_r1 <- st_within(coordenadas, r1, sparse = FALSE)

plot(x,y, pch=19)
plot(r1, add=TRUE)
plot(r2, add=TRUE)
points(x[inter_r1], y[inter_r1], pch=19, col=alpha("red", 0.8))
points(x[dentro_r1], y[dentro_r1], pch=19, col=alpha("blue", 0.5))

### Podemos verificar cuÃ¡l es el punto sobre el borde utilizando la funciÃ³n
toca_r1 <- st_touches(coordenadas, r1, sparse=FALSE)

data.frame(x=x[toca_r1],
           y=y[toca_r1])

# ==============================================================================
# AnÃ lisis Geospacial a travÃ©s de mapas de R
## GrÃ¡ficando con las funciones bÃ¡sicas de R
### Fondo celeste
par(bg = 'lightblue')

### Si queremos hacer 2 grÃ¡ficos
par(mfrow=c(1, 2))

### Grafico de la isla
plot(nz[1], col = "gray80", main = "Nueva Zelanda")
plot(nz[2], main = "Nueva Zelanda", key.pos = 1)
par(mfrow=c(1, 1))

# ------------------------------------------------------------------------------
## GrÃ ficando con el paquete tmap

### Mapa con bordes
bordes <- tm_shape(nz) +
  tm_borders()

### Agregamos fill de un solo color
con_color <- bordes + tm_fill(col="red", alpha=0.3)

### Agregamos fill segun el area de la region
### Cuidado! no usamos aes(col= Land_area) cual ggplot2
area_region <- tm_shape(nz) +
  tm_borders() +
  tm_fill(col = "Land_area")

### mostramos los mapas juntos
tmap_arrange(bordes, con_color, area_region)

# ------------------------------------------------------------------------------
### El ejercicio anterior apunta a modificar a mano los valores de breaks. 
### El paquete tm puede asignar los valores de corte de manera automÃ¡tica segÃºn 
### un estilo o style (ver ?tm_polygons()):

### Los estilos se modifican mediante tm_fill(â€¦, style=â€¦)

### style = pretty (default), redondea a nÃºmeros enteros y los separa equidistantes.
### style = equal divide los valores de entrada en cortes de igual rango. Este 
### tipo de estilo provoca que los mapas tengan poca variadad de color si tenemos 
### una distribuciÃ³n asimÃ©trica en la variable que elegimos para colorear.
### style = quantile divide en cuantiles nuestras observaciones de modo que el 
### mismo nÃºmero de observaciones entra en cada corte.
### style = jenks identifica grupos con valores similares y maximiza las diferencias 
### entre grupos.
### style = cont nos proporciona un numero continuo de colores en un rango cromÃ¡tico. 
### Podemos utilizarlo cuando queremos representar gradientes de terreno 
### (grÃ¡ficos tipo raster).
### style = cat nos sirve para representar variables categÃ³ricas de modo que 
### cada categorÃ­a serÃ¡ coloreada con un color Ãºnico.

### Se pueden cambiar el estilo de manera global
area_region + tm_style("classic")
?tm_style()

titulo_leyenda = expression("Ãrea (km"^2*")")
tm_shape(nz) +
  tm_borders() +
  tm_fill(col = "Land_area", title = titulo_leyenda) +
  tm_compass(type = "4star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 125, 250), size = 1,
               position = c("right", "bottom"))+
  tm_layout(frame.lwd = 5, legend.outside = TRUE,
            legend.outside.position = c("right", "bottom"),
            legend.bg.color = NA, 
            legend.format = list(text.separator = "-"))

# ==============================================================================
# Proyecciones

## Para transformar el mundo de esfera a plano, utilizamos ciertas transformaciones 
## matemÃ¡ticas o proyecciones. A continuaciÃ³n muestro algunas proyecciones conocidas, 
## utilizando datos de GDP per cÃ¡pita.
## https://github.com/mtennekes/tmap/tree/master/demo/WorldFacets

## La base para construir distintas proyecciones es cambiar el argumento projection 
## en el comienzo del llamado.

## longitud-latitud
### tm_shape(World, projection = "longlat") + ...
## robinson
### tm_shape(World, projection = "robin") + ...
## eck4
### tm_shape(World, projection = "eck4") + ...

## Podemos investigar otras transformaciones con ggplot2. Para hacerlo, podemos 
## utilizar la funciÃ³n st_transform() presente en sf y la siguiente transformaciÃ³n:

### Transformar el objeto sf World
world2 <- sf::st_transform(
  world,
  "+proj=laea +y_0=0 +lon_0=40 +lat_0=10 +ellps=WGS84 +no_defs"
)

# Graficar el nuevo objeto
ggplot() + geom_sf(data = world2) +
  theme_bw()

# ==============================================================================
# Mapas con el paquete facets
## Podemos hacer un grÃ¡fico de facets con tm_facets(). Sin embargo, tm_facets() 
## serÃ¡ explorada en la secciÃ³n 7.9. Luego, en esta secciÃ³n me voy a centrar en 
## ggplot2 para mostrar algo de variedad en la forma de crear y presentar mapas 
## con datos provenientes de distintas fuentes. Utilizaremos los datos provenientes 
## del paquete gapminder. Aprovecho para mencionar que la base de datos de gapminder 
## no tiene el cÃ³digo iso. Debemos obtenerlos del objeto country_codes.

# obtenemos iso
# Juntamos con gapminder 
gapminder <- gapminder %>% left_join(country_codes) %>%
  rename(name_long = country)


# Unir los datos con los bordes y filtrar algunos aÃ±os
# forzamos el join por iso_a3
# Por ejmplo, en un dataset EE.UU., aparece como "United States"
# y en el otro como "United States of America".
data_del_mundo <- world %>% left_join(gapminder, by="name_long") %>%
  filter(year %in% seq(1977, 2007, 10))

# Graficar
ggplot() +
  geom_sf(data=data_del_mundo, aes(fill=pop.y))+
  facet_wrap(~year)+
  theme_bw()

## Este mapa tiene algo extraÃ±o. Ocurre que el dataset gapminder no contiene 
## informaciÃ³n para todos los paÃ­ses (Puedes corroborarlo corriendo 
## filter(gapminder, name=="Russia")). Un recurso es graficar los bordes de 
## dichos paÃ­ses sin fill. Para ello, utilizamos dos llamados a geom_sf().
ggplot() +
  geom_sf(data=world)+
  geom_sf(data=data_del_mundo, aes(fill=pop.y))+
  facet_wrap(~year)+
  theme_bw()+
  # agregamos viridis para mejor contraste
  viridis::scale_fill_viridis() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# ==============================================================================
# MAPAS ANIMADOS
## Una mejor forma de graficar el mismo set de datos es animarlo. En el siguiente 
## cÃ³digo usamos un llamado a tm_shape() para que los paÃ­ses faltantes sean 
## graficados desde World, mientras que nuestros datos de poblaciÃ³n sean 
## graficados desde data_del_mundo via qtm (un abreviado de tm_shape()+ tm_fill()).
## El orden de los llamados determina quÃ© capa va arriba, del mismo modo que ocurre 
## en ggplot2. Vamos a utilizar tm_facets(), que nos brinda la opciÃ³n de producir 
## mÃºltiples grÃ¡ficos segÃºn una columa de nuestro dataset de entrada. 
## En particular tm_facets() puede ser llamada con el argumento by=... si 
## queremos viÃ±etas o along=...si queremos un objeto animado.

# Creamos la animaciÃ³n como objeto
world_anim <- tm_shape(world) +
  tm_borders()+
  qtm(data_del_mundo, "pop.y", fill.palette = "viridis") +
  tm_facets(along = "year", drop.units = TRUE)+
  tm_legend(show = FALSE)

tmap_animation(world_anim, filename = "world_anim.gif", delay = 50)

# ==============================================================================
# MAPAS INTERACTIVOS
## En el mundo de hoy dÃ­a, muchos dispositivos tienen acceso a informaciÃ³n en 
## archivos de tipo .html (cualquier dispositivo que tenga un navegador de internet). 
## Esto hace que distintas aplicaciones se beneficien de insertar contenido .html 
## en productos multimedia (ver CapÃ­tulo 8). En esta secciÃ³n, exploramos cÃ³mo 
## crear mapas interactivos, que permiten al usuario navegar por el mapa que 
## hemos creado e interactuar con la informaciÃ³n que presentamos.

## El paquete tmap permite hacer grÃ¡ficos interactivos con mucha facilidad. De 
## hecho, no es necesario cambiar el cÃ³digo con el que creamos el mapa. 
## SÃ³lo debemos cambiar el modo de representaciÃ³n (de estÃ¡tico a dinÃ¡mico, 
## usando tmap_mode("view")).
tmap_mode("view")
## tmap_arrange(nz_quantile, nz_jenks, sync = TRUE)

# Usamos tmap_mode("plot") para establecer a los datos de nuevos
tmap_mode("plot")

# ------------------------------------------------------------------------------
# LEAFLET
## leaflet utiliza el sistema de pipes (%>%) para construir los grÃ¡ficos a partir 
## de la llamada la funciÃ³n leaflet(). A continuaciÃ³n se reproduce el ejemplo 
## bÃ¡sico, puede ser encontrado en la pÃ¡gina del paquete.
m <- leaflet() %>%
  # Agregamos el mapa OpenStreetMap default
  addTiles() %>%  
  addMarkers(lng=174.768, lat=-36.852,
             popup="Lugar de nacimiento de R")
# Incorporar el mapa
m 

# ==============================================================================
## MANEJO DE DATOS PARA ASIGNACION DE UTM usados en librerÃ­as de R
### Convertir un punto con lat y long hacia el sistema UTM
lat <- 38.90
lon <- (-120.71)

### Punto ubicado en California, USA.
cord.dec <- sp::SpatialPoints(cbind(lon, lat), proj4string = CRS("+proj=longlat"))
cord.dec2 <- coordinates(spTransform(cord.dec, CRS("+init=epsg:32610")))

###epsg igual a 32610 es el cÃ³digo asociado que incluye el condado de el Dorado, CA-USA
### http://www.gisandbeers.com/equivalencia-codigos-epsg-sistemas-referencia/#:~:text=El%20EPSG%204326%20corresponde%20al,obsoleto%20en%20la%20Pen%C3%ADnsula%20Ib%C3%A9rica.

# ------------------------------------------------------------------------------
# create data with coordinates given by longitude and latitude
d <- data.frame(long = rnorm(100, 0, 1), lat = rnorm(100, 0, 1))
coordinates(d) <- c("long", "lat")

# assign CRS WGS84 longitude/latitude
proj4string(d) <- CRS("+proj=longlat +ellps=WGS84
                      +datum=WGS84 +no_defs")

# reproject data from longitude/latitude to UTM zone 35 south
d_new <- spTransform(d, CRS("+proj=utm +zone=35 +ellps=WGS84
                      +datum=WGS84 +units=m +no_defs +south"))
d_new$UTMx <- coordinates(d_new)[, 1]
d_new$UTMy <- coordinates(d_new)[, 2]

# ------------------------------------------------------------------------------
states <- data.frame(state.x77, state.center)
head(states)
states <- states[states$x > -121,]
coordinates(states) <- c("x", "y")
proj4string(states) <- CRS("+proj=longlat +ellps=clrk66")
summary(states)
state.ll83 <- spTransform(states, CRS("+proj=longlat +ellps=GRS80"))
summary(state.ll83)
state.merc <- spTransform(states, CRS=CRS("+proj=merc +ellps=GRS80"))
state.merc <- spTransform(states, CRS=CRS("+proj=merc +ellps=GRS80 +units=us-mi"))
summary(state.merc)

# ==============================================================================
# BÃºsqueda de Coordenadas
## UtilitzaciÃ³ de la bases de dades
head(available_features())

## instalaciones y establecimientos pÃºblicos
head(available_tags("amenity"))

## tiendas
available_tags("shop")

# ------------------------------------------------------------------------------
## MERCADONA:
### construcciÃ³n de la consulta
#rectÃ¡ngulo de selecciÃ³n para la PenÃ­nsula IbÃ©rica
m <- c(-10, 30, 5, 46)

#construcciÃ³n de la consulta
q <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("name", "Mercadona") %>%
  add_osm_feature("shop", "supermarket")

str(q) #estructura de la consulta

#consulta 
mercadona <- osmdata_sf(q)

#mapa final del resultado
ggplot(mercadona$osm_points)+
  geom_sf(colour = "#08519c",
          fill = "#08306b",
          alpha = .5,
          size = 1,
          shape = 21)+
  theme_void()

# ==============================================================================
## Carreguem les dades
url <- "https://analisi.transparenciacatalunya.cat/resource/f7hf-52sc.json?$limit=10000"
datos_json <- fromJSON(url)

# Vamos a crear una nueva variable que sea la media de personas por mesa electoral
datos_json$cociente <- as.numeric(datos_json$cens_del_local)/as.numeric(datos_json$meses_del_local)

# Fem un filtre nomÃ©s a barcelona
taulesBarcelona <- datos_json[which(datos_json$nombre00 == "BARCELONA"), ]

## Vamos a extraer las coordenadas (auque ya las tenemos), a partir de la funciÃ³n de tidygeocoder
# create a dataframe with addresses
some_addresses <- tibble::as_tibble(taulesBarcelona[, c("nombre_local", "direcci_n_ocurrencia_1")])

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(direcci_n_ocurrencia_1, method = 'osm', lat = latitude , long = longitude)

## Amb la funciÃ³ reverse_geocode Ã©s pot buscar la direcciÃ³ a partir de la geolocalitzaciÃ³

## Podem pintar aquestes escoles en un mapa de Barcelona
leaflet() %>% 
  addTiles() %>% 
  addMarkers(
    data = lat_longs,
    popup = paste0("CENTRO: ", sprintf(
      "<strong>%s</strong>",
      lat_longs$nombre_local
    ), "\nDIRECCION: ", sprintf(
      "<strong>%s</strong>",
      lat_longs$direcci_n_ocurrencia_1
    )) %>%
      lapply(htmltools::HTML)
  )

# ==============================================================================
# Bibliografia: 
## https://r.geocompx.org/adv-map
## https://bookdown.org/matiasandina/R-intro/estadistica-espacial.html
## https://dominicroye.github.io/es/2018/acceso-a-la-base-de-datos-de-openstreetmaps-desde-r/
## https://dominicroye.github.io/en/2019/visualize-urban-growth/
## https://bookdown.org/robinlovelace/geocompr/adv-map.html
## https://r.geocompx.org/adv-map.html
## https://ropenspain.github.io/mapSpain/articles/x02_mapasesp.html#un-ejemplo-rÃ¡pido
## https://geanders.github.io/navy_public_health/3-1-geographical-data-in-a-tidy-format.html#geographical-data-in-a-tidy-format
## https://geanders.github.io/navy_public_health/3-2-basic-mapping.html#basic-mapping
# ==============================================================================