#CARGA DATOS

df <- read.csv('llibres_imputat.csv')

df$X <- NULL
df_num <- df[sapply(df, is.numeric)]

pca <- prcomp(df_num[complete.cases(df_num),], scale = T)
pc <- pca$x[,1:2]

df <- df[pc[,1] > -150,]

# Guardar la base de datos editada
write.csv(df, file = 'data_preprocesada.csv', row.names = FALSE)

#Transformación variable temporal 'fecha en la que se publicarón los libros'
rutaDatos <- "data_preprocesada.csv"
data <- read.csv(rutaDatos)



#TIME-SERIES CLUSTERING
#---Hacemos varias transformaciones a nuestra variable publishDate----

#----------EXTRAEMOS LOS MESES ------------
# Eliminamos los sufijos de ordinalidad 
data$publishDate <- gsub("\\b\\d+(st|nd|rd|th)\\b", "", data$publishDate)

#eliminamos el dia de las fechas en formato "August 1st 2018"
meses <- sub(".*\\b(\\w+)\\s+\\d+.*", "\\1", data$publishDate)

# Función para convertir los nombres de los meses a números
month_name_to_number <- function(month_name) {
  month_names <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")
  month_name <- tolower(month_name)
  month_number <- match(month_name, tolower(month_names))
  if (!is.na(month_number)) {
    return(month_number)
  } else {
    return(month_name)
  }
}

# Aplicamos la función a las fechas que contienen los meses escritos
english_dates <- meses
english_dates[grepl("\\b(January|February|March|April|May|June|July|August|September|October|November|December)\\b", english_dates)] <- 
  sapply(english_dates[grepl("\\b(January|February|March|April|May|June|July|August|September|October|November|December)\\b", english_dates)], 
         function(x) {
           month_name_to_number(x)
         })

print(english_dates)

# Eliminamos las filas que contienen solo el año YYYY o la palabra "Published"
english_dates_elim <- english_dates[!(english_dates %in% c("Published") | grepl("^\\d{4}$", english_dates))]

print(english_dates_elim)

# Extraemos el mes de las fechas en el formato "MM/DD/YY"
fechas_modificadas <- gsub("^(\\d{2}).*", "\\1", english_dates_elim[grepl("^\\d{2}/\\d{2}/\\d{2}$", english_dates_elim)])
fechas_nuevas <- english_dates_elim
fechas_nuevas[grepl("^\\d{2}/\\d{2}/\\d{2}$", fechas_nuevas)] <- fechas_modificadas

print(fechas_nuevas)

# Eliminamos el 0 de los meses en formato numérico
meses_def <- gsub("^0(\\d{1})", "\\1", fechas_nuevas)

print(meses_def)

#----EXTRAEMOS LOS AÑOS--------------
# Eliminamos las filas con solo el año "YYYY"
data <- data[!grepl("^\\d{4}$", data$publishDate), ]
# Eliminamos las filas con la palabra "Published"
año <- data[!grepl("\\bPublished\\b", data$publishDate), "publishDate"]

print(año)

# Eliminamos los meses escritos manteniendo el año
año_sin_meses_ingles <- gsub("\\b(January|February|March|April|May|June|July|August|September|October|November|December)\\b", "", año)

print(año_sin_meses_ingles)

# Función para acabar extraer el año de las fechas en el formato "MM/DD/YY"
modificar_años <- function(fechas) {
  # Extraemos los últimos dos dígitos de la fecha
  ultimos_dos_digitos <- substr(fechas, nchar(fechas) - 1, nchar(fechas))
  # Convertir a número
  ultimos_dos_digitos <- as.numeric(ultimos_dos_digitos)
  # Asignamos el siglo según los últimos dos dígitos
  siglo <- ifelse(ultimos_dos_digitos <= 24, "20", "19")
  # Crear los años completos
  años_completos <- paste0(siglo, sprintf("%02d", ultimos_dos_digitos))
  return(años_completos)
}

# Aplicamos la función
años_def <- modificar_años(año_sin_meses_ingles)

print(años_def)

#--------copiamos pegamos las fechas modificadas al data----------------

# Filtramos las filas que no tienen solo un año YYYY y que no contienen la palabra "Published"
data <- subset(data, !(grepl("^\\d{4}$", publishDate) | grepl("Published", publishDate)))

#View(data)

#Verificamos que todas las variables tienen el mismo tamaño
# Convertir meses_def en una columna de fechas
meses_def_columna <- as.data.frame(matrix(unlist(meses_def), ncol = 1, byrow = TRUE))
names(meses_def_columna) <- "fecha"

# Convertir años_def en una columna de fechas
años_def_columna <- as.data.frame(matrix(unlist(años_def), ncol = 1, byrow = TRUE))
meses_def_columna <- as.data.frame(matrix(unlist(meses_def), ncol = 1, byrow = TRUE))
names(meses_def_columna) <- "fecha"

# Convertir años_def en una columna de fechas
años_def_columna <- as.data.frame(matrix(unlist(años_def), ncol = 1, byrow = TRUE))
names(años_def_columna) <- "fecha"


#View(años_def_columna)

# Número de filas de la variable publishDate
filas_publishDate <- nrow(data)

# Número de filas de la variable meses_de
filas_meses_def <- nrow(meses_def_columna)

# Número de filas de la variable años_def
filas_años_def <- nrow(años_def_columna)

# Mostramos los resultados
print(paste("Número de filas de publishDate:", filas_publishDate))
print(paste("Número de filas de meses_def:", filas_meses_def))
print(paste("Número de filas de años_def:", filas_años_def))
#Es correcto, hemos verificado que tienen 9541 filas.

# Calcular las fechas en el formato YYYY-MM
fechas <- paste(años_def, meses_def, sep="-")

# Creamos un nuevo dataframe con la columna publishDate actualizada
nuevo_data <- data.frame(publishDate = fechas)

# Reemplazamos la columna publishDate en tu base de datos 'data' con las nuevas fechas
data$publishDate <- nuevo_data$publishDate

#View(data)


#------miramos cuantos libros hay en cada estrella-----------

# Redondear los valores de la columna "rating" hacia abajo (eliminando los decimales)
data$rating <- floor(data$rating)

# O bien, redondear los valores de la columna "rating" hacia arriba
# data$rating <- ceiling(data$rating)

# Verificamos que ahora la columna "rating" no contenga números decimales
unique(data$rating)

# Obtener la cantidad de libros para cada categoría de rating
cantidad_por_rating <- table(data$rating)

print("Cantidad de libros para cada categoría de rating:")
print(cantidad_por_rating)

# Asi obtenemos el numero de estrellas predominante de cada libro, con el valor más alto entre las variables de estrellas
data$stars <- max.col(data[, c("n1stars", "n2stars", "n3stars", "n4stars", "n5stars")], ties.method = "first")

print(data$stars)
#View(data)

# Contamos cuántos libros tienen cada cantidad de estrellas
num_books_by_stars <- table(data$stars)

print(num_books_by_stars)

#-------creamos la tabla fechas antiguas------------
# Instalamos el paquete reshape2 si no está instalado
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}

library(reshape2)

# Creamos la tabla pivotante, para comparar las fechas de publicación con los precios y con las estrellas que lo valoran
tabla_temporal <- dcast(data, publishDate ~ stars, value.var = "price", fun.aggregate = length)

print(tabla_temporal)


#----tabla fechas mas recientes------------

library(dplyr)
library(reshape2)

# Filtramos los datos para incluir solo libros publicados desde 2018
data_filtrada <- filter(data, substr(publishDate, 1, 4) >= "2018")

# Creamos la tabla pivotante, para comparar las fechas de publicación con los precios y con las estrellas que lo valoran
tabla_temporal <- dcast(data_filtrada, publishDate ~ stars, value.var = "price", fun.aggregate = length)

print(tabla_temporal)

#podemos ver como los precios han subido significativamente con el pasar de los años


#---analizamos el precio con la fecha de publicación, para ver los cambios que ha habido pero visto desde una grafica

install.packages("dtwclust")
library(dtw)
library(tidyverse)
library(dtwclust)

# Agrupamos por fecha y calcular la media de los precios
mean_prices <- aggregate(price ~ publishDate, data, mean)
#View(mean_prices)

# Añadir el día 01 a todas las fechas en la columna publishDate
data$publishDate <- paste(data$publishDate, "01", sep = "-")

# Convertir la columna publishDate al formato correcto de fecha
data$publishDate <- as.Date(data$publishDate, format = "%Y-%m-%d")

# Agrupar por fecha y calcular la media de los precios
mean_prices <- aggregate(price ~ publishDate, data, mean)

# Ordenar los datos por fecha de publicación
mean_prices <- mean_prices[order(mean_prices$publishDate), ]

# Convertir mean_prices en una serie de tiempo (time series)
library(xts)
time_series <- xts(mean_prices$price, order.by = mean_prices$publishDate)

# Graficar la serie de tiempo
plot(time_series)


#---tabla com la media de los precios---------------

#PRiMERA TABLA
mean_prices <- aggregate(price ~ publishDate + stars, data, mean)
library(reshape2)

# Creamos la tabla pivotante para comparar las fechas de publicación con las estrellas y la media de precios
tabla_temporal <- dcast(mean_prices, publishDate ~ stars, value.var = "price")
# Reemplazar NAs con 0 en tabla_temporal
tabla_temporal[is.na(tabla_temporal)] <- 0

print(tabla_temporal)

# Creamos la tabla pivotante, para comparar las fechas de publicación con los precios y con las estrellas que lo valoran
tabla_temporal <- dcast(data, publishDate ~ stars, value.var = "price", fun.aggregate = length)


print(tabla_temporal)

#SEGUNDA TABLA

# Filtramos los datos para incluir solo libros publicados desde 2018
data_filtrada <- filter(data, substr(publishDate, 1, 4) >= "2018")

# Creamos la tabla pivotante, para comparar las fechas de publicación con los precios y con las estrellas que lo valoran
#tabla_temporal <- dcast(data_filtrada, publishDate ~ stars, value.var = "price", fun.aggregate = length)

#print(tabla_temporal)

# Calculamos la media de los precios para cada combinación de fecha y estrellas
mean_prices <- aggregate(price ~ publishDate + stars, data_filtrada, mean)
library(reshape2)

# Creamos la tabla pivotante para comparar las fechas de publicación con las estrellas y la media de precios
tabla_filtrada <- dcast(mean_prices, publishDate ~ stars, value.var = "price")
# Reemplazar NAs con 0 en tabla_temporal
tabla_filtrada[is.na(tabla_filtrada)] <- 0

print(tabla_filtrada)

#---Cluster de la serie temporal-----------------
library(dplyr)

# Definir patrones de nombres de columnas que deseas eliminar
patrones <- c("^Genre_", "^n\\d+stars$")

# Filtrar las variables basadas en los patrones definidos
data_filtered <- select(data, -stars, -bbeVotes, -bbeScore, -likedPercent, -numRatings, -pages, -rating, -title, -bookFormat, -description, -language, -matches(patrones), -granEditorial, -esSerie, -teAwards)

# Ver las primeras filas del nuevo dataframe
head(data_filtered)


# Convierte las fechas a cadenas de caracteres y elimina "2001" del principio
#data_filtered$publishDate <- gsub("^2001 -", "", as.character(data_filtered$publishDate))
head(data_filtered)
# Elimina el guion del inicio de las fechas
#data_filtered$publishDate <- gsub("^-", "", as.character(data_filtered$publishDate))
head(data_filtered)
#View(data_filtered)

# Convertir la variable publishDate al formato de fecha adecuado
data_filtered$publishDate <- as.Date(data_filtered$publishDate, format = "%Y-%m-%d")
# Crea una variable `year` a partir de `publishDate`
data_filtered$year <- year(data_filtered$publishDate)
data_filtered <- select(data_filtered, -publishDate)

# Verificar los primeros registros después de la conversión
head(data_filtered)

# Lista de países que queremos mantener
paises_deseados <- c("United States", "United Kingdom", "France", "Italy", "Canada", "Germany", "Japan", "Egypt", "Ireland", "Australia")

# Filtrar la base de datos para incluir solo los países deseados
data_random <- subset(data_filtered, pais %in% paises_deseados)

# Obtener 300 libros de forma aleatoria
#data_random <- sample_n(data_filtered, 2000)
#View(data_random)


# Transformem la bbdd a la taula necessaria
data_random[, "price"] <- as.numeric(data_random[, "price"])
data_random[, "pais"] <- gsub(" ", "_", data_random[, "pais"])
data_random_ <- reshape2::dcast(data_random, pais ~ year, value.var = "price", fun.aggregate = sum, na.rm = TRUE)
rownames(data_random_) <- data_random_[, "pais"]
data_random_[, "pais"] <- NULL

# Mostrar la tabla resultante
print(data_random_)
#View(data_random_)


# Calculamos la distancia DTW entre las series temporales de precios
distMatrix <- proxy::dist(data_random_, method = "DTW")

# Imprimimos la matriz de distancias
#print(distMatrix)


# Realizar el clustering jerárquico
hc <- hclust(distMatrix, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6)
k <- 3
clusters <- cutree(hc, k = k)

data_random_ %>% 
  proxy::dist(method = "DTW") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() -> dend
install.packages("dendextend")

library(dendextend)
# Convertir el dendrograma a un objeto dendrogram
dend <- as.dendrogram(hc)

# Asignar colores a las ramas según los clusters
dend <- color_branches(dend, k = k)

# Graficar el dendrograma con etiquetas ajustadas
plot(dend, main = "Dendrograma", xlab = "Observaciones", ylab = "Altura", cex = 0.6)
#--------------

# ==============================================================================
library(dtwclust)

## Partitional
pc <- tsclust(data_random_[, -1], type = "partitional", k = 3, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)

# ------------------------------------------------------------------------------
## Hierarchical
hc <- tsclust(data_random_, type = "hierarchical", k = 2, 
              distance = "dtw_basic", trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))
plot(hc)

# profiling clases paises
# Filtrar los datos por clase
clase_US <- subset(data_random, pais == "United States")
clase_resto <- subset(data_random, pais != "United States")

# Resumen estadístico por clase
summary_clase_US <- summary(clase_US$price)
summary_clase_resto <- summary(clase_resto$price)

# Visualizar los resúmenes
print("Resumen estadístico para libros de United States:")
print(summary_clase_US)
print("Resumen estadístico para libros del resto de países:")
print(summary_clase_resto)

library(ggplot2)

# Histogramas de precios por clase
ggplot() +
  geom_histogram(data = clase_US, aes(x = price), fill = "lightblue", alpha = 0.6) +
  geom_histogram(data = clase_resto, aes(x = price), fill = "salmon", alpha = 0.6) +
  labs(title = "Distribución de precios por clase de países",
       x = "Precio",
       y = "Frecuencia") +
  facet_wrap(~ pais)






##############cosas

data_filtered_d <- select(data, -pais, -bbeVotes, -bbeScore, -likedPercent, -numRatings, -pages, -rating, -title, -bookFormat, -description, -language, -matches(patrones), -granEditorial, -esSerie, -teAwards)

# Verificar los primeros registros después de la conversión
head(data_filtered_d)

# Lista de categorías de estrellas que queremos mantener
estrellas_deseadas <- c("1", "2", "3", "4", "5")

# Filtrar la base de datos para incluir solo las categorías de estrellas deseadas
data_randomm <- subset(data_filtered_d, stars %in% estrellas_deseadas)

# Obtener 300 libros de forma aleatoria (opcional)
# data_random <- sample_n(data_filtered, 2000)
View(data_randomm)

# Transformar la base de datos a la tabla necesaria
# Convertir la columna de precio a numérica
data_randomm$price <- as.numeric(data_randomm$price)

# Convertir la columna de estrellas a carácter
data_randomm$stars <- as.character(data_randomm$stars)


# Extraer el año de la columna publishDate
data_randomm$year <- format(as.Date(data_randomm$publishDate), "%Y")

# Cargar la librería data.table
library(data.table)

# Moldear los datos
data_randomm_ <- dcast(as.data.table(data_randomm), stars ~ year, value.var = "price", fun.aggregate = sum, na.rm = TRUE)

# Establecer los nombres de fila
rownames(data_randomm_) <- data_randomm_$stars

# Eliminar la columna de estrellas
data_randomm_$stars <- NULL

# Mostrar la tabla resultante
print(data_randomm_)

View(data_randomm_)

# Calculamos la distancia DTW entre las series temporales de precios
distMatrix <- proxy::dist(data_randomm_, method = "DTW")

# Imprimir la matriz de distancias (opcional)
print(distMatrix)

# Realizar el clustering jerárquico
hc <- hclust(distMatrix, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6)
k <- 3
clusters <- cutree(hc, k = k)

data_randomm_ %>% 
  proxy::dist(method = "DTW") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() -> dend
install.packages("dendextend")

library(dendextend)
# Convertir el dendrograma a un objeto dendrogram
dend <- as.dendrogram(hc)

# Asignar colores a las ramas según los clusters
dend <- color_branches(dend, k = k)

# Graficar el dendrograma con etiquetas ajustadas
plot(dend, main = "Dendrograma", xlab = "Observaciones", ylab = "Altura", cex = 0.6)
#--------------

# ==============================================================================
library(dtwclust)

## Partitional
pc <- tsclust(data_randomm_[, -1], type = "partitional", k = 2, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)

# ------------------------------------------------------------------------------
## Hierarchical
hc <- tsclust(data_randomm_, type = "hierarchical", k = 2, 
              distance = "dtw_basic", trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))
plot(hc)

#PROFILING
library(dplyr)

# Resumen estadístico por clase
summary_by_class <- data_randomm %>%
  group_by(stars) %>%
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    n = n()
  )

# Visualizar el resumen
print(summary_by_class)

library(ggplot2)

# Boxplot por clase
ggplot(data_randomm, aes(x = stars, y = price)) +
  geom_boxplot() +
  labs(title = "Boxplot de precios por clase de estrellas",
       x = "Estrellas",
       y = "Precio")

# Histograma por clase
ggplot(data_randomm, aes(x = price)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~ stars) +
  labs(title = "Histograma de precios por clase de estrellas",
       x = "Precio",
       y = "Frecuencia")


####profling 2 clases

# Filtrar los datos por clase
clase_1_3 <- subset(data_randomm, stars %in% c("1", "2", "3"))
clase_4_5 <- subset(data_randomm, stars %in% c("4", "5"))

# Resumen estadístico por clase
summary_clase_1_3 <- summary(clase_1_3$price)
summary_clase_4_5 <- summary(clase_4_5$price)

# Visualizar los resúmenes
print("Resumen estadístico para libros con 1, 2 o 3 estrellas:")
print(summary_clase_1_3)
print("Resumen estadístico para libros con 4 o 5 estrellas:")
print(summary_clase_4_5)

library(ggplot2)

# Histogramas de precios por clase
ggplot() +
  geom_histogram(data = clase_1_3, aes(x = price), fill = "lightblue", alpha = 0.6) +
  geom_histogram(data = clase_4_5, aes(x = price), fill = "salmon", alpha = 0.6) +
  labs(title = "Distribución de precios por clase de estrellas",
       x = "Precio",
       y = "Frecuencia") +
  facet_wrap(~ stars)


##### serie diferenciada por clusters estrellas
install.packages("tidyr")
library(tidyr)

data_randomm$publishDate <- as.Date(data_randomm$publishDate)

# Agrupar por clases y calcular el resumen de ventas por año
library(dplyr)

# Crear una nueva variable para las clases agrupadas
data_randomm <- data_randomm %>%
  mutate(clase = case_when(
    stars %in% c("1", "2", "3") ~ "Clase 1-3",
    stars %in% c("4", "5") ~ "Clase 4-5"
  ))

# Agrupar por clases y calcular el resumen de precios por año
summary_data <- data_randomm %>%
  group_by(clase, year = lubridate::year(publishDate)) %>%
  summarize(total_price = sum(price))

# Visualizar las series temporales diferenciadas por clusters
ggplot(summary_data, aes(x = year, y = total_price, color = clase)) +
  geom_line() +
  labs(title = "Precio a lo largo de los años por clases de estrellas",
       x = "Año",
       y = "Precio",
       color = "Clase") +
  theme_minimal()


#serie diferenciada por clusters con paises, 2 clases

# Asegurarse de que publishDate esté en formato de fecha
View(data_random)
# Crear una nueva variable para las clases agrupadas por país
data_random <- data_random %>%
  mutate(clase_pais = ifelse(pais == "United_States", "United States", "Otros Paises"))

# Agrupar por clases de país y calcular el resumen de precios por año
summary_data <- data_random %>%
  group_by(clase_pais, year = year) %>%
  summarize(total_price = sum(price))

# Visualizar las series temporales diferenciadas por clases de país
ggplot(summary_data, aes(x = year, y = total_price, color = clase_pais)) +
  geom_line() +
  labs(title = "Precio a lo largo de los años por clases de país",
       x = "Año",
       y = "Precio",
       color = "Clase de País") +
  theme_minimal()


#3clases

library(dplyr)
library(ggplot2)


# Crear una nueva variable para las clases agrupadas por país
data_random <- data_random %>%
  mutate(clase_pais = case_when(
    pais == "United_States" ~ "United States",
    pais %in% c("United_Kingdom") ~ "UK",
    TRUE ~ "Otros Paises"
  ))

# Agrupar por clases de país y calcular el resumen de precios por año
summary_data <- data_random %>%
  group_by(clase_pais, year = year) %>%
  summarize(total_price = sum(price))

# Visualizar las series temporales diferenciadas por clases de país
ggplot(summary_data, aes(x = year, y = total_price, color = clase_pais)) +
  geom_line() +
  labs(title = "Precio a lo largo de los años por clases de país",
       x = "Año",
       y = "Precio",
       color = "Clase de País") +
  theme_minimal()
