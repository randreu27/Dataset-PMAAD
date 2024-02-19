### Script D2 Llibres ###

# Importació de les Dades #

lldt <- read_csv("https://raw.githubusercontent.com/randreu27/Dataset-PMAAD/main/llibres_with_setting.csv")

View(lldt)

# Anàlisis de la variable "series" #

serie <- lldt$series
head(serie)

NoSer <- table(is.na(serie))
NoSer

barplot(NoSer, 
        main = "Tots els llibres pertanyen a una sèrie?",
        xlab = "Te Sèrie (Sí) o No (No)",
        ylab = "Nombre de Llibres",
        col = "skyblue",
        ylim = c(0, 30000),
        names.arg = c("Sí", "No"),
        beside = TRUE)

# Anàlisis de la variable "rating" #

ratings <- lldt$rating

hist(ratings, freq = FALSE, col = "lightblue", main = "Histograma de Notes", ylim = c(0,1.5))
curve(dnorm(x, mean = mean(ratings), sd = sd(ratings)), col = "darkred", lwd = 2, add = TRUE)

# Anàlisis de la variable "languages" #

lang <- lldt$language
head(lang)

lang_freq <- table(lang)
sorted_lang_freq <- sort(lang_freq, decreasing = TRUE)
num_lang <- 10
top_lang_freq <- sorted_lang_freq[1:min(length(sorted_lang_freq), num_lang)]

barplot(top_lang_freq, 
        col = "lightblue", 
        main = "Languages",
        xlab = "Languages", 
        ylab = "Frequency")

# Anàlisis de la variable "bookFormat" #

format <- lldt$bookFormat
head(format)

# Calcular las frecuencias de los formatos
format_freq <- table(format)


# Calcular las frecuencias de los formatos
format_freq <- table(format)

# Crear el gráfico de barras
barplot(format_freq, 
        col = c("lightblue", "lightgreen", "lightyellow", "lightcoral"), 
        main = "Distribución de Formatos",
        xlab = "Formato", 
        ylab = "Frecuencia",
        ylim = c(0, 30000))

# Agregar etiquetas al gráfico
text(x = barplot(format_freq), y = format_freq, labels = format_freq, pos = 3, col = "black", ylim = c(0, 30000))

