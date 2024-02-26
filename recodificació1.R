# Carregar DF
library(readr)
books <- read.csv("https://raw.githubusercontent.com/randreu27/Dataset-PMAAD/main/llibres_with_setting.csv")

# Interpretació NA
books$isbn[books$isbn == '9999999999999'] <- NA
books$genres[books$genres == '[]'] <- NA
books$characters[books$characters == '[]'] <- NA
# Awards buit hem considerat que és que no té premis, no que no se sàpiguen
books$ratingsByStars[books$ratingsByStars == '[]'] <- NA
books$setting[books$setting == '[]'] <- NA

# Neteja caràcters
books$genres <- gsub("\\[|\\]|'", '', books$genres)
books$characters <- gsub("^\\[('|\")|('|\")\\]$", '', books$characters)
books$awards <- gsub("^\\[(\"|')?|('|\")?\\]$", "", books$awards)
books$ratingsByStars <- gsub("\\[|\\]|'", '', books$ratingsByStars)
books$setting <- gsub("\\[|\\]|'", '', books$setting)
books$price <- gsub('\\.(.*\\.)', '\\1', books$price)

# Separació
books$genres <- strsplit(books$genres, ", ")
books$characters <- strsplit(books$characters, "('|\"), ('|\")")
books$awards <- strsplit(books$awards, "('|\"), ('|\")")
books$ratingsByStars <- strsplit(books$ratingsByStars, ", ")
books$setting <- strsplit(books$setting, ", ")

# Numèric
 # i noves variables
books[c('n5stars', 'n4stars', 'n3stars', 'n2stars', 'n1stars')] <-
  as.numeric(do.call(rbind, books$ratingsByStars))
 # al preprocessing hauriem de comprovar que la suma doni el num de ratings tot
books$price <- as.numeric(books$price)

# Neteja variables
books$X <- NULL
books$ratingsByStars <- NULL

# Bivariate Analysis #
# pages vs ratings
pages <- books$pages
ratings <- books$rating

library(ggplot2)

ggplot(books, aes(x = pages, y = ratings)) +
  geom_point() +
  labs(x = 'Pàgines', y = 'Valoració', title = 'Pàgines VS Valoracions')

# genres vs setting
genres <- books$genres
setting <- books$setting

library(vcd)

variable1 <- unlist(genres)
variable2 <- unlist(setting)

min_length <- min(length(variable1), length(variable2))
var1 <- variable1[1:min_length]
var2 <- variable2[1:min_length]

# Creamos una tabla de contingencia
contingency_table <- table(var1, var2)

# Creamos el gráfico de mosaico
mosaic(contingency_table, main = "Gèneres vs Localització")
