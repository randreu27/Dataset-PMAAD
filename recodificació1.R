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

# genres vs ratings
genres <- books$genres
ratings <- books$rating

genres <- unlist(genres)

lengths_genres <- lengths(genres)

proporcion <- sum(lengths_genres) / length(ratings)

proporcion_redondeada <- round(proporcion)

ratings_rep <- rep(ratings, proporcion_redondeada)

ratings_rep <- ratings_rep[1:sum(lengths_genres)]


data <- data.frame(Genre = genres, Rating = ratings_rep)

library(dplyr)
ratings_avg <- data %>%
  group_by(Genre) %>%
  summarize(Average_Rating = mean(Rating))

ratings_avg <- ratings_avg[order(ratings_avg$Average_Rating, decreasing = TRUE), ]

library(ggplot2)

ggplot(ratings_avg, aes(x = reorder(Genre, Average_Rating), y = Average_Rating)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 2, position = position_dodge(width = 2)) +
  geom_text(aes(label = round(Average_Rating, 2)), vjust = -0.5, size = 3.5, position = position_dodge(width = 0.7)) +
  labs(x = "Género", y = "Calificación Promedio", title = "Calificación Promedio por Género") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x para mejor visualización
