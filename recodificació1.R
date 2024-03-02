# Carregar DF
library(readr)
library(ggplot2)
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

# ELIMINACIÓ DE VARIABLES

books$bookId <- NULL
books$isbn <- NULL
books$publisher <- NULL
books$firstPublishDate <- NULL
books$coverImg <-NULL
books$author <- NULL
books$characters <- NULL
books$edition <- NULL

# TRANSFORMACIÓ DE VARIABLES
# Series
books$Te_Serie <- ifelse(is.na(books$series), 0, 1)
books$series <- NULL

# Awards
num_awards = c()
for (i in books$awards){
  num = 0
  for (elem in i){
    num = num + 1
  }
  num_awards <- c(num_awards, num)
}
books$numAwards <- num_awards
books$awards <- NULL

# Genres
genre_counts <- table(unlist(books$genres))

popular_genres <- names(sort(genre_counts, decreasing = TRUE))[1:15]


assigned_genres <- sapply(books$genres, function(genres) {
  common_genre <- intersect(popular_genres, genres)
  if (length(common_genre) > 0) {
    return(common_genre[1])
  } else {
    return(NA)
  }
})
books$popular_genre <- assigned_genres
books$genres <- NULL

# plot
assigned_genres_df <- data.frame(Genre = assigned_genres)
genre_counts <- table(assigned_genres_df$Genre)
ggplot(data = assigned_genres_df, aes(x = Genre)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribución de géneros asignados",
       x = "Género",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#saveRDS(books, file = "cleaned_Dataset.rds")

# # # PREPOCESSING # # #
# # Eliminació de NA no imputables # #
books <- books[complete.cases(books$description), ]
books <- books[complete.cases(books$language), ]
books <- books[complete.cases(books$bookFormat), ]
books <- books[complete.cases(books$publishDate), ]
books <- books[complete.cases(books$popular_genre), ]

# # Estudi de Imputabilitat de les variables restants # #
#install.packages("naniar")
library(naniar)

# General #
mcar_test(books[, c("rating", "pages", "numRatings", "likedPercent", "bbeScore", "bbeVotes", "price", "n5stars", "n4stars", "n3stars", "n2stars", "n1stars")])

# Variable Pages # 
mcar_test(books[, c("rating", "pages", "numRatings", "bbeScore", "bbeVotes")])

# Variable likedPercent # 
mcar_test(books[, c("rating", "numRatings", "likedPercent", "bbeScore", "bbeVotes")])

# Variable price # 
mcar_test(books[, c("rating", "numRatings", "bbeScore", "bbeVotes", "price")])

# Variables nstars # 
mcar_test(books[, c("rating", "numRatings", "bbeScore", "bbeVotes", "n5stars", "n4stars", "n3stars", "n2stars", "n1stars")])