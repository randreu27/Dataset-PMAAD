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

# ELIMINACIÓ DE VARIABLES

books$bookId <- NULL
books$isbn <- NULL
books$publisher <- NULL
books$firstPublishDate <- NULL
books$coverImg <-NULL
books$author <- NULL

# TRANSFORMACIÓ DE VARIABLES
# Series
books$Te_Serie <- ifelse(is.na(books$series), 0, 1)
books$series <- NULL

# Awards
books$Te_Premis <- sapply(books$awards, function(x) if (length(x) > 0) 1 else 0)
books$awards <- NULL
