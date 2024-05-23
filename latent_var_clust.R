##############################################
###                                        ###
###      Latent Variables Clustering       ###
###                                        ###
##############################################

###      PCA Identification for Latent Variables       ###

# IMPORTEM LES DADES
data <- read.csv('llibres_imputat.csv')

# AGAFEM LES VARIABLES NUMÈRIQUES
data_num <- data[, -c(1,3,4,5, 7, 12:64)]

data_num <- scale(data_num)

# Creem el PCA
pca <- prcomp(data_num)

# Visualitzem la inèrcia acumulada
barplot(100*cumsum(pca$sdev[1:dim(data_num)[2]]^2)/dim(data_num)[2])
percInerAccum<-100*cumsum(pca$sdev[1:dim(data_num)[2]]^2)/dim(data_num)[2]
percInerAccum

pca$sdev
innerProj<- pca$sdev^2 
innerProj
totalInner<- sum(innerProj)
totalInner
pinerEix<- 100*innerProj/totalInner
pinerEix
barplot(pinerEix)

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

nd = 3

print(pca)
attributes(pca)
pca$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
View(pca$x)
dim(pca$x)
dim(data_num)
data_num[2000,]
pca$x[2000,]

Psi = pca$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(data_num)
etiq = c("rating", "pages", "numRatings", "likedPercent", "bbeVotes", "price")
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# PLOT OF INDIVIDUALS

biplot(pca, xlim = c(-5, 5), ylim = c(-5, 5), scale = 0, col = c(5,1))
biplot(pca, choices = c(2,3), xlim = c(-5, 5), ylim = c(-5, 5), scale = 0, col = c(5,1))
biplot(pca, choices = c(1,3), xlim = c(-5, 5), ylim = c(-5, 5), scale = 0, col = c(5,1))




###      Clustering with Latent Variables       ###

#Agafem les variables latents originals sense escalar
#dcon<- data[, c(2, 6, 10, 11)] #"rating", "pages", "bbeVotes", "price"
#dcon<- data[, c(6, 8, 9, 11)]  #"pages", "numRatings", "likedPercent", "price"
dcon<- data[, c(2, 6, 8, 9, 10, 11)]

# HIERARCHICAL CLUSTERING

d  <- dist(dcon[1:50,])
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
plot(h1)

d  <- dist(dcon)
h1 <- hclust(d,method="ward")  # NOTICE THE COST
plot(h1)

# BUT WE ONLY NEED WHERE THERE ARE THE LEAPS OF THE HEIGHT

## Apliquem el mètode del colze per a trobar el número de clústers ideal, o com a mínim un raonable
Wss <- numeric(10)
for (i in 1:10) {
  kmeans_result <- kmeans(dcon, i)
  Wss[i] <- sum(kmeans_result$withinss)
}

plot(1:10, Wss[1:10], type = "b", xlab = "Nombre de clústers (k)", ylab = "Distàncies intraclasses", main = "Mètode del colze")

# WHERE ARE THER THE LEAPS? WHERE WILL YOU CUT THE DENDREOGRAM?, HOW MANY CLASSES WILL YOU OBTAIN?

nc = 4

c4 <- cutree(h1,nc)

c4[1:20]

nc = 3

c3 <- cutree(h1,nc)

c3[1:20]


table(c4)
table(c3)
table(c4,c3)

# PLOT OF THE INDIVIDUALS
library(ggplot2)
c3 <- as.factor(c3)

# Añadimos 'c3' a 'dcon'
dcon$c3 <- c3

# Creamos una lista con los nombres de las variables
#variables <- c("rating", "pages", "bbeVotes", "price")
#variables <- c("pages", "numRatings", "likedPercent", "price")
variables <- c("rating", "pages", "numRatings", "likedPercent", "bbeVotes", "price")

# Generamos todas las combinaciones de pares de variables
combinaciones <- combn(variables, 2, simplify = FALSE)

# Para cada par de variables, creamos un gráfico
for (par in combinaciones) {
  p <- ggplot(dcon, aes_string(x = par[1], y = par[2], color = "c3")) +
    geom_point(alpha = 0.6, size = 2) +
    labs(color = "Cluster") +
    theme_minimal() +
    ggtitle(paste("Gráfico de", par[1], "vs", par[2]))
  
  print(p)
}
