df <- read.csv('~/GitHub/Dataset-PMAAD/llibres_imputat.csv')

df$X <- NULL
df_num <- df[sapply(df, is.numeric)]

pca <- prcomp(df_num[complete.cases(df_num),], scale = T)
pc <- pca$x[,1:2]
plot(pc, pch = 20)

df <- df[pc[,1] > -150,]

#write.csv(df, file = , row.names = F)

# La segona component es distribueix de manera similar entre els individus, amb
# una petita correlació negativa amb pa primera component
# En canvi la primera component té una distribució exponencial, si analitzem
# quines variables ho provoquen podem trobar algun outlier en els punts extrems

pca$rotation[,1][abs(pca$rotation[,1]) > max(abs(pca$rotation[,1]))*0.9]

# Les valoracions, que són exponencials, aporten la maxima variança

plot(df$numRatings, df$n5stars, pch=20, col='blue',
     xlab='numRatings', ylab='nXstars', main='Rating relation')
points(df$numRatings, df$n4stars, pch=20, col='red')
points(df$numRatings, df$n3stars, pch=20, col='green')
points(df$numRatings, df$n2stars, pch=20, col='purple')
points(df$numRatings, df$n1stars, pch=20, col='orange')
legend('topleft',
       legend=c('1 star', '2 stars', '3 stars', '4 stars', '5 stars'),
       col=c('orange', 'purple', 'green', 'red', 'blue'), pch=10)

# # # HIERERCHICAL CLUSTERING # # #

attach(df)

dcon <- data.frame(rating, pages, numRatings, likedPercent, bbeScore, bbeVotes, price, n5stars, n4stars, n3stars, n2stars, n1stars)
dim(dcon)

Wss <- numeric(10)
for (i in 1:10) {
  kmeans_result <- kmeans(dcon, i)
  Wss[i] <- sum(kmeans_result$withinss)
}
plot(1:10, Wss[1:10], type = "b", xlab = "Nombre de clústers (k)", ylab = "Distàncies intraclasses", main = "Mètode del colze")
# 3 clusters????

set.seed(42)
sampl <- dcon[sample(nrow(dcon), 100), ]
d <- dist(sampl)
d1 <- dist(dcon)
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
h2 <- hclust(d1,method="ward.D")  # NOTICE THE COST
plot(h1, hang = -1)
plot(h2, hang = -1)
# tres clusters.

nc = 3
c3 <- cutree(h2,nc)
table(c3)

cdg <- aggregate(as.data.frame(dcon),list(c3),mean)
head(cdg)

plot(rating,pages,col=c3,main="Clustering de preu-rating a tres classes")
legend("topleft",c("class1","class2","class3"),pch=1,col=c(1:3))

pairs(dcon[,1:12], col=c3)

# # # PROFILING # # #

