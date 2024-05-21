##############################################
###                                        ###
###      Latent Variables Clustering       ###
###                                        ###
##############################################

# IMPORTEM LES DADES
data <- read.csv('llibres_imputat.csv')

# AGAFEM LES VARIABLES NUMÈRIQUES
data_num <-data[, -c(1,3,4,5, 7, 15)]

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
