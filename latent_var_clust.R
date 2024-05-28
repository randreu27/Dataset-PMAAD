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
llibres <- data[, -c(1, 3, 7)]

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

# SELECTION OF THE SIGIFICANT DIMENSIONS (keep 80% of total inertia)

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
h1 <- hclust(d,method="ward.D")  # NOTICE THE COST
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

library(ggplot2)


freq_c4 <- data.frame(
  R = c("Bestseller", "Destacats", "Adaptacions", "Nínxol"),
  Freq = c(161, 784, 1966, 7356)
)

freq_c4$R <- factor(freq_c4$R, levels = freq_c4$R)

# Crea el countplot
ggplot(freq_c4, aes(x = R, y = Freq)) +
  geom_bar(fill = "skyblue", color = "black", stat = "identity") +
  labs(title = "Classes vs freqüència", x = "Classe", y = "Freqüència") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


table(c3)
table(c4,c3)
data$c4 <- c4

llibres_clase <- data[, c('title', 'c4')]

# PLOT OF THE INDIVIDUALS
library(ggplot2)
c4 <- as.factor(c4)

# Añadimos 'c3' a 'dcon'
dcon$c4 <- c4
data$c4 <- c4

# Creamos una lista con los nombres de las variables
#variables <- c("rating", "pages", "bbeVotes", "price")
#variables <- c("pages", "numRatings", "likedPercent", "price")
variables <- c("rating", "pages", "numRatings", "likedPercent", "bbeVotes", "price")

# Generamos todas las combinaciones de pares de variables
combinaciones <- combn(variables, 2, simplify = FALSE)

# Para cada par de variables, creamos un gráfico
for (par in combinaciones) {
  p <- ggplot(dcon, aes_string(x = par[1], y = par[2], color = "c4")) +
    geom_point(alpha = 0.6, size = 2) +
    labs(color = "Cluster") +
    theme_minimal() +
    ggtitle(paste("Gráfico de", par[1], "vs", par[2]))
  
  print(p)
}


# # # PROFILING # # #

#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}




ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}


#source("file")
#dades contain the dataset
dades<-dcon
#dades<-dd[filtro,]
#dades<-df
K<-dim(dades)[2]
par(ask=TRUE)


#P must contain the class variable
#P<-dd[,3]
P<-c4
nous_nivells <- c("Bestseller", "Destacats", "Adaptacions", "Nínxol")

# Canvia els nivells del vector P
P <- factor(P, levels = 1:4, labels = nous_nivells)
#P<-dd[,18]
nameP<-"classe"
#P<-df[,33]

nc<-length(levels(factor(P)))
nc
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(dades)))
nameP<-"Class"
n<-dim(dades)[1]

for(k in 1:K){
  if (is.numeric(dades[,k])){ 
    print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
    
    boxplot(dades[,k]~P, main=paste("Boxplot of", names(dades)[k], "vs", nameP ), horizontal=TRUE)
    
    barplot(tapply(dades[[k]], P, mean),main=paste("Means of", names(dades)[k], "by", nameP ))
    abline(h=mean(dades[[k]]))
    legend(0,mean(dades[[k]]),"global mean",bty="n")
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {print(summary(dades[P==s,k]))}
    o<-oneway.test(dades[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(dades[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(dades[,k], P)
    print("p-values ValorsTest: ")
    print(pvalk[,k])      
  }else{
    if(class(dd[,k])=="Date"){
      print(summary(dd[,k]))
      print(sd(dd[,k]))
      #decide breaks: weeks, months, quarters...
      hist(dd[,k],breaks="weeks")
    }else{
      #qualitatives
      print(paste("Variable", names(dades)[k]))
      table<-table(P,dades[,k])
      #   print("Cross-table")
      #   print(table)
      rowperc<-prop.table(table,1)
      
      colperc<-prop.table(table,2)
      #  print("Distribucions condicionades a files")
      # print(rowperc)
      
      #ojo porque si la variable es true o false la identifica amb el tipus Logical i
      #aquest no te levels, por tanto, coertion preventiva
      
      dades[,k]<-as.factor(dades[,k])
      
      
      marg <- table(as.factor(P))/n
      print(append("Categories=",levels(as.factor(dades[,k]))))
      
      #from next plots, select one of them according to your practical case
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
      
      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(colperc[,c],col=paleta[c]) }
      legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
      
      #condicionades a classes
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
      
      #with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]))
      paleta<-rainbow(length(levels(dades[,k])))
      for(c in 1:length(levels(dades[,k]))){lines(rowperc[,c],col=paleta[c]) }
      legend("topright", levels(dades[,k]), col=paleta, lty=2, cex=0.6)
      
      #amb variable en eix d'abcisses
      marg <-table(dades[,k])/n
      print(append("Categories=",levels(dades[,k])))
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      #x<-plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), xaxt="n")
      #text(x=x+.25, y=-1, adj=1, levels(CountryName), xpd=TRUE, srt=25, cex=0.7)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
      
      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      #condicionades a columna 
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
      
      #with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(dades)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      table<-table(dades[,k],P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)
      
      #diagrames de barres apilades                                         
      
      paleta<-rainbow(length(levels(dades[,k])))
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
      
      barplot(table(dades[,k], as.factor(P)), beside=FALSE,col=paleta )
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
      #diagrames de barres adosades
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta )
      
      barplot(table(dades[,k], as.factor(P)), beside=TRUE,col=paleta)
      legend("topright",levels(as.factor(dades[,k])),pch=1,cex=0.5, col=paleta)
      
      print("Test Chi quadrat: ")
      print(chisq.test(dades[,k], as.factor(P)))
      
      print("valorsTest:")
      print( ValorTestXquali(P,dades[,k]))
      #calcular els pvalues de les quali
    }
  }
}#endfor


#descriptors de les classes més significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}




library(dplyr)
library(tidyr)

# Seleccionar les columnes de gènere i la columna de classe
genre_columns <- names(llibres)[14:64]
class_column <- "c4"

# Agregar les dades per classe i gènere
genre_freq <- llibres %>%
  select(starts_with("Genre"), all_of(class_column)) %>%
  group_by_at(vars(all_of(class_column))) %>%
  summarise(across(starts_with("Genre"), sum)) %>%
  pivot_longer(cols = starts_with("Genre"), names_to = "Genre", values_to = "Freq")


# Primer, calculem la proporció de cada gènere a cada nivell de 'c4'
genre_proportions <- genre_freq %>%
  group_by(c4) %>%
  mutate(Total = sum(Freq)) %>%
  ungroup() %>%
  mutate(Proportion = Freq / Total) %>%
  filter(Freq >= 10) # Filtrar per evitar outliers

# Calculem la proporció mitjana de cada gènere entre tots els nivells de 'c4'
average_genre_proportions <- genre_proportions %>%
  group_by(Genre) %>%
  summarise(AverageProportion = mean(Proportion)) %>%
  ungroup()

# Calculem la proporció de sobrerepresentació per cada gènere a cada nivell
sobrerepresentacio <- genre_proportions %>%
  left_join(average_genre_proportions, by = "Genre") %>%
  mutate(Sobrerepresentacio = Proportion / AverageProportion)

# Ara, calculem la mitjana entre la proporció normal i la sobrerepresentació
sobrerepresentacio <- sobrerepresentacio %>%
  mutate(AverageScore = (Proportion + Sobrerepresentacio) / 5)

# Seleccionem els 5 gèneres més rellevants per cada classe, basats en l'AverageScore
top_genres_per_class <- sobrerepresentacio %>%
  group_by(c4) %>%
  top_n(5, AverageScore) %>%
  ungroup() %>%
  arrange(c4, desc(AverageScore))

# Creem un gràfic per cada classe
# Continuem creant un gràfic per cada classe
plots <- list()
for(c4_level in unique(top_genres_per_class$c4)) {
  plots[[as.character(c4_level)]] <- ggplot(top_genres_per_class %>% filter(c4 == c4_level), aes(x = reorder(Genre, AverageScore), y = AverageScore, fill = Genre)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Top 5 Gèneres a", nous_nivells[strtoi(c4_level)]),
         x = "Genre",
         y = "Average Score") +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip() # Gira el gràfic per a millorar la legibilitat
}

# Imprimim els gràfics
for(c4_level in names(plots)) {
  print(plots[[c4_level]])
}



