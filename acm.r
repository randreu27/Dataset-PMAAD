##############################################
###                                        ###
###                 ACM                    ###
###                                        ###
##############################################

##PACKAGES REQUIRED  ###
library(FactoMineR)
library(Matrix)
library(factoextra)
library("corrplot")
?MCA

### Exploring our dataset for the MCA example ###
data <- read.csv('llibres_imputat.csv')

data <- data[, -c(1, 3, 7)]

#MCA ANALYSIS by using LOGICAL TABLE

res.mca0<-MCA(data, quanti.sup=c(1, 4:11, 13:30), graph=FALSE) ### this MCA contains 18 active variables, an additional variable (19) as numerical and the rest of
### categorical features are supplementary (features from columns 20 to 36)

##if you do not put graph=FALSE, all results and graphs are shown.### This option is good when you want quick results from MCA
###Check we have 18 active categorical features for the MCA and a total of 45 modalities (K), so the max number of MC dimensions is 27 (k-p)

names(res.mca0)
print(res.mca0)
summary(res.mca0)

###EIGENVALUES
res.mca0$eig
head(res.mca0$eig)
length(res.mca0$eig[,1])
#barplot(res.mca0$eig[,1],main="EIGENVALUES",names.arg=1:nrow(res.mca0$eig))
#round(res.mca0$eig,2)
fviz_screeplot(res.mca0, addlabels = TRUE, ylim = c(0, 45))
###
totalInertia<- sum(res.mca0$eig[,1]) ### Equal to (k-p)/p = (45-18)/18
pinerEix<- 100*res.mca0$eig[,1]/totalInertia ### Equal to res.mca0$eig[,2]
(cumsum(pinerEix))

#### Think about the total number of important to dimension to be kept in this example
#### Total number of categorical features is 18, so keep dimensions for all eigenvalues greater than 1/p = 1/18 = 0.0555
#### This analysis suggests o keep the first 11 (or 12) components. However, the analysis will be performed with Dim1 and Dim2

# Contributions
# CATEGORICAL VARIABLES - MODALITIES
res.mca0$var$contrib
(res.mca0$var$contrib[,1])
sum(res.mca0$var$contrib[,1]) ###Look that sum of total contributions for all variables in each Dim i is 100%, here we were checking with Dim 1.
# Individuals
res.mca0$ind$contrib
sum(res.mca0$ind$contrib[,1])

### GRAPHICAL ANALYSIS - BIPLOTS
### BIPLOT
fviz_mca_biplot(res.mca0,repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
#The plot above shows a global pattern within the data. Rows (individuals) are represented
# by blue points and columns (variable categories) by red triangles
# The distance between any row points or column points gives a measure of their similarity
# (or dissimilarity). Row points with similar profile are closed on
# the factor map. The same holds true for column points.Supplementary variables are shown in green color.

##plot individus
plot(res.mca0,invisible=c("var","quali.sup"),cex=0.7)
#Coordinate of the individulas on the factor map
res.mca0$ind$coord
## or 
ind <- get_mca_ind(res.mca0)
ind

#plot Variables
plot(res.mca0,invisible=c("ind","quali.sup"), cex=0.5)
### Step by step analysis for variables
var <- get_mca_var(res.mca0)
var

#The components of the get_mca_var() can be used in the plot of rows as follow:
#var$coord: coordinates of variables to create a scatter plot
#var$cos2: represents the quality of the representation for variables on the factor map.
#var$contrib: contains the contributions (in percentage) of the variables to the definition of the dimensions.

# coordinates of the variables
res.mca0$var$coord
##or var$coord
fviz_mca_var(res.mca0, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
#The plot above helps to identify variables that are the most correlated
# with each dimension. The squared correlations between variables and the dimensions
# are used as coordinates.


#OR
#FactorsMatrix<-as.data.frame(res.mca0$var$coord)
#plot(FactorsMatrix[,1],FactorsMatrix[,2])
# Check that is more user-friendly the options offered by factoextra and factominer

#Coordinates of variable categories
head(round(var$coord, 2), 4)
fviz_mca_var(res.mca0, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_mca_var(res.mca0, col.var="black", shape.var = 15,
             repel = TRUE)
#Variable categories with a similar profile are grouped together.
#Negatively correlated variable categories are positioned on opposite sides of the plot
#origin (opposed quadrants).
#The distance between category points and the origin measures the quality
#of the variable category on the factor map. Category points that are away from
# the origin are well represented on the factor map.

#Contribution of variable categories to the dimensions
head(round(var$contrib,2), 4)
# Total contribution to dimension 1 and 2
fviz_contrib(res.mca0, choice = "var", axes = 1:2, top = 15)
fviz_mca_var(res.mca0, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

##With Individuals you can perform the same previous analysis
ind
fviz_mca_ind(res.mca0, 
             label = "none", # hide individual labels
             habillage = "tearoom", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca0, 
             label = "none", # hide individual labels
             habillage = "where", # color by groups 
             palette = c("#00AFBB", "#E7B800","#00AF00"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_ellipses(res.mca0, c("tearoom", "where"),
              geom = "point")

###### Dimension description#######
######he function dimdesc() [in FactoMineR] can be used to identify the most
####### correlated variables with a given dimension:
res.desc <- dimdesc(res.mca0, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]


########
######## BY USING BURT TABLE#######################

res.mca<-MCA(tea, quanti.sup=19, quali.sup=c(20:36), method="Burt")
plot(res.mca,invisible=c("ind","quali.sup"), cex=0.5)
plot(res.mca0,invisible=c("ind","quali.sup"), cex=0.5)

###Check that results are the same for BURT and logical table. There are differences about eigenvalues and inertia because Burt table has a different dimension
### relation to dimensions in the logical table
head((res.mca$ind$coord)==(res.mca0$ind$coord))
head((res.mca$var$contrib)==(res.mca0$var$contrib))
head((res.mca$ind$contrib)==(res.mca0$ind$contrib))

#plot individuals and modalities
plot(res.mca,invisible="quali.sup")
plot(res.mca,invisible="quali.sup", cex=0.5)

#Active Modalities and Additional Modalities
plot(res.mca,invisible="ind", cex=0.4)

#Only additional modalities
plot(res.mca,invisible=c("ind","var"), cex=0.45)


#vaps

res.mca$eig
round(res.mca$eig,2)

barplot(res.mca$eig[,1],main="EIGENVALUES",names.arg=1:nrow(res.mca$eig))

totalIner<- sum(res.mca$eig[,1])
pinerEix<- 100*res.mca$eig[,1]/totalIner

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
## With 11th dimensions by using BURT TABLE we choose to kept 11 dimensions, similar as we did with the Logical Table (remind that criteria in Logical Table
## is about 1/p
barplot(cumsum(pinerEix))
cumsum(pinerEix)
# veps Coordinates
res.mca$ind$coord

#Modalities coordinates
c<-res.mca$var$coord


#contributions
res.mca$ind$contrib
res.mca$var$contrib

#contributions to each dimension i and test-values 
dimdesc(res.mca)

#arrodonint a 4 decimals
a<-lapply(dimdesc(res.mca),lapply,round,4)
a

b<-lapply(dimdesc(res.mca),lapply,signif,3)
b
a[[1]]$quali
b[[1]]$quali
a[1]
a[2]

a[[1]]$quali
b[[1]]$quali


#
plotellipses(res.mca,keepvar=c("relaxing","resto","SPC","where"), cex=0.4)

plotellipses(res.mca,keepvar=c("quali"))
plotellipses(res.mca,keepvar=c("quali.sup"))

###################FINAL ACTIVITY##############################
# Once you know the script to perform MCA Analysis:
## 1)Analyze tea dataset by using all plots
#   2) Extract some conclusions about variables (labels of how interpret dim1 and dim2)
#   3) Perform individuals analysis by getting possible groups (interpret those groups)
#   4) Perform Factor Analysis for Mixed Data (FAMD) by checking (tea dataset is mixed, you have categorical features and one numerical feature):
#  http://www.sthda.com/english/articles/21-courses/72-factor-analysis-of-mixed-data-using-factominer/ (check quick script and course video of the link)
# For further details check:
http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/
  #   5) Perform MFA (Multiple factor Analysis MFA) by checking:
  ## http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/
  ## IMPORTANT --> Consider 3 groups of variables to perform MFA in tea dataset --> Group 1 (Categorical features) 1:18, Group 2 (Categorical features) 20:38,
  ### and add an additional variable (numerical feature): column 19 (age). Group 1 is associated with tea-drinking behaviour and Group 2 as image of the product
  ### and description.
  ####
  ##### *********IMPORTANT --> Activities 4 and 5 are proposed as a homework and will be discused in theory class with Dante.
  
  ###ADDENDUM --An alternative to tea dataset analysis with only a few of variables and introduction to library "ade4" as an extra option to MCA 
  
  newtea = tea[, c("Tea", "How", "how", "sugar", "where", "always")]
res.mca1<-MCA(newtea, method="Burt", graph=FALSE)
cats = apply(newtea, 2, function(x) nlevels(as.factor(x)))
cats



mca1_vars_df = data.frame(res.mca1$var$coord, Variable = rep(names(cats), cats))
mca1_obs_df = data.frame(res.mca1$ind$coord)

library(ggplot2)
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")


ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")


#package(ade4)
dudi.acm performs the multiple correspondence analysis of a factor table.
acm.burt an utility giving the crossed Burt table of two factors table.
acm.disjonctif an utility giving the complete disjunctive table of a factor table.
boxplot.acm a graphic 


data(ours)
summary(ours)
boxplot(dudi.acm(ours, scan = FALSE))
## Not run: 
data(banque)
banque.acm <- dudi.acm(banque, scann = FALSE, nf = 3)
scatter.dudi(banque.acm)

apply(banque.acm$cr, 2, mean)
banque.acm$eig[1:banque.acm$nf] # the same thing
boxplot.acm(banque.acm)

scatter(banque.acm)

s.value(banque.acm$li, banque.acm$li[,3])

bb <- acm.burt(banque, banque)
bbcoa <- dudi.coa(bb, scann = FALSE)
plot(banque.acm$c1[,1], bbcoa$c1[,1])
# mca and coa of Burt table. Lebart & coll. section 1.4

bd <- acm.disjonctif(banque)
bdcoa <- dudi.coa(bd, scann = FALSE)
plot(banque.acm$li[,1], bdcoa$li[,1]) 
# mca and coa of disjonctive table. Lebart & coll. section 1.4
plot(banque.acm$co[,1], dudi.coa(bd, scann = FALSE)$co[,1]) 

## End(Not run)
