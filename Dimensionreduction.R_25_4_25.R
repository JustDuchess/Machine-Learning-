getwd()
setwd("C:\\Users\\HP\\Desktop\\DA TERM 2")

getwd()

library(dplyr)
library(mlbench)
library(caret)

data1 <- read.csv("C:/Users/HP/Desktop/DA TERM 2/diabetes_25_4_25 - Copy.csv")
str(data1)

set.seed(5)
cormatrix <- cor(data1[,1:8])
cormatrix
highlyCormatrix <- findCorrelation(cormatrix, cutoff=0.5)
print(highlyCormatrix)

ncol(data1)
ncol(cormatrix)

#another selection of feature selection
data1 <- read.csv("C:/Users/HP/Desktop/DA TERM 2/diabetes_25_4_25 - Copy.csv")
set.seed(5)
cnt <- rfeControl(functions=rfFuncs, method="cv", number=10)
output <- rfe(data1[,1:8], data1[,9], sizes=c(1:8), rfeControl=cnt)
print(output)

predictors(output)

plot(output, type=c("g", "o"))

print(output)

pcaCharts <- function(P) {
  P.var <- P$sdev ^ 2
  P.pvar <- P.var/sum(P.var)
  print("Variance proportion:")
  print(P.pvar)
  par(mfrow=c(2,2))
  plot(P.pvar,xlab="Principal_component", ylab="Variance proportion", ylim=c(0,1.5), type= 'o')
  plot(cumsum(P.pvar),xlab="Principal component", ylab="Cumulative_variance", ylim=c(0,1.5), type='o')
  screeplot(P)
  screeplot(P,type="l")
  par(mfrow=c(1,1))
}



#work from isioma

pcaCharts <- function(P) {
  P.var <- P$sdev ^ 2
  P.pvar <- P.var/sum(P.var)
  print("Variance proportion:")
  print(P.pvar)
  par(mfrow=c(2,2))
  plot(P.pvar,xlab="Principal_component", ylab="Variance proportion", ylim=c(0,1.5), type= 'o')
  plot(cumsum(P.pvar),xlab="Principal component", ylab="Cumulative_variance", ylim=c(0,1.5), type='o')
  screeplot(P)
  screeplot(P,type="l")
  par(mfrow=c(1,1))
}

head(USArrests)

US.PCA <- prcomp(USArrests,center = TRUE,scale. = TRUE)
names(US.PCA)

print(US.PCA)

summary(US.PCA) ## PC with up to 95% variance in the model is acceptable
par(mar=c(1,1,1,1))
pcaCharts(US.PCA)
??pcaCharts

biplot(US.PCA, scale = .5, cex = .8)

data(USArrests)
arrests.pca <- prcomp(USArrests, scale. = TRUE)
arrests.pca

pca.out <- arrests.pca
pca.out$rotation <- -pca.out$rotation
pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out,scale = .5, cex= .8)

biplot(US.PCA.out,scale=.5, cex =.8)
pca.out$rotation[, 1:2]



install.packages("devtools")
install.packages("ggbiplot")
install_github("Vqv/ggbiplot")
library(devtools)
library(ggbiplot)
library(githubr)

biplot_g<-ggbiplot(pca.out, obs.scale = 2, var.scale = 2,
                   labels = row.names(USArrests),
                   ellipse = TRUE, circle = TRUE)
biplot_g<-biplot_g + scale_color_discrete(name = '')
biplot_g<-biplot_g + theme(legend.direction = 'vertical',
                           legend.position = 'Bottom')
print(biplot_g)


## first step, acquire the standard deviation from US.PCA
US.PCA$sdev
## NEXT, OBTAIN THE VARIANCE FROM us.pca
US.PCA$sdev^1
## find the components with a variance above 1
which(US.PCA$sdev^3>1)

## you can also use the screeplot to select components with a variance above.
screeplot(US.PCA, type = "line")
abline(h = 1, col = "blue", lty = 9)


install.packages("RnavGraphImageData")
install.packages("vegan")
library(RnavGraphImageData)
library(vegan)
data(digits)
data_number<- digits 
data_number
sample.data_number = matrix(data_number[,8000],ncol = 16, byrow=FALSE)
image(t(sample.data_number)[,nrow(sample.data_number):.5])  

set.seed(3)
data_number.idx = sample(1:ncol(data_number),size = 300)
data_number.select = data_number[,data_number.idx] 

data_number.Transpose = t(data_number.select)
data_number.dist = vegdist(data_number.Transpose, method="euclidean") 

data_number.isomap = isomap(data_number.dist,k = 6, ndim=9, fragmentedOK = TRUE)
plot(data_number.isomap) 
data_number.st = spantree(data_number.dist)
data_number.plot = plot(data_number.isomap, main="Isometric mapping k = 6")



#install.packages("https://cran.r-project.org/src/contrib/Archive/snowfall/snowfall_1.84-6.3.tar.gz",
#repos = NULL, type = "source")

#install.packages("https://cran.r-project.org/src/contrib/Archive/snowfall/snowfall_1.84-6.3.tar.gz",
#repos = NULL, type = "source")

#UPDATE YOUR R TO VERSION 4.5.0
install.packages(c('scatterplot3d', 'snowfall'))

install.packages("https://cran.r-project.org/src/contrib/Archive/lle/lle_1.1.tar.gz",
                 repos = NULL, type = "source")



library(lle)

results = lle( data1, m=12, k=40)
str(results)

plot( results$Y, main="Embedded_data", xlab=expression(y[1]), 
      ylab=expression(y[2]), col="green" )


plot_lle( results$Y, X, FALSE, col="red", inter=TRUE )

data("lle_scurve_data")



#mine
head(USArrests)

US.PCA <- prcomp(USArrests, center = TRUE,scale. = TRUE)
names(US.PCA)

print(US.PCA)

summary (US.PCA)

pcaCharts(US.PCA)

biplot(US.PCA,scale=.5, cex=.8)

ncol(pcaCharts)
ncol(pcaCharts)


head(USArrests)

US.PCA <- prcomp(USArrests, center = TRUE,scale. = TRUE)
names(US.PCA)

print(US.PCA)

summary (US.PCA)

pcaCharts(US.PCA)
biplot(US.PCA, scale = .5, cex = .8)

data(USArrests)
arrests.pca <- prcomp(USArrests, scale. = TRUE)


pca.out <- arrests.pca
pca.out$rotation <- pca.out$rotation
pca.out$x <- -pca.out$x
biplot(US.PCA.out,scales=.5, cex=.8)

pca.out$rotation[, 1:2]

install.packages("devtools")
library("devtools")
library(remotes)
install_github("vgv/ggbiplot")
library(ggbiplot)
library(githubr)
biplots_g <- ggbiplot(pca.out, obs.scale = 2, var.scale =2,
                      labels = row.names(USArrests),
                      ellipse = TRUE, circle = TRUE)
biplot_g<-biplot_g + scale_color_discrete(name = '')

biplot_g<-biplot_g + theme(legend.direction = 'vertical',
                              legend.position = 'Bottom')

print(biplot_g)

US.PCA$sdev

US.PCA$sdev ^ 1

which(US.PCA$sdev ^ 3> 1)

screeplot(US.PCA, type="line")
abline(h=1, col="blue", lty= 9)

install.packages("RnavGraphImageData")
install.packages("vegan")
library(RnavGraphImageData)
library(vegan)
data(digits)

data_number <- digits
data_number
sample.data_number = matrix(data_number[, 8000], ncol = 16, byrow = FALSE)
image(t(sample.data_number)[,nrow(sample.data_number):.5])

set.seed(3)
data_number.idx = sample(1:ncol (data_number), size = 300)
data_number.select = data_number[,data_number.idx]  

data_number.Transpose = t(data_number.select)
data_number.dist = vegdist(data_number.Transpose, methods ="euclidean")

lines(data_number.st, data_number.plot, col ="green")




install.packages(lle)
library(lle)
results = lle (x=x, m=12, k=40)
str(results)

plot(results$Y, main="Embedded_data", xlab=expression(y[1]),
     ylap = expression(y[2]), col(="green")
plots_lle(results, x, FALSE, col="red", inter = TRUE)
     