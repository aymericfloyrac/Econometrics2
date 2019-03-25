require(gdata)
library(lattice)
library(stargazer)
setwd('/Users/aymeric/Documents/ENSAE/2A/Semestre 2/econo2')
data <- read.xls('data.xls')
head(data)

######## CORRELATION DES NOTES ##########
notes <- data[,c('im24','if39','nif24','nim40')]
cm<-cor(notes)
levelplot(cm,
          col.regions = heat.colors(100)[length(heat.colors(100)):1],
          main="matrice de corrélation")
cm
stargazer(cm)

######## ANALYSE DES NOTEURS ##########
means<-apply(notes,2,mean)
sds<-apply(notes,2,sd)
meansd <- rbind(means,sds)

stargazer(meansd)

##création des z-scores
zmaker<-function(col){
  return (col-mean(col))/sd(col)
}

