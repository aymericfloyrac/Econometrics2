require(gdata)
require(Hmisc)
library(lattice)
library(stargazer)
library(ggplot2)
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

for (col in colnames(notes)){
  data[,paste('z',col,sep='_')] <-zmaker(data[,col])
}
##un score qui résume
data$beautyscore<-apply(data[,c('z_nim40','z_im24','z_if39','z_nif24')],MARGIN = 1,mean)

######## STATISTIQUES DESCRIPTIVES ##########
relevantdata <- data[,c()]
describe(data)


######## QUESTION 5 ##########
#1
question5<-function(df){
  reg<-lm(voteshare~beautyscore,df)
  summary(reg)
  reg$fitted.values
  df$prediction <- reg$fitted.values
  
  ggplot(df, aes(beautyscore, y = value)) + 
    geom_point(aes(y = voteshare),col='blue') + 
    geom_line(aes(y = prediction),col='red')+
    ylab('voteshare')
}

question5(data)

#2
data_sortants <- data[data$incumbentcandidate==1,]
data_opposants <- data[data$incumbentcandidate==0,]

question5(data_sortants)
question5(data_opposants)  
