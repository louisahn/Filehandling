install.packages("mixlm")
install.packages("car")
install.packages("caret")
install.packages("reshape2")
install.packages("ggplot2")
library(ggplot2) 
library(reshape2)

library(mixlm)
library(car)
library(caret)
library(dplyr)

#data load
setwd("~/Documents/AR Data analysis")
data1 = read.csv("AR2017.csv", header=T, sep=',')
head(data1)
#Linear regression
linear_d1 = lm(success~ Solarsystem+ MissionsDB+ 
                 MissionsCtrl+ Notebook+ ConceptsDB+
                 AlienDB+PeriodicTable+Spectra+CommunicationCenter+
                 Gate+Console+ToolBar+ProbeDesign 
               , data=data1)
#Results
summary(linear_d1) 

#remove column
data2= data1[,1:13]
head(data2)
#Linear regression
linear_d2 = lm(success~ Solarsystem+ MissionsDB+ 
                 MissionsCtrl+ Notebook+ ConceptsDB+
                 AlienDB+PeriodicTable+Spectra+CommunicationCenter+
                 Gate+Console+ToolBar 
               , data=data2)
#Results
summary(linear_d2) 

#Remove Outlier
outlierTest(linear_d2)  #no outlier when boneferonni p>0.05                   ref: http://blog.daum.net/buillee/626
data2 = data2[, -c(13, 22, 45)]

#zero variance var check and remove
nearZeroVar(data2, saveMetrics = T)
data2 = data2[, -nearZeroVar(data2)]

#find high pearson R var and then use it.
r =FSelector::linear.correlation(success~ 
               Solarsystem+ MissionsDB+ 
               MissionsCtrl+ Notebook+ ConceptsDB+AlienDB+
               PeriodicTable+Spectra+CommunicationCenter+
               Gate+Console+ToolBar , data2)
#kai square test. see indenpendency of variables
k =FSelector::chi.squared(success~ Solarsystem+ MissionsDB+ 
              MissionsCtrl+ Notebook+ ConceptsDB+AlienDB+
              PeriodicTable+Spectra+CommunicationCenter+
              Gate+Console+ToolBar , data2)
k
cutoff.k(k, 5)

#remove multicollinearity
vif(linear_d2)
sqrt(vif(linear_d2))>2

#find best linear model
linear_best2 = stepWiseBack(linear_d2, alpha.enter = 0.05, 
                            alpha.remove = 0.05, full=TRUE)
summary(linear_best2)


#try logistic regr
data2$success = factor(data2$success)
str(data2)
g1 = glm(success~ Solarsystem+ MissionsDB+ 
           MissionsCtrl+ Notebook+ ConceptsDB
         +AlienDB+PeriodicTable+Spectra+
           CommunicationCenter+Gate+Console+ToolBar 
         , data=data2, family='binomial')
summary(g1)
install.packages("pscl")
library(pscl)
pR2(g1) # with Mcfadden R2(r2cu), model fit check.                ref: http://www.dodomira.com/2016/02/12/logistic-regression-in-r/


#let's do DT,                                             ref: http://blog.naver.com/960125_hds/221046240198
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
set.seed(123)
trainIdx = sample(1:nrow(data2), size=nrow(data2)*0.7)
train= data2[trainIdx,]
test= data2[-trainIdx,]
dt = rpart(success~ Solarsystem+ MissionsDB+ 
             MissionsCtrl+ Notebook+ ConceptsDB
           +AlienDB+PeriodicTable+Spectra+
             CommunicationCenter+Gate+Console+ToolBar
           , train, method="class")
rpart.plot(dt, cex=0.5)

pdt = predict(dt, test, type="class")
#library(caret)
#install.packages('e1071', dependencies=TRUE)
confusionMatrix(pdt, test$success)                        #ref: http://blog.naver.com/liberty264/221015964454 

dt = rpart(success~ Solarsystem+ MissionsDB
           +AlienDB+PeriodicTable+Spectra
           , train, method="class")


#visialize
radius <- sqrt( data1$MissionsDB/ pi )
symbols(data1$success, data1$ProbeDesign, circles=radius
        ,inches=0.35, fg="white", bg="red", xlab="Success", ylab="ProbeDesign")
text(data1$success, data1$ProbeDesign, data1$AlienDB, cex=0.5)

#heatmap
data2 = subset(data1[,1:7], success<10)
melted = melt(data2,id="success")
p=ggplot(melted, aes(x=success,y=variable,fill=value))+
  geom_tile(colour="black", linetype="dashed")+
  scale_y_discrete(limits=c("Solarsystem","MissionsDB","MissionsCtrl",
                            "Notebook","ConceptsDB","AlienDB" ))+
  scale_fill_gradient(low="yellow",high="red",breaks=c(0,200,400,600,800,1000),limits=c(0,1000))
p
data3 = data1[c("success", "PeriodicTable",
                "Spectra","CommunicationCenter","Gate", "Console","ToolBar" )]
data3 = subset(data3, success<20)
melted = melt(data3,id="success")
p2=ggplot(melted, aes(x=success,y=variable,fill=value))+
  geom_tile(colour="black", linetype="dashed")+
  scale_y_discrete(limits=c("PeriodicTable",
                            "Spectra","CommunicationCenter","Gate", "Console","ToolBar" ))+
  scale_fill_gradient(low="yellow",high="red",breaks=c(0,200,400,600,800,1000),limits=c(0,1000))
p2


#library(reshape2)
melted = melt(data1,id="success")
head(melted, 50)
p=ggplot(melted, aes(x=success,y=variable,fill=value))+
  geom_tile(colour="black", linetype="dashed")+
  scale_y_discrete(limits=c("Solarsystem","MissionsDB","MissionsCtrl",
                            "Notebook","ConceptsDB","AlienDB", "PeriodicTable",
                            "Spectra","CommunicationCenter","Gate", "Console","ToolBar"))+
  scale_fill_gradient(low="yellow",high="red",breaks=c(0,200,400,600,800,1000),limits=c(0,1000))
p
#theme(axis.text.x=element_text(angle=70, hjust=1)
#      ,axis.title.y = element_text(vjust=0.5)
#      ,axis.text.y=element_text(angle=70, hjust=0.6))+
  

data2 = subset(data1, success<10)
boxplot(data1$total)
head(data1)
m1 = lm(success~ Solarsystem+ MissionsDB+ MissionsCtrl+ Notebook+ ConceptsDB+AlienDB+PeriodicTable+Spectra+CommunicationCenter+Gate+Console+ToolBar , data=data1)
summary(m1)


#find high correlation var
install.packages("FSelector")
library(FSelector)
#find high pearson R var
r =FSelector::linear.correlation(success~ 
                          Solarsystem+ MissionsDB+ 
                          MissionsCtrl+ Notebook+ ConceptsDB+AlienDB+
                          PeriodicTable+Spectra+CommunicationCenter+
                          Gate+Console+ToolBar , data1)
cutoff.k(r, 3)
m1 = lm(success~ MissionsCtrl+ Gate+Console , data=data1)
summary(m1)



distinct(data1, success)
boxplot(data1$success)  # let's remove outlier that is success >14

data2 = subset(data1, success<13)
data2 = subset(data2, success>0)
head(data2)
boxplot(data2$success)  
m2 = lm(success~ Solarsystem+ MissionsDB+ MissionsCtrl+ Notebook+ ConceptsDB+AlienDB+PeriodicTable+Spectra+CommunicationCenter+Gate+Console+ToolBar , data=data2)
summary(m2) # R2 is very low. 
outlierTest(m2)  #no outlier when boneferonni p>0.05











#normalize. do it when clustering.

data3 = as.data.frame(scale(data1))
library(ggplot2)
ggplot(data=data2) + 
  geom_histogram(mapping = aes(x=Solarsystem) )
hist(data2$Solarsystem)
boxplot(data2)


ggplot(data=data2) + 
  geom_point(mapping = aes(x=Solarsystem, y=PeriodicTable) )

with(data2, {
  + plot(Solarsystem, MissionsDB, pch=success )
})


data2= data1[,1:13]
head(data2)
featurePlot(data2[,2:4], data2$success,"paires")
featurePlot(iris[,1:4], iris$Species,"ellipse")
summary(data2)


outlierTest(m1)
data1[c(171,58),]
#remove outlier
data2 = data1[-c(14,109),]

m2 = lm(success~ Solarsystem+ MissionsDB+ MissionsCtrl+ Notebook+ ConceptsDB+AlienDB+PeriodicTable+Spectra+ProbeDesign , data=data2)
summary(m2)
outlierTest(m2)
data2[c(42),]
#data 42 seems ok.
#Multicollinearity, if this value is over 2, then we have multicolinearity.
vif(m2)
sqrt(vif(m2))>2

#remove notebook according to sqrt(vif(m2))>2, 
m3 = lm(success~ Solarsystem+ MissionsDB+ MissionsCtrl+  ConceptsDB+AlienDB+PeriodicTable+Spectra+ProbeDesign , data=data2)
sqrt(vif(m3))>2
summary(m3)

plot(data2$MissionsDB, data2$ConceptsDB)
plot(data2$PeriodicTable, data2$Spectra)

#remove missionctrl, conceptdb
m4 = lm(success~ Solarsystem+ MissionsDB+ AlienDB+PeriodicTable+Spectra+ProbeDesign , data=data2)
summary(m4) #low Rsqure
sqrt(vif(m4))>2
outlierTest(m4)
data2[c(42),]
data3 = data2[-c(42),]

m5 = lm(success~ Solarsystem+ MissionsDB+ AlienDB+PeriodicTable+Spectra+ProbeDesign , data=data3)
summary(m5) #low Rsqure
sqrt(vif(m5))>2
outlierTest(m5)
data3[c(42),]

summary(m5)

base4 = stepWiseBack(m1, alpha.enter = 0.05, alpha.remove = 0.05, full=TRUE)
summary(base4)
par(mfrow = c(2, 2))
plot(m3)


#performance divide
data2 = subset(data1, success>4)
m2 = lm(success~ Solarsystem+ MissionsDB+ MissionsCtrl+ Notebook+ ConceptsDB+AlienDB+PeriodicTable+Spectra+ProbeDesign , data=data2)
summary(m2)

data3 = subset(data1, success<5)
m3 = lm(success~ Solarsystem+ MissionsDB+ MissionsCtrl+ Notebook+ ConceptsDB+AlienDB+PeriodicTable+Spectra+ProbeDesign , data=data3)
summary(m3)


