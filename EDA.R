install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

setwd("C:/신보람/비어플/이상거래")
data <- read.csv("creditcard.csv",header=T)
str(data)               # 284807 obs. of 31 variables
colSums(is.na(data))    # no missing value
apply(data,2,summary)
data$Class <- as.factor(data$Class)

 
aa = c(284315, 492)
windows()
ggplot(aa, aes(Class, fill=Class))+geom_bar(stat="identity")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkblue"))+ 
  scale_x_continuous(labels =c("정상거래","이상거래"))

windows()
barplot(table(data$Class),col=c("#FAC3BE","#FF9999"))

windows()
ggplot(data,aes(x=Class, y=V1, fill=Class)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#FAC3BE","#BFD3D1")) +
  ggtitle("V1") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(legend.title=element_blank(), legend.position="none")
  

vv1 = data[,c(2:29,31)]
vv = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14",
       "V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25",
       "V26","V27","V28")
head(vv1)
str(vv1)

windows()
par(mfrow=c(1,2))
for (i in 1:2){
  ggplot(vv1,aes(x=Class, y=vv1[,1], fill=Class)) +
    geom_boxplot() +
    scale_fill_manual(values=c("#FAC3BE","#BFD3D1")) +
    ggtitle(vv[1]) +
    theme(plot.title = element_text(hjust=0.5,size=17)) +
    theme(legend.title=element_blank(), legend.position="none") +
    theme(axis.title.y=element_blank())
}



ggplot(vv1,aes(x=Class, y=vv1[,28], fill=Class)) +
  geom_boxplot(outlier.size=0.7) +
  scale_fill_manual(values=c("#FAC3BE","#BFD3D1")) +
  ggtitle(vv[28]) +
  theme(plot.title = element_text(hjust=0.5,size=17)) +
  theme(legend.title=element_blank(), legend.position="none") +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())


windows()
ggplot(data, aes(x = V28)) +
  #geom_histogram(aes(y = ..density.., color = Class, fill = Class),  alpha = 0.4, position = "identity") +
  geom_density(aes(color = Class,fill=Class), size=0.1,alpha=0.68) +
  scale_color_manual(values=c("#F8A8A2","#9FBDBA")) +
  scale_fill_manual(values=c("#F8A8A2","#9FBDBA")) +
  ggtitle("V28") +
  theme(plot.title = element_text(hjust=0.5,size=17)) +
  theme(legend.title=element_blank(), legend.position="none") +
  theme(axis.title.y=element_blank(),axis.title.x=element_blank()) 
  


#FCDEDC
#FAC3BE
#F8A8A2
#F68A82

#DDE7E6
#BFD3D1
#9FBDBA
#83A9A5


### hour ###

windows()
hist(data$Time,breaks=50,col="#FAC3BE")
abline(v=60*60*24,col="red",lwd=2)

cutline <- c()
for (i in 0:48){
  cutline[i+1] <- 60*60*i
}
cutline

label <- c()
for (i in 1:48){
  label[i] <- i
}
label

data$hour <- cut(data$Time, breaks=cutline, labels=label, inclued.lowest=TRUE, right=FALSE)
data$hour <- as.numeric(factor(data$hour,level=label))
data$hour <- data$hour - 1

data[which(data$hour >= 24),]$hour <- data[which(data$hour >= 24),]$hour - 24
unique(data$hour)
tail(data[which(data$hour==23),c("Time","hour")])

windows()
mosaicplot(~hour + Class, data=data, color=T)

aa <-as.matrix(xtabs(~hour+Class, data) %>% prop.table(1))
barplot(t(aa),col=c("#EDEDED","#BFD3D1"))


str(data)
# write.csv(data,"credit2.csv")



### scaling ###

# Min-Max scaling
MinMax <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data2 <- data
for (i in 1:30){
  data2[,i] <- MinMax(data[,i])
}


# Standardization
standard <- function(x){
  return((x-mean(x))/sd(x))
}

data3 <- data
for (i in 1:30){
  data3[,i] <- standard(data[,i])
}


# Robust normalization
robust <- function(x){
  return((x-median(x))/(summary(x)[5]-summary(x)[2]))
}


for (i in 1:30){
  data[,i] <- robust(data[,i])
}


windows()
par(mfrow=c(2,2))
hist(data$Amount,breaks=50)
hist(data2$Amount,breaks=50)
hist(data3$Amount,breaks=50)
hist(data4$Amount,breaks=50)


windows()
par(mfrow=c(2,2))
hist(data$V1,breaks=50)
hist(data2$V1,breaks=50)
hist(data3$V1,breaks=50)
hist(data4$V1,breaks=50)


windows()
par(mfrow=c(2,2))
hist(data$V4,breaks=50)
hist(data2$V4,breaks=50)
hist(data3$V4,breaks=50)
hist(data4$V4,breaks=50)




### Amount ###
# plot(data$Amount,data$Class)
boxplot(data$Amount~data$Class)

data$AmountMinMax <- MinMax(data$Amount)
data$AmountStandard <- standard(data$Amount)
data$AmountRobust <- robust(data$Amount)


# kernel density plot
d <- density(data$Amount)
d2 <- density(data$AmountMinMax)
d3 <- density(data$AmountStandard)
d4 <- density(data$AmountRobust)
windows()
hist(data$Amount,breaks=100)
plot(d,xlim=c(0,1000))
lines(d2,col='red')
lines(d3,col='blue')
lines(d4,col='green')

windows()
par(mfrow=c(2,2))
hist(data$Amount,breaks=100)
lines(d,col="red")
hist(data$AmountMinMax,breaks=100)
lines(d2,col="red")
hist(data$AmountStandard,breaks=100)
lines(d3,col="red")
hist(data$AmountRobust,breaks=100)
lines(d4,col="red")


### V ###

# dataMatrix <- as.matrix(data)
# windows()
# heatmap(dataMatrix)

# install.packages("polycor")
# library(polycor)
# polyserial(data$V28,data$Class)
# cor(data$V28,data$Class)

scaling <- function(x){
  xMinMax <- MinMax(x)
  xStandard <- standard(x)
  xRobust <- robust(x)
  dd <- density(x)
  dd2 <- density(xMinMax)
  dd3 <- density(xStandard)
  dd4 <- density(xRobust)
  windows()
  par(mfrow=c(2,2))
  hist(x,breaks=20)
  lines(dd,col="red")
  hist(xMinMax,breaks=20)
  lines(dd2,col="red")
  hist(xStandard,breaks=20)
  lines(dd3,col="red")
  hist(xRobust,breaks=20)
  lines(dd4,col="red")
}
scaling(data$V4)


CompareClass <- function(x,y){
  class1 <- density(x)
  class0 <- density(y)
  windows()
  plot(class0,col="red")
  lines(class1,col="blue")
  legend("topright",legend=c("Class0","Class1"),col=c("red","blue"),lty=c(1,1))
}
CompareClass(data[which(data$Class==1),]$V1,data[which(data$Class==0),]$V1)
CompareClass(data[which(data$Class==1),]$V2,data[which(data$Class==0),]$V2)
CompareClass(data[which(data$Class==1),]$V28,data[which(data$Class==0),]$V28)

# windows()
# class1 <- density(data[which(data$Class==1),]$V19)
# class0 <- density(data[which(data$Class==0),]$V19)
# plot(class0,col="red")
# lines(class1,col="blue")
# legend("topright",legend=c("Class0","Class1"),col=c("red","blue"),lty=c(1,1))

sd(data$V1)^2
sd(data$V2)^2
sd(data$V3)^2
sd(data$V4)^2
sd(data$V5)^2
sd(data$V6)^2
sd(data$V7)^2
sd(data$V8)^2
sd(data$V9)^2
sd(data$V10)^2
sd(data$V11)^2
sd(data$V12)^2
sd(data$V13)^2
sd(data$V14)^2
sd(data$V15)^2
sd(data$V16)^2
sd(data$V17)^2
sd(data$V18)^2
sd(data$V19)^2
sd(data$V20)^2
sd(data$V21)^2
sd(data$V22)^2
sd(data$V23)^2
sd(data$V24)^2
sd(data$V25)^2
sd(data$V26)^2
sd(data$V27)^2
sd(data$V28)^2


### K-Means ###
set.seed(2021)
install.packages("cluster")
library(cluster)
dataMatrix <- as.matrix(data[,c(2:3)])
data.dist <- dist(dataMatrix)
