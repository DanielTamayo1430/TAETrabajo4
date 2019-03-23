
# Cargamos las librerias =================================
library(caret)
library(rpart)        #decision tree
library(adabag)       #boosting
library(nnet)         #neural network
library(kernlab)      #SVMs
library(class)        #k-nearest neighbors
library(datasets)
library(reshape2)
# --------------------------------------------------------

# Cargamos la data ===========================================
data <- read.csv('mnist_train.csv')
  for(col in 2:ncol(data)) {
    names(data)[col] <- paste("pixel",col-1,sep="")
  }


data[data!="0" && col(data) != 1]<-1

dataF <- data

dataF$X5 <- as.factor(dataF[,1])
# check data balance
table(dataF$X5)
# --------------------------------------------------------

# Cheuqueamos la data ===========================================
plt.img <- function(imgdata, X5s, nrow=28, ncol=28){
  if (dev.cur() !=1) dev.off
  par(mfrow = c(2, 10), mar=c(4,1,4,1))
  for (i in seq(nrow(imgdata))){
    img1 <- matrix(data.matrix(imgdata[i,]),nrow,ncol)
    image(img1[,28:1],axes = FALSE, 
          col = grey(seq(0, 1, length = 255)),
          xlab= as.character(X5s[i]),
          cex.lab=3)
  } 
}

plt.img(dataF[1:20,2:ncol(dataF)],dataF[1:10,1])


#Division de la data------------------------
set.seed(1)
inTrain <- createDataPartition(y=dataF$X5,p=0.75, list=FALSE)
train <- dataF[inTrain,]
train1 <- train[createDataPartition(y=train$X5,p=0.2, list=FALSE),]
train2 <- train[createDataPartition(y=train$X5,p=0.4, list=FALSE),]
train3 <- train[createDataPartition(y=train$X5,p=0.6, list=FALSE),]
train4 <- train[createDataPartition(y=train$X5,p=0.8, list=FALSE),]

test <- dataF[-inTrain,]


## Data training and testing------------------------------------

## Arboles de decisión --------------------------------- 

# Obtenemos el arbol completo
library(rpart)

dtfit <- rpart(X5~., method='class', data=train, control=rpart.control(minsplit=5, cp=0.0))

par(mfrow = c(2,2))
plot(dtfit)
plotcp(dtfit)

# Podamos el árbol y probamos
treesize <-as.factor(c(32,36,45,48,69,73,75,83))
er=numeric(8)
for (i in seq(8)) {
  pfit <- prune(dtfit, cp=dtfit$cptable[i+16,'CP'])
  fitX5s <- predict(pfit, newdata=test, type='class')
  er[i] <-1-confusionMatrix(data=fitX5s, test$X5)$overall[1]
}
plot(treesize, er, type='p', main='Error Rate vs Tree size',
     xlab='Size of Tree', ylab='Error rate of prediction')

# Árbol podado
plot(prune(dtfit, cp=dtfit$cptable[21,'CP']))

# error vs tamaño del set de entrenamiento
er <- numeric(5)
for (i in 1:5) {
  if (i==1) {train0=train1}
  if (i==2) {train0=train2}
  if (i==3) {train0=train3}
  if (i==4) {train0=train4}
  if (i==5) {train0=train}
  time0 <- proc.time()
  fit <- rpart(X5~., method='class', data=train0, control=rpart.control(minsplit=5, cp=0.0025))
  time1 <- proc.time()
  fitX5s <- predict(fit, newdata=test, type='class')
  time2 <- proc.time()
  er[i] <-1-confusionMatrix(data=fitX5s, test$X5)$overall[1]
  if (i==5) { resultArbol=c((time1-time0)[3],(time2-time1)[3], er[5])}
}
par(mfrow = c(1,1)); 
plot(seq(0.2,1,0.2),er, type='o', pch=22, cex=1, col='blueviolet', lwd=2,
     xlab='training size', ylab='error rate (%)', main='Árbol de decisión')

## Creamos el Archivo para shiny

imgAsDf <- matrix(, nrow = 1, ncol = 785)
imgAsDf2 <- as.matrix(imgAsDf)
pixels <- paste("pixel" , (1:ncol(imgAsDf)) -1, sep="")

newdata.frame <- data.frame( )

for(row in 1:nrow(imgAsDf)) {
  for(col in 1:ncol(imgAsDf)) {
    newdata.frame[1, paste(c("pixel", col), collapse = "")]<-as.numeric(test[4,][1,col])
  }
}
load("modelDD.RData")


modelDD <- rpart(X5~., method='class', data=train, control=rpart.control(minsplit=5, cp=0.0025))
fitX5s <- (predict(modelDD, newdata.frame, type='class'))
fitX5s <- (predict(modelDD,test[4,] , type='class'))
bbb <- test[4,]
aaa <- levels(fitX5s)[fitX5s]
plt.img(test[4,2:ncol(test)],test[4,1])

modelDD<-dtfit
save(modelDD,file = "modelDD.RData")

#### Redes Neuronales  -----------------------------------------

# Tasa de error vs iteración máxima
library(nnet)

er <- numeric(3)
for (i in 1:3){
  set.seed(1)
  time0 <- proc.time()
  fit <- nnet(X5 ~ ., data=train, size = 15, rang = 0.1, decay = 0.1, maxit = 50*i, MaxNWts=12000)
  time1 <- proc.time()
  fitX5s <- predict(fit, newdata=test, type='class')
  time2 <- proc.time()
  er[i]=1-confusionMatrix(table(data=fitX5s, test$X5))$overall[1]
  if (i==3) {resultRedes=c((time1-time0)[3],(time2-time1)[3], er[3])}
}
save(er, resultRedes, file='nnet.Rdata')
# to save the time to output a pdf, I saved the nnet outputs in nnet.Rdata

load('nnet.Rdata')
par(mfrow = c(1,1)); 
plot(c(50,100),c(er[1],er[2]), type='o', pch=22, cex=1, col='blueviolet', lwd=2,
     xlab='max iteration', ylab='error rate (%)', main='Redes Neurales')

#Archivo para shiny

fit <- nnet(X5 ~ ., data=train, size = 15, rang = 0.1, decay = 0.1, maxit = 200, MaxNWts=12000)
modelNN<-fit

fitX5s <- (predict(modelNN, newdata.frame, type='class'))
fitX5s <- (predict(modelNN,test[4,] , type='class'))
bbb <- test[4,]
aaa <- levels(fitX5s)[fitX5s]
plt.img(test[4,2:ncol(test)],test[4,1])

save(modelNN,file = "modelNN.RData")

#### Maquina de soporte vectorial--------------------------------------
# Error vs tamaño de entrenamiento (Dos núcleos)
library(kernlab)

er1 <- numeric(5)
er2 <- numeric(5)
for (i in 1:5) {
if (i==1) {train0=train1}
if (i==2) {train0=train2}
if (i==3) {train0=train3}
if (i==4) {train0=train4}
if (i==5) {train0=train}

fit1 <- ksvm(X5~., train0, type="C-svc",kernal='vanilladot', C=10,cross=10)  # linear kernel
time0 <- proc.time()
fit2 <- ksvm(X5~., train0, type="C-svc",kernal='rbfdot', C=10,cross=10)  # Radial kernel
time1 <- proc.time()

fitX5s1 <- predict(fit1, newdata=test)

time2 <- proc.time()
fitX5s2 <- predict(fit2, newdata=test)
time3 <- proc.time()

er1[i]<-1-confusionMatrix(data=fitX5s1, test$X5)$overall[1]
er2[i]<-1-confusionMatrix(data=fitX5s2, test$X5)$overall[1]
if (i==5) {resultSVM=c((time1-time0)[3],(time3-time2)[3], er2[5])}
}
save(er1,er2, resultSVM, file='SVM.Rdata')


# Para Shiny


## Creamos el Archivo para shiny

imgAsDf <- matrix(, nrow = 1, ncol = 785)
imgAsDf2 <- as.matrix(imgAsDf)
pixels <- paste("pixel" , (1:ncol(imgAsDf)) -1, sep="")

newdata.frame <- data.frame( )

for(row in 1:nrow(imgAsDf)) {
  for(col in 1:ncol(imgAsDf)) {
    newdata.frame[1, paste(c("pixel", col), collapse = "")]<-as.numeric(test[4,][1,col])
  }
}
load("modelDD.RData")


fit1 <- ksvm(X5~., train0, type="C-svc",kernal='rbfdot', C=10,cross=10)  # Radial kernel
fitX5s1 <- predict(fit1, test[4,])

imgAsDf <- matrix(, nrow = 1, ncol = 785)
imgAsDf2 <- as.matrix(imgAsDf)

newdata.frame <- data.frame( )

for(row in 1:nrow(imgAsDf)) {
  for(col in 1:ncol(imgAsDf)) {
    newdata.frame[1, paste(c("pixel", col), collapse = "")]<-as.numeric(test[4,][1,col])
  }
}
fitX5s1 <- predict(modelSVM, newdata.frame)

aaa <- levels(fitX5s1)[fitX5s1]
plt.img(test[4,2:ncol(test)],test[4,1])

modelSVM<-fit1
save(modelSVM,file = "modelSVM.RData")


# plot
load('SVM.Rdata')
par(mfrow = c(1,1))
plot(seq(0.2,1,0.2),er1, type='o', pch=21, cex=1, col='blue', lwd=2,
xlab='training size', ylab='error rate (%)', main='Máquinas de soporte vectorial')
lines(seq(0.2,1,0.2),er2, type='o', pch=22, cex=1, col='red', lwd=2)
legend(0.2, 0.14, c('Linear', "Gaussian"), cex=0.8, col=c('blue','red'), pch=21:22, bty='n')

## Resultados

perf <- rbind(resultArbol,resultRedes,resultSVM)
par(mfrow = c(1,3), mar=c(4,4,4))
barplot(perf[,1],names.arg=c('DT','NN','SVM'),
xlab='Different algorithms', ylab='Training time (s)', main='Training Time')

barplot(perf[,2],names.arg=c('DT','NN','SVM'),
xlab='Different algorithms', ylab='Testing time (s)', main='Testing Time')

barplot(perf[,3],names.arg=c('DT','NN','SVM'),
xlab='Different algorithms', ylab='Error rate (%)', main='Error Rate')

  