whitewine<-read.csv("C:/Users/alice/Desktop/STAT_6210_10 Data Analysis/Project3/winequality-white.csv")
redwine<-read.csv("C:/Users/alice/Desktop/STAT_6210_10 Data Analysis/Project3/winequality-red.csv")

#take a look at the data
#whitewine
dim(whitewine)
str(whitewine)
summary(whitewine)

#redwine
dim(redwine)
str(redwine)
summary(redwine)

#check the distribution of each variable
#whitewine
library(ggplot2)
library(reshape)
whitewine.melt<-melt(whitewine)
#ggplot(whitewine.melt,aes(x=value))+geom_density()+facet_wrap(~variable,scales="free")
ggplot(whitewine.melt,aes(x=value))+geom_histogram()+facet_wrap(~variable,scales="free")
cor(whitewine)
library(corrplot)
corrplot(cor(whitewine))
corrplot(cor(whitewine),method = "number")

#redwine
redwine.melt<-melt(redwine)
#ggplot(redwine.melt,aes(x=value))+geom_density()+facet_wrap(~variable,scales="free")
ggplot(redwine.melt,aes(x=value))+geom_histogram()+facet_wrap(~variable,scales="free")
cor(redwine)
library(corrplot)
corrplot(cor(redwine))
corrplot(cor(redwine),method = "number")



#training/test data

sub <- sample(nrow(whitewine), round(nrow(whitewine) * 0.7))
whitewinetrain <- whitewine[sub, ]
whitewinetest <- whitewine[-sub, ]

sub <- sample(nrow(redwine), round(nrow(redwine) * 0.7))
redwinetrain <- redwine[sub, ]
redwinetest <- redwine[-sub, ]


#Try linear regression
whitewinelireg<-lm(quality~.,data=whitewinetrain)
summary(whitewinelireg)
whitewineoriglm<-glm(quality~.,data=whitewinetrain)
cv.glm(whitewinetrain,whitewineoriglm,K=10)$delta[1]
#MSE Residual standard error: 0.7705
redwinelireg<-lm(quality~.,data=redwinetrain)
summary(redwinelireg)



#Best Subset Selection
#whitewine
library(leaps)
whitewineregfit.full <- regsubsets(quality~.,whitewinetrain,nvmax = 11)
summary(whitewineregfit.full)
whitewineregfit.summary <- summary(whitewineregfit.full)
names(whitewineregfit.summary)
#???
plot(whitewineregfit.summary$rsq)
#until 7 not change too much
plot(whitewineregfit.summary$bic)
#The lowest BIC is achieved by the 7th model
#???
coef(whitewineregfit.full,7)
#We will also check the models chosen by best subset selection using cross validation.
#samething???
library(boot)
CVmse <- rep(0,11)
for(i in 1:11){
  tempCols <- which(whitewineregfit.summary$which[i,-1]==TRUE)
  tempCols <- c(tempCols,12)
  tempCols <- as.numeric(tempCols)
  tempGLM <- glm(quality~.,data=whitewinetrain[,tempCols])
  tempCV <- cv.glm(tempGLM,data=whitewinetrain[,tempCols],K = 10)
  CVmse[i] <- tempCV$delta[1]
}
#???
plot(CVmse)
CVmse
#also 7?
min(CVmse)

#redwine
redwineregfit.full <- regsubsets(quality~.,redwinetrain,nvmax = 11)
summary(redwineregfit.full)
redwineregfit.summary <- summary(redwineregfit.full)
names(redwineregfit.summary)
plot(redwineregfit.summary$rsq)
plot(redwineregfit.summary$bic)
#The lowest BIC is achieved by the 5th model
#???
coef(redwineregfit.full,5)

library(boot)
CVmse <- rep(0,11)
for(i in 1:11){
  tempCols <- which(redwineregfit.summary$which[i,-1]==TRUE)
  tempCols <- c(tempCols,12)
  tempCols <- as.numeric(tempCols)
  tempGLM <- glm(quality~.,data=redwinetrain[,tempCols])
  tempCV <- cv.glm(tempGLM,data=redwinetrain[,tempCols],K = 10)
  CVmse[i] <- tempCV$delta[1]
}
#???
plot(CVmse)
#also 7
min(CVmse)
CVmse
library(MASS)
#Forward Stepwise Selection
#whitewine
whitewineregfit.fwd <- regsubsets(quality~.,whitewinetrain,nvmax=11,method="forward")
summary(whitewineregfit.fwd)
whitewineregfit.fwd.summary<-summary(whitewineregfit.fwd)
#???
CVmse <- rep(0,11)
for(i in 1:11){
  tempCols <- which(whitewineregfit.fwd.summary$which[i,-1]==TRUE)
  tempCols <- c(tempCols,12)
  tempCols <- as.numeric(tempCols)
  tempGLM <- glm(quality~.,data=whitewinetrain[,tempCols])
  tempCV <- cv.glm(tempGLM,data=whitewinetrain[,tempCols],K = 10)
  CVmse[i] <- tempCV$delta[1]
}

plot(CVmse)
CVmse

templmf <- lm(quality~1,data=whitewinetrain) 
tempScopef <- formula(lm(quality~.,whitewinetrain)) 
stepf <- stepAIC(templmf, scope=tempScopef, direction="forward") 
stepf$anova

#redwine
redwineregfit.fwd <- regsubsets(quality~.,redwinetrain,nvmax=11,method="forward")
summary(redwineregfit.fwd)
#???

templmf <- lm(quality~1,data=redwinetrain) 
tempScopef <- formula(lm(quality~.,redwinetrain)) 
stepf <- stepAIC(templmf, scope=tempScopef, direction="forward") 
stepf$anova



#Backward Stepwise Selection
#whitewine
whitewineregfit.bwd <- regsubsets(quality~.,whitewinetrain,nvmax=11,method="backward") 
summary(whitewineregfit.bwd)


templmb <- lm(quality~.,data=whitewinetrain) 
tempScopeb <- formula(lm(quality~1,whitewinetrain)) 
stepb <- stepAIC(templmb, scope=tempScopeb, direction="backward", trace=F) 
stepb$anova

#redwine
redwineregfit.bwd <- regsubsets(quality~.,redwinetrain,nvmax=11,method="backward") 
summary(redwineregfit.bwd)


templmb <- lm(quality~.,data=redwinetrain) 
tempScopeb <- formula(lm(quality~1,redwinetrain)) 
stepb <- stepAIC(templmb, scope=tempScopeb, direction="backward", trace=F) 
stepb$anova


#LASSO
#whitewine
library(glmnet)
whitewinelasso.cv <- cv.glmnet(x=as.matrix(whitewinetrain[,-12]),y=as.matrix(whitewinetrain[,12]),alpha=1,nfolds = 10)
plot(whitewinelasso.cv)
names(whitewinelasso.cv)
log(whitewinelasso.cv$lambda.min)
min(whitewinelasso.cv$cvm)

whitewinelasso.fit <- glmnet(x=as.matrix(whitewinetrain[,-12]),y=as.matrix(whitewinetrain[,12]),alpha=1,lambda=c(1,exp(-4.936626))) 
whitewinelasso.fit$beta[,2]
selectnames<-names(whitewinelasso.fit$beta[whitewinelasso.fit$beta[,2]!=0,2])
selectnamesv<-as.vector(selectnames)

#redwine
library(glmnet)
redwinelasso.cv <- cv.glmnet(x=as.matrix(redwinetrain[,-12]),y=as.matrix(redwinetrain[,12]),alpha=1,nfolds = 10)
plot(redwinelasso.cv)
names(redwinelasso.cv)
log(redwinelasso.cv$lambda.min)
min(redwinelasso.cv$cvm)

redwinelasso.fit <- glmnet(x=as.matrix(redwinetrain[,-12]),y=as.matrix(redwinetrain[,12]),alpha=1,lambda=c(1,exp(-4.524623))) 
redwinelasso.fit$beta[,2]
redwineselectnames<-names(redwinelasso.fit$beta[redwinelasso.fit$beta[,2]!=0,2])
redwineselectnamesv<-as.vector(redwineselectnames)




#model1
#whitewine
whitewinefinalglm <- glm(quality~ volatile.acidity+residual.sugar+
                           chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
                           density+pH +sulphates+alcohol,
                         data=whitewinetrain)


whitewinefinallm1 <- lm(quality~.-fixed.acidity-citric.acid,
                        data=whitewinetrain)
summary(whitewinefinallm1)
#redwine
redwinefinalglm <- glm(quality~ fixed.acidity+volatile.acidity+
                         chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH
                       +sulphates+alcohol,
                       data=redwinetrain)





redwinefinallm1 <- lm(quality~ fixed.acidity+volatile.acidity+
                        chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH
                      +sulphates+alcohol,
                      data=redwinetrain)
#prediction
#whitewine
whitewineprediction<-round(predict(whitewinefinalglm,newdata=whitewinetest))
dataframewhitewineprediction<-as.data.frame(whitewineprediction)
dataframewhitewineprediction$difference<-(dataframewhitewineprediction$whitewineprediction-whitewinetest$quality)^2
(sum(dataframewhitewineprediction$difference))/nrow(dataframewhitewineprediction)

mean(whitewineprediction==whitewinetest$quality)
mean(abs(whitewineprediction - whitewinetest$quality))

#redwine
redwineprediction<-round(predict(redwinefinalglm,newdata=redwinetest))
dataframeredwineprediction<-as.data.frame(redwineprediction)
dataframeredwineprediction$difference<-(dataframeredwineprediction$redwineprediction-redwinetest$quality)^2
(sum(dataframeredwineprediction$difference))/nrow(dataframeredwineprediction)
hist(dataframeredwineprediction$redwineprediction)
hist(redwinetest$quality)

mean(redwineprediction==redwinetest$quality)
mean(abs(redwineprediction - redwinetest$quality))

#boots
#whitewine
whitewineXmean<-mean(whitewineprediction)
boots<-matrix(0,1000,1)
for (i in 1:1000){
  samplew<-as.data.frame(whitewinetrain[sample(1:nrow(whitewinetrain),1,replace=FALSE),])
  bpredw<-predict(whitewinefinalglm,samplew)
  boots[i]<-bpredw
}

whitewineprediction<-predict(whitewinefinalglm,newdata=whitewinetest)
hist(boots-whitewineXmean,100)
hist(whitewineprediction-whitewineXmean,100)

whitewineXmean+quantile(boots-whitewineXmean,c(0.025,0.975),na.rm=TRUE)
whitewineXmean+quantile(whitewineprediction-whitewineXmean,c(0.025,0.975),na.rm=TRUE)

#redwine
redwineXmean<-mean(redwineprediction)
boots<-matrix(0,1000,1)
for (i in 1:1000){
  sampler<-as.data.frame(redwinetrain[sample(1:nrow(redwinetrain),1,replace=FALSE),])
  bpredr<-predict(redwinefinalglm,sampler)
  boots[i]<-bpredr
}
redwineprediction<-predict(redwinefinalglm,newdata=whitewinetest)
hist(boots-redwineXmean,100)
hist(whitewineprediction-whitewineXmean,100)

redwineXmean+quantile(redwineprediction-redwineXmean,c(0.025,0.975),na.rm=TRUE)
redwineXmean+quantile(boots-redwineXmean,c(0.025,0.975),na.rm=TRUE)



#check resudual
#whitewine
whitewineselectedvariables<-cbind(whitewinetrain[,selectnamesv],whitewinetrain$quality)
names(whitewineselectedvariables)[10]<-paste("quality")
whitewineselectedvariables.melt<-melt(whitewineselectedvariables)
whitewine.melt3 <- cbind(whitewineselectedvariables.melt,resid=whitewinefinallm1$residuals)
ggplot(whitewine.melt3,aes(x=value,y=resid))+geom_point()+geom_smooth(method="loess")+facet_wrap(~variable,scales="free")
#redwine
redwineselectedvariables<-cbind(redwinetrain[,redwineselectnamesv],redwinetrain$quality)
names(redwineselectedvariables)[10]<-paste("quality")
redwineselectedvariables.melt<-melt(redwineselectedvariables)
redwine.melt3 <- cbind(redwineselectedvariables.melt,resid=redwinefinallm1$residuals)
ggplot(redwine.melt3,aes(x=value,y=resid))+geom_point()+geom_smooth(method="loess")+facet_wrap(~variable,scales="free")





cv.glm(whitewinetrain,whitewinefinalglm,K=10)$delta[1]





#polynomial regression
#whitewine
residual.sugarMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-residual.sugar+poly(residual.sugar,i),data=whitewineselectedvariables)
  tempCV <- cv.glm(whitewineselectedvariables,templm,K = 10)
  residual.sugarMSE[i] <- tempCV$delta[1]
}
plot(residual.sugarMSE)
which.min(residual.sugarMSE)

chloridesMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-chlorides+poly(chlorides,i),data=whitewineselectedvariables)
  tempCV <- cv.glm(whitewineselectedvariables,templm,K = 10)
  chloridesMSE[i] <- tempCV$delta[1]
}
plot(chloridesMSE)
which.min(chloridesMSE)
#3

free.sulfur.dioxideMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-chlorides-free.sulfur.dioxide+poly(chlorides,3)+poly(free.sulfur.dioxide,i),data=whitewineselectedvariables)
  tempCV <- cv.glm(whitewineselectedvariables,templm,K = 10)
  free.sulfur.dioxideMSE[i] <- tempCV$delta[1]
}
plot(free.sulfur.dioxideMSE)
which.min(free.sulfur.dioxideMSE)
#1,5,8

total.sulfur.dioxideMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-chlorides-total.sulfur.dioxide+poly(chlorides,3)+poly(total.sulfur.dioxide,i),data=whitewineselectedvariables)
  tempCV <- cv.glm(whitewineselectedvariables,templm,K = 10)
  total.sulfur.dioxideMSE[i] <- tempCV$delta[1]
}
plot(total.sulfur.dioxideMSE)
which.min(total.sulfur.dioxideMSE)
#2,4,5

densityMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-chlorides-total.sulfur.dioxide-density+poly(chlorides,3)+poly(total.sulfur.dioxide,5)+poly(density,i),data=whitewineselectedvariables)
  tempCV <- cv.glm(whitewineselectedvariables,templm,K = 10)
  densityMSE[i] <- tempCV$delta[1]
}
plot(densityMSE)
which.min(densityMSE)

finalLmwhitewinepoly <- lm(quality~volatile.acidity+residual.sugar+poly(chlorides,3)+poly(free.sulfur.dioxide,5)+poly(total.sulfur.dioxide,5)+poly(density,2)+pH+sulphates+alcohol,data=whitewineselectedvariables)

finalglmwhitewinepoly <- glm(quality~volatile.acidity+residual.sugar+poly(chlorides,3)+poly(free.sulfur.dioxide,5)+poly(total.sulfur.dioxide,5)+poly(density,2)+pH+sulphates+alcohol,data=whitewineselectedvariables)
cv.glm(whitewineselectedvariables,finalglmwhitewinepoly,K=10)$delta[1]

summary(finalLmwhitewinepoly)
whitewine.melt4 <- cbind(whitewineselectedvariables.melt,resid=finalLmwhitewinepoly$residuals)
ggplot(whitewine.melt4,aes(x=value,y=resid))+geom_point()+geom_smooth(method="loess")+facet_wrap(~variable,scales="free")


#redwine

volatile.acidityMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-volatile.acidity+poly(volatile.acidity,i),data=redwineselectedvariables)
  tempCV <- cv.glm(redwineselectedvariables,templm,K = 10)
  volatile.acidityMSE[i] <- tempCV$delta[1]
}
plot(volatile.acidityMSE)
which.min(volatile.acidityMSE)


chloridesMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-volatile.acidity-chlorides+poly(volatile.acidity,5)+poly(chlorides,i),data=redwineselectedvariables)
  tempCV <- cv.glm(redwineselectedvariables,templm,K = 10)
  chloridesMSE[i] <- tempCV$delta[1]
}
plot(chloridesMSE)
which.min(chloridesMSE)
#2,4,8
free.sulfur.dioxideMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-volatile.acidity-chlorides-free.sulfur.dioxide+poly(volatile.acidity,5)+poly(chlorides,2)+poly(free.sulfur.dioxide,i),data=redwineselectedvariables)
  tempCV <- cv.glm(redwineselectedvariables,templm,K = 10)
  free.sulfur.dioxideMSE[i] <- tempCV$delta[1]
}
plot(free.sulfur.dioxideMSE)
which.min(free.sulfur.dioxideMSE)
#2
total.sulfur.dioxideMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-volatile.acidity-chlorides-free.sulfur.dioxide-total.sulfur.dioxide+poly(volatile.acidity,5)+poly(chlorides,2)+poly(free.sulfur.dioxide,2)+poly(total.sulfur.dioxide,i),data=redwineselectedvariables)
  tempCV <- cv.glm(redwineselectedvariables,templm,K = 10)
  total.sulfur.dioxideMSE[i] <- tempCV$delta[1]
}
plot(total.sulfur.dioxideMSE)
which.min(total.sulfur.dioxideMSE)

#1
sulphatesMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-volatile.acidity-chlorides-free.sulfur.dioxide-sulphates+poly(volatile.acidity,5)+poly(chlorides,2)+poly(free.sulfur.dioxide,2)+poly(sulphates,i),data=redwineselectedvariables)
  tempCV <- cv.glm(redwineselectedvariables,templm,K = 10)
  sulphatesMSE[i] <- tempCV$delta[1]
}
plot(sulphatesMSE)
which.min(sulphatesMSE)
#4
alcoholMSE <- rep(0,10)
for(i in 1:10){
  templm <- glm(quality~.-volatile.acidity-chlorides-free.sulfur.dioxide-sulphates-alcohol+poly(volatile.acidity,5)+poly(chlorides,2)+poly(free.sulfur.dioxide,2)+poly(sulphates,4)+poly(alcohol,i),data=redwineselectedvariables)
  tempCV <- cv.glm(redwineselectedvariables,templm,K = 10)
  alcoholMSE[i] <- tempCV$delta[1]
}
plot(alcoholMSE)
which.min(alcoholMSE)
#1

finalLmredwinepoly <- lm(quality~fixed.acidity+poly(volatile.acidity,5)+poly(chlorides,5)+poly(free.sulfur.dioxide,4)+poly(total.sulfur.dioxide,2)+density+pH+poly(sulphates,4)+poly(alcohol,2),data=redwineselectedvariables)
summary(finalLmredwinepoly)

finalglmredwinepoly <- glm(quality~fixed.acidity+poly(volatile.acidity,5)+poly(chlorides,5)+poly(free.sulfur.dioxide,4)+poly(total.sulfur.dioxide,2)+density+pH+poly(sulphates,4)+poly(alcohol,2),data=redwineselectedvariables)
cv.glm(redwineselectedvariables,finalglmredwinepoly,K=10)$delta[1]

redwine.melt4 <- cbind(redwineselectedvariables.melt,resid=finalLmredwinepoly$residuals)
ggplot(redwine.melt4,aes(x=value,y=resid))+geom_point()+geom_smooth(method="loess")+facet_wrap(~variable,scales="free")

#KNN
library(class)
library(gmodels)
whitewinetrainsamp<-sample(4898,4000)
whitewinetestsamp<-sample(4898,1000)
whitewinetrainindeva<-whitewine[whitewinetrainsamp,-12]
whitewinetraindeva<-whitewine[whitewinetrainsamp,12]
whitewinetestindeva<-whitewine[whitewinetestsamp,-12]
whitewinetestdeva<-whitewine[whitewinetestsamp,12]
whitewineknn<-knn(whitewinetrainindeva,whitewinetestindeva,factor(whitewinetraindeva),k=10,prob=TRUE)
result=CrossTable(whitewineknn,whitewinetestdeva,prop.chisq=FALSE,prob.t=FALSE,dnn=c("predicted","actual"))

hist(whitewine$quality)


#spline


library(boot)
whitewinefinallm1 <- lm(quality~.-fixed.acidity-citric.acid,
                        data=whitewinetrain)
whitewinefinalglm <- glm(quality~ volatile.acidity+residual.sugar+
                           chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
                           density+pH +sulphates+alcohol,
                         data=whitewinetrain)
summary(whitewinefinallm1)
cv.glm(whitewinetrain,whitewinefinalglm,K=10)$delta[1]



library(mgcv)
library(gamclass)
#white
whitewinesplines<-gam(quality~+s(volatile.acidity)+
                        s(residual.sugar)+s(chlorides)+s(free.sulfur.dioxide)+s(total.sulfur.dioxide)+
                        s(density)+s(pH)+s(sulphates)+s(alcohol),data=whitewinetrain)

summary (whitewinesplines1)
CVgam(quality~s(volatile.acidity)+
        s(residual.sugar)+s(chlorides)+s(free.sulfur.dioxide)+s(total.sulfur.dioxide)+
        s(density)+s(pH)+s(sulphates)+s(alcohol),data=whitewinetrain,nfold=10)


CVgam(quality~s(fixed.acidity)+s(volatile.acidity)+s(citric.acid)+
        s(residual.sugar)+s(chlorides)+s(free.sulfur.dioxide)+s(total.sulfur.dioxide)+
        s(density)+s(pH)+s(sulphates)+s(alcohol),data=whitewinetrain,nfold=10)




#red
redwinesplines<-gam(quality~s(fixed.acidity)+s(volatile.acidity)+s(chlorides)+s(free.sulfur.dioxide)+s(total.sulfur.dioxide)+
                      s(density)+s(pH)+s(sulphates)+s(alcohol),data=redwinetrain)

summary(redwinesplines)

CVgam(quality~s(fixed.acidity)+s(volatile.acidity)+s(chlorides)+s(free.sulfur.dioxide)+s(total.sulfur.dioxide)+
        s(density)+s(pH)+s(sulphates)+s(alcohol),data=redwinetrain,nfold=10)

#logistic

whitewinetrain$qualityf<-factor(whitewinetrain$quality)
redwinetrain$qualityf<-factor(redwinetrain$quality)

whitewinetrain$out<-relevel(whitewinetrain$qualityf,ref="3")
redwinetrain$out<-relevel(redwinetrain$qualityf,ref="3")

library(nnet)
whitewinemullogimodel<-multinom(out~fixed.acidity+volatile.acidity+citric.acid+
                                  residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
                                  density+pH+sulphates+alcohol,data=whitewinetrain)

redwinemullogimodel<-multinom(out~fixed.acidity+volatile.acidity+citric.acid+
                                residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+
                                density+pH+sulphates+alcohol,data=redwinetrain)


summary(whitewinemullogimodel)
summary(redwinemullogimodel)

#predict

prwhitewine<-predict(whitewinemullogimodel,whitewinetest)
prredwine<-predict(redwinemullogimodel,redwinetest)
library(caret)
confusionMatrix(prwhitewine, whitewinetest$quality)
confusionMatrix(prredwine, redwinetest$quality)
par(mfrow=c(2,2))
barplot(table(whitewinetest$quality),main="quality of white wine testing set")
barplot(table(prwhitewine),main="prediction of white wine testing set")
barplot(table(redwinetest$quality),main="quality of red wine testing set")
barplot(table(prredwine),main="prediction of red wine testing set")


# #misclassifacation error
whitewinecm<-table(prwhitewine,whitewinetest$quality)
print(whitewinecm)
redwinecm<-table(prredwine,redwinetest$quality)
print(redwinecm)
1-sum(diag(whitewinecm))/sum(whitewinecm)
1-sum(diag(redwinecm))/sum(redwinecm)

mean(prwhitewine==whitewinetest$quality)
nuwhitewine<-as.numeric(prwhitewine)+2
mean(abs(nuwhitewine - whitewinetest$quality))
prredwine
nuredwine<-as.numeric(prredwine)+2
nuredwine
mean(prredwine==redwinetest$quality)
mean(abs(nuredwine - redwinetest$quality))


library(randomForest)

#Tree tree
#white
library(tree)
whitewinetr<-tree(quality~.,data=whitewinetrain)
plot(whitewinetr)
text(whitewinetr,pretty=0)
summary(whitewinetr)

cv.treewhitewinetr=cv.tree(whitewinetr)
plot(cv.treewhitewinetr$size,cv.treewhitewinetr$dev,type="b")

whitewineprtr<-prune.tree(whitewinetr,best=5)

whitewineprtrpre<-predict(whitewineprtr,whitewinetest)
whitewineprtrpre
mean(abs(whitewineprtrpre - whitewinetest$quality))
whitewineprtrpre<-round(whitewineprtrpre)
whitewineprtrpre
whitewinetarget<-whitewinetest$quality
table(whitewineprtrpre, whitewinetarget)

#red
redwinetr<-tree(quality~.,data=redwinetrain)
plot(redwinetr)
text(redwinetr,pretty=0)
summary(redwinetr)

cv.treeredwinetr=cv.tree(redwinetr)
plot(cv.treeredwinetr$size,cv.treeredwinetr$dev,type="b")

redwineprtr<-prune.tree(redwinetr,best=9)

redwineprtrpre<-predict(redwineprtr,redwinetest)
mean(abs(redwineprtrpre - redwinetest$quality))
redwineprtrpre<-round(redwineprtrpre)
redwineprtrpre
redwinetarget<-redwinetest$quality
table(redwineprtrpre,redwinetarget)
#Decision Tree
library(rpart)
library(rpart.plot)

whitewinedetreem<-rpart(quality ~ ., data = whitewinetrain)
whitewinedetreem
rpart.plot(whitewinedetreem,main="white wine training set")

whitewinedetreepre<-round(predict(whitewinedetreem,whitewinetest,method="class"))
whitewinedetreepre
table(whitewinedetreepre,whitewinetest$quality)
mean(whitewinedetreepre==whitewinetest$quality)
mean(abs(whitewinedetreepre - whitewinetest$quality))



redwinedetreem<-rpart(quality ~ ., data = redwinetrain)
redwinedetreem
rpart.plot(redwinedetreem,main="red wine training set")

redwinedetreepre<-round(predict(redwinedetreem,redwinetest,method="class"))
redwinedetreepre
#table(redwinedetreepre,redwinetest$quality)
mean(redwinedetreepre==redwinetest$quality)
mean(abs(redwinedetreepre - redwinetest$quality))




#random forest
whitewinerf_model<-randomForest(quality~.,whitewinetrain,ntree=5000)
whitewinerf_model

whitewinepremodel<-round(predict(whitewinerf_model,whitewinetest))
whitewinepremodel
whitewinetarget<-whitewinetest[,12]
table(whitewinepremodel,whitewinetarget)
mean(whitewinepremodel==whitewinetest[,12])
mean(abs(whitewinepremodel - whitewinetest$quality))

importance(whitewinerf_model)
varImpPlot(whitewinerf_model)

redwinerf_model<-randomForest(quality~.,redwinetrain,ntree=5000)
redwinerf_model

redwinepremodel<-round(predict(redwinerf_model,redwinetest))
redwinepremodel
redwinetarget<-redwinetest[,12]
table(redwinepremodel,redwinetarget)
mean(redwinepremodel==redwinetest[,12])
mean(abs(redwinepremodel - redwinetest$quality))

importance(redwinerf_model)
varImpPlot(redwinerf_model)




library(class)
whitewinetraintarget<-whitewinetrain[,12]
whitewinetesttarget<-whitewinetest[,12]
kwhite<-round(sqrt(nrow(whitewine)))

whitewineknn<-knn(train=whitewinetrain,test=whitewinetest,cl=whitewinetraintarget,k=1)
whitewineknn

table(whitewineknn,whitewinetesttarget)
confusionMatrix(whitewineknn, whitewinetesttarget)

mean(whitewineknn==whitewinetesttarget)
wk<-as.numeric(whitewineknn)+2
wk
mean(abs(wk - whitewinetest$quality))

library(class)
redwinetraintarget<-redwinetrain[,12]
redwinetesttarget<-redwinetest[,12]
kred<-round(sqrt(nrow(redwine)))

redwineknn<-knn(train=redwinetrain,test=redwinetest,cl=redwinetraintarget,k=1)
redwineknn

table(redwineknn,redwinetesttarget)
confusionMatrix(redwineknn, redwinetesttarget)
mean(redwineknn==redwinetesttarget)
rk<-as.numeric(redwineknn)+2
rk
mean(abs(rk - redwinetest$quality))