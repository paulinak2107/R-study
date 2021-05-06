library(car)
library(caret)
library(ROCit)
library(MASS)
library(carData)
library(Hmisc)
library(caret)
library(PPROC)


g<-Greene

#podzia³
set.seed(1234)
g.samples <- createDataPartition(g$decision, p = 0.75, list = FALSE)
g.train<-g[g.samples,]
g.test<-g[-g.samples,]

#regresja liniowa
g.lr<-glm(decision ~judge+location+language+nation+rater, data=g.train, family=binomial(link="logit"))
g.predict<-predict(g.lr, newdata=g.test, type = "response")
g.true <- g.test$decision
#g.result <- ifelse(g.predict>0.50, "yes", "no")


#ROC
rocit.obj <- rocit(score=g.predict,class=g.test$decision)
summary(rocit.obj)
plot(rocit.obj)




# punkt optymalny

best.yi.index <- which.max(rocit.obj$TPR-rocit.obj$FPR)
a<-best.cutoff <- rocit.obj$Cutoff[best.yi.index];a
best.tpr <- rocit.obj$TPR[best.yi.index]
best.fpr <- rocit.obj$FPR[best.yi.index]
sprintf("Best Cutoff = %.2f (TPR = %.3f, FPR = %.3f)", best.cutoff, best.tpr, best.fpr)


#macierz pomy³ek
g.result2 <- as.factor(ifelse(g.predict>a, "yes", "no"))

g.conf <- confusionMatrix(g.result2, g.true, positive = "yes")
g.conf

g.conf$byClass["F1"]

