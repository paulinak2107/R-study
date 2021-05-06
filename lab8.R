library(MASS)

tr<-Pima.tr
te<-Pima.te

tr.lr <- glm(type~npreg+glu+bp+skin+bmi+ped+age, data=tr, family=binomial(link = "logit"))
#tr.lr <- glm(type~., data=tr, family=binomial(link = "logit"))
summary(tr.lr)


pima.predict <- predict(tr.lr, te, type="response")
# glu, ped s¹ statystycznie istotne


pima.result<-ifelse(pima.predict < 0.5, 0, 1)
pima.true<-ifelse(te$type == "Yes", 1, 0)

pima.accuracy <- 1-mean(abs(pima.true-pima.result))
pima.accuracy

mean(pima.true)

