#install.packages("car")
library("car")
data("iris")
iris

levels(iris$Species)

SL <-iris$Sepal.Length
SW <-iris$Sepal.Width
PL <-iris$Petal.Length
PW <-iris$Petal.Width


#sprawdzenie normalno�ci
SL.norm<-shapiro.test(SL); SL.norm; SL.norm$p>0.05; SLpvalue <- SL.norm$p  #p<0,05 odrzucamy H0
SW.norm<-shapiro.test(SW); SW.norm; SW.norm$p>0.05; SWpvalue <- SW.norm$p  #p>0,05 nie ma podstaw do odrzucenia H0
PL.norm<-shapiro.test(PL); PL.norm; PL.norm$p>0.05; PLpvalue <- PL.norm$p  #p<0,05 odrzucamy H0
PW.norm<-shapiro.test(PW); PW.norm; PW.norm$p>0.05; PWpvalue <- PW.norm$p  #p<0,05 odrzucamy H0
 

#test r�wno�ci wariancji mi�dzygrupowej
leveneTest(Sepal.Length ~ Species, iris) #p=0.002   p<0.05  odrzucamy H0
leveneTest(Sepal.Width ~ Species, iris)  #p=0.56    p>0,05  nie ma podstaw do odrzucenia H0
leveneTest(Petal.Length ~ Species, iris) #p<0.0001  p<0.05  odrzucamy H0
leveneTest(Petal.Width ~ Species, iris)  #p<0.0001  p<0.05  odrzucamy H0

#ANOVA
iris.aov <- aov(Sepal.Width~Species, iris)
summary(iris.aov)
#p=2e-16 P-value wynosi <0.0001, co oznacza, �e mo�na odrzuci� hipotez� zerow� o tym, �e 
#�rednie w grupach s� takie same

#wykres pude�kowy
boxplot(Sepal.Width~Species, iris, col=c("blue", "green", "red"))

