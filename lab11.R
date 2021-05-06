library(ggplot2)
library(car)
library(MASS)
library(datasets)
library(caret)
library(klaR)
data("iris")


iris.scaled<-scale(iris[,-5])
iris2<-data.frame(iris.scaled,Species=iris$Species)
iris<-iris2
 iris2.lda <- lda(Species ~. , data = iris2)
 iris2.lda
 
 
 proj <- as.matrix(iris2[,-5]) %*% iris2.lda$scaling
 iris2.proj <- data.frame(proj, klasy=iris2[,5])
 g <- ggplot(iris2.proj, aes(x=LD1, y=LD2))
 g + geom_point(aes(color=klasy), size=3)

 
 iris2.lda.cv <- train(Species ~., data = iris2, method="lda", trControl = trainControl(method = "cv", number=10))
 iris2.lda.cv
 