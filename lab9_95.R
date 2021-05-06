library(carData)
library(ggplot2)
library(car)

pensja <- Salaries
pensja

pensja$rank <- as.factor(pensja$rank)

pensja$sex <- as.factor(pensja$sex)
pensja$sex

pensja.Female <- pensja[pensja$sex=="Female",]
pensja.Female

pensja.Male <- pensja[pensja$sex=="Male",]
pensja.Male




table(pensja$rank, pensja$discipline, pensja$sex)

theme_set(theme_bw())
g <- ggplot(pensja.Male)
g + geom_boxplot(aes(x=rank, y=salary, color=discipline)) + labs(x="rank", y="salary", color="discipline")+  ggtitle("Zarobki profesorów - mê¿czyzn w USA")


theme_set(theme_bw())
g <- ggplot(pensja.Female)
g + geom_boxplot(aes(x=rank, y=salary, color=discipline)) + labs(x="rank", y="salary", color="discipline")+  ggtitle("Zarobki profesorów - kobiet w USA")


#pensja.aov <- aov(salary ~ sex+rank+discipline:salary, data=pensja)
pensja.aov <- aov(salary ~ sex*rank*discipline, data=pensja)
summary(pensja.aov)


pensja.aov1 <- aov(salary ~ sex*rank*discipline, data=pensja)
summary(pensja.aov1)


Anova(pensja.aov, type="III")
Anova(pensja.aov1, type="III")

