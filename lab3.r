library(fields)
library(Hmisc)


data("ChickWeight")
ChickWeight

ramka <-data.frame(ChickWeight)
ramka2 <- ramka[which(ramka$Diet==1),]
ramka2
t <-ramka2$Time
m <- ramka2$weight
plot(t, m, xlab="dzien", ylab="waga")

stats.bin(t,m)
xy.sb <- stats.bin(t, m)
with(xy.sb, points(centers, stats["mean",], col="red", pch=19, cex=1.5))

xy.lm <- lm(xy.sb$stats[2,] ~ xy.sb$center)
summary(xy.lm)
xy.lm$coefficients

b <- xy.lm$coefficients[1]
a <- xy.lm$coefficients[2]

lines(t, a*t+b, col="blue", lwd=2)

