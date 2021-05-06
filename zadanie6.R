dane = read.table("http://www.if.pw.edu.pl/~paluch/MSR/zad6.txt")

dane
d <-dane$V1
st <- shapiro.test(d)
st
st$p >0.05

##### p=6,624e-15   <0.05  odrzucamy



#######################################################

h <- hist(d)
chh <-chisq.test(h$counts, p = dpois(h$mids, lam = 4),rescale.p = TRUE)
chh
##### p<2.2e-16  odrzucamy


############################################
c<-ks.test(d, "pexp", 0.25);
c


#### p=0.936   nie ma podstaw do odrzucenia

##################################################

s <- seq(0,25, length.out = 100)
plot(h$mids, h$density, pch=19 , ylab="p(x)", xlab="x")
lines(s,dexp(s, 0.25), col="blue", lwd=2)


