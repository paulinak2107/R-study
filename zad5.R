

#p-value


alfa <- 0.05
mu <- 23

plik = read.table("country_side.csv", header=T)
plik

#X <-plik[-1,]

# Proba
xx <-plik[,"WeightInitial"]
#xx <- as.numeric(x)
#xx
n <- length(xx)
# Pakiet R ma wbudowany test T-Studenta dla modelu II
wynik <- t.test(xx, alternative = "greater", mu=mu); wynik

p <- wynik$p.value
p
p> alfa


#################################################
#prawdopodobieñstwo

mu1 <- 24

moc <- power.t.test(n,delta=abs(mu1-mu),sd=sd(xx),sig.level = alfa, type="one.sample", alternative="one.side"); moc
prawdopodobienstwo <-1-moc$power
prawdopodobienstwo


#################################
#liczba pomiarów

moc <- power.t.test(delta=abs(mu1-mu), sd=sd(xx), sig.level = alfa, type="one.sample", alternative = "one.sided", power = 0.8); moc
liczba <- ceiling(moc$n)
liczba


##########################################################
#test jedej wariancji - pdf

#wartosc statystyki testowej
#zbior krytyczny

ch2 <- (n-1)*var(xx)/(20)
ch2
w1 <- qchisq(0.1/2,df=n-1);w1
w2 <- qchisq(1-0.1/2,df=n-1); w2

#wpada do przedzia³u 

