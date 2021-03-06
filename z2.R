
n <- 100000 #liczba punkt�w
m <- 2 #�rednia
s <- 0.5 #odchylenie
a <- 0.5 #min
b <- 4 #max


rozklad <- rnorm(n, m, s) # losujemy n liczb, ich �rednia m, odchylenie standardowe s

h <- hist(rozklad) #histogram

x <- seq(a, b, .01) 
dn <- dnorm(x,m,s) # g�sto�� w pkt x, kt�ry ma �r. m i odchylenie s

plot(h$mids, h$density, xlim=c(0.5,3.5), ylim = c(0,max(dn) + 0.05), pch = 19, xlab="x", ylab="f(x)") #mind srodki denesity 
lines(x, dn, col = "red") 

