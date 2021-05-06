library(ggplot2)

data("mtcars")

mtcars
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am)<- c("Automatic", "Manual")


car<- ggplot(mtcars) 

kolor <- c(Manual="cyan",Automatic="pink")      #wektor koloru - ustawnienia - > linijê ni¿ej ustawiamy zmianê koloru fill, czyli wype³nienia 

z<-car + geom_point(aes(x = hp, y = qsec, fill = am, size = mpg), shape = 21) +scale_fill_manual(values=kolor)     #shape=21 ko³a,  22-kwadraty

final <- z + labs( x = "moc [KM]", y = "czas na 1/4 mili [s]", fill = "biegi") + ggtitle("Dane z magazynu Motor Trend (1974)")

final

