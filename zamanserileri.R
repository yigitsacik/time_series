rm(list = ls())

library(fpp)
library(forecast)
library(haven)
library(stats)
library(ggplot2)
library(readxl)

data <- AirPassengers

#Veri zaman serisi olarak tanimlandi


data_ts<-ts(data,frequency = 12)
ts.plot(data_ts,gpars = list(xlab="Time",ylab="Passengers"),lwd=2)

#ACF VE PACF grafiklerine bakildi.
Acf(data_ts,lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(data_ts,lag.max = 42, ylim=c(-1,1), lwd=3)

Acf(diff(data_ts,12),lag.max = 42,  ylim=c(-1,1), lwd=3)

##########AYRIŞTIRMA MODELLERİ##########
##########TOPLAMSAL AYRIŞTIRMA
#TOPLAMSAL AYRIŞTIRMA
#Zaman Serisi Grafiğine baktığımızda mevsimsellikten şüphe ediyoruz.
#ACF grafiğine baktığımızda da emin oluyoruz ki mevsimsellik var trend yok
#toplamsal ayrıştırma yaparken seride olmayan bileşenin etkisi 0'dır.
library(fpp)
#periyot=12
#Trent eğilimini belirlemek için seriyi “merkezsel hareketli ortalama” metodunu kullanırız.
#serimizde trent olmadığı için trenti 0 kabul ederiz.
#orj. serimizde trent mevcut değil.
#periyot
periyot<- data_ts
periyot

#serinin periyoduna sahip mevsimsel bilesen serisinin ACF grafiginden periyodu bulalim:
Acf(periyot,lag.max = 42,  ylim=c(-1,1), lwd=2,col="lightpink")
#periyot=12
#Merkezsel Hareketli Ortalama hesabi
ma_ts<- ma(data_ts, order = 12, centre = TRUE)  #germe sayisi=12

#Mevsimsel bilesenin bulunusu (hata terimi de mevcut)
Mevsim<- data_ts-ma_ts
Mevsim

#Mevsim serisinin ortalamalari
donemort<-t(matrix(data=Mevsim,nrow=12))

colMeans(donemort, na.rm = T)
sum(colMeans(donemort, na.rm = T))
mean(colMeans(donemort, na.rm = T))
#mevsimsel endeks degerlerinin bulunusu
endeks<- colMeans(donemort, na.rm = T)-mean(colMeans(donemort, na.rm = T))
endeks

#endeks degerlerini seri boyunca yazdirma islemi
indeks<-  matrix(data = endeks, nrow =105)
indeks #mevsimsel endeks serisi
#extra olarak indeks degerlerini seri boyunca yazdirmak istersek
indeks_alternatif<- decompose(data_ts, "additive")

#trent bileseni bulalım (hata terimi de mevcut)
trenthata<-data_ts-indeks

#seriyi hatadan arindirmak icin trenthata serisine dogrusal regresyon islemi uygulanir.
trent<-tslm(trenthata~trend)
#ciktida yer alan fitted values orijinal serinin trent bilesenidir. 

#tahmin serisini bulalim: (mevsimsel endeks+saf trent serisi)
tahmin<- indeks+trent[["fitted.values"]]

#hata serisini bulalim:
hata<- data_ts-indeks-trent[["fitted.values"]]
######Modelin Guvenilirligi######
#Toplamsal modelin ele alinan seri uzerinde gecerli bir model olup olmadigini kontrol edelim
#(yani tahminleri guvenilir mi???)

#orijinal seri ile tahmin serisinin uyumu

plot( window(data_ts), 
      xlab="Zaman", ylab="",lty=1, col="blue", lwd=2)
lines( window(tahmin) ,lty=3,col="orange",lwd=3)
legend("topright",c(expression(paste(data)),
                    expression(paste(Tahmin ))),
       lwd=c(2,2),lty=c(1,3), cex=0.5, col=c("blue","orange"))

#hatalar akgurultu mu?

Acf(hata,main="Hata", lag.max = 42, ylim=c(-1,1))
Pacf(hata,main="Hata",lag.max = 42, ylim=c(-1,1))
Box.test(hata, lag = 42, type = "Ljung")

##########ÇARPIMSAL AYRIŞTIRMA
#mevsimsel bileseni bulunmasi (Zt/MHO) (hata terimi de mevcut)
Mevsim1 <- data_ts/ma_ts

#her bir periyot icin ortalama degerlerinin hesabi
donemort1<-t(matrix(data=Mevsim1, nrow =12))
colMeans(donemort1, na.rm = T)
#toplam
sum(colMeans(donemort1, na.rm = T))
#ortalamalarin ortalamasi
mean(colMeans(donemort1, na.rm = T))
#mevsimsel endeks degerlerinin bulunusu
endeks1<- colMeans(donemort1, na.rm = T)/mean(colMeans(donemort1, na.rm = T))
#mean(endeks1)=1?
#endeks degerlerini seri boyunca yazdirma islemi
indeks1<-  matrix(data = endeks1, nrow = 105)
#trent serisi (hata da mevcut) (orijinal seri/mevsimsel endeks serisi)
trenthata1<- data_ts-indeks1
#hatadan arindirma islemi
trent1<- tslm(trenthata1~trend)
tahmin1<- indeks1*trent1[["fitted.values"]] #tahmin=endeks*orijinal serinin trent bileseni
#hata serisi
hata1<- data_ts-tahmin1
#orijinal seri ile tahmin serisinin uyumu
plot( window(data_ts), 
      xlab="Zaman", ylab="",lty=1, col="midnightblue", lwd=2)
lines( window(tahmin1) ,lty=3,col="mediumaquamarine",lwd=3)
legend("topleft",c(expression(paste(veri )),
                   expression(paste(Tahmin ))),
       lwd=c(2,2),lty=c(1,3), cex=0.6, col=c("midnightblue","mediumaquamarine"))

#hatalar akgurultu mu?
Acf(hata1,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=2,col="plum1")
Pacf(hata1,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=2,col="plum2")
Box.test(hata1, lag = 42, type = "Ljung")


##########REGRESYON ANALIZI##########
length(data)
#144 gozlem var 

t<-1:1:length(data)
length(t)

##########TOPLAMSAL MODEL

sin1<-sin(2*3.1416*t/12)
cos1<-cos(2*3.1416*t/12)
data_j_1<-as.data.frame(cbind(data, t, sin1, cos1))
names(data_j_1)<- c("y", "t", "sin1", "cos1")
attach(data_j_1)
regresyon.model1<-lm(y~t+sin1+cos1)
summary(regresyon.model1)
dwtest(y~t+cos1)

#1. model icin tahmin serisi, hata serisi ve tahminin alt ve üst sınırlarına ait seriler

tahmin1<-predict(regresyon.model1)
sinir1<-predict(regresyon.model1, interval = 'confidence' ,level = .95)
hata1<-resid(regresyon.model1)
plot( window(y),
      xlab="", ylab="", type="l", lty=3, col=2, lwd=2)
lines(window(sinir1[,2]) ,type="l",lty=1,col=4,lwd=2)
lines(window(sinir1[,3]) ,type="l",lty=1,col=3,lwd=2)
legend("topleft",c(expression(paste(data)),
                   expression(paste(Altsinir)),
                   expression(paste(Üstsinir))),
       lwd=c(2,2,2),lty=c(3,1,1), cex=0.7, col=c(2,4,3))

#Hatalar akgurultu mu?
Acf(hata1,lag.max = 42,  ylim=c(-1,1), lwd=3)
Box.test(hata1, lag = 42, type = "Ljung")

##########CARPIMSAL MODEL
sin1<-t*sin(2*3.1416*t/12)
cos1<-t*cos(2*3.1416*t/12)

data_t_j_1<-as.data.frame(cbind(data, t, sin1, cos1))
names(data_t_j_1)<- c("y", "t", "sin1", "cos1")
attach(data_t_j_1)

regresyon.model2<-lm(y~t+sin1+cos1)
summary(regresyon.model2)

dwtest(y~t+cos1)
tahmin2<-predict(regresyon.model2)
sinir2<-predict(regresyon.model2, interval = 'confidence' ,level = .95)
hata2<-resid(regresyon.model2)

plot( window(y),
      xlab="", ylab="", type="l", lty=3, col=2, lwd=2)
lines(window(sinir1[,2]) ,type="l",lty=1,col=4,lwd=2)
lines(window(sinir1[,3]) ,type="l",lty=1,col=3,lwd=2)
legend("topleft",c(expression(paste(data)),
                   expression(paste(Altsinir)),
                   expression(paste(Üstsinir))),
       lwd=c(2,2,2),lty=c(3,1,1), cex=0.7, col=c(2,4,3))

#Hatalar akgurultu mu?
Acf(hata2,lag.max = 42,  ylim=c(-1,1), lwd=3)
Box.test(hata2, lag = 42, type = "Ljung")

##########USTEL DUZLESTIRME YONTEMI##########
##########TOPLAMSAL WINTERS YONTEMI

Winters1<- ets(data_ts, model = "AAA")
summary(Winters1)
tahmin1<- Winters1[["fitted"]]



plot( window(data_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin1) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(Orjinalseri)),
                   expression(paste(Winters1Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

hata2<- Winters1[["residuals"]]
Box.test (hata2, lag = 42, type = "Ljung")

Acf(hata2,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata2,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

checkresiduals(Winters1, lag = 42)
ongoru <- forecast(Winters1,h=5)
ongoru[["mean"]]

##########CARPIMSAL WINTERS YONTEMI

Winters2<- ets(data_ts, model = "MAM")
summary(Winters2)
tahmin1<- Winters2[["fitted"]]

plot( window(data_ts), 
      xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin1) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(Orjinalseri)),
                   expression(paste(Winters2Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))


hata1<- Winters2[["residuals"]]
Box.test (hata1, lag = 42, type = "Ljung")

Acf(hata1,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata1,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

ongoru <- forecast(Winters2,h=5)
ongoru[["mean"]]

data <- as.data.frame(ongoru[["mean"]])
data <- cbind(c("1972-10","1972-11","1972-12","1973-01","1973-02"),data)
names(data) <- c("Donem","Ongoru")
data

##########BOX JENKINS MODELLERİ##########
Acf(data_ts,lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(data_ts,lag.max = 42, ylim=c(-1,1), lwd=3)

Acf(diff(data_ts,12),lag.max = 42, ylim=c(-1,1), lwd=3)
Pacf(diff(data_ts,12),lag.max = 42, ylim=c(-1,1), lwd=3)

data_diff<-diff(data_ts,12)

#P=0 Q=0
data_arima1 <- Arima(data_ts, order = c(1,0,0), seasonal= c(0,1,0), include.constant=TRUE)
coeftest(data_arima1)
summary(data_arima1)
#P=1 Q=0
data_arima2 <- Arima(data_ts, order = c(1,0,0), seasonal= c(1,1,0), include.constant=TRUE)
coeftest(data_arima2)
summary(data_arima2)
#P=2 Q=0
data_arima3 <- Arima(data_ts, order = c(1,0,0), seasonal= c(2,1,0), include.constant=TRUE)
coeftest(data_arima3)
summary(data_arima3)
#P=0 Q=1
data_arima4 <- Arima(data_ts, order = c(1,0,0), seasonal= c(0,1,1), include.constant=TRUE)
coeftest(data_arima4)
summary(data_arima4)
#P=0 Q=2
data_arima5 <- Arima(data_ts, order = c(1,0,0), seasonal= c(0,1,2), include.constant=TRUE)
coeftest(data_arima5)
summary(data_arima5)
#P=1 Q=1
data_arima6 <- Arima(data_ts, order = c(1,0,0), seasonal= c(1,1,1), include.constant=TRUE)
coeftest(data_arima6)
summary(data_arima6)
#P=1 Q=2
data_arima7 <- Arima(data_ts, order = c(1,0,0), seasonal= c(1,1,2), include.constant=TRUE)
coeftest(data_arima7)
summary(data_arima7)
#P=2 Q=2

tahmin<- data_arima2 [["fitted"]]
hata<- data_arima2 [["residuals"]]

plot(window(data_ts), 
     xlab="Zaman", ylab="",lty=1, col=4, lwd=2)
lines( window(tahmin) ,lty=3,col=2,lwd=3)
legend("topleft",c(expression(paste(data)),
                   expression(paste(Tahmin))),
       lwd=c(2,2),lty=c(1,3), cex=0.7, col=c(4,2))

Acf(hata,main="Hata", lag.max = 42,  ylim=c(-1,1), lwd=3)
Pacf(hata,main="Hata",lag.max = 42, ylim=c(-1,1), lwd=3)

Box.test (hata, lag = 42, type = "Ljung")

ongoru<-forecast(data_arima2 , h=5)
ongoru[["mean"]]

data <- as.data.frame(ongoru[["mean"]])
names(data) <- c("Donem","Ongoru")
data