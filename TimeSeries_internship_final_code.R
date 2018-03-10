
#####################################Data Exploration

data <- read.csv("c:/Users/karima/Desktop/my_work_TS/data_sorted.csv", sep=";",dec=",", head=TRUE,na.strings="N/A")
View(data)
str(data)
class(data)
start(data$Date)
end(data$Date)

colnames(data)<- c("Date","MY","Year","Month","consommation","importation","production",
                   "importation.PB","production.éléctricité.combustible","consom_eléctricité_HT",
                   "consom_eléctricité_MT","consom_eléctricité_BT","consom.eléctricité_tot")

###################################Data Visualisation
attach(data)
v=data$consommation
vv=ts(v,start=c(2010,1),end=c(2016,12),frequency=12)
vv
z=plot.ts(vv,xlab='Année',ylab='La consommation mensuelle',
          main="Evolution de la consommation mensuelle des produits pétroliers")
str(vv)

aggregate(vv)
aggregate(vv,FUN=mean)
plot(aggregate(vv))

boxplot(vv~cycle(vv))
boxplot(vv)

abline(reg =lm(vv~data$Date))

monthplot(vv,ylab="Consommation en Kt")

ggmonthplot(vv,ylab="Consommation en Kt",main="Monthplot")+theme(plot.title = element_text(hjust = 0.5))

decompose(vv, type = "multiplicative")
plot(decompose(vv, type = "multiplicative"))

acf(vv)

plot(decompose(diff(vv), type = "multiplicative"))



###################################Correlogramme
plot(Acf(vv,lag.max=20, plot=TRUE))
plot(Acf(diff(vv),lag.max=20))
autoplot(Acf(vv,lag.max=20, plot = TRUE),main="Series(vv)")
autoplot(Acf(diff(vv),lag.max=20, plot = TRUE),main="Series(diff_vv)")


pacf(vv,lag.max=30)
Pacf(diff(vv),lag.max=20)
autoplot(Pacf(vv,lag.max=30, plot = FALSE))
autoplot(Pacf(diff(vv), plot = FALSE),main="Series diff(vv)")
ggtsdiag(auto.arima(diff(vv)),size=0.8)



################################ Boxplot
tst <- data.frame(cbind(as.matrix(vv), as.matrix(cycle(vv))))
library(ggplot2)
ggplot(tst, aes(as.factor(tst[,2]), tst[,1])) +
  ylab("Consommation") + 
  xlab("Month") + 
  ggtitle("Dispersion par mois") + 
  stat_boxplot(geom ='errorbar')+ 
  geom_boxplot(outlier.shape = NA) +  
  geom_jitter(aes(colour=tst[,1]), position = position_jitter(width = 0.2)) +
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Consommation")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(),
        legend.title=element_text(), legend.key = element_rect(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))


########################### Stationnarity test 

library(forecast)
library(ggplot2) 
library(tseries)
adf.test(vv,alternative = c("stationary","explosive"))
adf.test(diff((vv)),alternative = c("stationary","explosive"))

kpss.test(vv, null="Trend")
kpss.test(diff((vv)), null="Trend")


install_github('sinhrks/ggfortify')
library(forecast)
library(devtools)
library(ggfortify)
library(timeSeries)
library(changepoint)
library(methods)

d.arima=autoplot(vv, ts.colour = ('dodgerblue3'),xlab='Date',ylab='Consommation des pdts pétroliers',
                 main="Evolution mensuelle de la consommation  des produits pétroliers",size=0.8)
d.arima+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))  +theme_bw() #+stat_smooth(method=lm, se=FALSE)


d.arima=autoplot(vv, ts.colour = ('dodgerblue3'),xlab='Date',ylab='Consommation des pdts pétroliers',
                 main="Méthode de la bande",size=0.8)
d.arima+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5))


class(d.arima)
g=autoplot(stl(vv, s.window = 'periodic'), ts.colour = 'blue',size=0.6)
g


############## Additionnal 
library(ggplot2)
library(gridExtra)
temp <- decompose(vv,type="multiplicative")
temp <- decompose(vv, type = c("additive", "multiplicative"))
tst <- data.frame( actual = as.matrix(temp$x[]), date = as.Date(temp$x),seasonal = as.matrix(temp$seasonal),
                   trend = as.matrix(temp$trend), random = as.matrix(temp$random), type = temp$type)

a <- ggplot(tst, aes(tst[,2], tst[,1]))+
  geom_line(size=0.8)+
  ylab("observed") + 
  xlab("") + 
  ggtitle(paste("Decomposition of",  tst$type, "time series", sep=" "))+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"), 
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))+
  theme(plot.title = element_text(hjust = 0.5))

b <- ggplot(tst, aes(tst[,2], tst[,3]))+
  geom_line(size=0.8)+
  ylab("seasonal") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

c <- ggplot(tst, aes(tst[,2], tst[,4]))+
  geom_line(size=0.8)+
  ylab("trend") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

d <- ggplot(tst, aes(tst[,2], tst[,5]))+
  geom_line(size=0.8)+
  ylab("random") + 
  xlab("") + 
  ggtitle("")+
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = mean(tst[,1]), name="Tooth Length")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"),
        plot.margin=unit(c(0,0.5,-0.4,0.5), "cm"))

grid.arrange(a,b,c,d, ncol = 1)


################################## Residuals
d.forecast <- forecast(vv, level = c(80,95), h = 50)
p=autoplot(d.forecast,predict.size=0.8,xlab='Date',ylab='Consommation en Kt',
           main="Prévision de la consommation mensuelle des produits pétroliers")

################################# Prediction
p+ggtitle("Prévision de la consommation mensuelle des produits pétroliers") +
  xlab('Time') + ylab("Consommation")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), legend.title=element_text(), legend.key = element_rect(colour = "black"))
#theme_bw() 
#autoplot(vv, ts.colour ='blue', ts.geom = 'bar', fill = 'blue',xlab='Date',ylab='Consommation des pdts pétroliers',
#main="Evolution de la consommation mensuelle des produits pétroliers")


################################# Regression
library(ggfortify)
autoplot(lm(consommation ~ importation+production+importation.PB+production.éléctricité.combustible+consom.eléctricité_tot, data = data), label.size = 3)

par(mfrow = c(1, 2))
m=lm(consommation ~ importation+production+importation.PB+production.éléctricité.combustible+consom.eléctricité_tot, data = data)
autoplot(m, which = 1:6, ncol = 3, label.size = 3)+ theme_bw()


######################### Seasonnality
library(ggplot2)
library(forecast)
theme_set(theme_classic())


############ Seasonnal Plot
ggseasonplot(vv) + labs(title="méthode du profil")+geom_line(size=1)+
  theme(plot.title = element_text(hjust = 0.5))

# 
# ggplot(vv, aes(x=Month, y=consommation, col=Year)) +
# geom_line(size=2) + theme_bw() + theme(legend.position="top") +
# scale_color_manual(values=c("#1f78b4", "#ff7f00", "#33a02c", "#6a3d9a",
#                    "#e31a1c", "#b15928", "#a6cee3", "#fdbf6f", "#b2df8a",
#                    "green","lightblue","red"))



library(ggseas)
library(seasonal)
library(ggplot2)

ap_df <- tsdf(vv)
ggsdc(ap_df, aes(x =x, y =y), method = "seas") + 
  geom_line(size=0.7)+ylab("") + xlab("Year")

############ Seasonal adjustment
#if stat_seas works it will generally be better
ggplot(ap_df, aes(x = x, y = y)) +
  geom_line(colour = "grey50") +
  stat_seas(colour = "blue") +
  stat_stl(s.window = 7, colour = "red")+
  ylab("Consommation en Kt") + xlab("Year")


stlconsom= stl(vv,s.window="periodic")
plot(stlconsom)
tsTconsomWithoutSeasonal = stlconsom$time.series[,2] + stlconsom$time.series[,3]
plot(tsTconsomWithoutSeasonal)


temp <- decompose(vv, type = "multiplicative")
tst <- data.frame( actual = as.matrix(temp$x[]), date = as.Date(temp$x),seasonal = as.matrix(temp$seasonal),
                   trend = as.matrix(temp$trend), random = as.matrix(temp$random), type = temp$type)

#detrended_a = actual - trend
#detrended_m = actual/ trend


library(seasonal)
library(tidyr)
library(dplyr)
library(showtext)
library(ggplot2)
library(scales)

mod <- seas(vv)

plot(mod)
plot(resid(mod), main = "Residuals")
qqnorm(resid(mod), main = "Residuals compared to Normal")
pacf(resid(mod), "Partial ACF of residuals")

summary(mod)

############### Plot after seasonnal adjustment

apparel_sa <- data_frame(Time = time(vv),Original =vv,SA = final(mod))
apparel_sa %>%
  gather("variable", "value", -Time) %>% 
  mutate(variable = gsub("SA", "Seasonally adjusted by SEATS", variable)) %>%
  ggplot(aes(x = Time, y = value, colour = variable)) +
  geom_line() +
  labs(colour = "", x = "") +
  scale_y_continuous("Consommation en Kt") +
  ggtitle("Evolution dans le temps ") +
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",se=FALSE, formula = y1~x)

####################### Model Detection & Forecast

fit <-auto.arima(diff(vv), d =1, D =0, max.p = 2, max.q = 1,max.P = 1, max.Q = 1,
                 stationary = TRUE, ic ="aic",stepwise=FALSE,trace=FALSE,
                 test = c("kpss", "adf", "pp"),seasonal.test = c("ocsb", "ch"))

#c("aic", "aicc", "bic")
summary(fit)

d <- diff(vv)
s=diffinv(d, xi = 1)
plot(forecast(s,level = c(80, 95)))

d.forecast <- forecast(s, level = c(80,95))

p=autoplot(d.forecast,predict.size=0.8,xlab='Time',ylab='Consommation en Kt',
           main="Prévision de la consommation mensuelle des produits pétroliers",is.date = FALSE)
p+ geom_forecast(h=36, level=c(50,80,95))

p+ scale_x_discrete(name ="Time", limits=c("2010","2012","2015","2017"))

p+ggtitle("Prévision de la consommation mensuelle des produits pétroliers") +
  xlab('Time') + ylab("Consommation")+
  theme(axis.line = element_line(), axis.text=element_text(color='black'), 
        axis.title = element_text(colour = 'black'), legend.text=element_text(), 
        legend.title=element_text(), legend.key = element_rect(colour = "black"))
#theme_bw()

library(magrittr)
library(ggplot2)
autoplot(d.forecast,mapping = NULL,predict.size=0.8,xlab='Date',ylab='Consommation en Kt',
         main="Prévision de la consommation mensuelle des produits pétroliers") +
  geom_forecast(d.forecast, show.legend=FALSE) +
  labs(x="Time", y="Consommation en Kt", "Forecasts from ARIMA model")


################################# Model validation
library(forecast)  
Acf(residuals(fit))
Box.test(residuals(fit), lag=24, fitdf=4, type="Ljung-Box")
#Donc les résidus ne sont pas autocorrélés.
library(nortest)
ad.test(fit$resid) #Les résidus sont normaux.
library(normtest)
library(tseries)
jarque.bera.test(res) # test de normalité des résidus
library(ggfortify)
ggtsdiag(fit, gof.lag = 20, conf.int = TRUE,size=0.8)
qqnorm(fit$residuals, asp = 1)
qqline(fit$residuals, asp=1)
#Since the correlogram shows that none of the sample autocorrelations
#for lags 1-20 exceed the significance bounds, and the p-value for the Ljung-Box
#test is 0.7, we can conclude that there is very little evidence for non-zero
#autocorrelations in the forecast errors at lags 1-20.

#Box.test : H0: The data are independently distributed (i.e. the correlations
#in the population from which the sample is taken are 0, so that any observed 
#correlations in the data result from randomness of the sampling process).

res <- residuals(fit)
hist(res, breaks="FD", xlab="Residuals", 
     main="Histogram of residuals", ylim=c(0,20))
x <- -50:50
lines(x, 560*dnorm(x,0,sd(res)),col=2)

library(tsoutliers)
outliers <- tso(vv,types = c("AO","LS","TC","SLS"))
outliers
plot(outliers)



##########################################################################################################################
x1=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
y1=vv[c(max)]

#Find local minima and maxima
find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
max=find_peaks(as.vector(vv), m = 1)
vv[c(max)]
min=find_peaks(-1*(as.vector(vv)), m = 1)
vv[c(min)]
abline(vv[c(max)],vv[c(min)],col="purple",lty=3,lwd=3)
plot(vv[c(max)],type = "o", col = "red", xlab = "Month", ylab = "Rain fall",
     main = "Rain fall chart")
plot(vv[c(min)],type = "o", col = "red", xlab = "Month", ylab = "Rain fall",
     main = "Rain fall chart")

x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
y <- vv[c(max)] # plot scatterplot and the regression line
my.formula <- lm(y ~ x)
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2)

x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
y <- vv[c(min)] # plot scatterplot and the regression line
mod1 <- lm(y ~ x)
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2)

lines(boneMaleSmooth$x,boneMaleSmooth$y+error90_male, col="purple",lty=3,lwd=3)

#2éme méthode
localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

x <- as.vector(vv)
localMaxima(x) 

# Apply a locally weighted regression
p + stat_smooth(method = "loess", formula = y ~ x, size = 1)


##############################################################################################################################

