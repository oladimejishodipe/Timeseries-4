# Install R packages and invoke them with library function:
#You don't need all packages I have here. But it does not hurt if you install all
install.packages("rio")
library(openxlsx)
library(dplyr)
library(rio)
install.packages('extrafont')
install.packages("ggplot2")

library(extrafont)
library("ggplot2")
library(dplyr)
font_import()
install.packages('forecast', dependencies = TRUE)
install.packages("tidyverse")
library(rugarch)
library(fGarch)
library(forecast)
library(tidyverse)
library(tstools)
library(broom)
library(extrafont)
loadfonts()
font_install("fontcm")
library(fpp2)


# Here, I read in the data I  uploaded  to the Global Environment
enerP.raw <- read.csv("HW1.csv",
                       header=TRUE)

# Here, I turned the data in to time-series
enerP<- ts(enerP.raw[,2], start=c(1990,1),
             frequency=12)


# This is not useful: windowsFonts(A = windowsFont("Times New Roman"))  # Specify font

#Here, I plot the time-series data
plot(enerP, col="red",
     main="US Monthly Retail Price of Electricity",
     sub="Fig.1.1",
     xlab="Monthly",
     ylab="Electric Price",
     col.axis = "black",
     family = "Times New Roman"
     )

#Here, I ran AR(1)
enerP.ar1 <- arima(enerP, order=c(1,0,0))

# I generated the coeficient of the AR(1) model
enerP.ar1$coef

#  The coef. =   ar1 &  intercept    = 0.9897578  & 8.9645024

#stationary test- Null is the series stationary
Box.test((enerP), lag=10, type="Ljung-Box" )
#data:  (enerP)
#X-squared = 3233.5, df = 10, p-value < 2.2e-16  We reject the null

#Differencing my data
denerP<-diff(enerP)
plot(denerP, col="black",
     main="Differenced Retail Price of Electricity",
     xlab="Monthly",
     ylab="Price (Difference)",
     col.axis = "black")
abline(h=0, col="darkgrey", lty=3,lwd=3)

# Not needed: windowsFonts(A = windowsFont("Times New Roman"))  # Specify font

# I plot the difference data
plot1<-plot(denerP, col="red",
     main="US Monthly Retail Price of Electricity \n Difference",
     sub="Fig.1.2",
     xlab="Monthly",
     ylab="D.[Electric Price]",
     col.axis = "black",
     family = "Times New Roman"
)
# I an horizontal line on the graph
abline(h=0, col="darkgrey", lty=3,lwd=3)


#acf(denerP)   --- Not Needed
#pacf(denerP)   - Not Needed


# I ran the AR(1) again
denerP.ar1 <- arima(denerP, order=c(1,0,0))
denerP.ar1$coef
#ar1 &  intercept   0.23559646 & 0.01043918 - Now ar1 coef is small- now stationary and can be confirmed by
#Box.test((denerP), lag=10, type="Ljung-Box" ) #- I don't know still reject. but the arima value is small and the
#the distribution is mean reverting...

#Outlier - 2006, June, 2008, June, 2021 Feb and March.

#AR(1) Model
denerP.ar1 <- arima(denerP, order=c(1,0,0))
#class(denerP.ar1)
#names(denerP.ar1)
denerP.ar1$coef
#ar1 &  intercept   0.23559646 & 0.01043918


#ARtoMA = I converted AR to MA
denerPtoma <- ARMAtoMA(ar=coef(denerP.ar1)[1],
                   lag.max=30)

# I plot the MA  - IRF
plot(denerPtoma, col=1,
     main="Shock to Retail Price of Electricity",
     xlab="monthly",
     ylab="Shock Effect",
     col.axis = "black")
lines(denerPtoma,
      col="red")

# Not needed: windowsFonts(A = windowsFont("Times New Roman"))  # Specify font
# Still the same plot: I don't why I put this twice, but it is nice I leave, it makes me feel good that I running a lot of codes (ahh ahhh)
plot(denerPtoma, col="red",
     main="Shock to Retail Price of Electricity",
     sub="Fig.1.3",
     xlab="Monthly",
     ylab="Shock Effect",
     col.axis = "black",
     family = "Times New Roman"
)

# I impose line on my graph
lines(denerPtoma,
      col="black")
abline(h=0, col="darkgrey", lty=3,lwd=3)



#MA (1) Model - I ran MA and converted it to AR
denerP.ma1 <- arima(denerP, order=c(0,0,1))
denerP.ma1
#MAtoACFto AR(1)
denerPacf <- ARMAacf(ma=coef(denerP.ma1)[1], lag.max=30)
denerPtoAR1 <- acf2AR(denerPacf)[30,]
denerPtoAR1


# Here, I ran all candidata model --- from line 146- 230
denerP.crit <- function(p,q) {
   denerP.fit <- arima(denerP, order=c(p,0,q))
   return(c(aic=AIC(denerP.fit), sic=BIC(denerP.fit)))
}


l1<- denerP.crit(1,1)
l2<- denerP.crit(1,2)
l3<- denerP.crit(1,3)
l4<- denerP.crit(1,4)
l5<- denerP.crit(1,5)
l6<- denerP.crit(1,6)
l7<- denerP.crit(1,7)
l8<- denerP.crit(1,8)
l9<- denerP.crit(1,9)
l10<- denerP.crit(1,11)
l11<- denerP.crit(1,12)

l12<- denerP.crit(2,1)
l13<- denerP.crit(2,2)
l14<- denerP.crit(2,3)
l15<- denerP.crit(2,4)
l16<- denerP.crit(2,5)
l17<- denerP.crit(2,6)
l18<- denerP.crit(2,7)
l19<- denerP.crit(2,8)
l20<- denerP.crit(2,9)
l21<- denerP.crit(2,10)
l22<- denerP.crit(2,11)
l23<- denerP.crit(2,12)



l24<- denerP.crit(3,1)
l25<- denerP.crit(4,1)
l26<- denerP.crit(5,1)
l27<- denerP.crit(6,1)
l28<- denerP.crit(7,1)
l29<- denerP.crit(8,1)
l30<- denerP.crit(9,1)
l31<- denerP.crit(10,1)
l32<- denerP.crit(11,1)
l33<- denerP.crit(12,1)
l34<- denerP.crit(3,2)
l35<- denerP.crit(4,2)
l36<- denerP.crit(5,2)
l37<- denerP.crit(6,2)
l40<- denerP.crit(9,2)
l41<- denerP.crit(10,2)
l42<- denerP.crit(11,2)
l43<- denerP.crit(12,2)
l46<- denerP.crit(6,3)
l47<- denerP.crit(6,3)
l50<- denerP.crit(9,3)
l53<- denerP.crit(12,3)
l54<- denerP.crit(3,4)
l55<- denerP.crit(3,5)
l58<- denerP.crit(3,8)
l61<- denerP.crit(3,11)
l62<- denerP.crit(12,4)
l63<- denerP.crit(4,5)
l64<- denerP.crit(4,8)
l65<- denerP.crit(4,10)
l66<- denerP.crit(4,11)
l67<- denerP.crit(4,12)



Info.Crit<- data.frame(l1, l2, l3, l4,
                      l5, l6, l7, l8,
                      l9, l10, l11, l12,
                      l13, l14, l15, l16,
                      l17, l18, l19, l20,
                      l21, l22, l23, l24,
                      l25, l26, l27, l28,
                      l29, l30, l31, l32,
                      l33, l34, l35, l36,
                      l37,  l40,
                      l41, l42, l43,
                      l46, l47,
                      l50, l54, l53, l55,
                      l58, l61,l62,
                      l63,l64,l65,l66,l67)


Info.CritT<-t(Info.Crit)
Info.CritT


#l53 is the best model with lowest aic and sic= -384.43 & -317.63, from here I will be working with l53 model

#----------------------------------------------------------------------------------------------------------
# The line from #239 to 246 not relevant.
#I generated some numbers, so I could not plot them directly, I took to excel and re-imported them.
denerPPP.raw <- read.csv("HW222.csv",
                      header=TRUE)
denerPF<- ts(denerPPP.raw[,2], start=c(1990,2),
           frequency=12)

denerPF1<- ts(denerPPP.raw[,3], start=c(1990,2),
             frequency=12)

#----------------------------------------------------------------------------------------------------------

# Based on the AIC--- This is my candidate model. I used it to predict for additional 12 periods after the sample
FdenerP.fit <- arima(denerP, order=c(12,0,3))
FdenerP.fit
ForecastdenerP<-predict(FdenerP.fit,12 , enerppre=newener)
ForecastdenerP

# This plot is not needed
plot(denerPF, col="red",
main="12 Month Forecast of Changes of RPE \n June021 - May 2022",
sub="Fig.1.4",
xlab="Monthly",
ylab="D.[Electric Price]",
col.axis = "black",
family = "Times New Roman")
lines(denerPF1, col="blue", pch=9)
abline(h=0, col="darkgrey", lty=3,lwd=3)


# I calculate two month forecast by hand - this code run line 270 to 305
handprediction1 <-
   (denerP[376]-FdenerP.fit$coef[16])*FdenerP.fit$coef[1] +
   (denerP[375]-FdenerP.fit$coef[16])*FdenerP.fit$coef[2] +
   (denerP[374]-FdenerP.fit$coef[16])*FdenerP.fit$coef[3] +
   (denerP[373]-FdenerP.fit$coef[16])*FdenerP.fit$coef[4] +
   (denerP[372]-FdenerP.fit$coef[16])*FdenerP.fit$coef[5] +
   (denerP[371]-FdenerP.fit$coef[16])*FdenerP.fit$coef[6] +
   (denerP[370]-FdenerP.fit$coef[16])*FdenerP.fit$coef[7] +
   (denerP[369]-FdenerP.fit$coef[16])*FdenerP.fit$coef[8] +
   (denerP[368]-FdenerP.fit$coef[16])*FdenerP.fit$coef[9] +
   (denerP[367]-FdenerP.fit$coef[16])*FdenerP.fit$coef[10] +
   (denerP[366]-FdenerP.fit$coef[16])*FdenerP.fit$coef[11] +
   (denerP[365]-FdenerP.fit$coef[16])*FdenerP.fit$coef[12] +
   (FdenerP.fit$residuals[376])*FdenerP.fit$coef[13] +
   (FdenerP.fit$residuals[375])*FdenerP.fit$coef[14] +
   (FdenerP.fit$residuals[374])*FdenerP.fit$coef[15] +
   FdenerP.fit$coef[16]
handprediction1

handprediction2 <-
   (handprediction1- FdenerP.fit$coef[16])*FdenerP.fit$coef[1] +
   (denerP[376]- FdenerP.fit$coef[16])*FdenerP.fit$coef[2] +
   (denerP[375]- FdenerP.fit$coef[16])*FdenerP.fit$coef[3] +
   (denerP[374]- FdenerP.fit$coef[16])*FdenerP.fit$coef[4] +
   (denerP[373]- FdenerP.fit$coef[16])*FdenerP.fit$coef[5] +
   (denerP[372]- FdenerP.fit$coef[16])*FdenerP.fit$coef[6] +
   (denerP[371]- FdenerP.fit$coef[16])*FdenerP.fit$coef[7] +
   (denerP[370]- FdenerP.fit$coef[16])*FdenerP.fit$coef[8] +
   (denerP[369]- FdenerP.fit$coef[16])*FdenerP.fit$coef[9] +
   (denerP[368]- FdenerP.fit$coef[16])*FdenerP.fit$coef[10] +
   (denerP[367]- FdenerP.fit$coef[16])*FdenerP.fit$coef[11] +
   (denerP[366]- FdenerP.fit$coef[16])*FdenerP.fit$coef[12] +
   (FdenerP.fit$residuals[376])*FdenerP.fit$coef[14] +
   (FdenerP.fit$residuals[375])*FdenerP.fit$coef[15] +
   FdenerP.fit$coef[16]
handprediction2




# I start the GARCH estimation here
#GARCH Model

generP<-garchFit(~garch(1,1), data=denerP)
generP
summary(generP)
predict(generP,2)
#                                  Stat      P-value
#LM Arch Test       R    TR^2   36.26529  0.00029366  - We reject constant variance. There is volatility

generPP<- ts(generP@fitted, end=c(2021,5), frequency=12)
class(generPP)


# I did the same here, I downloaded by forecast and re- uploaded it so as to plot the forecast data
# If you experience any trouble doing this, I can be of help
HW11111.raw <- read.csv("HW11111.csv",
                      header=TRUE)
HW111112<- ts(HW11111.raw[,2], start=c(1990,1),
           frequency=12)


# I plot the data the actual and the forecast
plot(HW111112,
     col=1,
     main="Electricity Price Volatility (Garch[1,1])",
     xlab="Month",
     ylab="Size",
     col.axis = "black")

# I forecast for two periods
predict(generPP,2)
#meanForecast meanError standardDeviation
#1   0.01190661 0.3061482         0.3061482     May,2021
#2   0.01190661 0.3066527         0.3066527     June,2021



# I plotted the garch(1,1) prediction
plot(generP@h.t,
     col=1,
     main="Electricity Price Volatility (Garch[1,1])",
     xlab="Month",
     ylab="Size",
     col.axis = "")



# Still garch (1,1) prediction graph
plot(generP@h.t, col="red",
     main="RPE GARCH(1,1) Forecast",
     sub="Fig.1.5",
     xlab="Monthly",
     ylab="Change in RPE",
     col.axis = "black",
     family = "Times New Roman"
)


#This is lines (368-393)  not relevant
cvgenerP<- generP@h.t

plot(HW111112, col="red",
     main="Fitted Volatility",
     sub="Fig.1.5",
     xlab="Monthly",
     ylab="Conditional Variannce",
     col.axis = "black",
     family = "Times New Roman")
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)
lines(HW111112, col="red", pch=9)

genercv<- data.frame(cvgenerP, stringsAsFactors = FALSE)





#### All the codes below are not relevant

#### All the codes below are not relevant

#### All the codes below are not relevant




install.packages("tseries")
install.packages("rugarch")
install.packages("zoo")
install.packages("FinTs")
install.packages("e1071")
library("tseries")
library("rugarch")
library("zoo")
library("FinTs")
library("e1071")

denerPx=ugarchspec(variance.model = list(garchorder=c(1,1), mean.model= list(armaorder=c(0,0))))
denerxfit=ugarchfit(denerPx, data=denerP)
denerxfit
summary(denerxfit)

denerfit=ugarchforecast(denerxfit, n.ahead=48)
denerfit

#Here, you will chose from the options that it brings, so I choose 1 for series

plot(denerxfit, col="red",
     main="RPE GARCH(1,1) Forecast",
     sub="Fig.1.5",
     xlab="Monthly",
     ylab="Change in RPE",
     col.axis = "black",
     family = "Times New Roman"
)
abline(h=0, col="darkgrey", lty=3,lwd=3)



fvol.raw <- read.csv("HW1222222.csv",
                      header=TRUE)
fenerP<- ts(fvol.raw[,2], start=c(2021,6),
           frequency=12)


plot(fenerP, col="red",
     main="RPE GARCH(1,1) Forecast",
     sub="Fig.1.5",
     xlab="Monthly",
     ylab="Change in RPE",
     col.axis = "black",
     family = "Times New Roman"
)
abline(h=0, col="darkgrey", lty=3,lwd=3)





























auto.arima(denerP)

predict(generP@h.t,12)

denerPPPppp<- data.frame(generP@h.t, stringsAsFactors = FALSE)

generP@h.t

plot(ts(generP@h.t, end=c(2021,4), frequency=12))

FdenerP.fit <- arima(enerP, order=c(12,1,3), transform.pars = FALSE, fixed = c(NA,0, 0,0,0,0,0, NA, NA, 0,NA,NA, NA, NA, NA,NA))



FdenerP.fit
predict(FdenerP.fit,12)

FdenerP.fit <- auto.arima(enerP)
FdenerP.fit
forednerP<-forecast(FdenerP.fit, h=12)
forednerP

FdenerP.fit <- arima(enerP, order=c(12,1,3), transform.pars = FALSE, fixed = c(NA,0, 0,0,0,0,0, NA, NA, 0,NA,NA, NA, NA, NA,NA))
FdenerP.fit
predict(FdenerP.fit,12)

plot2<-ggplot(data = denerPPP, aes(x=enerPPP.raw, y=enerPPP.raw)) +
   geom_point()+ geom_smooth(method="lm")


p <- ggplot(data= denerPPP, aes(x=enerPPP.raw, y=enerPPP.raw)) +
   geom_line() +
   xlab("")

start_date<-as.Date("1990/1/1")
datee<-seq(start_date, by="month", length.out=376)
denerPPP<- data.frame(datee,denerP, stringsAsFactors = FALSE)


generPPP<- data.frame(generPP, stringsAsFactors = FALSE)
export(generPPP, "volenerP.xlsx")

write.table(generPPP, file="C:\Users\oladi\OneDrive\Desktop\Timeseriesenerrerr.csv", row.names = F, sep=",")


