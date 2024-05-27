#install.packages("yfR")
#install.packages("fpp2")
#install.packages("vars")
#install.packages("tseries")
#install.packages("urca")
#install.packages("stats")
#install.packages("changepoint")
#install.packages("dplyr")
#install.packages("uroot")
#install.packages("forecast")


# Loading the libraries

library(fpp2)
library(vars)
library(tseries)
library(urca)
library(stats)
library(changepoint)
library(dplyr)
library(uroot)
library(yfR)
library(PerformanceAnalytics)
library(xts)
library(zoo)
library(tsibble) 
library(readxl)
library(fpp3)
library(fpp2)
library(forecast)

library(fpp2)
library(vars)
library(tseries)
library(urca)
library(stats)
library(changepoint)
library(dplyr)
library(uroot)
library(TSA)
library(FinTS)







prices = yf_get("NVDA",first_date = "2017-01-01",last_date = "2024-04-01",freq_data = "monthly")

prices2 = data.frame(prices[,c(2,7)],row.names = 1)


#Q1 <- quantile(prices2$price_close, 0.25)
#Q3 <- quantile(prices2$price_close, 0.75)
#IQR <- Q3 - Q1

# Define the lower and upper bounds to identify outliers
#lower_bound <- Q1 - 1.5 * IQR
#upper_bound <- Q3 + 1.5 * IQR

# Filter out the outliers
#clean_nvda_data <- prices2[prices2$price_close >= lower_bound & prices2$price_close <= upper_bound, ]

# View the cleaned dataframe
#head(clean_nvda_data)

#boxplot(clean_nvda_data)



#install.packages("PerformanceAnalytics")

price_close_xts <- as.xts(prices2$price_close, order.by = as.Date(rownames(prices2)))
prices2$rentabilities <- Return.calculate(price_close_xts, method = "log")
prices2<- prices2[(-1),]
rentabilities_xts <- ?as.xts(prices2$rentabilities)

?plot

plot(prices2$rentabilities, type="l")
dim(prices2$rentabilities)

#y <- tsibble(
#  Year = 2017:2024, #indexul seriei de timp
#  Observation = c(11,12,12,12,12,12,12,3), #valorile observate din fiecare an
#  index = Year
#)

rent_ts <- as.ts(prices2$rentabilities)

# Grafice de sezonalitate

ggseasonplot(rent_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("") +
  ggtitle("")

ggseasonplot(rent_ts, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

# DATA ARE NOT SEASONAL !!!!


hegy.test(prices2$rentabilities)


# Detectarea stationaritatii prin metoda grafica
autoplot(prices2$rentabilities) +
  ggtitle('NVDA Rentability Overview 2017-2024') +
  theme_bw() # nestationara deoarece avem schimbari de trend


ggAcf(prices2$rentabilities) # forta <=> stationara

tseries::adf.test(prices2$rentabilities) # stationara
tseries::adf.test(prices2$rentabilities, k=1) # stationara
tseries::adf.test(prices2$rentabilities, k=2) # stationara
tseries::adf.test(prices2$rentabilities, k=3) # stationara
tseries::adf.test(prices2$rentabilities, k=4) # stationara
tseries::adf.test(prices2$rentabilities, k=5) # stationara
tseries::adf.test(prices2$rentabilities, k=6) # nestationara
tseries::adf.test(prices2$rentabilities, k=7) # nestationara
tseries::adf.test(prices2$rentabilities, k=8) # nestationara
tseries::adf.test(prices2$rentabilities, k=9) # nestationara
tseries::adf.test(prices2$rentabilities, k=10) # nestationara

# Radacina unitara pentru elementele deterministe
rw_none <- ur.df(prices2$rentabilities, type='none', selectlags = c("AIC"))
summary(rw_none) # valoarea testului statistics |-5.1214| > |-2.6| / |-1.95| / |-1.61| (tau)
# seria este stationara 
# probabilitatea z.lag.1 = 1.97*10^(-6) < 0.1 deci seria este stationara

rw_ct <- ur.df(prices2$rentabilities, type='trend', selectlags = c("AIC"))
summary(rw_ct) 


# Testul KPSS (Kwiatkowski-Phillips-Schmidt-Shin)
# H0: seria este stationara 
# H1: seria nu este stationara

#summary(ur.kpss(goog200))
prices2$rentabilities %>% ur.kpss() %>% summary() # valoarea testului 0.1432 < toate valorile critice
# seria este stationara 


# Testul Quandt Likelihood Ratio (QLR)

# Nu aplicam QLR pentru ca seria este stationara.



# Seria indicelui brusier google pe 200 de zile consecutive
autoplot(prices2$rentabilities) +
  ggtitle('Google stock price for 200 consecutive days') +
  theme_bw()

ggAcf(prices2$rentabilities) + theme_bw()
ggPacf(prices2$rentabilities) + theme_bw()


# Corelograma seriilor de timp este o diagrama a statisticilor de corelatie 
# Functia ggtsdisplay ne ofera graficul seriei, ACF si PACF in acelasi plot 
ggtsdisplay(prices2$rentabilities) 


# Simularea unui proces AR(1)
ar1 <- as.ts(data$ar1) 
ggtsdisplay(ar1) # un proces pur autoregresiv este considerat atunci cand 
#lagurile ACF descresc lent, iar dupa primele n laguri ale PACF-ului 
# identificam o scadere brusca 

# Testarea modelului AR(1)
ar1_model <- Arima(ar1, order = c(1,0,0), include.mean = FALSE) # ARIMA(p,d,q) 
summary(ar1_model)

coeftest(ar1_model) # testarea coeficientului ar1

test_arima <- auto.arima(prices2$rentabilities,seasonal=FALSE)
summary(test_arima)
coeftest(test_arima)


# Testearea reziduurilor modelului AR1
residuals_ar1 <- residuals(test_arima)
ggtsdisplay(residuals_ar1) # nu avem autocorelatie in reziduuri conform ACF 
Box.test(residuals_ar1, lag=1,type="Lj") # nu avem autocorelare in reziduuri
jarque.bera.test(residuals_ar1)
ArchTest(residuals_ar1,lag=1)

