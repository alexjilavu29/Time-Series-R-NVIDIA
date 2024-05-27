
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
setwd("~/Facultate/ASE/Anul 3/Semestrul 2/Serii de Timp/Proiect ST")

# Citirea valorilor din fisierul absolventi.csv si crearea unui dataframe
absolventi <- read.csv("absolventi.csv", header = TRUE, sep = ",")
absolventi <- absolventi[1:32,]
absolventi <- absolventi[order(absolventi$An),]

# Citirea valorilor din fisierul unitati_invatamant.csv si crearea unui dataframe
unitati_invatamant <- read.csv("unitati_invatamant.csv", header = TRUE, sep = ",")
unitati_invatamant <- unitati_invatamant[1:32,]
unitati_invatamant <- unitati_invatamant[order(unitati_invatamant$An),]

# Citirea valorilor din absolventi si unitati_invatamant si crearea unui dataframe cu row.names = Ani
absolventi_unitati <- data.frame(absolventi[,2], unitati_invatamant[,2], row.names = absolventi$Ani)
colnames(absolventi_unitati)=c("Absolventi","Unitati")



# Scatterplot
ggplot(data = absolventi_unitati) + 
  geom_point(mapping = aes(x = absolventi_unitati$Absolventi, y = absolventi_unitati$Unitati)) +
  xlab('Numar Absolventi') +
  ylab('Numar Unitati de Invatamant') + 
  ggtitle('Norul de puncte dintre numarul de absolventi si numarul de unitati de invatamant')+
  theme_bw()


# Declaram variabilele de tip ts
absolventi_ts <- ts(absolventi_unitati$Absolventi, start = 1990, frequency = 1)
unitati_ts <- ts(absolventi_unitati$Unitati, start = 1990, frequency = 1)

absolventi_ts <- window(absolventi_ts, start=1990, end=2021)
unitati_ts <- window(unitati_ts, start=1990, end=2021)


# Normalizarea datelor
absolventi_ts <- (absolventi_ts - mean(absolventi_ts)) / sd(absolventi_ts)
unitati_ts <- (unitati_ts - mean(unitati_ts)) / sd(unitati_ts)


# Calculul procentajelor de schimbare
absolventi_diff <- diff(absolventi_ts)
unitati_diff <- diff(unitati_ts)





# Graficul seriilor
autoplot(cbind(absolventi_ts,unitati_ts)) +
  ylab('') +
  ggtitle('Graficul seriilor') +
  theme_bw()

autoplot(cbind(absolventi_diff,unitati_diff)) +
  ylab('') +
  ggtitle('Graficul seriilor diferentiate') +
  theme_bw()

# Determinarea persistentei modelului
ggtsdisplay(absolventi_ts)
ggtsdisplay(unitati_ts)

ggtsdisplay(absolventi_diff)
ggtsdisplay(unitati_diff)


# Testarea stationaritatii seriilor (am ales varianta cea mai complexa a ADF)
adf.absolventi <- ur.df(absolventi_ts, type = "trend", selectlags = "AIC")
summary(adf.absolventi) # serie nestationara

adf.unitati <- ur.df(unitati_ts, type = "trend", selectlags = "AIC")
summary(adf.unitati) # serie nestationara

ndiffs(absolventi_ts)
ndiffs(unitati_ts)


# Testarea stationaritatii seriilor DIFERENTIATE (am ales varianta cea mai complexa a ADF)
adf.absolventi <- ur.df(absolventi_diff, type = "trend", selectlags = "AIC")
summary(adf.absolventi) # serie nestationara

adf.unitati <- ur.df(unitati_diff, type = "trend", selectlags = "AIC")
summary(adf.unitati) # serie nestationara


ndiffs(absolventi_diff)
ndiffs(unitati_diff)


