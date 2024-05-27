
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
absolventi <- absolventi[2:32,]
absolventi <- absolventi[order(absolventi$Ani),]

# Citirea valorilor din fisierul unitati_invatamant.csv si crearea unui dataframe
unitati_invatamant <- read.csv("unitati_invatamant.csv", header = TRUE, sep = ",")
unitati_invatamant <- unitati_invatamant[2:32,]
unitati_invatamant <- unitati_invatamant[order(unitati_invatamant$Ani),]

# Citirea cresterii pib-ului din fisierul crestere_pib.csv si crearea unui dataframe
crestere_pib <- read.csv("crestere_pib.csv", header = TRUE, sep = ",")
crestere_pib <- crestere_pib[2:32,]
crestere_pib <- crestere_pib[order(crestere_pib$Ani),]

# Citirea numarului de biblioteci din fisierul biblioteci.csv si crearea unui dataframe
biblioteci <- read.csv("biblioteci.csv", header = TRUE, sep = ",")
biblioteci <- biblioteci[2:32,]
biblioteci <- biblioteci[order(biblioteci$Ani),]

# Citirea valorilor din absolventi si unitati_invatamant si crearea unui dataframe cu row.names = Ani
absolventi_unitati <- data.frame(absolventi[,2], unitati_invatamant[,2], row.names = absolventi$Ani)

# Adaugarea cresterii pib-ului in dataframe-ul absolventi_unitati
absolventi_unitati$Crestere_PIB <- crestere_pib[,2]

# Adaugarea numarului de biblioteci in dataframe-ul absolventi_unitati
absolventi_unitati$Biblioteci <- biblioteci[,2]
colnames(absolventi_unitati)=c("Absolventi","Unitati","Crestere PIB","Biblioteci")

# Scatterplot
ggplot(data = absolventi_unitati) + 
  geom_point(mapping = aes(x = absolventi_unitati$Absolventi, y = absolventi_unitati$Unitati)) +
  xlab('Numar Absolventi') +
  ylab('Numar Unitati de Invatamant') + 
  ggtitle('Norul de puncte dintre numarul de absolventi si numarul de unitati de invatamant')+
  theme_bw()

ggplot(data = absolventi_unitati) + 
  geom_point(mapping = aes(x = absolventi_unitati$Unitati, y = absolventi_unitati$`Crestere PIB`)) +
  xlab('Numar Unitati de Invatamant') +
  ylab('Cresterea PIB-ului') + 
  ggtitle('Norul de puncte dintre nr de unitati de invatamant si % de crestere a PIB-ului')+
  theme_bw()

# Declaram variabilele de tip ts
absolventi_ts <- ts(absolventi_unitati$Absolventi, start = 1991, frequency = 1)
unitati_ts <- ts(absolventi_unitati$Unitati, start = 1991, frequency = 1)
crestere_pib_ts <- ts(absolventi_unitati$`Crestere PIB`, start = 1991, frequency = 1)
biblioteci_ts <- ts(absolventi_unitati$Biblioteci, start = 1991, frequency = 1)

absolventi_ts <- window(absolventi_ts, start=1991, end=2021)
unitati_ts <- window(unitati_ts, start=1991, end=2021)
crestere_pib_ts <- window(crestere_pib_ts, start=1991, end=2021)
biblioteci_ts <- window(biblioteci_ts, start=1991, end=2021)


# Normalizarea datelor
absolventi_ts <- (absolventi_ts - mean(absolventi_ts)) / sd(absolventi_ts)
unitati_ts <- (unitati_ts - mean(unitati_ts)) / sd(unitati_ts)
crestere_pib_ts <- (crestere_pib_ts - mean(crestere_pib_ts)) / sd(crestere_pib_ts)
biblioteci_ts <- (biblioteci_ts - mean(biblioteci_ts)) / sd(biblioteci_ts)


# Calculul procentajelor de schimbare
absolventi_diff <- diff(absolventi_ts)
unitati_diff <- diff(unitati_ts)
crestere_pib_diff <- diff(crestere_pib_ts)
biblioteci_diff <- diff(biblioteci_ts)





# Graficul seriilor
autoplot(cbind(absolventi_ts,unitati_ts,crestere_pib_ts,biblioteci_ts)) +
  ylab('') +
  ggtitle('Graficul seriilor') +
  theme_bw()

autoplot(cbind(absolventi_diff,unitati_diff,crestere_pib_diff,biblioteci_diff)) +
  ylab('') +
  ggtitle('Graficul seriilor diferentiate') +
  theme_bw()


# Determinarea persistentei modelului
ggtsdisplay(absolventi_ts)
ggtsdisplay(unitati_ts)
ggtsdisplay(crestere_pib_ts)
ggtsdisplay(biblioteci_ts)

ggtsdisplay(absolventi_diff)
ggtsdisplay(unitati_diff)
ggtsdisplay(crestere_pib_diff)
ggtsdisplay(biblioteci_diff)


# Testarea stationaritatii seriilor (am ales varianta cea mai complexa a ADF)
adf.absolventi <- ur.df(absolventi_ts, type = "trend", selectlags = "AIC")
summary(adf.absolventi) # serie 

adf.unitati <- ur.df(unitati_ts, type = "trend", selectlags = "AIC")
summary(adf.unitati) # serie 

adf.crestere_pib <- ur.df(crestere_pib_ts, type = "trend", selectlags = "AIC")
summary(adf.crestere_pib) # serie 

adf.biblioteci <- ur.df(biblioteci_ts, type = "trend", selectlags = "AIC")
summary(adf.biblioteci) # serie


ndiffs(absolventi_ts)
ndiffs(unitati_ts)
ndiffs(crestere_pib_ts)
ndiffs(biblioteci_ts)


# Testarea stationaritatii seriilor DIFERENTIATE (am ales varianta cea mai complexa a ADF)
adf.absolventi <- ur.df(absolventi_diff, type = "trend", selectlags = "AIC")
summary(adf.absolventi) # serie 

adf.unitati <- ur.df(unitati_diff, type = "trend", selectlags = "AIC")
summary(adf.unitati) # serie 

adf.crestere_pib <- ur.df(crestere_pib_diff, type = "trend", selectlags = "AIC")
summary(adf.crestere_pib) # serie 

adf.biblioteci <- ur.df(biblioteci_diff, type = "trend", selectlags = "AIC")
summary(adf.biblioteci) # serie


ndiffs(absolventi_diff)
ndiffs(unitati_diff)
ndiffs(crestere_pib_diff)
ndiffs(biblioteci_diff)


