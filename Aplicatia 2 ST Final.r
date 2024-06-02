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
library(urca)
library(forecast)
library(vars)
library(tidyverse)
library(tsDyn)
library(dynlm)
library(aTSA)
library(readr)
setwd("D:/Desktop/CSIE/An 3/Sem 2 2023 - 2024/Serii de timp")

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



#Normalizarea datelor
absolventi_norm <- (absolventi_ts - mean(absolventi_ts)) / sd(absolventi_ts)
unitati_norm <- (unitati_ts - mean(unitati_ts)) / sd(unitati_ts)
crestere_pib_norm <- (crestere_pib_ts - mean(crestere_pib_ts)) / sd(crestere_pib_ts)
biblioteci_norm <- (biblioteci_ts - mean(biblioteci_ts)) / sd(biblioteci_ts)


# Calculul procentajelor de schimbare
absolventi_diff <- diff(absolventi_ts)
unitati_diff <- diff(unitati_ts)
crestere_pib_diff <- diff(crestere_pib_ts)
biblioteci_diff <- diff(biblioteci_ts)



# Graficul seriilor
autoplot(cbind(absolventi_ts,unitati_ts,biblioteci_ts)) +
  ylab('') +
  ggtitle('Graficul seriilor') +
  theme_bw()

autoplot(cbind(absolventi_diff,unitati_diff,biblioteci_diff)) +
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
summary(adf.absolventi) # serie nestationara

adf.unitati <- ur.df(unitati_ts, type = "trend", selectlags = "AIC")
summary(adf.unitati) # serie nestationara
 
adf.crestere_pib <- ur.df(crestere_pib_ts, type = "trend", selectlags = "AIC")
summary(adf.crestere_pib) # serie stationara

adf.biblioteci <- ur.df(biblioteci_ts, type = "trend", selectlags = "AIC")
summary(adf.biblioteci) # serie nestationara


ndiffs(absolventi_ts)
ndiffs(unitati_ts)
ndiffs(crestere_pib_ts)
ndiffs(biblioteci_ts)


# Testarea stationaritatii seriilor DIFERENTIATE (am ales varianta cea mai complexa a ADF)
adf.absolventi <- ur.df(absolventi_diff, type = "trend", selectlags = "AIC")
summary(adf.absolventi) # serie stationara

adf.unitati <- ur.df(unitati_diff, type = "trend", selectlags = "AIC")
summary(adf.unitati) # serie stationara

adf.crestere_pib <- ur.df(crestere_pib_diff, type = "trend", selectlags = "AIC")
summary(adf.crestere_pib) # serie stationara

adf.biblioteci <- ur.df(biblioteci_diff, type = "trend", selectlags = "AIC")
summary(adf.biblioteci) # serie stationara


ndiffs(absolventi_diff)
ndiffs(unitati_diff)
ndiffs(biblioteci_diff)


# Crearea setului de date cu care vom lucra mai departe
absolventi_unitati<-data.frame(absolventi_norm,unitati_norm,biblioteci_norm)
colnames(absolventi_unitati)=c("Absolventi","Unitati","Biblioteci")



# Testul Engle - Granger
coint.test(y = absolventi_unitati$Absolventi,X = absolventi_unitati$Unitati,d = 1) 
# Pentru p-value de la no trend si liniar trend avem:
# 0.0942 si 0.1 > 0.05 => respingem ipoteza nula => serii cointegrate
coint.test(y = absolventi_unitati$Unitati,X = absolventi_unitati$Absolventi,d = 1) #  serii cointegrate

coint.test(y = absolventi_unitati$Absolventi,X = absolventi_unitati$Biblioteci,d = 1) # serii cointegrate
coint.test(y = absolventi_unitati$Biblioteci,X = absolventi_unitati$Absolventi,d = 1) # serii cointegrate

coint.test(y = absolventi_unitati$Unitati,X = absolventi_unitati$Biblioteci,d = 1) # cointegrare
coint.test(y = absolventi_unitati$Biblioteci,X = absolventi_unitati$Unitati,d = 1) # cointegrare

#Concluzie: cele 3 serii sunt cointegrate intre ele



# Cointegrarea Johansen
# Selectarea lagului optim
lagselect <- VARselect(absolventi_unitati, lag.max = 12, type = 'const') #lag=12 - date anuale
lagselect$selection 
# 6 laguri conform AIC, HQ si SC
# Pentru a testa Johansen avem nevoie de laguri selectate - 1 => 6 - 1 = 5

# Testul Johansen - metoda Trace
ctest1 <- ca.jo(absolventi_unitati, type = 'trace', ecdet = 'const',K=5)
summary(ctest1) 

# Testul Johansen - metoda valorilor proprii maxime
ctest2 <- ca.jo(absolventi_unitati, type = 'eigen', ecdet = 'const',K=5)
summary(ctest2)
# Ambele metode de cointegrare Johanses confirma ca avem cel mult 2 relatii de cointegrare



# Modelul VECM - metoda de estimare 2OLS cu o relatie de cointegrare
Model1 <- VECM(absolventi_unitati,
               lag = 5, 
               r=1, 
               estim = ('2OLS'),
               LRinclude = 'const')
summary(Model1)



# Diagnosticul pe reziduuri
# Transformam obiectul VECM in obiect VAR
Model1VAR <- vec2var(ctest1, r = 1)

# Autocorelare
Serial1 <- serial.test(Model1VAR, lags.pt = 5, type = 'PT.asymptotic')
Serial1 # avem autocorelare in reziduuri; avem o limitare in model

# Heteroschedascitate
Arch1 <- vars::arch.test(Model1VAR, lags.multi = 1, multivariate.only = TRUE)
Arch1 # reziduuri homosechedastice; p-value=0.91 > 0.1

# Normalitate
Norm1 <- normality.test(Model1VAR, multivariate.only = TRUE)
Norm1 # reziduurile nu sunt normal distribuite; sunt normal distribuite pentru un prag de seminifactie de 10%



# Cauzalitate Granger
modelVar <- VAR(absolventi_unitati, p = 5, type = 'const', season = NULL, exog = NULL)
GrangerAbsolventi <- causality(modelVar, cause = "Absolventi" )
GrangerAbsolventi #nu avem cauzalitate; ambele p-value>0.1
GrangerUnitati <- causality(modelVar, cause = 'Unitati')
GrangerUnitati #avem cauzalitate; ambele p-value<0.1
GrangerBiblioteci <- causality(modelVar, cause = 'Biblioteci')
GrangerBiblioteci #cauzalitate partiala; pentru testul Granger p-value>0.1, insa pentru testul Volt p-value<0.1



# Functia de raspuns la impuls doar asupra seriei "Unitati de invatatmant"
Absolventi_irf <- irf(Model1VAR, impulse = 'Absolventi', response = 'Unitati', n.ahead= 10, boot = TRUE)
plot(Absolventi_irf, ylab = 'Unitati', main = 'Absolventi shock to Unitati')
Absolventi_irf <- irf(Model1VAR, impulse = 'Biblioteci', response = 'Unitati', n.ahead= 10, boot = TRUE)
plot(Absolventi_irf, ylab = 'Unitati', main = 'Bibioteci shock to Unitati')



# Descompunerea variantei
FEVD1 <- fevd(Model1VAR,n.ahead=10)
FEVD1
plot(FEVD1) 



# Prognoza 
forecast <- predict(Model1VAR, n.ahead = 4, ci = 0.99) # prognoza pe 4 ani

plot(forecast, name = 'Absolventi')
plot(forecast, name = 'Unitati')
plot(forecast, name = 'Biblioteci')

fanchart(forecast, name = 'Absolventi')
fanchart(forecast, name = 'Unitati')
fanchart(forecast, name = 'Biblioteci')
