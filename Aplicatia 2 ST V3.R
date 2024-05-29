
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

# Citirea cresterii pib-ului din fisierul crestere_pib.csv si crearea unui dataframe
crestere_pib <- read.csv("crestere_pib.csv", header = TRUE, sep = ",")
crestere_pib <- crestere_pib[2:32,]
crestere_pib <- crestere_pib[order(crestere_pib$Ani),]


# Citirea valorilor din fisierul absolventi.csv si crearea unui dataframe
absolventi <- read.csv("absolventi.csv", header = TRUE, sep = ",")
absolventi <- absolventi[2:32,]
absolventi <- absolventi[order(absolventi$Ani),]

# Citirea valorilor din fisierul unitati_invatamant.csv si crearea unui dataframe
unitati_invatamant <- read.csv("unitati_invatamant.csv", header = TRUE, sep = ",")
unitati_invatamant <- unitati_invatamant[2:32,]
unitati_invatamant <- unitati_invatamant[order(unitati_invatamant$Ani),]

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

# Declararea variabilelor de tip „time series”
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


# Calculul primei diferențe
absolventi_diff <- diff(absolventi_ts)
unitati_diff <- diff(unitati_ts)
biblioteci_diff <- diff(biblioteci_ts)


crestere_pib_diff <- diff(crestere_pib_ts)


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
ggtsdisplay(biblioteci_ts)

ggtsdisplay(crestere_pib_ts)

ggtsdisplay(absolventi_diff)
ggtsdisplay(unitati_diff)
ggtsdisplay(crestere_pib_diff)
ggtsdisplay(biblioteci_diff)


# Testarea stationaritatii seriilor 
adf.absolventi <- ur.df(absolventi_ts, type = "trend", selectlags = "AIC")
summary(adf.absolventi) # serie nestationara

adf.unitati <- ur.df(unitati_ts, type = "trend", selectlags = "AIC")
summary(adf.unitati) # serie nestationara

adf.biblioteci <- ur.df(biblioteci_ts, type = "trend", selectlags = "AIC")
summary(adf.biblioteci) # serie nestationara

adf.crestere_pib <- ur.df(crestere_pib_ts, type = "trend", selectlags = "AIC")
summary(adf.crestere_pib) # serie stationara

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



# Testarea stationacointegrare Johanses confirma ca avem cel mult 2 relatii de cointegrare
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









# Testul Engle - Granger

# H0: seriile nu sunt cointegrate
# H1: seriile sunt cointegrate

# Pas 1 am testat stationaritatea

# Pas 2: aplicam testul de cointegrare pe seriile reziduurilor
# d - operatorul de diferentiere, am diferentiat o data seriile ca sa fie stationare, deci va fi d=1

# Testul calculeaza o regresie simpla intre cele doua variabile si testeaza daca reziduurile
# sunt sau nu stationare
coint.test(y = absolventi_unitati$Absolventi,X = absolventi_unitati$Unitati,d = 1) 
# ne uitam la p-value de la no trend si liniar trend
# 0.0942 si 0.1 > 0.05 merge => respingem nula => serii cointegrate
coint.test(y = absolventi_unitati$Unitati,X = absolventi_unitati$Absolventi,d = 1) #  serii cointegrate

coint.test(y = absolventi_unitati$Absolventi,X = absolventi_unitati$Biblioteci,d = 1) # serii cointegrate
coint.test(y = absolventi_unitati$Biblioteci,X = absolventi_unitati$Absolventi,d = 1) # serii cointegrate

coint.test(y = absolventi_unitati$Unitati,X = absolventi_unitati$Biblioteci,d = 1) # cointegrare
coint.test(y = absolventi_unitati$Biblioteci,X = absolventi_unitati$Unitati,d = 1) # cointegrare

#Cele 3 serii sunt cointegrate intre ele


# Cointegrarea Johansen

# H0: nu exista cointegrare
# H1: exista cel putin o relatie de cointegrare

# Selectarea lagului 
lagselect <- VARselect(absolventi_unitati, lag.max = 12, type = 'const') #lag=12 - date anuale
lagselect$selection 
# 6 laguri conform AIC, HQ si SC
# Pentru a testa Johansen avem nevoie de laguri selectate - 1 => 6 - 1 = 5

# Testul Johansen - metoda Trace
ctest1 <- ca.jo(absolventi_unitati, type = 'trace', ecdet = 'const',K=5)
summary(ctest1) 

# Interpretare - r reprezinta rankul matricei A
# ne uitam la r = 0 
# valoarea testului 91.93 > toate valorile critice => avem cel putin o relatie de cointegrare
# ne uitam la r <= 1
# valoarea testului 34.85 > valorile critile => avem cel putin 2 relatii de cointegrare 
# ne uitam la r <= 2
# valoarea testului 6.51 < 9.24 si 12.97 => avem cel mult 2 relatii de cointegrare


# Testul Johansen - metoda valorilor proprii maxime
ctest2 <- ca.jo(absolventi_unitati, type = 'eigen', ecdet = 'const',K=5)
summary(ctest2)
# r = 0 test > val critice => avem cel putin o relatie de cointegrare
# r <= 1 test > val critice => avem 2 relatii de cointegrare
# r <= 2 test < val critice = > avem cel mult 2 relatii de cointegrare

# Ambele metode de cointegrare Johanses confirma ca avem cel mult 2 relatii de cointegrare

# Modelele VECM (vector error correction models) pot avea 2 metode de estimare
# 2OLS - two stage atunci cand avem o singura relatie de cointegrare
# denumita si metoda Engle Granger
# ML maximum likelihood - cand avem doua sau mai multe relatii de cointegrare
# denumita si metoda Johansen

# Modelul VECM - metoda de estimare 2OLS - atunci cand avem o relatie de cointegrare
Model1 <- VECM(absolventi_unitati,
               lag = 5, 
               r=1, 
               estim = ('2OLS'),
               LRinclude = 'const')
summary(Model1)
# relatia de cointegrare este data de randul r1
# Absolventi are valoarea 1 => variabila de referinte
# Pt interpretarea relatiilor se ia semn opus:
# Absolventii si unitatile de invatamant au o relatie negativa (creste una, scade alta)
# Absolventii si bibliotecile au o relatie pozitiva (creste una, creste si cealalta)
# Absolventii si constanta au o relatie negativa
# Toate 3 relatii sunt puternice, intrucat valorile sunt peste 1

# ECT - error correction term - trebuie sa fie negativ si semnificativ ca sa avem 
# relatie pe termen lung

# ECUATIA ABSOLVENTI
# ECT = -1.58 este negativ si semnificativ, deci avem relatie pe termen lung
# Nr absolventilor este influentat de nr absolventilor si numarul institutiilor de invatamant de acum 2 ani si de acum 4 ani 


# ECUATIA UNITATI
# ECT - Nu exista relatii pe termen lung
# Numarul unitatilor de invatamant este influentat in proportie de 90% de nr de absolventi si nr de biblioteci de la lagul 1
# si in proportie de 95% - de nr de unit de invat de acum 3 ani

# ECUATIA BIBLIOTECI
# ECT - nesemnificativ 
# Nr de biblioteci este infleuntat pozitiv doar de nr de unit de invat de acum 2 ani


# Diagnosticul pe reziduuri

# Trebuie sa transformam obiectul VECM in obiect VAR
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















dataset <- cbind(absolventi_ts, unitati_ts, biblioteci_ts)
absolventi_unitati <- data.frame(absolventi_ts, unitati_ts, biblioteci_ts, row.names = absolventi$Ani)
colnames(absolventi_unitati)=c("Absolventi","Unitati","Biblioteci")



# Cauzalitate Granger
head(dataset)
modelVar <- VAR(dataset, p = 5, type = 'const', season = NULL, exog = NULL)
GrangerAbsolventi <- causality(modelVar, cause = 'absolventi_ts' )
GrangerAbsolventi
GrangerGDP <- causality(modelVar, cause = 'Unitati')
GrangerGDP
GrangerGDP <- causality(modelVar, cause = 'Biblioteci')
GrangerGDP

# p-value>0.1 => respingem nula => nu exista cauzalitate granger
# 


# Functia de raspuns la impuls
GDPirf <- irf(Model1VAR, impulse = 'Absolventi', response = 'Unitati', n.ahead= 10, boot = TRUE)
plot(GDPirf, ylab = 'GDP', main = 'CPI shock to GDP')
GDPirf <- irf(Model1VAR, impulse = 'M3', response = 'GDP', n.ahead= 10, boot = TRUE)
plot(GDPirf, ylab = 'GDP', main = 'M3 shock to GDP')

# Descompunerea variantei
FEVD1 <- fevd(Model1VAR,n.ahead=10)
FEVD1
plot(FEVD1) # socul in PIB vine din partea CPI cu aprox 10%
# socul in CPI reprezinta cam 2-3% din partea GDP si 1-2% din partea M3
# socul in M3 reprezinta cam 2-3% din partea GDP si CPI insumate

# Prognoza 
forecast <- predict(Model1VAR, n.ahead = 4, ci = 0.99) # prognoza pe 4 trimestre

plot(forecast, name = 'GDP')
plot(forecast, name = 'CPI')
plot(forecast, name = 'M3')

fanchart(forecast, name = 'GDP')
fanchart(forecast, name = 'CPI')
fanchart(forecast, name = 'M3')

