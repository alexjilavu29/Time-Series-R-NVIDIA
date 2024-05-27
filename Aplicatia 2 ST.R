
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
prices

prices_volume <- data.frame(prices[,c(2,3,7)], row.names = 1)


# Scatterplot
ggplot(data = prices_volume) + 
  geom_point(mapping = aes(x = prices_volume$volume, y = prices_volume$price_close)) +
  xlab('Volum Tranzactionare') +
  ylab('Pret INDEX') + 
  ggtitle('Norul de puncte dintre volumul si pretul')+
  theme_bw()

# Declaram variabilele de tip ts
volume_ts <- ts(prices_volume$volume, start = 2017, frequency = 12)
prices_ts <-ts(prices_volume$price_close, start = 2017, frequency = 12)

volume_ts <- window(volume_ts, start=2017, end=c(2023,4))
prices_ts <- window(prices_ts, start=2017, end=c(2023,4))


# Normalizarea datelor
volume_ts <- (volume_ts - mean(volume_ts)) / sd(volume_ts)
prices_ts <- (prices_ts - mean(prices_ts)) / sd(prices_ts)

# Calculul procentajelor de schimbare
volume_diff <- diff(volume_ts)
prices_diff <- diff(prices_ts)




# Graficul seriilor
autoplot(cbind(volume_ts,prices_ts)) +
  ylab('') +
  ggtitle('Graficul seriilor') +
  theme_bw()

# Determinarea persistentei modelului
ggtsdisplay(volume_ts)
ggtsdisplay(prices_ts)

ggtsdisplay(volume_diff)
ggtsdisplay(prices_diff)

# Testarea stationaritatii seriilor (am ales varianta cea mai complexa a ADF)
adf.volume <- ur.df(volume_diff, type = "trend", selectlags = "AIC")
summary(adf.volume) # serie stationara

adf.prices <- ur.df(prices_diff, type = "trend", selectlags = "AIC")
summary(adf.prices) # serie stationara 

ndiffs(volume_ts)
ndiffs(prices_ts)


# Identificarea lagurilor optime
df <- cbind(volume_ts,prices_ts)
colnames(df) <- cbind('Volume','Prices')

lagselect <- VARselect(df,lag.max = 8, type = 'const')
lagselect
lagselect$selection # lagul 1 conform AIC, Hannan Quinn

# Implementarea VAR
model1 <- VAR(df, p = 1, type = 'const', season = NULL, exog = NULL)
summary(model1)

# Radacinile unitate (roots of the characteristic polynomial) < 1
# toate se afla in interiorul cercului unitate => model stabil
# Ecuatia PIB - PIB la lagul 1 semnificativ si constanta
# Model valid deoarece pentru testul F -  p<0.1
# Ecuatia somajului - somajul la lag 1 semnificativ, PIB lag 3, somaj lag3,
# pib lag 4, somaj lag 4 semnificative
# Testul F (36.5) are p < 0.1 => model valid
# Reziduurile nu se coreleaza GDP ~ Unemployment (-0.1 corelatie slaba)

# O alta modalitate de afisare a rezultatelor modelului - pt licenta
stargazer(model1[['varresult']], type = 'text')

# Diagnosticul pe reziduuri

# Autocorelarea
Serial1 <- serial.test(model1, lags.pt = 12, type = 'PT.asymptotic')
Serial1 # pvalue > 0.1 nu avem autocorelare in reziduuri

# Heteroscedasticitate
Arch1 <- vars::arch.test(model1,lags.multi = 12,multivariate.only = TRUE)
Arch1 # pvalue > 0.05 modelul nu prezinta heteroschedasticitate la 95%

# Normalitatea reziduurilor
Norm1 <- normality.test(model1, multivariate.only = TRUE)
Norm1 # pvalue JB < 0.05 reziduurile nu sunt normal distribuite

# Testarea pentru rupturi in serie
Stability1 <- stability(model1,type = 'OLS-CUSUM')
plot(Stability1) # model stabil deoarece seriile noastre nu depasesc intervalul rosu



# Cauzalitate Granger 

# Cauzalitatea Granger determina daca un model care utilizeaza valorile trecute
# si prezente ale lui X si valorile trecute si prezente ale lui Y prezinta erori
# mai mici de prognozare fata de un model care nu prezinta cauzalitate Granger
# Cu alte cuvinte cauzalitatea Granger raspunde la intrebarea: trecutul variabilei X
# ajuta la imbunatatirea predictiei valorilor lui y?

# Pentru a testa cauzalitatea Granger, trebuie sa ne asiguram ca seriile noastre sunt
# stationare si ca nu avem autocorelare

# Ipotezele cauzalitatii Granger
# H0: valorile cu lag ale lui X, nu explica variatia in Y 
# H1: valorile cu lag ale lui X, explica variatia in Y

# H0: variabila X nu prezinta cauzalitate Granger pentru variabila Y
# H1: Variabila X prezinta cauzalitate Granger pentru variabila Y

# Cauzalitatea Granger construita in jurul testului F isi poate pierde din putere
# atunci cand avem un numar mare de variabile si laguri
# Varianta alternativa a cauzalitatii Granger folosita in cazul in care avem multe laguri
# si un numar mare de serii in modelul VAR si se bazeaza pe testul Wald si distributia Chi-patrat
# Cu alte cuvinte cauzalitatea Granger-Wald denumita si cauzalitate instantanee
# raspunde la intrebarea: cunoasterea viitorului lui x ma ajuta sa prezic mai bine 
# viitorul lui y?

# Ipotezele pentru ambele teste de cauzalitate Granger raman la fel, in output 
# cea clasica o gasim sub forma de Granger, iar cea bazata pe Wald o gasim 
# sub forma Instant

GrangerVolume <- causality(model1, cause = 'Volume')
GrangerGDP # p > 0.1 => PIB nu prezinta cauzalitate Granger cu somajul

GrangerPrices <- causality(model1, cause = 'Prices')
GrangerUnemp # p > 0.1 => Somajul nu prezinta cauzalitate Granger cu PIB

# Functia de raspuns la impuls (IRF) 
# ATENTIE: functia de raspuns la impuls are sens dpdv economic doar in momentul
# in care exista Cauzalitate Granger
# Pentru acest model nu are sens dpdv economic, a fost aplicata doar in scop didactic
Volumeirf <- irf(model1, impulse = 'Prices', response = 'Volume', n.ahead = 20, boot = TRUE, ci=0.90) # n.ahead = perioade in viitor
# boot = TRUE pentru a crea intervale de incredere
# ci - nivelul de semnificatie
plot(Volumeirf, ylab = 'Volume', main = 'Raspunsul Volumului la socurile pretului')

Unemploymentirf <- irf(model1, impulse = 'Volume', response = 'Prices', 
                       n.ahead = 20, boot = TRUE, ci=0.90)
plot(Unemploymentirf, ylab = 'Prices', main = 'Shock from Volume')

# Interpretare: liniile punctate - intervalul de incredere
# Raspunsul PIB-ului daca urmeaza sa creasca rata somajului este sa scada in primele
# perioade, urmand sa recupereze 
# Din intervalele de incredere, observam ca putem avea o marja mare de eroare deoarece
# intervalele sunt destul de largi si ar am putea de fapt sa nu avem niciun
# efect al PIBului ca raspuns la cresterea ratei somajului

# Raspunsul ratei somajului la cresterea PIBului - relatie indecisiva intre cele doua 
# variabile (confirmata si de cauzalitatea Granger)


# Descompunerea variantei
# ATENTIE: descompunerea variantei are sens dpdv economic doar in momentul
# in care exista Cauzalitate Granger
# Pentru acest model nu are sens dpdv economic, a fost aplicata doar in scop didactic
FEVD <- fevd(model1, n.ahead = 10)
plot(FEVD) # graficul ne spune procentul de unde vine socul variabilei
# pentru ambele variabile socul vin de la variabila in sine si mai putin 
# de la cealalta variabila din model 
# In cazul somajului, in cea de a 10 a perioada, socul din partea variabilei PIB
# este de 10%

# Prognoza VAR
forecast <- predict(model1, n.ahead = 4, ci = 0.90) # prognoza pe 4 trimestre

plot(forecast, name = 'Volume')
plot(forecast, name = 'Prices')

fanchart(forecast, names='Volume')
fanchart(forecast, names='Prices')
















# Aplicatia 2 - VAR lunar

# Vom testa date legate de cheltuielile cu vacanta, la restaurant si cheltuielile
# cu vacanta, altele in Portugalia, date lunare din 1995 pana in 2015

# Incarcarea setului de date
restaurant <- read_excel("restaurant_spend.xlsx")
vacanta <- read_excel("holiday_spend.xlsx")


# Declaram variabilele de tip ts
restaurant <- ts(restaurant, start = c(1995,1), frequency = 12)
vacanta <-ts(vacanta, start = c(1995,1), frequency = 12)

# Graficul seriilor
autoplot(cbind(restaurant,vacanta)) +
  ylab('') +
  ggtitle('Graficul seriei multivariata - cheltuieli cu vacanta') +
  theme_bw()

# Determinarea persistentei modelului
ggtsdisplay(restaurant)
ggtsdisplay(vacanta) # serii ce par a fi nestationare

# Testarea stationaritatii seriilor
adf.res <- ur.df(restaurant, type = "trend", selectlags = "AIC")
summary(adf.res) # serie nestationara

adf.vac <- ur.df(vacanta, type = "trend", selectlags = "AIC")
summary(adf.vac) # serie nestationara 

# Corelograma primei diferente
ggtsdisplay(diff(restaurant)) # seria pare nestationara
ggtsdisplay(diff(vacanta)) # seria pare stationara

# Testarea stationaritatii seriilor diferentiate
adf.res2 <- ur.df(diff(restaurant), type = "trend", selectlags = "AIC")
summary(adf.res2) # serie stationara

adf.vac2 <- ur.df(diff(vacanta), type = "trend", selectlags = "AIC")
summary(adf.vac2) # serie stationara 


# Identificarea lagurilor optime
df <- cbind(restaurant,vacanta)
colnames(df) <- cbind('Restaurant','Vacanta')

lagselect <- VARselect(df,lag.max = 12, type = 'const')
lagselect
lagselect$selection # lagul 5 conform AIC, FPE


# Implementarea VAR
model <- VAR(diff(df), p = 5, type = 'const', season = NULL, exog = NULL)
summary(model)

stargazer(model[['varresult']], type = 'text')

# Diagnosticul pe reziduuri

# Autocorelarea
Serial <- serial.test(model, lags.pt = 12, type = 'PT.asymptotic')
Serial # pvalue > 0.1 nu avem autocorelare in reziduuri

# Heteroscedasticitate
Arch <- vars::arch.test(model,lags.multi = 1,multivariate.only = TRUE)
Arch # pvalue < 0.1 heteroschedasticitate

# Normalitatea reziduurilor
Norm <- normality.test(model, multivariate.only = TRUE)
Norm # pvalue JB < 0.1 reziduurile nu sunt normal distribuite

# Testarea pentru rupturi in serie
Stability <- stability(model,type = 'OLS-CUSUM')
plot(Stability) # model stabil in cazul cheltuielilor cu vacanta 
# si putin instabil in cazul cheltuielilor cu vacanta la restaurant

# Cauzalitate Granger
Granger_restaurant <- causality(model, cause = 'Restaurant')
Granger_restaurant # p < 0.1 => Restaurant prezinta cauzalitate Granger cu Vacanta

Granger_vacanta <- causality(model, cause = 'Vacanta')
Granger_vacanta # p > 0.1 => Vacanta prezinta cauzalitate Granger cu Restaurant

# Functia de raspuns la impuls (IRF) 
restaurant_irf <- irf(model, impulse = 'Vacanta', response = 'Restaurant', 
                      n.ahead = 10, boot = TRUE, ci=0.90) 
plot(restaurant_irf, ylab = 'Restaurant', 
     main = 'Raspunsul cheltuielilor de vacanta cu restaurantul la socurile cheltuielilor de vacanta')

vacanta_irf <- irf(model, impulse = 'Restaurant', response = 'Vacanta', 
                   n.ahead = 10, boot = TRUE, ci=0.90)
plot(vacanta_irf, ylab = 'Vacanta', 
     main = 'Raspunsul cheltuielilor de vacanta la socurile cheltuielilor de vacanta cu resaturantul')

# Descompunerea variante

FEVD <- fevd(model, n.ahead = 12)
FEVD
plot(FEVD) # aproximativ 4% din inovatiiile in restaurant sunt explicate de 
# inovatiile provenite din partea Vacantei
# aproximativ 9% din inovatiile in vacanta provin din partea restaurantului

# Prognoza VAR
model_forecast <- VAR(df, p = 5, type = 'const', season = NULL, exog = NULL)
forecast <- predict(model_forecast, n.ahead = 12, ci = 0.99) # prognoza pe 1 an

plot(forecast, name = 'Restaurant')
plot(forecast, name = 'Vacanta')

fanchart(forecast, names='Restaurant')
fanchart(forecast, names='Vacanta')


