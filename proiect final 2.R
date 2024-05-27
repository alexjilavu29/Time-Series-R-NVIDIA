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
#install.packages("PerformanceAnalytics")


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
prices


prices2 = data.frame(prices[,c(2,7)],row.names = 1)

prices_ts <- ts(prices2$price_close, start=2017, frequency = 12)
prices_ts
autoplot(prices_ts) +
  ylab("Price") + xlab("Data") +
  theme_bw()

training <- window(prices_ts, start=2017, end=c(2023,4))
training

ndiffs(training)

autoplot(training) +
  ylab("Price") + xlab("Data") +
  theme_bw()

test <- tail(prices_ts, 10) 
# window(y, start=2018, end=c(2020,12))
test

#training <- Return.calculate(price_close_xts, method = "log")
#prices2<- prices2[(-1),]
#plot(training, type="l")

rent_ts <- ts(training, start=2017, frequency = 12)
rent_ts


# ### Holt-Winter metodele sezoniere ### ----------------------------------


##adecvata seriilor ce prezinta tendinta si  componenta sezoniera.
##Metoda implica trei ecuatii de recurenta, si prin urmare trei  constante de netezire, una pentru 
##nivelul seriei, una pentru panta  dreptei de tendinta respectiv una pentru coeficientii sezonalitatii
## in functie de modelul de descompunere a seriei, aditiv sau multiplicativ, exista doua variante ale metodei
## HW multiplicativ si HW aditiv

fit1 <- hw(training,seasonal="additive") # HW aditiv
fit2 <- hw(training,seasonal="multiplicative") # HW multiplicativ
autoplot(training) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Price in dollars") +
  ggtitle("HW forecasts for Nvidia stock prices") +
  guides(colour=guide_legend(title="Forecast"))
# Datele arata un model sezonier evident, cu vârfuri observate in trimestrul martie al fiecarui an,
# corespunzator verii australiene.

# Verificarea acuratetii modelelor
round(accuracy(fit1),2)
round(accuracy(fit2),2)

summary(fit1)
summary(fit2)


# ### Modelul ETS - Exponential smoothing with state space  ---------------


# Fiecare model consta dintr-o ecuatie de masurare care descrie datele observate si unele ecuatii de 
# stare care descriu modul in care componentele sau starile neobservate (nivel, tendinta, sezonier) se
# modifica in timp. Prin urmare, acestea sunt denumite space state models.
fit_ets <- ets(training) # functia ets permite mai multi parametri, dar nu intra in scopul materiei de la licenta

summary(fit_ets) # acuratetea modelului

autoplot(fit_ets) # componentele modelului
# Valorile mici ale lui β si γ inseamna ca componente precum panta si sezonalitatea se schimba foarte
# putin in timp. 

fit_ets %>% forecast::forecast(h=5) %>% # prognoza modelului
  autoplot() +
  ylab("Price in dollars")
# Intervalele de predictie inguste indica faptul ca seria este relativ 
# usor de prevazut datorita tendintei puternice si sazonalitatii.

# ### Testarea acuratetii prognozelor modelelor ### -----------------------


# Pentru a putea compara acuratetea prognozelor vom folosi testul Diebold Mariano
# Testul Diebold Mariano
# H0: prognozele au aceeasi acuratete
# H1: prognozele au acuratete diferita

# fit1 - HW aditiv
# fit2 - HW multiplicativ
# fit_ets - ETS

dm.test(residuals(fit1),residuals(fit2))# deoarece p < 0.1 respingem H0
# modelul HW aditiv si HW multiplicativ au prognoze diferite
dm.test(residuals(fit1),residuals(fit_ets))# deoarece p < 0.1 respingem H0
# modelul HW aditiv si ETS au prognoze diferite
dm.test(residuals(fit2),residuals(fit_ets)) # deoarece p > 0.05 acceptam H0 pentru un prag de semnificatie de 5%
# modelul HW multiplicativ si ETS nu prezinta prognoze cu acuratete diferita

# In urma testarii Diebold-Mariano, exista diferente in acuratetea modelelor
# cu exceptia HW multiplicativ si ETS pentru ca probabilitatea este mai mare
# de 10%. In acest caz, este indicat sa se foloseasca pentru prognoza
# modelul cu indicatorii de acuratete (RMSE,MAE,MAPE, etc.) cei mai mici

round(accuracy(fit1),2)
round(accuracy(fit2),2)
round(accuracy(fit_ets),2)
#fit 2 are cele mai mici erori, deci continuam prognoza cu HW multiplicativ

# ### Diagnosticul pe reziduuri ### ---------------------------------------


# Dupa ce aplicam un model trebuie sa verificam daca reziduurile se autocoreleaza
# Reziduurile sunt utile pentru a verifica daca modelul a capturat in mod adecvat informatia din serie

# Un model bun de prognoza va produce reziduuri cu urmatoarele proprietati

# 1. reziduurile nu sunt corelate. Daca reziduurile sunt corelate
# inseamna ca informatia ramasa in reziduuri trebuie folosita in prognoza

# 2. reziduurile au media 0. Daca reziduurile au media diferita de 0, 
# rezultatele prognozate sunt biased

# 3. reziduurile au varianta constanta - testarea conditionata a heteroscedasticitatii

# 4. reziduurile sunt normal distribuite

res_hw_mlp <- residuals(fit2) 
autoplot(res_hw_mlp) + xlab("Day") + ylab("") +
  ggtitle("Residuals from HW multiplicative")

# Histograma reziduurilor
gghistogram(res_hw_mlp) + ggtitle("Histogram of residuals")

# Testarea prin Jarque-Berra a normalitatii reziduurilor
# H0: seria este normal distribuita
# H1: seria nu este normal distribuita

jarque.bera.test(res_hw_mlp) # deoarece p-value > 0.05, seria este distribuita normal

# Functia de autocorelatie a reziduurilor
ggAcf(res_hw_mlp) + ggtitle("ACF of residuals")

# Testul Box-Pierce
# H0: seria reziduurilor nu prezinta autocorelare 
# H1: seria reziduurilor prezinta autocorelare
Box.test(res_hw_mlp, lag=1)
Box.test(res_hw_mlp, lag=2)
Box.test(res_hw_mlp, lag=3)
Box.test(res_hw_mlp, lag=4)
Box.test(res_hw_mlp, lag=5)  #  p-value > 0.1 => seria reziduurilor nu prezinta autocorelare
Box.test(res_hw_mlp, lag=10) #  p-value > 0.1 => seria reziduurilor nu prezinta autocorelare 

# Testul Ljung-Box
# H0: seria reziduurilor nu prezinta autocorelare
# H1: seria reziduurilor prezinta autocorelare

Box.test(res_hw_mlp, lag=1,type="Lj")
Box.test(res_hw_mlp, lag=2, type="Lj")
Box.test(res_hw_mlp, lag=3, type="Lj")
Box.test(res_hw_mlp, lag=4, type="Lj")
Box.test(res_hw_mlp, lag=5, type="Lj")  
Box.test(res_hw_mlp, lag=10, type="Lj") # p-value > 0.1 => seria reziduurilor nu prezinta autocorelare
##Box Pierce este o varianta simplificata a testului Ljung-Box

# Exista o functie care automatizeaza procesul de diagnostic pe reziduuri
# Recomand testarea pas cu pas a reziduurilor si folosirea functiei checkresiduals
# doar atunci cand vrem sa testam rapid daca avem un model bun pentru prognoza
checkresiduals(fit1)
checkresiduals(fit2)

# Stationaritate ----------------------------------------------------------

# O serie de timp stationara nu prezinta modificari sistematice in medie si in varianta
# Seria stationara este acea serie ale carei valori oscileaza, mai mult sau mai putin aleator, 
# in jurul unui nivel de referinta - media, fiind deci intr-o stare de echilibru
# O serie de timp este stationara daca media, dispersia si autocovarianta (la diferite laguri/decalaje)
# raman aceleasi indiferent de momentul la care sunt masurate
# Seriile de timp nestationare prezinta trend, sezonalitate


# Detectarea stationaritatii prin metoda grafica
autoplot(training) +
  ggtitle('Monthly change in the Nvidia stock price') +
  theme_bw() 

# Detectarea stationaritatii cu ajutorul functiei de autocorelatie (ACF)

# Autocorelatia reprezinta gradul de similitudine dintre o serie de timp 
# si o versiune intarziata a acesteia pe intervale de timp succesive
# Autocorelatia masoara relatia dintre valoarea actuala a unei variabile si 
# valorile trecute

ggAcf(training) #avem multe laguri in afara liniilor punctate => nestationara
ggAcf(diff(training)) # aproape toate lagurile sunt in intervalul liniilor punctate => stationara

# Detectarea stationaritatii cu ajutorul testelor statistice # 

# Testul Augmented Dickey Fuller (ADF)
# H0: seria admite o radacina unitara si este nestationara 
# H1: seria nu admite o radacina unitara si este stationara

tseries::adf.test(training) # nestationara
tseries::adf.test(training, k=1) # stationara
tseries::adf.test(training, k=2) # stationara
tseries::adf.test(training, k=3) # stationara
tseries::adf.test(training, k=4) # stationara
tseries::adf.test(training, k=5) # stationara
tseries::adf.test(training, k=6) # stationara
tseries::adf.test(training, k=7) # stationara
tseries::adf.test(training, k=8) # stationara
tseries::adf.test(training, k=9) # stationara
tseries::adf.test(training, k=10) # stationara

# Pentru o acuratete cat mai mare a testului aplicam
# none - elemente deterministe
# drift - pentru constanta
# trend - pentru constanta si trend 

# Radacina unitara pentru elementele deterministe
rw_none <- ur.df(training, type='none', selectlags = c("AIC"))
summary(rw_none) # valoarea testului statistics |0.7635| < |-2.58| / |-1.95| / |-1.62| (tau)
# seria nu este stationara 
# probabilitatea z.lag.1 > 0.1 deci seria nu este stationara

rw_none <- ur.df(diff(training), type='none', selectlags = c("AIC"))
summary(rw_none) # valoarea testului statistics |-5.7847| > |-2.58| / |-1.95| / |-1.62| (tau)
# seria este stationara 
# probabilitatea z.lag.1 < 0.1 deci seria este stationara


# Radacina unitara pentru intercept
rw_t <- ur.df(training, type='drift', selectlags = c("AIC"))
summary(rw_t) # valoarea testului statistic |0.9956| < |tau2| => serie nestationara
# prob z.lag.1 > 0.1 => serie nestationara 

rw_t <- ur.df(diff(training), type='drift', selectlags = c("AIC"))
summary(rw_t) # valoarea testului statistic |17.7292| > |tau2| => serie stationara
# prob z.lag.1 < 0.1 => serie stationara 


# Radacina unitara in trend si intercept
rw_ct <- ur.df(training, type='trend', selectlags = c("AIC"))
summary(rw_ct) # valoarea testului statistic < |tau3| => serie stationara
# prob z.lag.1 > 0.1 => serie nestationara 

rw_ct <- ur.df(diff(training), type='trend', selectlags = c("AIC"))
summary(rw_ct) # valoarea testului statistic  > |tau2| => serie stationara
# prob z.lag.1 < 0.1 => serie stationara 

# Testul KPSS (Kwiatkowski-Phillips-Schmidt-Shin)
# H0: seria este stationara 
# H1: seria nu este stationara

training %>% ur.kpss() %>% summary() # valoarea testului 1.5873 > toate valorile critice
# seria este nestationara 

diff(training) %>% ur.kpss() %>% summary() # valoarea testului 0.1022 < toate valorile critice
# seria este stationara 


# Testul Phillips-Perron
# H0: seria admite o radacina unitate
# H1: seria nu admite o radacina unitate

PP.test(training) # serie nestationara ; p-value > 0.1
PP.test(diff(training)) # serie stationara; p-value <0.1

# Functia ndiffs ne spune de cate diferente in nivel avem nevoie pentru ts
# Aceasta functie nu captureaza si nevoia de diferenta sezoniera

ndiffs(training)
ndiffs(diff(training))

nsdiffs(training) # diferenta sezoniera nu e recomandata
?ndiffs

# ?Structural breaks -------------------------------------------------------
#NICI NU STIU DACA TRBUIE SA IL FACEM DACA SINCER

# Rupturile in structura se refera la punctele de cotitura ale coeficientilor regresiei seriilor
# studiaza daca seria este stationara cu sau fara ruptura in serie
# Chiar daca o serie este stationara in fiecare sectiune, majoritatea testelor standard pentru 
# stationaritate vor avea o tendinta catre non-respingerea radacinii unitate pentru aceasta serie. 
# Acest lucru este problematic, deoarece este necesar sa folosim tehnici de modelare diferite pentru 
# serii cu radacina unitate si pentru cele cu ruptura structurala.


# Testul Quandt Likelihood Ratio (QLR)

# Pas 1 - cream un obiect tibble care va contine variabila originala (lag0)
# si variabila originala cu un decalar (lag1)
data_qlr <- tibble(ylag0 = training,
                   ylag1 = lag(as.numeric(training))) 

# Pas 2 - aplicam o regresie falsa in care variabila dependenta este variabila
# cu lagul 0 (variabila originala) si variabila independenta este variabila
# cu 1 decalaj (lag1)
qlr <- Fstats(ylag0 ~ ylag1, data = data_qlr)

# Pasul 3 - estimam punctul in care avem structural change
breakpoints(qlr)

# Pasul 4 - testam semnificatia punctului
# daca p < 0.1 inseamna ca avem structural change semnificativ
sctest(qlr, type = "supF")

# Pasul 5 - reprezentam grafic 
autoplot(training) +
  #geom_vline(xintercept = 58, colour = 'red') + # identificam initial punctul in grafic
  # si aproximam valoarea pentru a trasa linia orizontala 
  geom_hline(yintercept = 255, colour = 'red')+
  ggtitle("Structural break for Nvidia stock price") +
  ylab("Price") +
  theme_bw()

### Testarea radacinii unitare atunci cand avem structural break ###

# Testul Zivot & Andrews se foloseste atunci cand avem o ruptura endogena in
# structura seriei
# H0: Serie are o radacina unitate cu un structural break
# H1: Serie nu are o radacina unitate cu un structural break

# Aplicarea testului
za_test <- ur.za(training, model = "intercept", lag = 0) 
# Statistica testului
summary(za_test) # p < 0.1 seria este nestationara cu structural break in nivel
#|test statistic| < |critical values| resping alternativa si accept nula 

plot(za_test) # graficul seriei ilustreaza ca seria noastra este nestationara
# si  are o ruptura in serie deoarece liniile orizontale se intersecteaza cu seria 
#LA NOI SOMEHOW NU SE INTERSECTEAZA


# Sezonalitate ------------------------------------------------------------

### Testarea radacinii unitare sezoniere ###

# Testul Hylleberg, Engle, Granger and Yoo (HEGY) este folosit pentru a 
# determina radacinile unitare sezoniere
# H0: seria prezinta radacina unitate sezoniera
# H1: seria nu prezinta radacina unitate sezoniera

hegy.test(training) # p > 0.1 acceptam nula, deci seria prezinta radacina unitate sezoniera
# si este nevoie sa diferentiem sezonier seria

hegy.test(diff(training))# p < 0.1 respingem nula, seria nu prezinta radacine unitate sezoniera
# deci nu mai este nevoie sa o diferentiem sezonier

ggseasonplot(training, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Price in USD") +
  ggtitle("Seasonal plot")

# Acestea sunt exact aceleasi date prezentate anterior, dar acum datele din fiecare sezon sunt 
# suprapuse. Un grafic sezonier permite observarea mai clara a modelului sezonier subiacent si 
# este util in special pentru identificarea anilor in care acest model se schimba.
# In acest caz, este clar ca exista o crestere mare a vanzrilor in ianuarie si in fiecare an.

ggsubseriesplot(training) +
  ylab("Price in USD") +
  ggtitle("Seasonal subseries plot")
# Liniile orizontale indica mediile pentru fiecare luna. Aceasta forma de grafic permite observarea 
# clara a modelului sezonier.


# Descompunerea multipilcativa a unei serii de timp
#training %>%
#  model(
#    classical_decomposition(training, type = "multiplicative")
#  ) %>%
#  components() %>%
#  autoplot() +
#  labs(title = "Classical multiplicative decomposition of total
#                  US retail employment")


fit <- auto.arima(diff(log(training))) #SALVAT DOAR DACA O SA NE TREBUIASCA FUNCTIA
coeftest(fit) # coeficientii sunt nesemnificativi, asa ca vom testa alte combinatii







# # ARIMA pentru date cu sezonalitate -------------------------------------


autoplot(training) +
  ggtitle("Monthly change in the Nvidia stock price") +
  ylab('Price in USD') +
  theme_bw() 

# Testarea sezonalitatii
ggsubseriesplot(training) + theme_bw() +
  ggtitle('Sesonality patterns') +
  ylab('Price in USD')
# sezonalitate puternica deoarece mediile pe luna prezinta diferente mari 

# Vom descompune seria printr-o tehnica mai puternica decat mediile mobile
# folosita in majoritatea cazurilor - STL
train_adj <- training %>% stl(s.window='periodic') %>% seasadj() 

autoplot(train_adj) +
  ggtitle("Seria lunara ajustata a preturilor actiunilor NVDA") +
  ylab('USD') +
  theme_bw()

# Retestarea sezonalitatii
ggsubseriesplot(train_adj) + theme_bw() +
  ggtitle('Sesonality patterns') +
  ylab('Monthly percentage change') # sezonalitatea este eliminata 

# Testarea stationaritatii seriei ajustate sezonier

# ADF 
rw_none <- ur.df(train_adj, type='none', selectlags = c("AIC"))
summary(rw_none) # nestationara, val test < val critice
rw_t <- ur.df(train_adj, type='drift', selectlags = c("AIC"))
summary(rw_t) # nestationara
rw_ct <- ur.df(train_adj, type='trend', selectlags = c("AIC"))
summary(rw_ct) # nestationara
# Conform testului ADF seria nestationara

# KPSS
train_adj %>% ur.kpss() %>% summary() # valoarea testului > val critice
# seria este nu este stationara

# Philips-Perron
PP.test(train_adj) # p > 0.5 serie nestationara

# Seria este nestationara analizand toate testele si vom avea nevoie sa o diferentiem
ndiffs(train_adj)

# Corelograma seriei
ggtsdisplay(train_adj) # conform ACF seria noastra este nestationara si vom produce 
# diferenta de ordin 1

ggtsdisplay(diff(train_adj)) # seria devine stationara dupa prima diferenta conform corelogramei
# vom testa din nou cu testele de stationaritate

# KPSS
diff(train_adj) %>% ur.kpss() %>% summary() # valoarea testului 0.1 <  valorile critice
# seria este stationara 

# Philips-Perron
PP.test(diff(train_adj)) # p < 0.01 serie stationara

# Identificarea modelului optim ARIMA(p,d,q)
ggtsdisplay(diff(train_adj)) # modelul optim conform corelogramei ar putea fi 
# ARIMA(0,1,0) deoarece avem 0 laguri semnificative la PACF pentru componenta AR
# d = 1 deoarece diferentiem o data seria pentru a obtine stationaritatea
# MA(0) deoarece nu avem niciun lag seminificativ

fit <- auto.arima(train_adj)
coeftest(fit)

fit1 <- Arima(train_adj, order=c(3,1,1)) #SALVAT DOAR DACA O SA NE TREBUIASCA FUNCTIA
coeftest(fit1) # coeficientii sunt nesemnificativi, asa ca vom testa alte combinatii


# Diagnosticul pe reziduuri al modelului
residuals_fit <- residuals(fit)
ggtsdisplay(residuals_fit) # avem autocorelare in reziduuri conform ACF 
Box.test(residuals_fit, lag=1,type="Lj") # nu avem autocorelare in reziduuri deoarece
# p > 0.1
Box.test(residuals_fit, lag=2,type="Lj") # nu avem autocorelare in reziduuri 
# la 95% deoarece pvalue > 0.05
Box.test(residuals_fit, lag=3,type="Lj") # avem autocorelare in reziduuri

checkresiduals(fit) # putem folosi la limita acest model pentru prognoza
jarque.bera.test(residuals_fit)
ArchTest(residuals_fit,lags = 1) #nu avem arch effects
# Graficul prognozei
autoplot(forecast(fit, h=5)) # graficul pe seria ajustata

# Testarea inversabilitatii
autoplot(fit)

# Graficul seriei originale+predictia
valorile_previzionate <- forecast(fit, h=5)
valori_previzionate_de_adaugat <- valorile_previzionate$mean
autoplot(training) +
  autolayer(valori_previzionate_de_adaugat, series = 'ARIMA(0,1,0)')


# SEMINAR 4 ---------------------------------------------------------------

# SARIMA 

# Atunci cand seria de timp prezinta sezonalitate puternica, o alternativa mai 
# buna a modelului ARIMA, este modelul SARIMA (seasonal autoregresive integrated moving average)
# deoarece tine cont si de sezonalitatea seriei

# Modelele de tip SARIMA(p,d,q)(P,D,Q)m se formeaza incluzand componenta sezoniera
# m reprezinta numarul de observatii per an

# Componenta sezoniera pentru AR si MA se identifica tot pe baza corelogramei
# De exemplu un model ARIMA(0,0,0)(0,0,1)12 va arata doar un lag (bat) semnificativ 
# la lagul 12 pe ACF si scadere lenta pe PACF la lagurile sezoniere

# Modelarea SARIMA este identica cu cea de la ARIMA pentru componentele AR si MA, 
# adaugandu-se la acestea si componentele SAR si SMA

# Exemplu pe date trimiestriale retail index

# Graficul seriei
autoplot(training) + ylab("Price in USD") + xlab("Year")  + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# Graficul de sezonalitate
ggsubseriesplot(training) +
  ylab("Price in USD") +
  ggtitle("Seasonal subseries plot") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# Corelograma seriei 
ggtsdisplay(training)
ggtsdisplay(diff(training))


# Testarea radacinii unitare
#none
rw_none <- ur.df(training, type='none', selectlags = c("AIC"))
summary(rw_none) # nonstationary ts
#drift
rw_t <- ur.df(training, type='drift', selectlags = c("AIC"))
summary(rw_t) # nonstationary ts
#trend
rw_ct <- ur.df(training, type='trend', selectlags = c("AIC"))
summary(rw_ct) # nonstationary ts

# KPSS
training %>% ur.kpss() %>% summary() # t > critical values => nonstationary ts

# Philips-Perron
PP.test(training) # p > 0.1 => nonstationary ts

# Testarea radacinii unitare sezoniere 
# Hegy 
hegy.test(training) # p > 0.1 for t_1 => seasonal unit root
# Canova Hansens test 
# H0: stable seasonal pattern
# H1: unstable seasonal pattern
ch.test(training) # p < 0.1 stable seasonal pattern only for half of the data

# Seria prezinta nestationaritate pe componenta sezoniera, dar si in nivel 

# Vom aplica diferenta sezoniera in prima faza
training %>% diff(lag=12) %>% ggtsdisplay() # seria ramane nestationara si este nevoie
# si de diferenta de ordin 1 
# la proiect, este nevoie sa testati din nou cu cu ADF, KPSS, PP

# Seria diferentiata sezonier si de ordin 1 
training %>% diff(lag=12) %>% diff() %>% ggtsdisplay() # seria pare sa devina stationara 
# si vom testa din nou cu ADF, KPSS, PP

training_diff <- training %>% diff(lag=12)

#none
rw_none <- ur.df(training_diff, type='none', selectlags = c("AIC"))
summary(rw_none) # stationary ts
#drift
rw_t <- ur.df(training_diff, type='drift', selectlags = c("AIC"))
summary(rw_t) # stationary ts 
#trend
rw_ct <- ur.df(training_diff, type='trend', selectlags = c("AIC"))
summary(rw_ct) # stationary ts

# KPSS
training_diff %>% ur.kpss() %>% summary() # t < critical values => stationary ts

# Philips-Perron
PP.test(training_diff) # p < 0.01 => stationary ts

ggtsdisplay(training_diff)

# Pe baza corelogramei putem identifica lagurile maximale pe toate componentele 
# Lagurile AR si MA se identifica similar cu ARIMA - AR(1), MA(1)
# Componenta SAR se identifica pe PACF pentru lagurile sezoniere SAR(1)
# Componenta SMA se identifica pe ACF pentru lagurile sezoniere SMA(1)
# (p,d,q)            (P,D,Q)

fit1 <- Arima(training_diff,order=c(1,1,4), seasonal=c(1,1,4))
coeftest(fit1) 

fit2 <- Arima(training,order=c(1,1,1), seasonal=c(0,1,1))
coeftest(fit2) 

fit3 <- Arima(training,order=c(0,1,1), seasonal=c(1,1,1))
coeftest(fit3) 

fit4 <- Arima(training,order=c(0,1,1), seasonal=c(0,1,1)) 
coeftest(fit4) 

fit5 <- Arima(training,order=c(1,1,0), seasonal=c(0,1,1))
coeftest(fit5)  

fit6 <- Arima(training,order=c(1,1,0), seasonal=c(1,1,0))
coeftest(fit6)  

arima_auto <- auto.arima(training_diff) #ARIMA(1,0,0)(0,0,1)[12]
summary(arima_auto) # AIC=605.77   AICc=606.44   BIC=614.4
coeftest(arima_auto) #toti sunt semificativi

summary(fit4) # AIC = 75.28   BIC = 81.51  ARIMA(0,1,1)x(0,1,1)[4]
summary(fit5) # AIC = 71.41   BIC = 77.64  ARIMA(1,1,0)x(0,1,1)[4]
summary(fit6) # AIC = 76.57   BIC = 82.8   ARIMA(1,1,0)x(1,1,0)[4]

# Modelul cu cele mai mici criterii informationale este SARIMA(1,1,0)(0,1,1)4
# si vom testa reziduurile

# Testarea reziduurilor 
checkresiduals(fit5)
#Autocorelare
Box.test(residuals(fit5),lag = 1, type = 'Lj')
Box.test(residuals(fit5),lag = 2, type = 'Lj')
Box.test(residuals(fit5),lag = 3, type = 'Lj')
Box.test(residuals(fit5),lag = 4, type = 'Lj')
Box.test(residuals(fit5),lag = 5, type = 'Lj') # reziduurile nu prezinta autocorelare
Box.test(residuals(fit5),lag = 8, type = 'Lj')
Box.test(residuals(fit5),lag = 12, type = 'Lj')
# Normalitate
jarque.bera.test(residuals(fit5)) # reziduuri normal distribuite
# Heteroschedasticitate
ArchTest(residuals(fit5), lags = 1)
ArchTest(residuals(fit5), lags = 2)
ArchTest(residuals(fit5), lags = 3)
ArchTest(residuals(fit5), lags = 4)
ArchTest(residuals(fit5), lags = 8)
ArchTest(residuals(fit5), lags = 12) # reziduurile nu prezinta heterosc

# Prognoza 
fit5 %>% forecast(h=12) %>% autoplot() + ylab('Retail index') +
  theme_bw() +theme(plot.title = element_text(hjust = 0.5))

# Estimare ETS
fit_ets <- ets(training)
summary(fit_ets) # RMSE = 0.35  MAE = 0.26 MAPE = 0.27
summary(fit5)    # RMSE = 0.39  MAE = 0.28 MAPE = 0.23
# Modelul ETS are erorile de prognoza mai mici

# Prognoza ETS
fit_ets %>% forecast::forecast(h=12) %>%
  autoplot() +
  ylab("Retail index") + 
  theme_bw() +theme(plot.title = element_text(hjust = 0.5))

# Testul Diebold Mariano
dm.test(residuals(fit_ets),residuals(fit5))# deoarece p > 0.11 acceptam H0
# modelele SARIMA(1,1,0)(0,1,1)[4] si ETS au prognoze similare

# Graficul celor doua modele 
autoplot(training) +
  autolayer(fit5 %>% forecast::forecast(h=12), series="SARIMA(1,1,0)(0,1,1)[4]", PI=FALSE) +
  autolayer(fit_ets %>% forecast::forecast(h = 12), series="ETS",PI=FALSE) +
  xlab("Time") +
  ylab("Retail index") +
  ggtitle("The forecast of retail index") +
  guides(colour=guide_legend(title="Forecast")) +
  theme_bw()

#########################################################

# Exemplu date lunare vanzare pastile 

# Seria logaritmana
lh02 <- log(training)
# Graficul celor doua serii
cbind("H02 sales (million scripts)" = training,
      "Log H02 sales"=lh02) %>%
  autoplot(facets=TRUE) + xlab("Year") + ylab("") + theme_bw()
# Vom merge mai departe cu seria logaritmata deoarece seria originala este nestationara
# si in varianta

# Graficul seriei
autoplot(lh02) + ylab("H02 sales (million scripts)") + xlab("Year") +
  ggtitle('Monthly H02 sales') + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# Graficul de sezonalitate
ggsubseriesplot(lh02) +
  ylab("H02 sales") +
  ggtitle("Seasonal subseries plot: monthly H02 sales") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

# Corelograma seriei 
ggtsdisplay(lh02)

# Testarea radacinii unitare sezoniere 
# Hegy 
hegy.test(lh02) # p > 0.1 => seasonal unit root

# Corelograma seriei diferentiate sezonier
ggtsdisplay(diff(lh02,12)) # serie pentru care trebuie sa testam stationaritatea
lh02_ses_diff <- diff(lh02,12)

# Testarea radacinii unitare
#none
rw_none <- ur.df(lh02_ses_diff, type='none', selectlags = c("AIC"))
summary(rw_none) # stationary ts
#drift
rw_t <- ur.df(lh02_ses_diff, type='drift', selectlags = c("AIC"))
summary(rw_t) # stationary ts 
#trend
rw_ct <- ur.df(lh02_ses_diff, type='trend', selectlags = c("AIC"))
summary(rw_ct) # stationary ts

# KPSS
lh02_ses_diff %>% ur.kpss() %>% summary() # t > critical values => nonstationary ts

# Philips-Perron
PP.test(lh02_ses_diff) # p < 0.1 => stationary ts
# seria este stationara dupa diferenta sezoniera

# Pe baza corelogramei putem identifica lagurile maximale pe toate componentele 
# AR(3), MA(9) - dar trebuie testat si varianta in care avem un model AR pur
# deoarece betele scad lent, SAR(2), SMA(0)


fit1 <- Arima(lh02,order=c(3,0,9), seasonal=c(2,1,0))
coeftest(fit1) #MA nesemnificativ

fit2 <- Arima(lh02,order=c(3,0,3), seasonal=c(2,1,0))
coeftest(fit2) # MA(1), AR(1) nesemnificativ

fit3 <- Arima(lh02,order=c(3,0,2), seasonal=c(2,1,0))
coeftest(fit3) # MA(2) neseminificativ

fit4 <- Arima(lh02,order=c(3,0,1), seasonal=c(2,1,0))
coeftest(fit4) # AR(1) neseminificativ

fit5 <- Arima(lh02,order=c(3,0,0), seasonal=c(2,1,0))
coeftest(fit5) # AR(1) neseminificativ

fit6 <- Arima(lh02,order=c(2,0,1), seasonal=c(1,1,0))
coeftest(fit6) # toti coef semnificativi

fit7 <- Arima(lh02,order=c(1,0,2), seasonal=c(1,1,0))
coeftest(fit7) # toti coef semnificativi

# O alta regula de identificare a componentelor SAR si SMA este ca 
# daca lagurile sezoniere sunt pozitive atunci avem componente SAR
# iar daca lagurile sunt negative atunci testam componente SMA

fit8 <- Arima(lh02,order=c(3,0,9), seasonal=c(0,1,1))
coeftest(fit8) #MA nesemnificativ

fit9 <- Arima(lh02,order=c(3,0,3), seasonal=c(0,1,1))
coeftest(fit9) # toti coef semnificativi

fit10 <- Arima(lh02,order=c(3,0,2), seasonal=c(0,1,1))
coeftest(fit10) # toti coef semnificativi

fit11 <- auto.arima(lh02)
coeftest(fit11) # toti coef semnificativi

summary(fit6) # AIC = -459.14   BIC = -443.17
summary(fit7) # AIC = -459.71   BIC = -443.42
summary(fit9) # AIC = -489.91   BIC = -463.85 RMSE = 0.06
summary(fit10) # AIC = -483.71   BIC = -460.91
summary(fit11) # AIC = -484.51   BIC = -465 RMSE = 0.06

# Modelul cu cele mai mici criterii informationale este SARIMA(3,0,3)(0,1,1)[12]
# si vom testa reziduurile (fit9)

# Testarea reziduurilor 
checkresiduals(fit9)
#Autocorelare
Box.test(residuals(fit9),lag = 1, type = 'Lj')
Box.test(residuals(fit9),lag = 2, type = 'Lj')
Box.test(residuals(fit9),lag = 3, type = 'Lj')
Box.test(residuals(fit9),lag = 4, type = 'Lj')
Box.test(residuals(fit9),lag = 5, type = 'Lj') # reziduurile nu prezinta autocorelare
Box.test(residuals(fit9),lag = 12, type = 'Lj')
Box.test(residuals(fit9),lag = 24, type = 'Lj')
# Normalitate
jarque.bera.test(residuals(fit9)) # reziduuri nu sunt normal distribuite
# Heteroschedasticitate
ArchTest(residuals(fit9), lags = 1)
ArchTest(residuals(fit9), lags = 2)
ArchTest(residuals(fit9), lags = 3)
ArchTest(residuals(fit9), lags = 4)
ArchTest(residuals(fit9), lags = 12) # reziduurile prezinta heterosc la lagurile > 12
ArchTest(residuals(fit9), lags = 15) 

# Prognoza 
fit9 %>% forecast(h=12) %>% autoplot() + ylab('Retail index') +
  theme_bw() +theme(plot.title = element_text(hjust = 0.5))


# Exemplu rata somajului - aplicatia 2 proiect

# Import the dataset
unemployment_rate <- read_excel("D:\\Desktop\\CSIE\\An 3\\Sem 2 2023 - 2024\\Serii de timp\\unemployment_rate (1).xlsx")

# Creating unemployment rate time series object called y for simplifying the code
y <- ts(unemployment_rate, start=2000, frequency = 12)
y
# Splitting the data intro training and test sets
training <- window(y, start=2000, end=c(2017,12))
test <- tail(y, 12*3) # window(y, start=2018, end=c(2020,12))

# Time series plot
autoplot(y) +
  ggtitle("The evolution of monthly Romanian unemployment rate") +
  xlab("Year") +
  ylab("%") +
  theme_bw()

# Descriptive statistics for the series
ggplot(y, aes(x=y)) + 
  geom_histogram(bins = 25)+
  labs(title = "Distribution of Romanian Unemployment Rate")+
  xlab("")+ylab("") + theme_bw()
summary(y)
sd(y)
skewness(y)
kurtosis(y)
jarque.bera.test(y)

Series <- c("Sample","Observations","Mean","Median","Maximum","Minimum",
            "Std.Dev.","Skewness","Kurtosis","Jarque-Bera","Probability")
`Unemployment Rate` <- c("2000M01 - 2020M12",252,6.542,6.800,9.500,3.600,
                         1.260,-0.517,-0.267,12.014,"0.002***")
summary_statistics <- as.data.frame(cbind(Series,`Unemployment Rate`))
summary_statistics %>% 
  gt() %>% 
  tab_header(title = md("The **Romanian Unemployment Rate** statistics for the period 2000M1-2020M12"))

# Seasonality subseries
ggsubseriesplot(y) +
  ylab("%") +
  ggtitle("Seasonal subseries plot: monthly unemployment rate") +
  theme_bw()

# Polar seasonal plot
ggseasonplot(y, polar=TRUE) +
  ylab("%") +
  ggtitle("Polar seasonal plot:monthly unemployment rate ") +
  theme_bw()

# Correlogram 
ggtsdisplay(y)

# Holt Winter Additive forecast and accuracy
hw_additive <- hw(training,seasonal="additive",h=60) # 12 * 3 = 36 (2018-2020) + 12 * 2 (2021-2022)
summary(hw_additive)
forecast::accuracy(hw_additive,test)

# Holt Winter Multiplicative forecast and accuracy
hw_multiplicative <- hw(training,seasonal="multiplicative",h=60)
summary(hw_multiplicative)
forecast::accuracy(hw_multiplicative,test)

# HW models accuracy 
`Model 1: Holt-Winters multiplicative method` <- c("Smoothing parameters:",
                                                   "Alpha(level) = 0.6928",
                                                   "Beta(trend)  = 0.0001",
                                                   "Gamma(seasonal) = 0.0001",
                                                   "AIC= 630.187",
                                                   "AICc= 633.278",
                                                   "BIC= 687.566")
`Model 2: Holt-Winters additive method` <-c("Smoothing parameters:",
                                            "Alpha(level) = 0.7503",
                                            "Beta(trend)  = 0.0001",
                                            "Gamma(seasonal) = 0.0001",
                                            "AIC= 645.789",
                                            "AICc= 648.8807",
                                            "BIC= 703.169")
empirical_results_HW <- as.data.frame(cbind(`Model 1: Holt-Winters multiplicative method`,`Model 2: Holt-Winters additive method`))
empirical_results_HW %>% gt() %>% tab_header(
  title = md("**The empirical results of HW for the forecast of unemployment rate**"))

# Forecasting performance
Metrics <- c("ME","RMSE","MAE","MPE","MAPE","MASE")
`HW multiplicative training` <- c(-0.0123,0.2770,0.2086,-0.3191,3.0367,0.3317)
`HW multiplicative test` <- c(-0.2669,0.6905,0.6524,-7.8322,15.1393,1.0373)
`HW additive training` <- c(0.006,0.2804,0.2108,-0.1258,3.0699,0.3353)
`HW additive test` <- c(-0.03710,0.7480,0.6273,-2.6100,13.8267,0.9974)

forecasting_performance_hw <- as.data.frame(cbind(Metrics,
                                                  `HW multiplicative training`,
                                                  `HW multiplicative test`,
                                                  `HW additive training`,
                                                  `HW additive test`))


forecasting_performance_hw %>% gt() %>% tab_header(
  title = md("**Forecasting Performance of Holt-Winters**"))

# Plot of both hw models
autoplot(y) +
  autolayer(hw_additive, series="HW additive forecasts", PI=FALSE) +
  autolayer(hw_multiplicative, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Time") +
  ylab("Monthly unemployment rate(%)") +
  ggtitle("The forecast of unemployment rate based on HW models for the period 2021-2022") +
  guides(colour=guide_legend(title="Forecast")) +
  theme_bw()

# Fitting ets model
fit.ets_training<-ets(training)
#summary(fit.ets_training)

# ETS table preparation
`ETS(M, N, M) model: Multi-plicative Error, No trend, Multiplicative Season`<-
  c("Smoothing parameters:","Alpha(level) = 0.7914","Gamma(seasonal) = 0.0001",
    "AIC= 627.799","AICc= 630.199","BIC= 678.428")

empirical_results_ETS <- as.data.frame(`ETS(M, N, M) model: Multi-plicative Error, No trend, Multiplicative Season`)
empirical_results_ETS %>% gt() %>% tab_header(
  title = md("**The empirical results of ETS models for the forecast of unemployment rate**"))

# Forecasting ETS
fit.ets_training %>% forecast::forecast(h = 60) %>%
  forecast::accuracy() 

# Forecasting table preparation
Metrics <- c("ME","RMSE","MAE","MPE","MAPE","MASE")
`ETS training` <- c(-0.0166,0.2788,0.2097,-0.3682,3.0569,0.3335)
forecasting_performance_ets <- as.data.frame(cbind(Metrics,
                                                   `ETS training`))

forecasting_performance_ets %>% gt() %>% tab_header(
  title = md("**Forecasting Performance of ETS model**"))

# Components plot
autoplot(fit.ets_training)

# Checking the residuals
checkresiduals(fit.ets_training)

# Forecast plot ETS with funchart
training %>% ets() %>% forecast::forecast(h=60) %>% autoplot()

# SARIMA 
# Display the first lag difference in order to help to identify the SARIMA
training%>% diff(lag=1) %>% ggtsdisplay()
# Create the new lagged 1 difference series
training_sarima <- training%>% diff(lag=1) 

# Checking ADF
summary(ur.df(training, type = c("none"), lags = 1))
summary(ur.df(training_sarima, type = c("none"), lags = 1))

summary(ur.df(training, type = c("drift"), lags = 1))
summary(ur.df(training_sarima, type = c("drift"), lags = 1))

summary(ur.df(training, type = c("trend"), lags = 1))
summary(ur.df(training_sarima, type = c("trend"), lags = 1))

# Checking Philip-Peron
summary(ur.pp(training,type = c("Z-alpha"), model = c("constant", "trend")))
summary(ur.pp(training_sarima,type = c("Z-alpha"), model = c("constant", "trend")))

summary(ur.pp(training,type = c("Z-alpha"), model = c("constant")))
summary(ur.pp(training_sarima,type = c("Z-alpha"), model = c("constant")))

summary(ur.pp(training,type = c("Z-alpha"), model = c("trend")))
summary(ur.pp(training_sarima,type = c("Z-alpha"), model = c("trend")))

`Unit Root` <- c("ADF level","PP level","ADF first difference","PP first difference")
`T&C` <- c("-3.56**","-3.52**","-15.87***","-16.20***")
`C` <- c("-2.58*",  "-2.72*",       "-15.90***",    "-16.01***")
`None` <- c("-0.90",    "-0.98",        "-15.91***",    "-16.01***")

unit_root <- as.data.frame(cbind(`Unit Root`,`T&C`,`C`,`None`))

unit_root %>% gt() %>% tab_header(
  title = md("**Unit root analysis of the Romanian unemployment rate**")) %>%
  tab_source_note(
    source_note = "Note: ***, **, * means stationary at 1%, 5% and 10%; T&C represents the most general model with a constant and trend; C is the model with a constant and without trend; None is the most restricted model without a drift and trend"
  )

# Structural breaks - Zivot Andrew test
za_trend <- ur.za(training, model =  "trend")
za_both <- ur.za(training, model =  "both")
summary(za_trend)
summary(za_both)

# Creating the table for ZA test
row1 <- c("Statistics","Allowing for break in trend","Allowing for break in both intercept and trend")
row2 <- c("Minimum t-stat p-value", "-4.139 (0.000)",   "-4.501 (0.000)")
row3 <- c("1%", "-4.93",    "-5.57")
row4 <- c("5%", "-4.42",    "-5.08")
row5 <- c("10%",    "-4.11",    "-4.82")
row6 <- c("Potential break point","2015M08","2009M06")

zivot_andrew <- as.data.frame(rbind(row1,row2,row3,row4,row5,row6))

zivot_andrew %>% gt() %>% tab_header(
  title = md("**Zivot-Andrews unit root test having structural break for unemployment rate**")) 

# Hegy Test
hegy.test(training) 

# Fitting the optimal SARIMA model
fit_sarima <- Arima(training, order=c(0,1,6), seasonal=c(1,0,1))

# Coefficients test
coeftest(fit_sarima)

# Summary of results
summary(fit_sarima)

# Check residuals plot
checkresiduals(fit_sarima, lag=48)

# Ljung Box test for residuals
Box.test(fit_sarima$residuals, lag = 12, type = c("Ljung-Box"))
Box.test(fit_sarima$residuals, lag = 24, type = c("Ljung-Box"))
Box.test(fit_sarima$residuals, lag = 36, type = c("Ljung-Box"))
Box.test(fit_sarima$residuals, lag = 48, type = c("Ljung-Box"))

jarque.bera.test(fit_sarima$residuals)

# Arch LM test for residuals
ArchTest(fit_sarima$residuals, lags=12)
ArchTest(fit_sarima$residuals, lags=24)
ArchTest(fit_sarima$residuals, lags=36)
ArchTest(fit_sarima$residuals, lags=48)


# Ljung Box and ARCH LM table
Lags <- c("Ljung-Box test","P-value","ARCH-LM test","P-value")
`12` <- c("2.9459", "0.9959",   "9.1184",   "0.6928")
`24` <- c("15.123", "0.9171",   "44.267",   "0.2345")
`36` <- c("25.531", "0.9029",   "51.336",   "0.1878")
`48` <- c("40.434", "0.7727",   "58.159",   "0.1495")

lb_archlm <- as.data.frame(cbind(Lags,`12`,`24`,`36`,`48`))


lb_archlm %>% gt() %>% tab_header(
  title = md("**Empirical results of Ljung-Box test and ARCH-LM test for model residuals**"))

# Forecast sarima
fit_sarima_accuracy<- fit_sarima %>% forecast::forecast(h=60)

# Check the accuracy
forecast::accuracy(fit_sarima_accuracy, test)

# Forecast plot
fit_sarima_accuracy %>% forecast::forecast(h=60) %>%autoplot()







