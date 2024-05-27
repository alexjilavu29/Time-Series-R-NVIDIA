
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
# Citirea preturilor din fisierul output_benzina_motorina.csv si crearea unui dataframe
gas_prices <- read.csv("output_benzina_motorina.csv", header = TRUE, sep = ",")
gas_prices <- gas_prices[order(gas_prices$Date),]
gas_prices <- gas_prices[1:3135,]
dim(gas_prices)

oil_prices <- read.csv("output_petrol.csv", header = TRUE, sep = ",")
oil_prices <- oil_prices[order(oil_prices$Data),]
oil_prices
oil_prices <- oil_prices[1:3135,]
dim(oil_prices)
colnames(oil_prices)<-c("Data","Petrol")

# Citirea primelor 3135 de inregistrari ale preturilor la benzina din gas_prices si preturilor la petrol din oil_prices si crearea unui dataframe cu row.names = Date
gas_oil <- data.frame(gas_prices[, c(2,3)], oil_prices[,2], row.names = gas_prices$Date)
# Redenumirea coloanelor
colnames(gas_oil) <- c("Benzina", "Motorina", "Petrol")

# Converteste datele zilnice in date lunare si atribuie unor noi variabile 
# Preturile lunare ale benzinei si motorinei
gas_oil_month <- gas_prices %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  mutate(Year = year(Date)) %>%
  mutate(Month = month(Date)) %>%
  group_by(Year, Month) %>%
  summarise(Benzina = mean(Benzina), Motorina = mean(Motorina))
# Preturile lunare ale petrolului
oil_prices_month <- oil_prices %>%
  mutate(Data = as.Date(Data, format = "%Y-%m-%d")) %>%
  mutate(Year = year(Data)) %>%
  mutate(Month = month(Data)) %>%
  group_by(Year, Month) %>%
  summarise(Petrol = mean(Petrol))
# Concatenarea datelor lunare 
gas_oil_month <- cbind(gas_oil_month, oil_prices_month$Petrol)
# Scrierea corecta a datei
gas_oil_month$Date <- as.Date(paste(gas_oil_month$Year, gas_oil_month$Month, "01", sep = "-"))
# Stergerea coloanelor Year si Month
gas_oil_month <- gas_oil_month[, -c(1,2)]
# Redenumirea coloanelor
colnames(gas_oil_month) <- c("Benzina","Motorina","Petrol","Date")
# Crearea unui data frame cu row.names = Date
gas_oil_month <- data.frame(gas_oil_month[,c(1,2,3)], row.names = gas_oil_month$Date)
dim(gas_oil_month)


# Scatterplot
ggplot(data = gas_oil) + 
  geom_point(mapping = aes(x = gas_oil$Benzina, y = gas_oil$Petrol)) +
  xlab('Pret Benzina') +
  ylab('Pret Baril de Petrol') + 
  ggtitle('Norul de puncte dintre benzina si petrol')+
  theme_bw()

# Scatterplot LUNAR
ggplot(data = gas_oil_month) + 
  geom_point(mapping = aes(x = gas_oil_month$Benzina, y = gas_oil_month$Petrol)) +
  xlab('Pret Benzina') +
  ylab('Pret Baril de Petrol') + 
  ggtitle('Norul de puncte dintre benzina si petrol')+
  theme_bw()

# Declaram variabilele de tip ts
benzina_ts <- ts(gas_oil$Benzina, start = c(2015,8), frequency = 12)
petrol_ts <-ts(gas_oil$Petrol, start = c(2015,8), frequency = 12)

benzina_ts <- window(benzina_ts, start=c(2015,8), end=c(2024,5))
petrol_ts <- window(petrol_ts, start=c(2015,8), end=c(2024,5))


# Normalizarea datelor
benzina_ts <- (benzina_ts - mean(benzina_ts)) / sd(benzina_ts)
petrol_ts <- (petrol_ts - mean(petrol_ts)) / sd(petrol_ts)

# Calculul procentajelor de schimbare
benzina_diff <- diff(benzina_ts)
petrol_diff <- diff(petrol_ts)




# Graficul seriilor
autoplot(cbind(benzina_ts,petrol_ts)) +
  ylab('') +
  ggtitle('Graficul seriilor') +
  theme_bw()

autoplot(cbind(benzina_diff,petrol_diff)) +
  ylab('') +
  ggtitle('Graficul seriilor diferentiate') +
  theme_bw()

# Determinarea persistentei modelului
ggtsdisplay(benzina_ts)
ggtsdisplay(petrol_ts)

ggtsdisplay(benzina_diff)
ggtsdisplay(petrol_diff)

# Testarea stationaritatii seriilor (am ales varianta cea mai complexa a ADF)
adf.benzina <- ur.df(benzina_ts, type = "trend", selectlags = "AIC")
summary(adf.benzina) # serie nestationara

adf.petrol <- ur.df(petrol_ts, type = "trend", selectlags = "AIC")
summary(adf.petrol) # serie nestationara 

ndiffs(benzina_ts)
ndiffs(petrol_ts)

# Testarea stationaritatii seriilor DIFERENTIATE (am ales varianta cea mai complexa a ADF)
adf.benzina <- ur.df(benzina_diff, type = "trend", selectlags = "AIC")
summary(adf.benzina) # serie nestationara

adf.petrol <- ur.df(petrol_diff, type = "trend", selectlags = "AIC")
summary(adf.petrol) # serie nestationara 

ndiffs(benzina_diff)
ndiffs(petrol_diff)

