# Time Series Analysis Project

## Motivation

This project aims to perform a comprehensive time series analysis on two distinct subjects:
1. The evolution of NVIDIA Corporation's stock (NVDA) on the stock exchange.
2. The analysis of the Romanian educational system over the past decades.

## Project Structure

The project is structured into several key sections:

1. **Introduction**
2. **Specialized Literature Review**
3. **Time Series Analysis**
4. **Conclusion**

## Repository Contents

This repository contains the R code and documentation for the complete analysis of both NVIDIA's stock evolution and the Romanian educational system.

### Introduction

The introductory section outlines the objectives of the project:
- For NVIDIA: Analyze the monthly stock data of NVDA from January 2017 to April 2024.
- For Romanian education: Analyze data on graduates, educational institutions, and libraries from 1991 to 2021.

### Specialized Literature Review

The literature review includes discussions of relevant studies and methodologies used in the domains of time series analysis and prediction, including:
- Stock prediction using methods like ARIMA, LSTM, and HMM.
- Analysis of educational systems and their correlation with economic factors.

### Time Series Analysis

The time series analysis is the core of the project, involving detailed steps such as:

#### NVIDIA Stock Analysis
- **Data Preparation:** Reading and cleaning the monthly stock data.
- **Stationarity Testing:** Verifying stationarity using statistical tests.
- **ARIMA Modeling:** Identifying and fitting ARIMA models.
- **Seasonal Analysis:** Applying Holt-Winters and ETS models.

#### Romanian Educational System Analysis
- **Data Preparation:** Structuring and cleaning data on graduates, educational institutions, and libraries.
- **Stationarity Testing:** Verifying stationarity using statistical tests.
- **Cointegration Analysis:** Testing the long-term relationship between educational infrastructure and graduate numbers.
- **Forecasting:** Using various models to predict future trends.

### Conclusion

The conclusions summarize the findings for both analyses:
- For NVIDIA, the Holt-Winters Multiplicative method provided the most accurate forecast for the NVDA stock trend.
- For the Romanian educational system, the analysis showed significant dependencies between the number of educational institutions and graduates, and identified long-term trends and potential areas of concern.

## Usage

Clone the repository and run the R scripts to replicate the analyses. Ensure you have the necessary libraries installed, including `tidyverse`, `forecast`, and `tseries`.

```bash
git clone https://github.com/alexjilavu29/Time-Series-R-NVIDIA.git
```
## Contributors

	•	Alexandru Jilavu
	•	Corina Lișiță
	•	Raluca-Andra Medeleanu


### Romanian Translation Below
----



# Proiect de Analiză a Seriilor de Timp

## Motivație

Acest proiect își propune să realizeze o analiză cuprinzătoare a seriilor de timp pe două subiecte distincte:
1. Evoluția acțiunilor NVIDIA Corporation (NVDA) pe bursă.
2. Analiza sistemului educațional românesc de-a lungul decadelor.

## Structura Proiectului

Proiectul este structurat în mai multe secțiuni cheie:

1. **Introducere**
2. **Revista Literaturii de Specialitate**
3. **Analiza Seriilor de Timp**
4. **Concluzie**

## Conținutul Repozitoriului

Acest repozitoriu conține codul R și documentația pentru analiza completă a evoluției acțiunilor NVIDIA și a sistemului educațional românesc.

### Introducere

Secțiunea introductivă descrie obiectivele proiectului:
- Pentru NVIDIA: Analiza datelor lunare ale acțiunilor NVDA din ianuarie 2017 până în aprilie 2024.
- Pentru educația din România: Analiza datelor despre absolvenți, instituții de învățământ și biblioteci din 1991 până în 2021.

### Revista Literaturii de Specialitate

Revista literaturii include discuții despre studii și metodologii relevante utilizate în domeniile analizei și prognozei seriilor de timp, inclusiv:
- Prognoza acțiunilor folosind metode precum ARIMA, LSTM și HMM.
- Analiza sistemelor educaționale și corelația lor cu factorii economici.

### Analiza Seriilor de Timp

Analiza seriilor de timp este esența proiectului, implicând pași detaliați precum:

#### Analiza Acțiunilor NVIDIA
- **Pregătirea Datelor:** Citirea și curățarea datelor lunare ale acțiunilor.
- **Testarea Staționarității:** Verificarea staționarității folosind teste statistice.
- **Modelarea ARIMA:** Identificarea și ajustarea modelelor ARIMA.
- **Analiza Sezonală:** Aplicarea metodelor Holt-Winters și ETS.

#### Analiza Sistemului Educațional Românesc
- **Pregătirea Datelor:** Structurarea și curățarea datelor despre absolvenți, instituții de învățământ și biblioteci.
- **Testarea Staționarității:** Verificarea staționarității folosind teste statistice.
- **Analiza Cointegrării:** Testarea relației pe termen lung între infrastructura educațională și numărul de absolvenți.
- **Prognoza:** Utilizarea diverselor modele pentru a prezice tendințele viitoare.

### Concluzie

Concluziile rezumă rezultatele analizelor pentru ambele subiecte:
- Pentru NVIDIA, metoda Holt-Winters Multiplicativă a furnizat cea mai precisă prognoză pentru tendința acțiunilor NVDA.
- Pentru sistemul educațional românesc, analiza a arătat dependențe semnificative între numărul de instituții de învățământ și absolvenți, identificând tendințe pe termen lung și posibile arii de îngrijorare.

## Utilizare

Clonați repozitoriul și rulați scripturile R pentru a replica analizele. Asigurați-vă că aveți instalate bibliotecile necesare, inclusiv `tidyverse`, `forecast` și `tseries`.

```bash
git clone https://github.com/alexjilavu29/Time-Series-R-NVIDIA.git
```

## Contributori

	•	Alexandru Jilavu
	•	Corina Lișiță
	•	Raluca-Andra Medeleanu