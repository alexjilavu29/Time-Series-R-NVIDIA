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