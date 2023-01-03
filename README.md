# ESG-Financial-Performance

## Overview and Note
This code relates to my Bachelor´s dissertation project, examining the relationship between ESG ratings and financial performance. Written in December 2020, this code is my first-ever data analytics project.

## Table of contents
* [General information](#general-information)
* [Methodology](#methodology)
* [Statistical methods](#statistical-methods)
* [Setup](#setup)

## General information
This project examines the relationship between the ESG compliance of corporations and its financial performance, both from an accounting perspective (represented by Return-on-Assets) and a capital market perspective (represented by the risk-adjusted return as described in Markowitz portfolio theory). The used sample contains over 110 corporations from different areas of the world. How to access the used data is described in chatper "Setup". I chose a relatively short observation period of only one calendar year (2019) due to the insufficient availability of ESG data (which is described in detail within the dissertation).

## Methodology
Financial data was extracted from financial reports and stock pricing history, using trustable online resources. For ESG compliance, I used the publicly available MSCI ESG ratings. I calculated risk-adjusted return (RRV) as the following: ![image](https://user-images.githubusercontent.com/98849197/152642169-a99f4e3e-8317-4715-b1cd-dca49bb8bf08.png)
, for r = return; and i=1;2;...n indicating the respective time of observation

and return-on-assets (ROA) as the following:
![image](https://user-images.githubusercontent.com/98849197/152642122-ab330bd7-2b56-4578-a446-b0346996d472.png)
, for I = Net Income; and A = total assets in period t

MSCI ESG-ratings are typically expressed by letter ratings, calculated in a moderately complex way from the raw data which is considering several aspects of environmental, social, and ethical compliance.  
![image](https://user-images.githubusercontent.com/98849197/152642537-2ac87959-992e-4339-86fa-041edbf7c78f.png)

For the purpose of my analysis, I transformed the letter ratings into a metric system (1-7).
![image](https://user-images.githubusercontent.com/98849197/152642567-4c03eea5-d7f8-4c69-a71e-406f3f7e0883.png)

Additionally, following controll variables were used: 
- Debt to equity ratio (LEVERAGE)
- Total assets (SIZE)
- Age of the company (AGE)
- Net Income (EARNINGS)
## Statistical methods
In a first step, I used typical methods from descriptive statistics (position measures and several plots/graphs) to gain an overview over the data and the distribution of the respective variables. 
Afterwards, I used two correlation matrices (one for the accounting-based model and one for the market-based model) to examine correlations.

For regression analysis, I started with two OLS models,

![image](https://user-images.githubusercontent.com/98849197/152643055-21b31c6f-110a-4c1e-847c-384515988ba5.png)
![image](https://user-images.githubusercontent.com/98849197/152643062-c3c27df5-a485-4844-916f-ab5bc1b0b29d.png) ,

which I tested on assumptions of the Gauss-Markov theorem, using following tests:
- RESET test
- DfBETAS value & Cook's distance
- Variance inflation factor
- Goldfeld-Quandt test & Breusch-Pagan test
- Shapiro-Wilk test

As one of the OLS models (ROA) does not comply with the assumption of normal distribution of residuals, I implemented a robust regression model, using Huber M-estimators. 

## Setup
1. To access the data/used sample, please use the following link (https://www.dropbox.com/scl/fi/05gwl8xg5d0i4zh7fw3il/Overview-FINAL.xlsx?dl=0&rlkey=iesmdq2xrab7z0q21n2h4fm8a) and use the the Excel file called "FINAL (ABGABE)". 
2. The R file contains the code for the analysis. Use the read-in in the R code (line 26) to implement the data from the excel file into the R workspace.
3. You then should be able to run the program. Please be aware that you have to manually edit the save space where you want to save the extracts from the STARGAZER package.

