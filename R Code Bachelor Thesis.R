library(rmarkdown)
library(mosaic)
library(readxl)
library(rgl)
library(car)
library(dplyr)
library(corrr)
library(lmtest)
library(stargazer)
library(psych)
library(sjmisc)
library(expss)
library(gt)
library(MVN)
library(Hmisc)
library(rstatix)
library(normtest)
library(MASS)
library(sfsmisc)
library(robust)
library(sandwich)
library(plotrix)
library(robust)




###Tabellen####



#Einlesen
Excel_IMP <- read_excel("C:/Users/nicoh/OneDrive/Desktop/Bachelor Thesis/FINAL(ABGABE)/Overview FINAL.xlsx")
View(Excel_IMP)
#rm(Excel_IMP)




####Variablendeklaration####
ESG <- Excel_IMP$ESG
ROA <- Excel_IMP$ROA
RRV <- Excel_IMP$RRV
LVRG <- log10(Excel_IMP$DebtToEquity) 
SIZE <- log10(Excel_IMP$TA) 
AGE <- Excel_IMP$AGE
  constantERNGS <- abs(min(Excel_IMP$`Net Income`))+1000 #Konstante für die Transformation von EARNINGS 
EARNINGS <- log10(Excel_IMP$`Net Income`+constantERNGS) 



#Untransformierte Variablen (für Deskriptive Tabelle)
DebtToEquity <- Excel_IMP$DebtToEquity
TotalAssets <- round(Excel_IMP$TA)
NetIncome <- round(Excel_IMP$`Net Income`)

#Grundmodelle
Reg1 <- lm(ROA~ESG+LVRG+SIZE+AGE) #Modell 1 (1.1)
Reg2<- lm(RRV~ESG+LVRG+SIZE+EARNINGS) #Modell 2

Mod.1_Residuals <- residuals(Reg1)
Mod.2_Residuals <- residuals(Reg2)


#####-Tabellen-#####
#Tabelle gesamt (Deskriptive Statistik)
printingtable <- data.frame(ROA, RRV, ESG, DebtToEquity, LVRG, TotalAssets, SIZE, AGE, NetIncome, EARNINGS)

#Korrelationstabellen
TABLEMOD1 <- data.frame(ROA, ESG, LVRG, SIZE, AGE)
TABLEMOD2 <- data.frame(RRV, ESG, LVRG, SIZE, EARNINGS)

tableExoMod1 <- data.frame(Mod.1_Residuals, ESG, SIZE, LVRG, AGE)
tableExoMod2 <- data.frame(Mod.2_Residuals, ESG, SIZE, LVRG, EARNINGS)


###--Deskriptive Statistik(TABLE)-####


#Erstellen der Funktion für Tabelle
FUN.summarystatistics <- function(DF.summary_dataframe){
  DF.SMRY <- data.frame(
    n    = sapply(DF.summary_dataframe, length),
    mean = sapply(DF.summary_dataframe, mean),
    sd = sapply(DF.summary_dataframe, sd),
    max = sapply(DF.summary_dataframe, max),
    min = sapply(DF.summary_dataframe, min),
    Q1 = sapply(DF.summary_dataframe, quantile, probs = 0.25),
    median = sapply(DF.summary_dataframe, median),
    Q3 = sapply(DF.summary_dataframe, quantile, probs = 0.75)
    
    
    
  )
  
  return(DF.SMRY)
  
  
  
  
}

#Tabelle "Deskriptive Statistik" 
stargazer(FUN.summarystatistics(printingtable), type="text", title = "Deskriptive Statistik", out="Descriptive.html" , summary = F, digits = 2)


hist(ESG, ylim = c(0,35), main = "Verteilung ESG")

####-----PLOTS-----####



#-----Modell 1 -----#
plot(ROA, ESG, main = "ROA-ESG")
lines(lowess(ROA, ESG, f=1), col = "red")

plot(ROA, LVRG, main = "ROA-LVRG")
lines(lowess(ROA, LVRG, f=1), col = "red")

plot(ROA, SIZE, main = "ROA-SIZE")
lines(lowess(ROA, SIZE, f=1), col = "red")

plot(ROA, AGE, main = "ROA-AGE")
lines(lowess(ROA, AGE, f=1), col = "red")

#-----Modell 2 -----#
plot(RRV, ESG, main = "RRV-ESG")
lines(lowess(RRV, ESG, f=1), col = "red")

plot(RRV, LVRG, main = "RRV-LVRG")
lines(lowess(RRV, LVRG, f=1), col = "red")

plot(RRV, SIZE, main = "RRV-SIZE")
lines(lowess(RRV, SIZE, f=1), col = "red")

plot(RRV, EARNINGS, main = "RRV-EARNINGS")
lines(lowess(RRV, EARNINGS, f=1), col = "red")


#Boxplots#
boxplot(ROA, main=" ROA")
boxplot(RRV, main= "RRV")
boxplot(ESG, main= "ESG")
boxplot(LVRG, main="LEVERAGE")
boxplot(SIZE, main="SIZE")
boxplot(AGE, main="AGE")
boxplot(EARNINGS, main="EARNINGS")

###-----Korrelationsmatrix-----####

KorrMatROA<- (cor_mat(TABLEMOD1, method = "pearson") %>% cor_mark_significant(cutpoints =   c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**" , "*",""))) 
stargazer(KorrMatROA, type="text", title = "Korrelationsmatrix Modell 1 (ROA)", summary = F, out= "COR_Model1.html", digits = 2) #Ausgabe für Thesis         

KorrMatRRV<- (cor_mat(TABLEMOD2, method = "pearson") %>% cor_mark_significant(cutpoints =   c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**" , "*",""))) 
stargazer(KorrMatRRV, type="text", title = "Korrelationsmatrix Modell 2 (RRV)", summary = F, out= "COR_Model2.html", digits = 2) #Ausgabe für Thesis         


###


####Modell 1####
#Grundmodell1 (ROA)####

summary(Reg1)


##-- Test auf Robustheit des Schätzers --##

#Annahme 1: Exogenität der Regressoren#
KorrMatEXOMOD1<- (cor_mat(tableExoMod1, method = "pearson") %>% cor_mark_significant(cutpoints =   c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**" , "*",""))) 
stargazer(KorrMatEXOMOD1, type="text", title = "--Exogenität Modell 1-- ----------------a", summary = F, out= "EXO1.html") #Ausgabe für Thesis         



#Annahme 2: Spezifikation des Modells (RESET test)

reset(Reg1, power = 2:3)

#Annahme 3: Ausreißer und Extremwerte

#DfBETAS Werte# 
#-> Wertegrenze nach Cohen: |2| 
dfbetasPlots(Reg1, main = "DfBETAS-Werte für Modell 1 (ROA)")

#Cooks D
#-> ALLES UNTER 1 IST OK (Cook, R., & Weisberg, S. (1982). Criticism and Influence Analysis in Regression. Sociological Methodology, 13, 313-361.) 

cooksd1<- cooks.distance(Reg1)
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance, Modell 1")  # plot cook's distance
abline(h = 1, col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>1, names(cooksd1),""), col="red")  # add labels
influentialROA <- c(as.numeric(names(cooksd1)[(cooksd1 > 1)]))
show(influentialROA)



#Annahme 4: Multikollinearität
VIF1 <- vif(Reg1)
stargazer(VIF1, summary = F, out="VIF1.html", type="text", title = "VIF Modell 1", digits = 2)

#Alles unter 10 ist okay

#Annahme 5: Homoskedastizität

gqtest(Reg1, alternative ="two.sided") #Goldfeld-Quandt Test: H0 = Homoskedastizität / H1 = Heteroskedastizität
bptest(Reg1)

# Annahme 6: Autokorrelation
#-> Keine zeitreihe, daher irrelevant


#Annahme 7. NV der Residuen

plot(Reg1,2, main = "Modell 1")
shapiro.test(rstandard(Reg1))


#!!!-- NV abgelehnt! Daher zwei Optionen: 1. robuste Regression, 2. Outliner entfernen--#
#!!! Zum Entfernen einzelner Beobachtungen wurde keine Begründung gefunden, daher ROBUSTE REGRESSION

#Versuch, ROA zu transformieren# 
TransformedROA <- log10(ROA+1)
                        
plot(lm(TransformedROA~ESG+LVRG+SIZE+AGE),2)
shapiro.test(rstandard(lm(TransformedROA~ESG+LVRG+SIZE+AGE)))

#-> NV nicht gegeben, daher robuste Regression!
rm(TransformedROA)


####Modell 1.2####
#---robust Regression---#
Reg1rob <- rlm(ROA~ESG+LVRG+SIZE+AGE, method = "M")

stargazer(Reg1, Reg1rob, title = "Modell 1 (ROA) ", style = "default",out = "RegressionROA.html") #Output Regressionsergebnisse ROA (robust und OLS)



# Robust F-Test: Wald test for multiple coefficients of rlm() Object.

f.robftest(Reg1rob, var = -1)
f.robftest(Reg1rob, var = "ESG") #-> ESG shows p value of ~ 0,1

summary(Reg1rob)
summary(Reg1)

#Im Huber Modell (Modell 1.2) untergewichtete Beobachtungen:
hweights <- data.frame(name = Excel_IMP$Unternehmen,residuals = Reg1rob$resid, weight = Reg1rob$w)
hweights2 <- hweights[order(Reg1rob$w), ]
hweights2[1:20, ]

#Kalkulation Determinationskoeffizient für Modell 1.2
weightsforHuber <- c(Reg1rob$w)
summary(lm(ROA~ESG+LVRG+SIZE+AGE, weights = weightsforHuber))






#####----------Grundmodell2 (RRV)----------#####
summary(Reg2)


##-- Test auf Robustheit des Schätzers --##

#Annahme 1: Exogenität der Regressoren#
KorrMatEXOMOD2<- (cor_mat(tableExoMod2, method = "pearson") %>% cor_mark_significant(cutpoints =   c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**" , "*",""))) 
stargazer(KorrMatEXOMOD2, type="text", title = "--Exogenität Modell 2-- --------------------------a", summary = F, out= "EXO2.html") #Ausgabe für Thesis 

#Annahme 2: Spezifikation des Modells (RESET test)

reset(Reg2, power = 2:3)


#Annahme 3: Ausreißer und Extremwerte

#DfBETAS Werte# 
#-> Wertegrenze nach Cohen: |2| 
dfbetasPlots(Reg2, main = "DfBETAS-Werte für Modell 2 (RRV)")

#Cooks D
#-> ALLES UNTER 1 IST OK (Cook, R., & Weisberg, S. (1982). Criticism and Influence Analysis in Regression. Sociological Methodology, 13, 313-361.) 

cooksd2<- cooks.distance(Reg2)
plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance, Modell 2")  # plot cook's distance
abline(h = 1, col="red")  # add cutoff line
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>1, names(cooksd2),""), col="red")  # add labels
influentialRRV <- c(as.numeric(names(cooksd2)[(cooksd2 > 1)]))
show(influentialRRV)


#Annahme 4: Multikollinearität
VIF2 <- vif(Reg2)
stargazer(VIF2, summary = F, out="VIF2.html", type="text", title = "VIF Modell 2", digits = 2)
#Alles unter 10 ist okay

#Annahme 5: Homoskedastizität


gqtest(Reg2, alternative ="two.sided") #Goldfeld-Quandt Test: H0 = Homoskedastizität / H1 = Heteroskedastizität
bptest(Reg2)

# Annahme 6: Autokorrelation
#-> Keine zeitreihe, daher irrelevant


#Annahme 7: NV der Residuen

plot(Reg2,2 , main = "Modell 2")
shapiro.test(rstandard(Reg2))


stargazer(Reg2, title = "Modell 2 (RRV)", style = "default",out = "RegressionRRV.html")






#Interpretation Regressionsergebnisse####

stargazer(Reg1, Reg1rob, Reg2, title = "Übersicht Regressionsergebnisse", style = "default",out = "ÜbersichtErgebnisse.html")
#Interpretation nach Cohen (1988) schwache = 0.02 , mittlere = 0.13, hohe = 0.26








