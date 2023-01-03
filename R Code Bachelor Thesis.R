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
library(MASS)
library(sfsmisc)
library(robust)
library(sandwich)
library(plotrix)
library(robust)


Excel_IMP <- read_excel("data_overview.xlsx") #relating to Excel file "FINAL (ABGABE)
View(Excel_IMP)
ESG <- Excel_IMP$ESG
ROA <- Excel_IMP$ROA
RRV <- Excel_IMP$RRV
LVRG <- log10(Excel_IMP$DebtToEquity) 
SIZE <- log10(Excel_IMP$TA) 
AGE <- Excel_IMP$AGE
  constantERNGS <- abs(min(Excel_IMP$`Net Income`))+1000 #transformation of 'EARNINGS'
EARNINGS <- log10(Excel_IMP$`Net Income`+constantERNGS) 

DebtToEquity <- Excel_IMP$DebtToEquity #transformation for the descriptive table
TotalAssets <- round(Excel_IMP$TA)
NetIncome <- round(Excel_IMP$`Net Income`)
Reg1 <- lm(ROA~ESG+LVRG+SIZE+AGE) #basic OLS model 1
Reg2<- lm(RRV~ESG+LVRG+SIZE+EARNINGS) #basic OLS mosel 2
Mod.1_Residuals <- residuals(Reg1)
Mod.2_Residuals <- residuals(Reg2)

printingtable <- data.frame(ROA, RRV, ESG, DebtToEquity, LVRG, TotalAssets, SIZE, AGE, NetIncome, EARNINGS) #descriptive table
TABLEMOD1 <- data.frame(ROA, ESG, LVRG, SIZE, AGE)
TABLEMOD2 <- data.frame(RRV, ESG, LVRG, SIZE, EARNINGS)
tableExoMod1 <- data.frame(Mod.1_Residuals, ESG, SIZE, LVRG, AGE)
tableExoMod2 <- data.frame(Mod.2_Residuals, ESG, SIZE, LVRG, EARNINGS)


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

stargazer(FUN.summarystatistics(printingtable), type="text", title = "Deskriptive Statistik", out="Descriptive.html" , summary = F, digits = 2) #descriptive table
hist(ESG, ylim = c(0,35), main = "Verteilung ESG")
#-----plots model 1 -----#
plot(ROA, ESG, main = "ROA-ESG")
lines(lowess(ROA, ESG, f=1), col = "red")
plot(ROA, LVRG, main = "ROA-LVRG")
lines(lowess(ROA, LVRG, f=1), col = "red")
plot(ROA, SIZE, main = "ROA-SIZE")
lines(lowess(ROA, SIZE, f=1), col = "red")
plot(ROA, AGE, main = "ROA-AGE")
lines(lowess(ROA, AGE, f=1), col = "red")

#-----plots model 2 -----#
plot(RRV, ESG, main = "RRV-ESG")
lines(lowess(RRV, ESG, f=1), col = "red")
plot(RRV, LVRG, main = "RRV-LVRG")
lines(lowess(RRV, LVRG, f=1), col = "red")
plot(RRV, SIZE, main = "RRV-SIZE")
lines(lowess(RRV, SIZE, f=1), col = "red")
plot(RRV, EARNINGS, main = "RRV-EARNINGS")
lines(lowess(RRV, EARNINGS, f=1), col = "red")
boxplot(ROA, main=" ROA")
boxplot(RRV, main= "RRV")
boxplot(ESG, main= "ESG")
boxplot(LVRG, main="LEVERAGE")
boxplot(SIZE, main="SIZE")
boxplot(AGE, main="AGE")
boxplot(EARNINGS, main="EARNINGS")

###-----correlation matrix-----####
KorrMatROA<- (cor_mat(TABLEMOD1, method = "pearson") %>% cor_mark_significant(cutpoints =   c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**" , "*",""))) 
stargazer(KorrMatROA, type="text", title = "Korrelationsmatrix Modell 1 (ROA)", summary = F, out= "COR_Model1.html", digits = 2) #Ausgabe f?r Thesis         
KorrMatRRV<- (cor_mat(TABLEMOD2, method = "pearson") %>% cor_mark_significant(cutpoints =   c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**" , "*",""))) 
stargazer(KorrMatRRV, type="text", title = "Korrelationsmatrix Modell 2 (RRV)", summary = F, out= "COR_Model2.html", digits = 2) #Ausgabe f?r Thesis         


#model 1 - testing of assumptions####
summary(Reg1)
KorrMatEXOMOD1<- (cor_mat(tableExoMod1, method = "pearson") %>% cor_mark_significant(cutpoints =   c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**" , "*",""))) 
stargazer(KorrMatEXOMOD1, type="text", title = "--Exogenit?t Modell 1-- ----------------a", summary = F, out= "EXO1.html") #Ausgabe f?r Thesis         
reset(Reg1, power = 2:3)

dfbetasPlots(Reg1, main = "DfBETAS-Werte f?r Modell 1 (ROA)") #using a threshold of |2| for DfBETAS, according to Cohen
 
cooksd1<- cooks.distance(Reg1) #using a threshold of 1 for Cooks distance, accoring to (Cook, R., & Weisberg, S. (1982). Criticism and Influence Analysis in Regression. Sociological Methodology, 13, 313-361.) 
plot(cooksd1, pch="*", cex=2, main="Influential Obs by Cooks distance, Modell 1")  # plot cook's distance
abline(h = 1, col="red")  # add cutoff line
text(x=1:length(cooksd1)+1, y=cooksd1, labels=ifelse(cooksd1>1, names(cooksd1),""), col="red")  # add labels
influentialROA <- c(as.numeric(names(cooksd1)[(cooksd1 > 1)]))
show(influentialROA)

VIF1 <- vif(Reg1) #using a threshold of 10
stargazer(VIF1, summary = F, out="VIF1.html", type="text", title = "VIF Modell 1", digits = 2)

gqtest(Reg1, alternative ="two.sided") 
bptest(Reg1)

plot(Reg1,2, main = "Modell 1")
shapiro.test(rstandard(Reg1))

TransformedROA <- log10(ROA+1) # model 1 does not comply with normal distribution, so I tried a transformation of ROA with log.
plot(lm(TransformedROA~ESG+LVRG+SIZE+AGE),2)
shapiro.test(rstandard(lm(TransformedROA~ESG+LVRG+SIZE+AGE)))
rm(TransformedROA) #still not comply with normal distribution, so removal of transformed variable

#---robust model---#
Reg1rob <- rlm(ROA~ESG+LVRG+SIZE+AGE, method = "M")

stargazer(Reg1, Reg1rob, title = "Modell 1 (ROA) ", style = "default",out = "RegressionROA.html") #Output Regressionsergebnisse ROA (robust und OLS)
f.robftest(Reg1rob, var = -1)
f.robftest(Reg1rob, var = "ESG") #-> ESG shows p value of ~ 0,1
summary(Reg1rob)
summary(Reg1)

#weighting for robust Huber-M estimator (robust model):
hweights <- data.frame(name = Excel_IMP$Unternehmen,residuals = Reg1rob$resid, weight = Reg1rob$w)
hweights2 <- hweights[order(Reg1rob$w), ]
hweights2[1:20, ]
weightsforHuber <- c(Reg1rob$w)
summary(lm(ROA~ESG+LVRG+SIZE+AGE, weights = weightsforHuber))






#model 2 - testing of assumptions####
summary(Reg2)

KorrMatEXOMOD2<- (cor_mat(tableExoMod2, method = "pearson") %>% cor_mark_significant(cutpoints =   c(0, 0.01, 0.05, 0.1, 1), symbols = c("***", "**" , "*",""))) 
stargazer(KorrMatEXOMOD2, type="text", title = "--Exogenit?t Modell 2-- --------------------------a", summary = F, out= "EXO2.html") #Ausgabe f?r Thesis 

reset(Reg2, power = 2:3)

dfbetasPlots(Reg2, main = "DfBETAS-Werte f?r Modell 2 (RRV)")

cooksd2<- cooks.distance(Reg2)
plot(cooksd2, pch="*", cex=2, main="Influential Obs by Cooks distance, Modell 2")  # plot cook's distance
abline(h = 1, col="red")  # add cutoff line
text(x=1:length(cooksd2)+1, y=cooksd2, labels=ifelse(cooksd2>1, names(cooksd2),""), col="red")  # add labels
influentialRRV <- c(as.numeric(names(cooksd2)[(cooksd2 > 1)]))
show(influentialRRV)


VIF2 <- vif(Reg2)
stargazer(VIF2, summary = F, out="VIF2.html", type="text", title = "VIF Modell 2", digits = 2)

gqtest(Reg2, alternative ="two.sided") #Goldfeld-Quandt Test: H0 = Homoskedastizit?t / H1 = Heteroskedastizit?t
bptest(Reg2)

plot(Reg2,2 , main = "Modell 2")
shapiro.test(rstandard(Reg2))

stargazer(Reg2, title = "Modell 2 (RRV)", style = "default",out = "RegressionRRV.html")

#table for interpretation of results, using stargazer####
stargazer(Reg1, Reg1rob, Reg2, title = "?bersicht Regressionsergebnisse", style = "default",out = "?bersichtErgebnisse.html")



#'THE END' - thank you for reading :) 





