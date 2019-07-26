#------------------------------#
## Author: Violetta Matzorou  ##
#------------------------------#

#------------------------------#
### Clear the console in R   ###
#------------------------------#
# ctrl + L

#------------------------------#
### Set the working directory ###
#------------------------------#

setwd("H:/Violetta/Coding in R")
# make sure to use correct path with "/"

#------------------------------#
###     Import libraries     ###
#------------------------------#
library(foreign)
library(plyr)
library(dplyr)
library(graphics)
library(lattice)
library(readxl)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(stringr)
library(reshape2)
library(countrycode)
library(plotly)
library(e1071)
library(tidyverse)
library(ggpubr)
library(car) # for LM functions

# Libraries needed for the imputation of data
library(VIM)
library(Hmisc)
library(missForest)


#-------------------------------------------------------#
###   Read the created data  from file Rscript_11june   ###
#-------------------------------------------------------#

data_kids = read.csv(file="data_ch_1617_11june.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)

# dimensions of the survey file
dim(data_kids)

# make the birth weight into kg
data_kids$gew_totaal <- data_kids$gew_totaal/1000

# drop X column (index), wrong birth weight and current weigh in gr
data_kids <- select(data_kids, -c(X, GEZVRGGeboorteGewicht, GEZAFLGewicht_gr))

# save the column names in a variable
col_names <- names(data_kids)

# save the descriptive statistics in a dataframe
sumDF <- data.frame(summary(data_kids))
# save the sumDF locally
write.csv(sumDF, file="sumDF.csv", row.names = TRUE)


#--------------------------------------#
###     Data Analysis - Plotting     ###
#--------------------------------------#

# make a plot for the prematurity vs BMI
# x: premature or not // y: BMI of the child
ggplot(data_kids, aes(x= as.factor(Premature_preg) , y= GEZAFLBMI)) + geom_boxplot(fill="gray", alpha=0.2, outlier.alpha = 0) +
  xlab("Premature pregnancy") + ylab("Child's BMI")

# make a plot of the education of mother versus the income percentile
qplot(data_kids$SOI2006NIVEAU1_MOEDER, geom="histogram", 
      main="Histogram of mother education level",bins=30, xlab='Mother Education tier')

qplot(data_kids$INHP100HGEST_MOEDER, geom="histogram", 
      main="Histogram of household income",bins=30, xlab='Income percentile')

# make a plot for the SES vs BMI
# x: SES is high or not  // y: BMI of the child
ggplot(data_kids, aes(x= as.factor(seshoog) , y= GEZAFLBMI)) + geom_boxplot(fill="gray", alpha=0.2, outlier.alpha = 0) +
  xlab("Postcode SES status (not high / high)") + ylab("Child's BMI")

# make a plot for the SES vs Consultation visits
# x: SES is high or not  // y: Consultation visits
ggplot(data_kids, aes(x= as.factor(seshoog) , y= GEZVRGConsultatiebureau)) + geom_boxplot(fill="gray", alpha=0.2, outlier.alpha = 0) +
  xlab("Postcode SES status (not high / high)") + ylab("Consultation visits")

# make a plot for the sex vs BMI
# x: SES is high or not  // y: BMI of the child
ggplot(data_kids, aes(x= as.factor(GEZHHBGeslachtOP) , y= GEZAFLBMI)) + geom_boxplot(fill="gray", alpha=0.2, outlier.alpha = 0) +
  xlab("Sex") + ylab("Child's BMI")



#---------------------------------------------#
###  Test Normality of the Target Variable  ###
#---------------------------------------------#
data_kids$GEZAFLGewicht = as.numeric(as.character(data_kids$GEZAFLGewicht))

# the current weight is the target called: GEZAFLGewicht
result_currentW <- shapiro.test(data_kids$GEZAFLGewicht)
result_currentW$p.value # p-value lower than 0.05 --> there is NO NORMALITY


#------------------------------------------------------------#
###  Data Analysis - Make Slope Variables / Interaction   ###
#-----------------------------------------------------------#

# feedback from Scott: make the variables of Age and Length as L*A and A^2 
# then feed them as factors in the models

data_kids$AgeLength <- data_kids$GEZHHBLeeftijdOP * data_kids$GEZAFLLengte #AL

data_kids$AgeSquared <- (data_kids$GEZHHBLeeftijdOP)^2 # A^2
  
data_kids$HeightRatio <- (data_kids$GEZAFLLengte /data_kids$GEZAFLGeboorteLengte) # L/BL

# save this again - final data with slope variables
write.csv(data_kids, file="DF_data_kids.csv", row.names = TRUE)


#---------------------------------------------#
###     Data Analysis - Correlation Map     ###
#---------------------------------------------#
# split data
data_kids_char <- data_kids %>% select(RINPERSOON_KIND, GEZREGProvincie, GEZREGGemeentecode, Postcode4cijferig, GEZHHBGeslachtOP)
data_kids_num <- data_kids %>% select(- RINPERSOON_KIND, - Postcode4cijferig, -GEZREGProvincie , -GEZREGGemeentecode, -GEZHHBGeslachtOP, -Filename)

# make a data completeness heatmap
for (i in 1:15){
  data_kids_num[,i] = as.numeric(data_kids_num[,i])
}

cormat <- round(cor(data_kids_num), 2) # make the correlation matrix
head(cormat)

melted_cormat <- melt(cormat)

#plot
ggplot(data=melted_cormat, aes(x=Var1, y = Var2, fill=value)) + geom_tile() +
  theme(axis.text.x = element_text(angle=90, hjust=1))



#---------------------------------------------#
###  Data Analysis - Linear  Models Group A ###
#---------------------------------------------#

##### Simple model A1##### 
# Y: current weight
# xi = birth weight + birth length
lm_model_A1 <- lm(formula = GEZAFLGewicht ~ gew_totaal + GEZAFLGeboorteLengte, data = data_kids)

sm_A1 <- summary(lm_model_A1)
cf_A1 <- coef(lm_model_A1)
ci_A1 <- confint(lm_model_A1)

rse_A1 <- summary(lm_model_A1)$sigma  # RSE, residuals standard error
viffac_A1 <- vif(lm_model_A1)            # Variance Inflation Factors


# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A1)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A1 <- AIC(lm_model_A1)
crit2_A1 <- BIC(lm_model_A1)
mse_A1 <- mean(sm_A1$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A2##### 
# Y: current weight
# xi = birth weight + length ratio 
lm_model_A2 <- lm(formula = GEZAFLGewicht~ gew_totaal + HeightRatio , data = data_kids)

sm_A2 <- summary(lm_model_A2)

cf_A2 <- coef(lm_model_A2)
ci_A2 <- confint(lm_model_A2)

rse_A2 <- summary(lm_model_A2)$sigma  # RSE, residuals standard error
viffac_A2 <- vif(lm_model_A2)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A2)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A2 <- AIC(lm_model_A2)
crit2_A2 <- BIC(lm_model_A2)
mse_A2 <- mean(sm_A2$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A3##### 
# Y: current weight
# xi = birth weight + AL
lm_model_A3 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength , data = data_kids)

sm_A3 <- summary(lm_model_A3)

cf_A3 <- coef(lm_model_A3)
ci_A3 <- confint(lm_model_A3)

rse_A3 <- summary(lm_model_A3)$sigma  # RSE, residuals standard error
viffac_A3 <- vif(lm_model_A3)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A3)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A3 <- AIC(lm_model_A3)
crit2_A3 <- BIC(lm_model_A3)
mse_A3 <- mean(sm_A3$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A4##### 
# Y: current weight
# xi = birth weight + length ratio + AL
lm_model_A4 <- lm(formula = GEZAFLGewicht~ gew_totaal + HeightRatio + AgeLength , data = data_kids)

sm_A4 <- summary(lm_model_A4)

cf_A4 <- coef(lm_model_A4)
ci_A4 <- confint(lm_model_A4)

rse_A4 <- summary(lm_model_A4)$sigma  # RSE, residuals standard error
viffac_A4 <- vif(lm_model_A4)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A4)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A4 <- AIC(lm_model_A4)
crit2_A4 <- BIC(lm_model_A4)
mse_A4 <- mean(sm_A4$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A5##### 
# Y: current weight
# xi = birth weight + AL + A^2
lm_model_A5 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared, data = data_kids)

sm_A5 <- summary(lm_model_A5)

cf_A5 <- coef(lm_model_A5)
ci_A5 <- confint(lm_model_A5)

rse_A5 <- summary(lm_model_A5)$sigma  # RSE, residuals standard error
viffac_A5 <- vif(lm_model_A5)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A5)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A5 <- AIC(lm_model_A5)
crit2_A5 <- BIC(lm_model_A5)
mse_A5 <- mean(sm_A5$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A6##### 
# Y: current weight
# xi = birth weight + AL + A^2 + sex

lm_model_A6 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared + Female, data = data_kids)

sm_A6 <- summary(lm_model_A6)

cf_A6 <- coef(lm_model_A6)
ci_A6 <- confint(lm_model_A6)

rse_A6 <- summary(lm_model_A6)$sigma  # RSE, residuals standard error
viffac_A6 <- vif(lm_model_A6)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_A6)
par(mfrow = c(1,1)) 

crit1_A6 <- AIC(lm_model_A6)
crit2_A6 <- BIC(lm_model_A6)
mse_A6 <- mean(sm_A6$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A7##### 
# Y: current weight
# xi = birth weight + birth length + + AL + A^2 + sex
lm_model_A7 <- lm(formula = GEZAFLGewicht~ gew_totaal + HeightRatio + AgeLength + AgeSquared + Female, data = data_kids)

sm_A7 <- summary(lm_model_A7)

cf_A7 <- coef(lm_model_A7)
ci_A7 <- confint(lm_model_A7)

rse_A7 <- summary(lm_model_A7)$sigma  # RSE, residuals standard error
viffac_A7 <- vif(lm_model_A7)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_A7)
par(mfrow = c(1,1))

crit1_A7 <- AIC(lm_model_A7)
crit2_A7 <- BIC(lm_model_A7)
mse_A7 <- mean(sm_A7$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group B ###
#---------------------------------------------#

##### Simple model B1 ##### 
# xi = same as A6 + preg duration
lm_model_B1 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                  + Female + GEZVRGDuurZwang, data = data_kids)

sm_B1 <- summary(lm_model_B1)

cf_B1 <- coef(lm_model_B1)
ci_B1 <- confint(lm_model_B1)

rse_B1 <- summary(lm_model_B1)$sigma  # RSE, residuals standard error
viffac_B1 <- vif(lm_model_B1)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_B1)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_B1 <- AIC(lm_model_B1)
crit2_B1 <- BIC(lm_model_B1)
mse_B1 <- mean(sm_B1$residuals^2)

#-------------------------------------------------------------------#

##### Simple model B2 ##### 
# xi = same as B1 + prematurity
lm_model_B2 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                  + Female  +GEZVRGDuurZwang + Premature_preg, data = data_kids)

sm_B2<-summary(lm_model_B2)

cf_B2 <- coef(lm_model_B2)
ci_B2 <- confint(lm_model_B2)

rse_B2 <- summary(lm_model_B2)$sigma  # RSE, residuals standard error
viffac_B2 <- vif(lm_model_B2)            # Variance Inflation Factors


# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_B2)
par(mfrow = c(1,1)) 

crit1_B2 <- AIC(lm_model_B2)
crit2_B2 <- BIC(lm_model_B2)
mse_B2 <- mean(sm_B2$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group C ###
#---------------------------------------------#

##### Simple model C1 ##### 
# xi = same as B2 + western mother
lm_model_C1 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                  + Female  +GEZVRGDuurZwang + as.factor(Premature_preg) + 
                    as.factor(westers_MOEDER), data = data_kids)

sm_C1 <- summary(lm_model_C1)


cf_C1 <- coef(lm_model_C1)
ci_C1 <- confint(lm_model_C1)

rse_C1 <- summary(lm_model_C1)$sigma  # RSE, residuals standard error
viffac_C1 <- vif(lm_model_C1)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_C1)
par(mfrow = c(1,1)) 

crit1_C1 <- AIC(lm_model_C1)
crit2_C1 <- BIC(lm_model_C1)
mse_C1 <- mean(sm_C1$residuals^2)

#-------------------------------------------------------------------#
##### Simple model C2 ##### 
# xi = same as C1 + mother educ + income centile
lm_model_C2 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                  + Female  +GEZVRGDuurZwang + as.factor(Premature_preg) + 
                    as.factor(westers_MOEDER) + as.factor(SOI2006NIVEAU1_MOEDER)
                  + INHP100HGEST_MOEDER, data = data_kids)

sm_C2<-summary(lm_model_C2)

cf_C2 <- coef(lm_model_C2)
ci_C2 <- confint(lm_model_C2)

rse_C2 <- summary(lm_model_C2)$sigma  # RSE, residuals standard error
viffac_C2 <- vif(lm_model_C2)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_C2)
par(mfrow = c(1,1)) 

crit1_C2 <- AIC(lm_model_C2)
crit2_C2 <- BIC(lm_model_C2)
mse_C2 <- mean(sm_C2$residuals^2)

#-------------------------------------------------------------------#

##### Simple model C3 ##### 
# xi = same as C2 + family structure
lm_model_C3 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                   + Female  +GEZVRGDuurZwang + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                   + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure), data = data_kids)

sm_C3 <- summary(lm_model_C3)

cf_C3 <- coef(lm_model_C3)
ci_C3 <- confint(lm_model_C3)

rse_C3 <- summary(lm_model_C3)$sigma  # RSE, residuals standard error
viffac_C3 <- vif(lm_model_C3)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_C3)
par(mfrow = c(1,1)) 

crit1_C3 <- AIC(lm_model_C3)
crit2_C3 <- BIC(lm_model_C3)
mse_C3 <- mean(sm_C3$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group D ###
#---------------------------------------------#

##### Simple model D1 ##### 
# xi = same as C3 + consultation visits
lm_model_D1 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                  + Female  + GEZVRGDuurZwang + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                    + GEZVRGConsultatiebureau, data = data_kids)

sm_D1 <- summary(lm_model_D1)

cf_D1 <- coef(lm_model_D1)
ci_D1 <- confint(lm_model_D1)

rse_D1 <- summary(lm_model_D1)$sigma  # RSE, residuals standard error
viffac_D1 <- vif(lm_model_D1)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_D1)
par(mfrow = c(1,1))

crit1_D1 <- AIC(lm_model_D1)
crit2_D1 <- BIC(lm_model_D1)
mse_D1 <- mean(sm_D1$residuals^2)

#-------------------------------------------------------------------#

##### Simple model D2 ##### 
# xi = same as D1 + smoking
lm_model_D2 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                  + Female  + GEZVRGDuurZwang + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                  + GEZVRGConsultatiebureau + as.factor(GEZVRGRokenEersteVerzorger), data = data_kids)

sm_D2<-summary(lm_model_D2)

cf_D2 <- coef(lm_model_D2)
ci_D2 <- confint(lm_model_D2)

rse_D2 <- summary(lm_model_D2)$sigma  # RSE, residuals standard error
viffac_D2 <- vif(lm_model_D2)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_D2)
par(mfrow = c(1,1))

crit1_D2 <- AIC(lm_model_D2)
crit2_D2 <- BIC(lm_model_D2)
mse_D2 <- mean(sm_D2$residuals^2)


#---------------------------------------------#
###  Data Analysis - Linear  Models Group E ###
#---------------------------------------------#
##### Simple model E1 ##### 
# xi = same as D1 + SES postcode
lm_model_E1 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                  + Female  + GEZVRGDuurZwang + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                  + GEZVRGConsultatiebureau + as.factor(seshoog), data = data_kids)

sm_E1 <- summary(lm_model_E1)

cf_E1 <- coef(lm_model_E1)
ci_E1 <- confint(lm_model_E1)

rse_E1 <- summary(lm_model_E1)$sigma  # RSE, residuals standard error
viffac_E1 <- vif(lm_model_E1)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_E1)
par(mfrow = c(1,1))

crit1_E1 <- AIC(lm_model_E1)
crit2_E1 <- BIC(lm_model_E1)
mse_E1 <- mean(sm_E1$residuals^2)

#-------------------------------------------------------------------#
##### Simple model E2 ##### 
# xi = same as E1 + urbanity
lm_model_E2 <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                  + Female  + GEZVRGDuurZwang + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                  + GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex), data = data_kids)

sm_E2<-summary(lm_model_E2)

cf_E2 <- coef(lm_model_E2)
ci_E2 <- confint(lm_model_E2)

rse_E2 <- summary(lm_model_E2)$sigma  # RSE, residuals standard error
viffac_E2 <- vif(lm_model_E2)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_E2)
par(mfrow = c(1,1))

crit1_E2 <- AIC(lm_model_E2)
crit2_E2 <- BIC(lm_model_E2)
mse_E2 <- mean(sm_E2$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group F ###
#---------------------------------------------#

##### Simple model F ##### 
# xi = same as E2 + veggies + fruits
lm_model_F <- lm(formula = GEZAFLGewicht~ gew_totaal + AgeLength + AgeSquared 
                 + Female  + GEZVRGDuurZwang + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                 + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                 + GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex) + 
                   as.factor(GEZPUBGroente5Dagen) + as.factor(GEZPUBFruit5Dagen) , data = data_kids)

sm_F<-summary(lm_model_F)

cf_F <- coef(lm_model_F)
ci_F <- confint(lm_model_F)

rse_F <- summary(lm_model_F)$sigma  # RSE, residuals standard error
viffac_F <- vif(lm_model_F)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_F)
par(mfrow = c(1,1))

crit1_F <- AIC(lm_model_F)
crit2_F <- BIC(lm_model_F)
mse_F <- mean(sm_F$residuals^2)


#---------------------------------------------#
###  Data Analysis - Linear  Models Group G ###
#---------------------------------------------#
##### Simple model G1 ##### 
# xi = same as E2 + Biking
lm_model_G1 <- lm(formula = GEZAFLGewicht~ AgeLength + Female + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                  + GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex) + 
                   GEZVRGMinutenFietsenKind + GEZVRGSchoolMinutenFietsenKind, data = data_kids)
#  AgeSquared removed for singularities

sm_G1 <- summary(lm_model_G1)

cf_G1 <- coef(lm_model_G1)
ci_G1 <- confint(lm_model_G1)

rse_G1 <- summary(lm_model_G1)$sigma  # RSE, residuals standard error
viffac_G1 <- vif(lm_model_G1)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_G1)
par(mfrow = c(1,1))

crit1_G1 <- AIC(lm_model_G1)
crit2_G1 <- BIC(lm_model_G1)
mse_G1 <- mean(sm_G1$residuals^2)

#-------------------------------------------------------------------#

##### Simple model G2 ##### 
# xi = same as G1 + sports subscriptions
lm_model_G2 <- lm(formula = GEZAFLGewicht~ AgeLength +  
                  + Female + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                  + GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex) + 
                    GEZVRGMinutenFietsenKind + GEZVRGSchoolMinutenFietsenKind + as.factor(GEZPUBAbonnementSport) +
                    as.factor(GEZPUBLidmaatschapSport), data = data_kids)
# AgeSquared
sm_G2<-summary(lm_model_G2)

cf_G2 <- coef(lm_model_G2)
ci_G2 <- confint(lm_model_G2)

rse_G2 <- summary(lm_model_G2)$sigma  # RSE, residuals standard error
viffac_G2 <- vif(lm_model_G2)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_G2)
par(mfrow = c(1,1))

crit1_G2 <- AIC(lm_model_G2)
crit2_G2 <- BIC(lm_model_G2)
mse_G2 <- mean(sm_G2$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group H ###
#---------------------------------------------#
# important variables for LUMC: health status, loneliness & satisfaction

##### Simple model H1 ##### 
# xi = same as A6 + LUMC var
lm_model_H1 <- lm(formula = GEZAFLGewicht~  AgeLength + AgeSquared +
                    Female + as.factor(HealthStatus) + as.factor(Loneliness) + 
                    as.factor(Satisfaction), data = data_kids)

sm_H1<-summary(lm_model_H1)
cf_H1 <- coef(lm_model_H1)
ci_H1 <- confint(lm_model_H1)

rse_H1 <- summary(lm_model_H1)$sigma  # RSE, residuals standard error
viffac_H1 <- vif(lm_model_H1)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_H1)
par(mfrow = c(1,1)) 

crit1_H1 <- AIC(lm_model_H1)
crit2_H1 <- BIC(lm_model_H1)
mse_H1 <- mean(sm_H1$residuals^2)

#-------------------------------------------------------------------#

##### Simple model H2 ##### 
# xi = same as E2 + LUMC var
lm_model_H2 <- lm(formula = GEZAFLGewicht~  AgeLength  
                  + Female  + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                  + GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex) +
                    as.factor(HealthStatus) + as.factor(Loneliness) + as.factor(Satisfaction), data = data_kids)

# AgeSquared is removed since it creates multicolinearities

sm_H2 <- summary(lm_model_H2)
cf_H2 <- coef(lm_model_H2)
ci_H2 <- confint(lm_model_H2)

rse_H2 <- summary(lm_model_H2)$sigma  # RSE, residuals standard error
viffac_H2 <- vif(lm_model_H2)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_H2)
par(mfrow = c(1,1))

crit1_H2 <- AIC(lm_model_H2)
crit2_H2 <- BIC(lm_model_H2)
mse_H2 <- mean(sm_H2$residuals^2)

#-------------------------------------------------------------------#

##### Simple model H3 ##### 
# xi = same as H2 + all the rest psychological variables
lm_model_H3 <- lm(formula = GEZAFLGewicht~  AgeLength  
                  + Female  + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                  + GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex) +
                    as.factor(HealthStatus) + as.factor(Loneliness) + as.factor(Satisfaction) + as.factor(Friendships) +
                    as.factor(Upset) + as.factor(Nervous), data = data_kids)
# AgeSquared removed because of multicolinearities
sm_H3 <- summary(lm_model_H3)
cf_H3 <- coef(lm_model_H3)
ci_H3 <- confint(lm_model_H3)

rse_H3 <- summary(lm_model_H3)$sigma  # RSE, residuals standard error
viffac_H3 <- vif(lm_model_H3)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_H3)
par(mfrow = c(1,1))

crit1_H3 <- AIC(lm_model_H3)
crit2_H3 <- BIC(lm_model_H3)
mse_H3 <- mean(sm_H3$residuals^2)

#-------------------------------------------------------------------#

##### Simple model H4 ##### 
# xi = same as D2 + all psychological variables

lm_model_H4 <- lm(formula = GEZAFLGewicht~  AgeLength +  
                  + Female  + as.factor(Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(SOI2006NIVEAU1_MOEDER) + INHP100HGEST_MOEDER + as.factor(fam_structure) 
                  + GEZVRGConsultatiebureau + as.factor(GEZVRGRokenEersteVerzorger) +
                    as.factor(HealthStatus) + as.factor(Loneliness) + as.factor(Satisfaction) + as.factor(Friendships) +
                    as.factor(Upset) + as.factor(Nervous), data = data_kids)
# AgeSquared is removed here as well 

sm_H4 <- summary(lm_model_H4)

cf_H4 <- coef(lm_model_H4)
ci_H4 <- confint(lm_model_H4)

rse_H4 <- summary(lm_model_H4)$sigma  # RSE, residuals standard error
viffac_H4 <- vif(lm_model_H4)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_H4)
par(mfrow = c(1,1))

crit1_H4 <- AIC(lm_model_H4)
crit2_H4 <- BIC(lm_model_H4)
mse_H4 <- mean(sm_H4$residuals^2)


#--------------------------------------------------#
###        Write & Save Modeling Results         ###
#--------------------------------------------------#

# manually select model names
model_names = c('lm_model_A1', 'lm_model_A2', 'lm_model_A3', 'lm_model_A4', 'lm_model_A5',
                'lm_model_A6', 'lm_model_A7', 'lm_model_B1', 'lm_model_B2', 'lm_model_C1',
                'lm_model_C2', 'lm_model_C3', 'lm_model_D1', 'lm_model_D2', 'lm_model_E1',
                'lm_model_E2', 'lm_model_F', 'lm_model_G1', 'lm_model_G2', 'lm_model_H1', 
                'lm_model_H2', 'lm_model_H3', 'lm_model_H4')

# create a list based on model names provided
list_models = lapply(model_names, get)

# set names
names(list_models) = model_names


# save all the models locally in TXT
capture.output(list_models, file = "lm_models_all.txt")

#------------------------------------------------------#
###        Write & Save All model criteria values    ###
#------------------------------------------------------#

all_crit1_vals <- c(crit1_A1, crit1_A2, crit1_A3, crit1_A4, crit1_A5, crit1_A6, crit1_A7,
                         crit1_B1, crit1_B2, crit1_C1, crit1_C2, crit1_C3, crit1_D1, crit1_D2,
                         crit1_E1, crit1_E2, crit1_F, crit1_G1, crit1_G2, crit1_H1, crit1_H2,
                         crit1_H3, crit1_H4)

all_crit2_vals <- c(crit2_A1, crit2_A2, crit2_A3, crit2_A4, crit2_A5, crit2_A6, crit2_A7,
                         crit2_B1, crit2_B2, crit2_C1, crit2_C2, crit2_C3, crit2_D1, crit2_D2,
                         crit2_E1, crit2_E2, crit2_F, crit2_G1, crit2_G2, crit2_H1, crit2_H2,
                         crit2_H3, crit2_H4)

all_mse_vals <- c(mse_A1, mse_A2, mse_A3, mse_A4, mse_A5, mse_A6, mse_A7,
                    mse_B1, mse_B2, mse_C1, mse_C2, mse_C3, mse_D1, mse_D2,
                    mse_E1, mse_E2, mse_F, mse_G1, mse_G2, mse_H1, mse_H2,
                    mse_H3, mse_H4)

#all_coef <- c(cf_A1, cf_A2, cf_A3, cf_A4, cf_A5, cf_A6, cf_A7,
 #             cf_B1, cf_B2, cf_C1, cf_C2, cf_C3, cf_D1, cf_D2,
  #            cf_E1, cf_E2, cf_F, cf_G1, cf_G2, cf_H1, cf_H2,
   #           cf_H3, cf_H4)

#all_confint <- c(ci_A1, ci_A2, ci_A3, ci_A4, ci_A5, ci_A6, ci_A7,
 #             ci_B1, ci_B2, ci_C1, ci_C2, ci_C3, ci_D1, ci_D2,
  #            ci_E1, ci_E2, ci_F, ci_G1, ci_G2, ci_H1, ci_H2,
   #           ci_H3, ci_H4)

all_rse_vals <- c(rse_A1, rse_A2, rse_A3, rse_A4, rse_A5, rse_A6, rse_A7,
                  rse_B1, rse_B2, rse_C1, rse_C2, rse_C3, rse_D1, rse_D2,
                  rse_E1, rse_E2, rse_F, rse_G1, rse_G2, rse_H1, rse_H2,
                  rse_H3, rse_H4)


#all_vif_vals <- c(viffac_A1, viffac_A2, viffac_A3, viffac_A4, viffac_A5, viffac_A6, viffac_A7,
 #                 viffac_B1, viffac_B2, viffac_C1, viffac_C2, viffac_C3, viffac_D1, viffac_D2,
  #                viffac_E1, viffac_E2, viffac_F, viffac_G1, viffac_G2, viffac_H1, viffac_H2,
   #               viffac_H3, viffac_H4)

# save those locally
capture.output(all_crit1_vals, file = "AIC_LM_ALL.txt")
capture.output(all_crit2_vals, file = "BIC_LM_ALL.txt")
capture.output(all_mse_vals, file = "MSE_LM_ALL.txt")
capture.output(all_rse_vals, file = "RSE_LM_ALL.txt")

#capture.output(all_coef, file = "Coef_LM_ALL.txt")
#capture.output(all_confint, file = "CI_LM_ALL.txt")

#capture.output(all_vif_vals, file = "VIF_LM_ALL.txt")



#------------------------------------------------------#
###        Analyse residuals for the TOP models      ###
#------------------------------------------------------#

# model: lm_model_A6
dA6 <- data_kids[c('GEZAFLGewicht', "gew_totaal", "AgeLength", 'AgeSquared', 'Female')]
dA6 <- dA6[rowSums(is.na(dA6)) < 1, ]

# obtain predicted and residual values
dA6$predicted <- predict(lm_model_A6)   # Save the predicted values
dA6$residuals <- residuals(lm_model_A6) # Save the residual values


# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(dA6, aes(x = AgeLength, y = GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = AgeLength, yend = predicted), alpha = .2) +
  
  # > Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()


#-------------------------------------------------------------------#


# model: lm_model_G1

dG1 <- data_kids[c('GEZAFLGewicht', 'gew_totaal', 'AgeLength', 'AgeSquared', 'Female',
                   'GEZVRGDuurZwang', 'Premature_preg', 'westers_MOEDER', 'SOI2006NIVEAU1_MOEDER',
                   'INHP100HGEST_MOEDER', 'fam_structure', 'GEZVRGConsultatiebureau',
                   'seshoog', 'UrbanityIndex', 'GEZVRGMinutenFietsenKind', 'GEZVRGSchoolMinutenFietsenKind')]
dG1 <- dG1[rowSums(is.na(dG1)) < 1, ]

# obtain predicted and residual values
dG1$predicted <- predict(lm_model_G1)   # Save the predicted values
dG1$residuals <- residuals(lm_model_G1) # Save the residual values

# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(dG1, aes(x = AgeLength, y = GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()


#-------------------------------------------------------------------#

# model: lm_model_G2

dG2 <- data_kids[c('GEZAFLGewicht', 'AgeLength', 'Female',
                   'westers_MOEDER', 'SOI2006NIVEAU1_MOEDER',
                   'INHP100HGEST_MOEDER', 'fam_structure', 'GEZVRGConsultatiebureau',
                   'seshoog', 'UrbanityIndex', 'GEZVRGMinutenFietsenKind', 
                   'GEZVRGSchoolMinutenFietsenKind', 'GEZVRGSchoolMinutenBuitenSpelen',
                   'GEZPUBAbonnementSport', 'GEZPUBLidmaatschapSport')]
dG2 <- dG2[rowSums(is.na(dG2)) < 1 , ]

# obtain predicted and residual values
dG2$predicted <- predict(lm_model_G2)   # Save the predicted values
dG2$residuals <- residuals(lm_model_G2) # Save the residual values

# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(dG2, aes(x = AgeLength, y = GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()

#-------------------------------------------------------------------#

# model: lm_model_H3

dH3 <- data_kids[c('GEZAFLGewicht', 'AgeLength', 'Female',
                   'Premature_preg', 'westers_MOEDER', 'SOI2006NIVEAU1_MOEDER',
                   'INHP100HGEST_MOEDER', 'fam_structure', 'GEZVRGConsultatiebureau',
                   'seshoog', 'UrbanityIndex', 'HealthStatus',  'Loneliness', 'Satisfaction',
                   'Friendships', 'Upset', 'Nervous')]
dH3 <- dH3[rowSums(is.na(dH3)) < 1 , ]

# obtain predicted and residual values
dH3$predicted <- predict(lm_model_H3)   # Save the predicted values
dH3$residuals <- residuals(lm_model_H3) # Save the residual values

# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(dH3, aes(x = AgeLength, y = GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()



#---------------------------------------#
###       Outliers Data Analysis      ###
#---------------------------------------#
# scope is to save the data in a dataframe and observe the values

# 14 outliers kept showing up
outliers_kids <- data_kids[c(31, 115, 308, 319, 348, 362, 414, 468, 623, 676, 731, 764, 852, 1022), ]

# save the descriptive statistics in a dataframe
sum_outliers_kids <- data.frame(summary(outliers_kids))
# save the sumDF locally
write.csv(sum_outliers_kids, file="sum_outliers_kids.csv", row.names = TRUE)

# make a scatter plot of birth length vs birth weight
ggplot(outliers_kids, aes(x=GEZAFLGeboorteLengte, y=gew_totaal)) + geom_point()  + xlab("Birth length (cm)") + ylab("Birth weight (gr)")


# make a scatter plot of current weight and age
ggplot(outliers_kids, aes(x= GEZAFLGewicht, y= GEZHHBLeeftijdOP)) + geom_point()  + xlab("Current weight(kg)") + ylab("Age")

# make a plot of current height and current weight
# I removed the + geom_point() to make it non disclosing
ggplot(outliers_kids, aes(x=GEZAFLLengte, y=GEZAFLGewicht))  +
  stat_smooth() + xlab('Current height(cm)') + ylab('Current weight(kg)')


# make a plot of the education of mother versus the income percentile
# I removed the + geom_point() to make it non disclosing
ggplot(outliers_kids, aes(x=SOI2006NIVEAU1_MOEDER, y=INHP100HGEST_MOEDER)) +
  stat_smooth() + xlab('Mother Education tier') + ylab('Income centile')


#------------------------------------------------------------#
###   Make the imputation of the DF and save result        ###
#------------------------------------------------------------#

#store the data into another dataframe as a copy

data_kids_imput_1 <- data.frame(data_kids)

#plot the missing data
mice_plot <- aggr(data_kids_imput_1, col=c('navyblue','yellow'), numbers=TRUE, sortVars= TRUE, labels=names(data_kids_imput_1),cex.axis=.5,
                  gap=3, ylab=c('Missing Data', 'Pattern'))

#------------------------------#
###       Library Hmisc      ###
#------------------------------#

# we use here the aregImpute()
impute_areg <- aregImpute(~ GEZPUBFruit5Dagen + GEZPUBGroente5Dagen + GEZAFLGewicht + 
                            GEZVRGDuurZwang + Premature_preg +fam_structure + 
                            GEZVRGConsultatiebureau + SOI2006NIVEAU1_MOEDER + INHP100HGEST_MOEDER +
                            GEZVRGSchoolMinutenFietsenKind  + GEZVRGSchoolMinutenBuitenSpelen + 
                            GEZVRGMinutenFietsenKind + GEZPUBAbonnementSport + GEZPUBLidmaatschapSport + 
                            GEZVRGRokenEersteVerzorger + GEZAFLLengte + GEZAFLGeboorteLengte + 
                            GEZAFLBMI + gew_totaal + HealthStatus + Loneliness + Nervous + 
                            Upset + Friendships + Satisfaction + GEZAFLGewicht + AgeLength + 
                            AgeSquared + HeightRatio, data=data_kids_imput_1, n.impute = 5, nk=0)

print(impute_areg)
# save the result in a file txt
capture.output(impute_areg, file = "result_imputation.txt")


# put the imputed values in a dataframe
data_kids_imputed <- impute.transcan(impute_areg, imputation = 5, data = data_kids_imput_1, 
                                     list.out = TRUE, pr=FALSE, check=FALSE)
data_kids_imputed <- data.frame(data_kids_imputed) # this is the Final Imputed dataframe (smaller than the actual dataframe)

# add a prefix to the imputed so the columns can be distinguished
#data_kids_imputed %>% dplyr::rename_all(paste0, "_IMP")
colnames(data_kids_imputed) <- paste("IMP", colnames(data_kids_imputed), sep = "_")

# save the output of the imputation to a CSV
write.csv(data_kids_imputed, file="DFImputation_result.csv", row.names = TRUE)

# read again the csv, like this we get rid of the class "impute"
# which cannot be plotted
data_kids_imputed = read.csv(file="DFImputation_result.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)


#---------------------------------------------------#
###      Make the Final DF with Imputed data      ###
#---------------------------------------------------#
# use the DF: data_kids_imputed

#merge the old df with values from the imputed 
full_kids_imputed <- cbind(data_kids_imput_1, data_kids_imputed)

# drop the old columns containing NA
full_kids_imputed <- select(full_kids_imputed, -c(GEZPUBFruit5Dagen, GEZPUBGroente5Dagen, GEZAFLGewicht, 
                                                  GEZVRGDuurZwang, GEZVRGConsultatiebureau, GEZVRGSchoolMinutenFietsenKind,
                                                  GEZVRGSchoolMinutenBuitenSpelen, GEZVRGMinutenFietsenKind, fam_structure,
                                                  GEZPUBAbonnementSport, GEZPUBLidmaatschapSport, GEZVRGRokenEersteVerzorger,
                                                  GEZAFLLengte, GEZAFLGeboorteLengte, GEZAFLBMI, gew_totaal, Premature_preg,
                                                  HealthStatus, Loneliness, Nervous, Upset, Friendships, Satisfaction, 
                                                  GEZAFLGewicht, AgeLength, AgeSquared, HeightRatio, INHP100HGEST_MOEDER,
                                                  SOI2006NIVEAU1_MOEDER))

# save the mixed dataset from Hmisc and CBS locally
write.csv(full_kids_imputed, file="DF_Hmisc_final.csv", row.names = TRUE)

# make the descriptive statistics of this dataframe with HM data
sumDF_HM <- data.frame(summary(full_kids_imputed))
# save the sumDF locally
write.csv(sumDF_HM, file="sumDF_HM.csv", row.names = TRUE)