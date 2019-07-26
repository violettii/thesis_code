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


#-------------------------------------------------------#
###    Read the output data  from Hmisc imputation    ###
#-------------------------------------------------------#

# HM: to show that the data came from the Hmisc imputation
# this file contains the full columns as the CBS provides + the imputed columns from Hmisc

data_kids_HM = read.csv(file="DF_Hmisc_final.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)


#---------------------------------------------#
###  Data Analysis - Linear  Models Group A ###
#---------------------------------------------#

##### Simple model A1##### 
# Y: current weight
# xi = birth weight + birth length
lm_model_A1_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_GEZAFLGeboorteLengte, data = data_kids_HM)

sm_A1_IMP <- summary(lm_model_A1_IMP)
cf_A1_IMP <- coef(lm_model_A1_IMP)
ci_A1_IMP <- confint(lm_model_A1_IMP)

rse_A1_IMP <- summary(lm_model_A1_IMP)$sigma  # RSE, residuals standard error
viffac_A1_IMP <- vif(lm_model_A1_IMP)


# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_A1_IMP)
par(mfrow = c(1,1)) 

crit1_A1 <- AIC(lm_model_A1_IMP)
crit2_A1 <- BIC(lm_model_A1_IMP)
mse_A1_IMP <- mean(sm_A1_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A2##### 
# Y: current weight
# xi = birth weight + length ratio 
lm_model_A2_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_HeightRatio , data = data_kids_HM)

sm_A2_IMP <- summary(lm_model_A2_IMP)
cf_A2_IMP <- coef(lm_model_A2_IMP)
ci_A2_IMP <- confint(lm_model_A2_IMP)

rse_A2_IMP <- summary(lm_model_A2_IMP)$sigma  # RSE, residuals standard error
viffac_A2_IMP <- vif(lm_model_A2_IMP)

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A2_IMP)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A2 <- AIC(lm_model_A2_IMP)
crit2_A2 <- BIC(lm_model_A2_IMP)
mse_A2_IMP <- mean(sm_A2_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A3##### 
# Y: current weight
# xi = birth weight + AL
lm_model_A3_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength , data = data_kids_HM)

sm_A3_IMP <- summary(lm_model_A3_IMP)
cf_A3_IMP <- coef(lm_model_A3_IMP)
ci_A3_IMP <- confint(lm_model_A3_IMP)

rse_A3_IMP <- summary(lm_model_A3_IMP)$sigma  # RSE, residuals standard error
viffac_A3_IMP <- vif(lm_model_A3_IMP)

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A3_IMP)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A3 <- AIC(lm_model_A3_IMP)
crit2_A3 <- BIC(lm_model_A3_IMP)
mse_A3_IMP <- mean(sm_A3_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A4##### 
# Y: current weight
# xi = birth weight + length ratio + AL
lm_model_A4_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_HeightRatio + IMP_AgeLength , data = data_kids_HM)

sm_A4_IMP <- summary(lm_model_A4_IMP)
cf_A4_IMP <- coef(lm_model_A4_IMP)
ci_A4_IMP <- confint(lm_model_A4_IMP)

rse_A4_IMP <- summary(lm_model_A4_IMP)$sigma  # RSE, residuals standard error
viffac_A4_IMP <- vif(lm_model_A4_IMP)

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A4_IMP)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A4 <- AIC(lm_model_A4_IMP)
crit2_A4 <- BIC(lm_model_A4_IMP)
mse_A4_IMP <- mean(sm_A4_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model A5##### 
# Y: current weight
# xi = birth weight + AL + A^2
lm_model_A5_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared, data = data_kids_HM)

sm_A5_IMP <- summary(lm_model_A5_IMP)
cf_A5_IMP <- coef(lm_model_A5_IMP)
ci_A5_IMP <- confint(lm_model_A5_IMP)

rse_A5_IMP <- summary(lm_model_A5_IMP)$sigma
viffac_A5_IMP <- vif(lm_model_A5_IMP)

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_A5_IMP)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_A5 <- AIC(lm_model_A5_IMP)
crit2_A5 <- BIC(lm_model_A5_IMP)
mse_A5_IMP <- mean(sm_A5_IMP$residuals^2)

#-------------------------------------------------------------------#

#################### Model A6_IMP ################### 

# Y: current weight
# xi = birth weight + AL + A^2 + sex

lm_model_A6_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared + 
                        Female, data = data_kids_HM)

sm_A6_IMP <- summary(lm_model_A6_IMP)
cf_A6_IMP <- coef(lm_model_A6_IMP)
ci_A6_IMP <- confint(lm_model_A6_IMP)

rse_A6_IMP <- summary(lm_model_A6_IMP)$sigma  # RSE, residuals standard error
viffac_A6_IMP <- vif(lm_model_A6_IMP)
# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_A6_IMP)
par(mfrow = c(1,1)) 


######## CRITERIA ########
crit1_A6 <- AIC(lm_model_A6_IMP)
crit2_A6 <- BIC(lm_model_A6_IMP)
mse_A6_IMP <- mean(sm_A6_IMP$residuals^2)

######## RESIDUALS ########
# model: lm_model_A6_IMP
dA6_IMP <- data_kids_HM[c('IMP_GEZAFLGewicht', "IMP_gew_totaal", "IMP_AgeLength", 
                               'IMP_AgeSquared', 'Female')]
dA6_IMP <- dA6_IMP[rowSums(is.na(dA6_IMP)) < 1, ]

# obtain predicted and residual values
dA6_IMP$predicted <- predict(lm_model_A6_IMP)   # Save the predicted values
dA6_IMP$residuals <- residuals(lm_model_A6_IMP) # Save the residual values

# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
ggplot(dA6_IMP, aes(x = IMP_AgeLength, y = IMP_GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = IMP_AgeLength, yend = predicted), alpha = .2) +
  
  # > Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()

#-------------------------------------------------------------------#

##### Simple model A7##### 
# Y: current weight
# xi = birth weight + birth length + + AL + A^2 + sex
lm_model_A7_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_HeightRatio + IMP_AgeLength + 
                        IMP_AgeSquared + Female, data = data_kids_HM)

sm_A7_IMP <- summary(lm_model_A7_IMP)
cf_A7_IMP <- coef(lm_model_A7_IMP)
ci_A7_IMP <- confint(lm_model_A7_IMP)

rse_A7_IMP <- summary(lm_model_A7_IMP)$sigma  # RSE, residuals standard error
viffac_A7_IMP <- vif(lm_model_A7_IMP)

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_A7_IMP)
par(mfrow = c(1,1))

crit1_A7 <- AIC(lm_model_A7_IMP)
crit2_A7 <- BIC(lm_model_A7_IMP)
mse_A7_IMP <- mean(sm_A7_IMP$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group B ###
#---------------------------------------------#

##### Simple model B1 ##### 
# xi = same as A6 + preg duration
lm_model_B1_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female + IMP_GEZVRGDuurZwang, data = data_kids_HM)

sm_B1_IMP <- summary(lm_model_B1_IMP)
cf_B1_IMP <- coef(lm_model_B1_IMP)
ci_B1_IMP <- confint(lm_model_B1_IMP)

rse_B1_IMP <- summary(lm_model_B1_IMP)$sigma  # RSE, residuals standard error
viffac_B1_IMP <- vif(lm_model_B1_IMP)

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_B1_IMP)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_B1 <- AIC(lm_model_B1_IMP)
crit2_B1 <- BIC(lm_model_B1_IMP)
mse_B1_IMP <- mean(sm_B1_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model B2 ##### 
# xi = same as B1 + prematurity
lm_model_B2_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female  + IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg), data = data_kids_HM)

sm_B2_IMP <- summary(lm_model_B2_IMP)
cf_B2_IMP <- coef(lm_model_B2_IMP)
ci_B2_IMP <- confint(lm_model_B2_IMP)

rse_B2_IMP <- summary(lm_model_B2_IMP)$sigma  # RSE, residuals standard error
viffac_B2_IMP <- vif(lm_model_B2_IMP)

# simple approach 
par(mfrow = c(2,2)) # split the plot panel into 2x2 grid
plot(lm_model_B2_IMP)
par(mfrow = c(1,1)) # return the plot space back to 1x1

crit1_B2 <- AIC(lm_model_B2_IMP)
crit2_B2 <- BIC(lm_model_B2_IMP)
mse_B2_IMP <- mean(sm_B2_IMP$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group C ###
#---------------------------------------------#

##### Simple model C1 ##### 
# xi = same as B2 + western mother
lm_model_C1_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female  + IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg) + westers_MOEDER, data = data_kids_HM)

sm_C1_IMP <- summary(lm_model_C1_IMP)
cf_C1_IMP <- coef(lm_model_C1_IMP)
ci_C1_IMP <- confint(lm_model_C1_IMP)

rse_C1_IMP <- summary(lm_model_C1_IMP)$sigma  # RSE, residuals standard error
viffac_C1_IMP <- vif(lm_model_C1_IMP)

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_C1_IMP)
par(mfrow = c(1,1)) 

crit1_C1 <- AIC(lm_model_C1_IMP)
crit2_C1 <- BIC(lm_model_C1_IMP)
mse_C1_IMP <- mean(sm_C1_IMP$residuals^2)
#-------------------------------------------------------------------#

##### Simple model C2 ##### 
# xi = same as C1 + mother educ + income centile
lm_model_C2_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female  +IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER, data = data_kids_HM)

sm_C2_IMP <- summary(lm_model_C2_IMP)
cf_C2_IMP <- coef(lm_model_C2_IMP)
ci_C2_IMP <- confint(lm_model_C2_IMP)

rse_C2_IMP <- summary(lm_model_C2_IMP)$sigma  # RSE, residuals standard error
viffac_C2_IMP <- vif(lm_model_C2_IMP)

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_C2_IMP)
par(mfrow = c(1,1)) 

crit1_C2 <- AIC(lm_model_C2_IMP)
crit2_C2 <- BIC(lm_model_C2_IMP)
mse_C2_IMP <- mean(sm_C2_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model C3 ##### 
# xi = same as C2 + family structure
lm_model_C3_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female  + IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER
                  + as.factor(IMP_fam_structure), data = data_kids_HM)

sm_C3_IMP <- summary(lm_model_C3_IMP)
cf_C3_IMP <- coef(lm_model_C3_IMP)
ci_C3_IMP <- confint(lm_model_C3_IMP)

rse_C3_IMP <- summary(lm_model_C3_IMP)$sigma  # RSE, residuals standard error
viffac_C3_IMP <- vif(lm_model_C3_IMP)

# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_C3_IMP)
par(mfrow = c(1,1)) 

crit1_C3 <- AIC(lm_model_C3_IMP)
crit2_C3 <- BIC(lm_model_C3_IMP)
mse_C3_IMP <- mean(sm_C3_IMP$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group D ###
#---------------------------------------------#

##### Simple model D1 ##### 
# xi = same as C3 + consultation visits
lm_model_D1_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female  +IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                  + IMP_GEZVRGConsultatiebureau, data = data_kids_HM)

sm_D1_IMP <- summary(lm_model_D1_IMP)
cf_D1_IMP <- coef(lm_model_D1_IMP)
ci_D1_IMP <- confint(lm_model_D1_IMP)

rse_D1_IMP <- summary(lm_model_D1_IMP)$sigma  # RSE, residuals standard error
viffac_D1_IMP <- vif(lm_model_D1_IMP)

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_D1_IMP)
par(mfrow = c(1,1))

crit1_D1 <- AIC(lm_model_D1_IMP)
crit2_D1 <- BIC(lm_model_D1_IMP)
mse_D1_IMP <- mean(sm_D1_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model D2 ##### 
# xi = same as D1 + smoking
lm_model_D2_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female  + IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                  + IMP_GEZVRGConsultatiebureau + as.factor(IMP_GEZVRGRokenEersteVerzorger), data = data_kids_HM)

sm_D2_IMP <- summary(lm_model_D2_IMP)
cf_D2_IMP <- coef(lm_model_D2_IMP)
ci_D2_IMP <- confint(lm_model_D2_IMP)

rse_D2_IMP <- summary(lm_model_D2_IMP)$sigma  # RSE, residuals standard error
viffac_D2_IMP <- vif(lm_model_D2_IMP)

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_D2_IMP)
par(mfrow = c(1,1))

crit1_D2 <- AIC(lm_model_D2_IMP)
crit2_D2 <- BIC(lm_model_D2_IMP)
mse_D2_IMP <- mean(sm_D2_IMP$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group E ###
#---------------------------------------------#
##### Simple model E1 ##### 
# xi = same as D1 + SES postcode
lm_model_E1_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female  +IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                  + IMP_GEZVRGConsultatiebureau + as.factor(seshoog), data = data_kids_HM)

sm_E1_IMP <- summary(lm_model_E1_IMP)
cf_E1_IMP <- coef(lm_model_E1_IMP)
ci_E1_IMP <- confint(lm_model_E1_IMP)

rse_E1_IMP <- summary(lm_model_E1_IMP)$sigma  # RSE, residuals standard error
viffac_E1_IMP <- vif(lm_model_E1_IMP)
# simple approach 
par(mfrow = c(2,2))
plot(lm_model_E1_IMP)
par(mfrow = c(1,1))

crit1_E1 <- AIC(lm_model_E1_IMP)
crit2_E1 <- BIC(lm_model_E1_IMP)
mse_E1_IMP <- mean(sm_E1_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model E2 ##### 
# xi = same as E1 + urbanity
lm_model_E2_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                  + Female  +IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                  + IMP_GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex), data = data_kids_HM)

sm_E2_IMP <- summary(lm_model_E2_IMP)
cf_E2_IMP <- coef(lm_model_E2_IMP)
ci_E2_IMP <- confint(lm_model_E2_IMP)

rse_E2_IMP <- summary(lm_model_E2_IMP)$sigma  # RSE, residuals standard error
viffac_E2_IMP <- vif(lm_model_E2_IMP)
# simple approach 
par(mfrow = c(2,2))
plot(lm_model_E2_IMP)
par(mfrow = c(1,1))

crit1_E2 <- AIC(lm_model_E2_IMP)
crit2_E2 <- BIC(lm_model_E2_IMP)
mse_E2_IMP <- mean(sm_E2_IMP$residuals^2)


#---------------------------------------------#
###  Data Analysis - Linear  Models Group F ###
#---------------------------------------------#

##### Simple model F ##### 
# xi = same as E2 + veggies + fruits
lm_model_F_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_gew_totaal + IMP_AgeLength + IMP_AgeSquared 
                 + Female  +IMP_GEZVRGDuurZwang + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                 + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                 + IMP_GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex)  + 
                   as.factor(IMP_GEZPUBGroente5Dagen) + as.factor(IMP_GEZPUBFruit5Dagen) , data = data_kids_HM)

sm_F_IMP <- summary(lm_model_F_IMP)
cf_F_IMP <- coef(lm_model_F_IMP)
ci_F_IMP <- confint(lm_model_F_IMP)

rse_F_IMP <- summary(lm_model_F_IMP)$sigma  # RSE, residuals standard error
viffac_F_IMP <- vif(lm_model_F_IMP)

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_F_IMP)
par(mfrow = c(1,1))

crit1_F <- AIC(lm_model_F_IMP)
crit2_F <- BIC(lm_model_F_IMP)
mse_F_IMP <- mean(sm_F_IMP$residuals^2)

#---------------------------------------------#
###  Data Analysis - Linear  Models Group G ###
#---------------------------------------------#
################## Model G1_IMP ##################### 

# xi = same as E2 + Biking
lm_model_G1_IMP <- lm(formula = IMP_GEZAFLGewicht ~ IMP_AgeLength + Female  + as.factor(westers_MOEDER) 
                      + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure)
                      + IMP_GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex)
                      + IMP_GEZVRGMinutenFietsenKind + IMP_GEZVRGSchoolMinutenFietsenKind,
                      data = data_kids_HM)

sm_G1_IMP <- summary(lm_model_G1_IMP)
cf_G1_IMP <- coef(lm_model_G1_IMP)
ci_G1_IMP <- confint(lm_model_G1_IMP)

rse_G1_IMP <- summary(lm_model_G1_IMP)$sigma  # RSE, residuals standard error
viffac_G1_IMP <- vif(lm_model_G1_IMP)

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_G1_IMP)
par(mfrow = c(1,1))

######## CRITERIA ########
crit1_G1 <- AIC(lm_model_G1_IMP)
crit2_G1 <- BIC(lm_model_G1_IMP)
mse_G1_IMP <- mean(sm_G1_IMP$residuals^2)

######## RESIDUALS ########

# model: lm_model_G1

dG1_IMP <- data_kids_HM[c('IMP_GEZAFLGewicht',  'IMP_AgeLength', 'Female', 'westers_MOEDER', 'IMP_SOI2006NIVEAU1_MOEDER',
                               'IMP_INHP100HGEST_MOEDER', 'IMP_fam_structure', 'IMP_GEZVRGConsultatiebureau',
                               'seshoog', 'UrbanityIndex', 'IMP_GEZVRGMinutenFietsenKind', 'IMP_GEZVRGSchoolMinutenFietsenKind')]
dG1_IMP <- dG1_IMP[rowSums(is.na(dG1_IMP)) < 1, ]

# obtain predicted and residual values
dG1_IMP$predicted <- predict(lm_model_G1_IMP)   # Save the predicted values
dG1_IMP$residuals <- residuals(lm_model_G1_IMP) # Save the residual values


# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(dG1_IMP, aes(x = IMP_AgeLength, y = IMP_GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = IMP_AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()



#-------------------------------------------------------------------#

#################### Model G2_IMP ################## 
# xi = same as G1 + sports subscriptions
lm_model_G2_IMP <- lm(formula = IMP_GEZAFLGewicht~  IMP_AgeLength + 
                      + Female  + as.factor(westers_MOEDER) 
                      + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                      + IMP_GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex)
                      + IMP_GEZVRGMinutenFietsenKind + IMP_GEZVRGSchoolMinutenFietsenKind +
                        IMP_GEZVRGSchoolMinutenBuitenSpelen + as.factor(IMP_GEZPUBAbonnementSport) +
                        as.factor(IMP_GEZPUBLidmaatschapSport), data = data_kids_HM)

sm_G2_IMP <- summary(lm_model_G2_IMP)
cf_G2_IMP <- coef(lm_model_G2_IMP)
ci_G2_IMP <- confint(lm_model_G2_IMP)

rse_G2_IMP <- summary(lm_model_G2_IMP)$sigma  # RSE, residuals standard error
viffac_G2_IMP <- vif(lm_model_G2_IMP)

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_G2_IMP)
par(mfrow = c(1,1))


######## CRITERIA ########
crit1_G2 <- AIC(lm_model_G2_IMP)
crit2_G2 <- BIC(lm_model_G2_IMP)
mse_G2_IMP <- mean(sm_G2_IMP$residuals^2)

######## RESIDUALS ########
# model: lm_model_G2_IMP

dG2_IMP <- data_kids_HM[c('IMP_GEZAFLGewicht', 'IMP_AgeLength', 'Female', 'westers_MOEDER', 'IMP_SOI2006NIVEAU1_MOEDER',
                               'IMP_INHP100HGEST_MOEDER', 'IMP_fam_structure', 'IMP_GEZVRGConsultatiebureau',
                               'seshoog', 'UrbanityIndex', 'IMP_GEZVRGMinutenFietsenKind', 'IMP_GEZVRGSchoolMinutenFietsenKind', 
                               'IMP_GEZVRGSchoolMinutenBuitenSpelen', 'IMP_GEZPUBAbonnementSport', 'IMP_GEZPUBLidmaatschapSport')]
dG2_IMP <- dG2_IMP[rowSums(is.na(dG2_IMP)) < 1 , ]

# obtain predicted and residual values
dG2_IMP$predicted <- predict(lm_model_G2_IMP)   # Save the predicted values
dG2_IMP$residuals <- residuals(lm_model_G2_IMP) # Save the residual values

# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(dG2_IMP, aes(x = IMP_AgeLength, y = IMP_GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = IMP_AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()



#---------------------------------------------#
###  Data Analysis - Linear  Models Group H ###
#---------------------------------------------#
# important variables for LUMC: health status, loneliness & satisfaction

##### Simple model H1 ##### 
# xi = same as A6 + LUMC var
lm_model_H1_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_AgeLength + IMP_AgeSquared +
                    Female + as.factor(IMP_HealthStatus) + as.factor(IMP_Loneliness) + 
                    as.factor(IMP_Satisfaction), data = data_kids_HM)

sm_H1_IMP <- summary(lm_model_H1_IMP)
cf_H1_IMP <- coef(lm_model_H1_IMP)
ci_H1_IMP <- confint(lm_model_H1_IMP)

rse_H1_IMP <- summary(lm_model_H1_IMP)$sigma  # RSE, residuals standard error
viffac_H1_IMP <- vif(lm_model_H1_IMP)
# simple approach 
par(mfrow = c(2,2)) 
plot(lm_model_H1_IMP)
par(mfrow = c(1,1)) 

crit1_H1 <- AIC(lm_model_H1_IMP)
crit2_H1 <- BIC(lm_model_H1_IMP)
mse_H1_IMP <- mean(sm_H1_IMP$residuals^2)

#-------------------------------------------------------------------#

##### Simple model H2 ##### 
# xi = same as E2 + LUMC var
lm_model_H2_IMP <- lm(formula = IMP_GEZAFLGewicht~ IMP_AgeLength  
                  + Female + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                  + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                  + IMP_GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex) +
                    as.factor(IMP_HealthStatus) + as.factor(IMP_Loneliness) + 
                    as.factor(IMP_Satisfaction), data = data_kids_HM)

sm_H2_IMP <- summary(lm_model_H2_IMP)
cf_H2_IMP <- coef(lm_model_H2_IMP)
ci_H2_IMP <- confint(lm_model_H2_IMP)

rse_H2_IMP <- summary(lm_model_H2_IMP)$sigma  # RSE, residuals standard error
viffac_H2_IMP <- vif(lm_model_H2_IMP)

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_H2_IMP)
par(mfrow = c(1,1))

crit1_H2 <- AIC(lm_model_H2_IMP)
crit2_H2 <- BIC(lm_model_H2_IMP)
mse_H2_IMP <- mean(sm_H2_IMP$residuals^2)

#-------------------------------------------------------------------#

########### Model H3_IMP ########### 

# xi = same as H2 + all the rest psychological variables
lm_model_H3_IMP <- lm(formula = IMP_GEZAFLGewicht~  IMP_AgeLength 
                      + Female  + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER) 
                      + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                      + IMP_GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(UrbanityIndex) +
                        as.factor(IMP_HealthStatus) + as.factor(IMP_Loneliness) + as.factor(IMP_Satisfaction) +
                        as.factor(IMP_Friendships) + as.factor(IMP_Upset) + as.factor(IMP_Nervous), data = data_kids_HM)

sm_H3_IMP <- summary(lm_model_H3_IMP)
cf_H3_IMP <- coef(lm_model_H3_IMP)
ci_H3_IMP <- confint(lm_model_H3_IMP)

rse_H3_IMP <- summary(lm_model_H3_IMP)$sigma  # RSE, residuals standard error
viffac_H3_IMP <- vif(lm_model_H3_IMP)

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_H3_IMP)
par(mfrow = c(1,1))


######## CRITERIA ########

crit1_H3 <- AIC(lm_model_H3_IMP)
crit2_H3 <- BIC(lm_model_H3_IMP)
mse_H3_IMP <- mean(sm_H3_IMP$residuals^2)

######## RESIDUALS ########
# model: lm_model_H3_IMP

dH3_IMP <- data_kids_HM[c('IMP_GEZAFLGewicht', 'IMP_AgeLength',  'Female',
                               'IMP_Premature_preg', 'westers_MOEDER', 'IMP_SOI2006NIVEAU1_MOEDER',
                               'IMP_INHP100HGEST_MOEDER', 'IMP_fam_structure', 'IMP_GEZVRGConsultatiebureau', 'seshoog', 'UrbanityIndex',
                               'IMP_HealthStatus',  'IMP_Loneliness', 'IMP_Satisfaction', 'IMP_Friendships', 'IMP_Upset', 'IMP_Nervous')]
dH3_IMP <- dH3_IMP[rowSums(is.na(dH3_IMP)) < 1 , ]

# obtain predicted and residual values
dH3_IMP$predicted <- predict(lm_model_H3_IMP)   # Save the predicted values
dH3_IMP$residuals <- residuals(lm_model_H3_IMP) # Save the residual values

# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(dH3_IMP, aes(x = IMP_AgeLength, y = IMP_GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = IMP_AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()

#-------------------------------------------------------------------#

##### Simple model H4 ##### 
# xi = same as D2 + all psychological variables

lm_model_H4_IMP <- lm(formula = IMP_GEZAFLGewicht~  IMP_AgeLength +  
                  + Female + as.factor(IMP_Premature_preg) + as.factor(westers_MOEDER)
                  + as.factor(IMP_SOI2006NIVEAU1_MOEDER) + IMP_INHP100HGEST_MOEDER + as.factor(IMP_fam_structure) 
                  + IMP_GEZVRGConsultatiebureau + as.factor(IMP_GEZVRGRokenEersteVerzorger) +
                    as.factor(IMP_HealthStatus) + as.factor(IMP_Loneliness) + as.factor(IMP_Satisfaction) + 
                    as.factor(IMP_Friendships) + as.factor(IMP_Upset) + as.factor(IMP_Nervous), data = data_kids_HM)

sm_H4_IMP <- summary(lm_model_H4_IMP)
cf_H4_IMP <- coef(lm_model_H4_IMP)
ci_H4_IMP <- confint(lm_model_H4_IMP)

rse_H4_IMP <- summary(lm_model_H4_IMP)$sigma  # RSE, residuals standard error
viffac_H4_IMP <- vif(lm_model_H4_IMP)
# simple approach 
par(mfrow = c(2,2))
plot(lm_model_H4_IMP)
par(mfrow = c(1,1))


crit1_H4 <- AIC(lm_model_H4_IMP)
crit2_H4 <- BIC(lm_model_H4_IMP)
mse_H4_IMP <- mean(sm_H4_IMP$residuals^2)


#--------------------------------------------------#
###        Write & Save Modeling Results         ###
#--------------------------------------------------#

# manually select model names
model_names = c('lm_model_A1_IMP', 'lm_model_A2_IMP', 'lm_model_A3_IMP', 'lm_model_A4_IMP', 'lm_model_A5_IMP',
                'lm_model_A6_IMP', 'lm_model_A7_IMP', 'lm_model_B1_IMP', 'lm_model_B2_IMP', 'lm_model_C1_IMP',
                'lm_model_C2_IMP', 'lm_model_C3_IMP', 'lm_model_D1_IMP', 'lm_model_D2_IMP', 'lm_model_E1_IMP',
                'lm_model_E2_IMP', 'lm_model_F_IMP', 'lm_model_G1_IMP', 'lm_model_G2_IMP', 'lm_model_H1_IMP', 
                'lm_model_H2_IMP', 'lm_model_H3_IMP', 'lm_model_H4_IMP')

# create a list based on model names provided
list_models = lapply(model_names, get)

# set names
names(list_models) = model_names


# save all the models locally in TXT
capture.output(list_models, file = "lm_models_HM_all.txt")

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

all_mse_vals <- c(mse_A1_IMP, mse_A2_IMP, mse_A3_IMP, mse_A4_IMP, mse_A5_IMP, mse_A6_IMP, mse_A7_IMP,
                  mse_B1_IMP, mse_B2_IMP, mse_C1_IMP, mse_C2_IMP, mse_C3_IMP, mse_D1_IMP, mse_D2_IMP,
                  mse_E1_IMP, mse_E2_IMP, mse_F_IMP, mse_G1_IMP, mse_G2_IMP, mse_H1_IMP, mse_H2_IMP,
                  mse_H3_IMP, mse_H4_IMP)

all_rse_vals <- c(rse_A1_IMP, rse_A2_IMP, rse_A3_IMP, rse_A4_IMP, rse_A5_IMP, rse_A6_IMP, rse_A7_IMP,
                  rse_B1_IMP, rse_B2_IMP, rse_C1_IMP, rse_C2_IMP, rse_C3_IMP, rse_D1_IMP, rse_D2_IMP,
                  rse_E1_IMP, rse_E2_IMP, rse_F_IMP, rse_G1_IMP, rse_G2_IMP, rse_H1_IMP, rse_H2_IMP,
                  rse_H3_IMP, rse_H4_IMP)

#all_coef <- c(cf_A1_IMP, cf_A2_IMP, cf_A3_IMP, cf_A4_IMP, cf_A5_IMP, cf_A6_IMP, cf_A7_IMP,
  #            cf_B1_IMP, cf_B2_IMP, cf_C1_IMP, cf_C2_IMP, cf_C3_IMP, cf_D1_IMP, cf_D2_IMP,
   #           cf_E1_IMP, cf_E2_IMP, cf_F_IMP, cf_G1_IMP, cf_G2_IMP, cf_H1_IMP, cf_H2_IMP,
    #          cf_H3_IMP, cf_H4_IMP)

#all_confint <- c(ci_A1_IMP, ci_A2_IMP, ci_A3_IMP, ci_A4_IMP, ci_A5_IMP, ci_A6_IMP, ci_A7_IMP,
 #                ci_B1_IMP, ci_B2_IMP, ci_C1_IMP, ci_C2_IMP, ci_C3_IMP, ci_D1_IMP, ci_D2_IMP,
  #               ci_E1_IMP, ci_E2_IMP, ci_F_IMP, ci_G1_IMP, ci_G2_IMP, ci_H1_IMP, ci_H2_IMP,
   #              ci_H3_IMP, ci_H4_IMP)




#all_vif_vals <- c(viffac_A1_IMP, viffac_A2_IMP, viffac_A3_IMP, viffac_A4_IMP, viffac_A5_IMP, viffac_A6_IMP, viffac_A7_IMP,
 #                viffac_B1_IMP, viffac_B2_IMP, viffac_C1_IMP, viffac_C2_IMP, viffac_C3_IMP, viffac_D1_IMP, viffac_D2_IMP,
  #                viffac_E1_IMP, viffac_E2_IMP, viffac_F_IMP, viffac_G1_IMP, viffac_G2_IMP, viffac_H1_IMP, viffac_H2_IMP,
   #               viffac_H3_IMP, viffac_H4_IMP)



# save those locally
capture.output(all_crit1_vals, file = "AIC_HM_ALL.txt")
capture.output(all_crit2_vals, file = "BIC_HM_ALL.txt")
capture.output(all_mse_vals, file = "MSE_HM_ALL.txt")

capture.output(all_rse_vals, file = "RSE_HM_ALL.txt")

#capture.output(all_vif_vals, file = "VIF_HM_ALL.txt")

#capture.output(all_coef, file = "Coef_HM_ALL.txt")
#cat(sapply(all_confint, toString), file = "Coef_HM_ALL.txt", sep = "\n")

#capture.output(all_confint, file = "CI_HM_ALL.txt")




