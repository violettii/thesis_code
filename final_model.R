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


#---------------------------------------------------------#
###   Read the created data  from file Rscript_11june   ###
#---------------------------------------------------------#

data_kids = read.csv(file="DF_data_kids.csv", header = TRUE,
                     sep = ',', stringsAsFactors = FALSE)

# dimensions of the survey file
dim(data_kids)

# drop X column (index)
data_kids <- select(data_kids, -c(X))


################ Explore a bit ####################

# first we need to filter out the high SES 
# column of interest: seshoog

high_ses_data <- data_kids_imputed[data_kids_imputed$seshoog == 1 , ]

# drop X.1 column (index)
high_ses_data <- select(high_ses_data, -c(X))
data_kids_imputed <- select(data_kids_imputed, -c(X))
# count unique provinces 
unique(high_ses_data$GEZREGProvincie)  # 11!

# province vs consulation visits
plot(factor(high_ses_data$GEZREGProvincie), high_ses_data$IMP_GEZVRGConsultatiebureau, ylab= "Occurences", space = 0.1, las=2, cex.names = 0.5 )

# groupby provinces and summarize the consultation visits 
high_ses_data %>% group_by(GEZREGProvincie) %>% summarise(visits = sum(IMP_GEZVRGConsultatiebureau))


# plot the following for provinces
plot(as.factor(high_ses_data$GEZREGProvincie), ylab= "Occurences", space = 0.2, las=2, cex.names = .8 , ylim = c(0,250))

# plot the following for mom education
plot(as.factor(high_ses_data$IMP_SOI2006NIVEAU1_MOEDER), ylab= "Occurences", space = 0.2, las=2, cex.names = .8 , ylim = c(0,350))




#--------------------------------------------------------------------#
###     Run one LM model: the significant variables // real data   ###
#--------------------------------------------------------------------#
# By the hierarchical principle: adding the age and length is necessary for the model
# since it has the slope variable / interaction
# They were added but removed from model due to singularities: GEZHHBLeeftijdOP + GEZAFLLengte

lm_model_Final <- lm(formula = GEZAFLGewicht ~ AgeLength +  Female + as.factor(fam_structure) + 
                       as.factor(SOI2006NIVEAU1_MOEDER) + GEZVRGConsultatiebureau + 
                       as.factor(seshoog) + as.factor(HealthStatus) + as.factor(Upset)  +
                       as.factor(Loneliness) + as.factor(Satisfaction) + 
                       as.factor(GEZPUBAbonnementSport) , data = data_kids)


# GEZVRGSchoolMinutenFietsenKind GEZVRGSchoolMinutenBuitenSpelen  GEZVRGMinutenFietsenKind
#   GEZPUBLidmaatschapSport GEZPUBAbonnementSport
# I tried these as well, they do not show up as significant here

sm <- summary(lm_model_Final)

cf <- coef(lm_model_Final)
ci <- confint(lm_model_Final)

rse <- summary(lm_model_Final)$sigma  # RSE, residuals standard error
viffac <- vif(lm_model_Final)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_Final)
par(mfrow = c(1,1))


######## CRITERIA ########
crit1_final <- AIC(lm_model_Final)
crit2_final <- BIC(lm_model_Final)
mse <- mean(sm$residuals^2)


######## RESIDUALS ########

# model: lm_model_Final

d_final <- data_kids[c('GEZAFLGewicht', 'AgeLength', 'Female', 
                          'fam_structure', 'SOI2006NIVEAU1_MOEDER', 'GEZVRGConsultatiebureau', 
                          'seshoog', 'HealthStatus', 'Upset', 'Loneliness', 'Satisfaction',
                       'GEZPUBAbonnementSport')]

d_final <- d_final[rowSums(is.na(d_final)) < 1, ]

# obtain predicted and residual values
d_final$predicted <- predict(lm_model_Final)   # Save the predicted values
d_final$residuals <- residuals(lm_model_Final) # Save the residual values

# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(d_final, aes(x = AgeLength, y = GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()


#--------------------------------------------------------------------#
###     Run one LM model: the significant variables // Hmisc data   ###
#--------------------------------------------------------------------#

# read the imputed data from the Hmisc algorithm 
data_kids_imputed = read.csv(file="DF_Hmisc_final.csv", header = TRUE, sep = ',', stringsAsFactors = FALSE)

lm_model_Final_IMP <- lm(formula = IMP_GEZAFLGewicht ~ IMP_AgeLength + Female
                     + as.factor(IMP_fam_structure) + as.factor(IMP_SOI2006NIVEAU1_MOEDER)  
                     + IMP_GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(IMP_HealthStatus) + as.factor(IMP_Upset) 
                     + as.factor(IMP_Loneliness) + as.factor(IMP_Satisfaction) + as.factor(IMP_GEZPUBAbonnementSport), data = data_kids_imputed)

# GEZVRGSchoolMinutenFietsenKind (.) GEZVRGSchoolMinutenBuitenSpelen (.)  IMP_GEZVRGMinutenFietsenKind
#  IMP_GEZPUBLidmaatschapSport (not signif)
# I tried these as well

sm_IMP <- summary(lm_model_Final_IMP)

cf_IMP <- coef(lm_model_Final_IMP)
ci_IMP <- confint(lm_model_Final_IMP)

rse_IMP <- summary(lm_model_Final_IMP)$sigma  # RSE, residuals standard error
viffac_IMP <- vif(lm_model_Final_IMP)            # Variance Inflation Factors

# simple approach 
par(mfrow = c(2,2))
plot(lm_model_Final_IMP)
par(mfrow = c(1,1))


######## CRITERIA ########
crit1_final_IMP <- AIC(lm_model_Final_IMP)
crit2_final_IMP <- BIC(lm_model_Final_IMP)
mse_IMP <- mean(sm_IMP$residuals^2)

######## RESIDUALS ########

# model: lm_model_Final_IMP

d_final_IMP <- data_kids_imputed[c('IMP_GEZAFLGewicht', 'IMP_AgeLength', 'Female', 
                       'IMP_fam_structure', 'IMP_SOI2006NIVEAU1_MOEDER', 'IMP_GEZVRGConsultatiebureau', 
                       'seshoog', 'IMP_HealthStatus', 'IMP_Upset', 'IMP_Loneliness', 'IMP_Satisfaction',
                       'IMP_GEZPUBAbonnementSport')]

d_final_IMP <- d_final_IMP[rowSums(is.na(d_final_IMP)) < 1, ]

# obtain predicted and residual values
d_final_IMP$predicted <- predict(lm_model_Final_IMP)   # Save the predicted values
d_final_IMP$residuals <- residuals(lm_model_Final_IMP) # Save the residual values


# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(d_final_IMP, aes(x = IMP_AgeLength, y = IMP_GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = IMP_AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()


#------------------------------------------------------------------#
###    Create fake data to represent more the lower SES - Vio    ###
#-----------------------------------------------------------------#

# first we need to filter out the low SES 
# column of interest: seshoog

low_ses_data <- data_kids_imputed[data_kids_imputed$seshoog == 0 , ]

# drop X.1 column (index)
low_ses_data <- select(low_ses_data, -c(X.1))
data_kids_imputed <- select(data_kids_imputed, -c(X.1))

################ Explore a bit ####################
# count unique provinces 
unique(low_ses_data$GEZREGProvincie)  # 11!

# province vs consulation visits
plot(factor(low_ses_data$GEZREGProvincie), low_ses_data$IMP_GEZVRGConsultatiebureau, ylab= "Occurences", space = 0.2, las=2, cex.names = .5 )

# groupby provinces and summarize the consultation visits 
low_ses_data %>% group_by(GEZREGProvincie) %>% summarise(visits = sum(IMP_GEZVRGConsultatiebureau))


# plot the following for provinces
plot(as.factor(low_ses_data$GEZREGProvincie), ylab= "Occurences", space = 0.2, las=2, cex.names = .8 , ylim = c(0,60))

# plot the following for mom education
plot(as.factor(low_ses_data$IMP_SOI2006NIVEAU1_MOEDER), ylab= "Occurences", space = 0.2, las=2, cex.names = .8 , ylim = c(0,100))


################  Now do the merge to make the artefact of data  ####################

# make a new DF which is based on the Hmisc imputed dataset 
# but has duplicated values for the low SES group (WG: for weighted)
data_HM_WG <- rbind(data_kids_imputed, low_ses_data)

data_HM_WG <- rbind(data_HM_WG, low_ses_data)

# save the mixed real+Hmisc+Low SES weighted data
write.csv(data_HM_WG, file="DF_HM_WT.csv", row.names = TRUE) # vio's data artefact

# save the summary of the descriptive statistics locally
sumDF_HM_WG <- data.frame(summary(data_HM_WG))
# save the sumDF locally
write.csv(sumDF_HM_WG, file="sumDF_HM_WG.csv", row.names = TRUE)

#---------------------------------------------------------------------#
### Run one LM model: the significant variables //  Weighted data  ###
#---------------------------------------------------------------------#

lm_model_Final_IMP_WG <- lm(formula = IMP_GEZAFLGewicht ~ IMP_AgeLength + Female
                         + as.factor(IMP_fam_structure) + as.factor(IMP_SOI2006NIVEAU1_MOEDER)  
                         + IMP_GEZVRGConsultatiebureau + as.factor(seshoog) + as.factor(IMP_HealthStatus) + as.factor(IMP_Upset) 
                         + as.factor(IMP_Loneliness) + as.factor(IMP_Satisfaction) + 
                           as.factor(IMP_GEZPUBAbonnementSport), data = data_HM_WG)


sm_IMP_WG <- summary(lm_model_Final_IMP_WG)


cf_IMP_WG <- coef(lm_model_Final_IMP_WG)
ci_IMP_WG <- confint(lm_model_Final_IMP_WG)

rse_IMP_WG <- summary(lm_model_Final_IMP_WG)$sigma  # RSE, residuals standard error
viffac_IMP_WG <- vif(lm_model_Final_IMP_WG)            # Variance Inflation Factors


# simple approach 
par(mfrow = c(2,2))
plot(lm_model_Final_IMP_WG)
par(mfrow = c(1,1))


######## CRITERIA ########
crit1_final_IMP_WG <- AIC(lm_model_Final_IMP_WG)
crit2_final_IMP_WG <- BIC(lm_model_Final_IMP_WG)
mse_IMP_WG <- mean(sm_IMP_WG$residuals^2)


######## RESIDUALS ########

# model: lm_model_Final_IMP_WG

d_final_IMP_WG <- data_HM_WG[c('IMP_GEZAFLGewicht', 'IMP_AgeLength', 'Female', 'IMP_fam_structure', 
                               'IMP_SOI2006NIVEAU1_MOEDER', 'IMP_GEZVRGConsultatiebureau', 
                               'seshoog', 'IMP_HealthStatus', 'IMP_Upset', 'IMP_Loneliness', 
                               'IMP_Satisfaction', 'IMP_GEZPUBAbonnementSport')]

d_final_IMP_WG <- d_final_IMP_WG[rowSums(is.na(d_final_IMP_WG)) < 1, ]

# obtain predicted and residual values
d_final_IMP_WG$predicted <- predict(lm_model_Final_IMP_WG)   # Save the predicted values
d_final_IMP_WG$residuals <- residuals(lm_model_Final_IMP_WG) # Save the residual values

# Option: COLOR UNDER/OVER
# Color mapped to residual with sign taken into account.
# i.e., whether actual value is greater or less than predicted
ggplot(d_final_IMP_WG, aes(x = IMP_AgeLength, y = IMP_GEZAFLGewicht)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = IMP_AgeLength, yend = predicted), alpha = .2) +
  
  # Color adjustments made here...
  geom_point(aes(color = residuals)) +  # Color mapped here
  scale_color_gradient2(low = "blue", mid = "grey", high = "red") +  # Colors to use here
  guides(color = FALSE) + geom_point(aes(y = predicted), shape = 1) + theme_bw()


#--------------------------------------------------#
###        Write & Save Modeling Results         ###
#--------------------------------------------------#

# manually select model names
model_names = c('lm_model_Final',  'lm_model_Final_IMP', 'lm_model_Final_IMP_WG')

# create a list based on model names provided
list_models_final = lapply(model_names, get)

# set names
names(list_models_final) = model_names

# save all the models locally in TXT
capture.output(list_models_final, file = "lm_models_final.txt")


#--------------------------------------------------#
###        Write & Save model criteria values    ###
#--------------------------------------------------#
all_crit1_vals <- c(crit1_final,crit1_final_IMP,crit1_final_IMP_WG)
all_crit2_vals <- c(crit2_final,crit2_final_IMP,crit2_final_IMP_WG)
all_mse_vals <- c(mse, mse_IMP, mse_IMP_WG)
all_rse_vals <- c(rse,rse_IMP,rse_IMP_WG)

# save those locally
capture.output(all_crit1_vals, file = "AIC_final.txt")
capture.output(all_crit2_vals, file = "BIC_final.txt")
capture.output(all_mse_vals, file = "MSE_final.txt")
capture.output(all_rse_vals, file = "RSE_final.txt")

#------------------#
# make them manually from Console
#all_VIF <-c(viffac, viffac_IMP, viffac_IMP_WG)

#all_CI <- c(ci, ci_IMP, ci_IMP_WG)

#all_coef <- c(cf, cf_IMP, cf_IMP_WG)
