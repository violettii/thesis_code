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
library(forcats)

#-------------------------------------------------#
###     Read the file for Cole's Definition     ###
#-------------------------------------------------#
data_kids_Cole = read.csv(file="ColesData_Result.csv", header = TRUE,
                     sep = ',', stringsAsFactors = FALSE)

# drop X column (index)
data_kids_Cole <- select(data_kids_Cole, -c(X))

# dimensions of the survey file
dim(data_kids_Cole)

# rename columns for ease
names(data_kids_Cole)[names(data_kids_Cole) == "GEZPUBObesitas"] <- "CBS_Ob"
names(data_kids_Cole)[names(data_kids_Cole) == "GEZAFLMatigOvergewicht"] <- "CBS_Ov"


#-------------------------------------------------------------#
###     New column for risk prone children based on Cole    ###
#-------------------------------------------------------------#

## FOR LOOP HERE
for (i in 1:nrow(data_kids_Cole)) {
  sex <- data_kids_Cole$GEZHHBGeslachtOP[i]
  age <- data_kids_Cole$GEZHHBLeeftijdOP[i]
  bmi <- data_kids_Cole$BMI[i]
  
  if (sex == 'M'){
    if (age>=0 & age<2){
      data_kids_Cole$risk_prone[i] <- NA}
    
    if (age>=2 & age<3){
      if (bmi >= 17.5 & bmi < 18.5){
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0}}
    
    if (age>=3 & age<4){
      if (bmi >= 17 & bmi < 18 ){
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0}}
    
    if (age>=4 & age<5){
      if (bmi >= 16.5 & bmi < 17.5 ){
        # make the warning column
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0}}
    
    if (age>=5 & age<6){
      if (bmi >= 16 & bmi < 17 ){
        # make the warning column
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0}}        
    
    if (age>=6){
      if (bmi >= 16.5 & bmi < 17.5 ){
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0}}
    
  } # End if for the MALE
  
  else if (sex == 'F'){
    if (age>=0 & age<2){
      data_kids_Cole$risk_prone[i] <- NA}
    
    
    if (age>=2 & age<3){
      if (bmi >= 17 & bmi < 18 ){
        # make the warning column
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0} }
    
    if (age>=3 & age<4){
      if (bmi >= 16.5 & bmi < 17.5 ){
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0}}
    
    if (age>=4 & age<5) {
      if (bmi >= 16 & bmi < 17 ){
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0}}
    
    if (age>=5 & age<6) {
      if (bmi >= 16 & bmi < 17 ){
        # make the warning column
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0} }
    
    if (age>=6){
      if (bmi >= 16 & bmi < 17 ){
        # make the warning column
        data_kids_Cole$risk_prone[i] <- 1}
      else {
        data_kids_Cole$risk_prone[i] <- 0} }
    
  } # Endif for the female
  
  else{ # in a case of a gender to be NA
    data_kids_Cole$risk_prone[i] <- NA}
  
} # End FOR loop


#-----------------------------------------#
###     Plots for Cole's Definitions    ###
#-----------------------------------------#

# proportional M vs F
barplot(prop.table(table(data_kids_Cole$GEZHHBGeslachtOP)), main = "Gender of Children") + theme_classic() # it is proportional
ggplot(data_kids_Cole, aes(as.factor(GEZHHBGeslachtOP))) + geom_bar(position = "fill")+ theme_minimal() 


qplot(data_kids_Cole$GEZHHBLeeftijdOP, geom="histogram", 
      main="Histogram for age", xlab = "Age (years)", bins =13,col=I("black"))

#---------------------#
# plot Overweight for CBS
ggplot(data.frame(data_kids_Cole), aes(x=factor(CBS_Ov)))+ 
  geom_bar() + theme_minimal() # this is a histogram

# plot Obese for CBS
ggplot(data.frame(data_kids_Cole), aes(x=factor(CBS_Ob)))+ 
  geom_bar() + theme_minimal() # this is a histogram

#---------------------#

# plot Overweight for Cole
ggplot(data.frame(data_kids_Cole), aes(x=factor(Cole_Ov)))+ 
  geom_bar() + theme_minimal() # this is a histogram

# plot Obese for Cole
ggplot(data.frame(data_kids_Cole), aes(x=factor(Cole_Ob)))+ 
  geom_bar() + theme_minimal() # this is a histogram

# plot Risk Prone for Cole
ggplot(data.frame(data_kids_Cole), aes(x=factor(risk_prone)))+ 
  geom_bar() + theme_minimal() # this is a histogram

# proportional // CBS
ggplot(data_kids_Cole, aes(x = CBS_Ov, fill = GEZHHBGeslachtOP)) + geom_bar(position = "fill") + theme_minimal()
ggplot(data_kids_Cole, aes(x = CBS_Ob, fill = GEZHHBGeslachtOP)) + geom_bar(position = "fill") + theme_minimal()

# proportional // Cole's Data
ggplot(data_kids_Cole, aes(x = Cole_Ov, fill = GEZHHBGeslachtOP)) + geom_bar(position = "fill") + theme_minimal()
ggplot(data_kids_Cole, aes(x = Cole_Ob, fill = GEZHHBGeslachtOP)) + geom_bar(position = "fill") + theme_minimal()


#---------------------#

# plot for single parents(fam_structure=0) vs obese & overweight
ggplot(data.frame(data_kids_Cole), aes(x=factor(fam_structure)))+ 
  geom_bar() + theme_minimal() # this is a histogram

ggplot(data_kids_Cole, aes(x = risk_prone, fill = as.factor(fam_structure) )) + geom_bar(position = "fill") + theme_minimal()

ggplot(data_kids_Cole, aes(x = Cole_Ov, fill = as.factor(fam_structure) )) + geom_bar(position = "fill") + theme_minimal()

ggplot(data_kids_Cole, aes(x = Cole_Ob, fill = as.factor(fam_structure) )) + geom_bar(position = "fill") + theme_minimal()

#---------------------#

# plot for western mother vs obese & overweight
ggplot(data_kids_Cole, aes(x = Cole_Ov, fill = as.factor(westers_MOEDER) )) + geom_bar(position = "fill") + theme_minimal()

ggplot(data_kids_Cole, aes(x = Cole_Ob, fill = as.factor(westers_MOEDER) )) + geom_bar(position = "fill") + theme_minimal()

ggplot(data_kids_Cole, aes(x = risk_prone, fill = as.factor(westers_MOEDER) )) + geom_bar(position = "fill") + theme_minimal()
#---------------------#

# plot for SES vs obese & overweight
ggplot(data_kids_Cole, aes(x = Cole_Ov, fill = as.factor(seshoog) )) + geom_bar(position = "fill") + theme_minimal()

ggplot(data_kids_Cole, aes(x = Cole_Ob, fill = as.factor(seshoog) )) + geom_bar(position = "fill") + theme_minimal()

ggplot(data_kids_Cole, aes(x = risk_prone, fill = as.factor(seshoog) )) + geom_bar(position = "fill") + theme_minimal()

#---------------------#

# plot for Urbanity vs obese & overweight
# as of CBS, 1: strong urban 5: strong rural
ggplot(data_kids_Cole, aes(x = Cole_Ov, fill = as.factor(UrbanityIndex) )) + geom_bar(position = "fill") + theme_minimal()

ggplot(data_kids_Cole, aes(x = Cole_Ob, fill = as.factor(UrbanityIndex) )) + geom_bar(position = "fill") + theme_minimal()

ggplot(data_kids_Cole, aes(x = risk_prone, fill = as.factor(UrbanityIndex) )) + geom_bar(position = "fill") + theme_minimal()


#------------------------------------------------#
###     Calculations for Cole's Definition     ###
#------------------------------------------------#
n <- nrow(data_kids_Cole) # parametric sample size

sum_CBS_ov <- sum(data_kids_Cole$CBS_Ov) #  overweight kids by CBS
sum_Cole_ov <-  sum(data_kids_Cole$Cole_Ov) # overweight kids by Cole

sum_CBS_ob <- sum(data_kids_Cole$CBS_Ob) # obese kids by CBS
sum_Cole_ob <- sum(data_kids_Cole$Cole_Ob) # obese kids by Cole

sum_risk_prone <- sum(data_kids_Cole$risk_prone) # sum the risk prone kids by Cole

sum_CBS_nm <- n - sum_CBS_ov - sum_CBS_ob # calculate the normal weight kids for CBS
sum_Cole_nm <- n - sum_Cole_ov - sum_Cole_ob # calculate the normal weight kids for Cole


# print absolute count: OV & OB
sprintf('Normal weight children by CBS: %i', sum_CBS_nm) # correct
sprintf('Overweight children by CBS: %i', sum_CBS_ov)
sprintf('Obese children by CBS: %i', sum_CBS_ob)


# print absolute count: OV & OB
sprintf('Normal weight children by Cole: %i', sum_Cole_nm) 
sprintf('Overweight children by Coles Definition: %i', sum_Cole_ov)
sprintf('Obese children by Coles Definition: %i', sum_Cole_ob)
sprintf('Risk-prone children following Coles Definition: %i', sum_risk_prone)


#-----------------------------------#
###     Results Communication     ###
#-----------------------------------#

# calculate percentages of Ov & Ob as stated by CBS
pct_ov <- sum_CBS_ov /n *100
pct_ob <- sum_CBS_ob /n *100

# calculate percentages of Ov & Ob as stated by Cole 
pct_ov_cole <-  sum_Cole_ov/n *100
pct_ob_cole <- sum_Cole_ob /n *100 
pct_risk_cole <- sum_risk_prone /n *100 


#-----------------------------------------------------#
###     Initiate the FOR LOOP for the agreement     ###
#-----------------------------------------------------#

count_disag <- 0    # counter for agreement of methods
count_ag <- 0     # counter for disagreement of methods

for (i in 1:nrow(data_kids_Cole)) {
  # make the temp variables
  # class: Overweight
  cbs_ov_lb <- data_kids_Cole$CBS_Ov[i]
  cole_ov_lb <- data_kids_Cole$Cole_Ov[i]
  
  # class: Obese
  cbs_ob_lb <- data_kids_Cole$CBS_Ob[i]
  cole_ob_lb <- data_kids_Cole$Cole_Ob[i]
  
  #----------------------------------------------------#
  
  if (cbs_ov_lb == cole_ov_lb & cbs_ob_lb == cole_ob_lb){
    count_ag <- count_ag +1}  # count the agreement of the methods
  else {
    count_disag <- count_disag +1}  # count the disagreement of the methods
  
} # end FOR


#----------------------------------------------------------#
###     Extra Calculations for Results Communication     ###
#----------------------------------------------------------#

# print agreement
sprintf('Agreement in the methods for a child health state: %i', count_ag) 

# print disagreement
sprintf('Disagreement in the methods for a child health state: %i', count_disag) 

########## Compute Agreement percentages ########## 
agreement_pct <- count_ag / n * 100
disagreement_pct <- count_disag / n * 100
  
# print the scores
sprintf('Percentage of agreement in children states: %f', agreement_pct)
sprintf('Percentage of disagreement in children states: %f', disagreement_pct)

#------------------------------------------#
###     Final Plots for Communication     ###
#------------------------------------------#

# TAKE 1
# make one df with only labels and plot again
labelsDF <- data_kids_Cole[c('CBS_Ov', 'CBS_Ob', 'Cole_Ov', 'Cole_Ob', 'risk_prone')]
# sum the data
sumdata <- data.frame(value = apply(labelsDF, 2, sum))
sumdata$key <- row.names(sumdata)

#plot
ggplot(data=sumdata, aes(x=key, y=value, fill=key)) + 
  geom_bar(colour="black", stat="identity") + aes(x = fct_inorder(key)) +
  geom_text(aes(label=value), vjust=1.5, colour = "black")


# TAKE 2
p <- ggplot(data=sumdata, aes(x=key, y=value)) + geom_bar(stat="identity") + 
  geom_text(aes(label=value), vjust=1.5, colour = "white")
p
p + aes(x = fct_inorder(key))


#--------------------------------#
###     Summary Statistics     ###
#--------------------------------#
sumDF_Cole <- data.frame(summary(data_kids_Cole))
# save the sumDF locally
write.csv(sumDF_Cole, file="sumDF_Cole.csv", row.names = TRUE)

#---------------------------------------------------#
###     Save the result for Cole's Definition     ###
#---------------------------------------------------#
results_obesity_prev <- c(sum_CBS_nm, sum_CBS_ov, sum_CBS_ob, sum_Cole_nm, sum_Cole_ov, sum_Cole_ob,
                          sum_risk_prone, count_ag, count_disag, agreement_pct, disagreement_pct, pct_ov,
                          pct_ob, pct_ov_cole, pct_ob_cole, pct_risk_cole)

capture.output(results_obesity_prev, file = "results_cole_cbs_prevalence.txt")

