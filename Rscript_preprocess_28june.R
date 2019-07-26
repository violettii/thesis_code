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

#------------------------------#
###   Read the GECON data    ###
#------------------------------#

# format is .sav or SPSS

survey2016 = read.spss("GECON2016V2.sav", to.data.frame = TRUE, stringsAsFactors=FALSE)

# dimensions of the survey file
dim(survey2016)

# get the column names
names(survey2016)

#------------------------------#

survey2017 = read.spss("GECON2017V1.sav", to.data.frame = TRUE, stringsAsFactors=FALSE)

# dimensions of the survey file
dim(survey2017)

# get the column names
names(survey2017)


#-------------------------------#
### Read the merged microdata ###
#-------------------------------#

# format is .dta or Stata

MergeBestand = read.dta("MergeBestand.dta")

# dimensions of the microdata file
dim(MergeBestand)

# get the column names
names(MergeBestand)

#------------------------------#
###   Rename column names    ###
#------------------------------#

names(MergeBestand)[names(MergeBestand) == "rinpersoon"] <- "RINPERSOON"

names(survey2016)[names(survey2016) == "RINpersoon"] <- "RINPERSOON"


#-------------------------------#
# Create columns with file name #
#-------------------------------#

# aim: in all files from the GECON make a column
#         which specifies the year of the survey
#         ie  GECON2017


survey2016["Filename"] <- NA
survey2016$Filename <- "GECON2016"

survey2017["Filename"] <- NA
survey2017$Filename <- "GECON2017"


#------------------------------#
###   Merging of the files   ###
###   Criterion: children    ###
#------------------------------#

# surveydf ---> Column name: RINPERSOON (parent & children)
# microdata ---> Column name: RINPERSOON (parent)
#                             RINPERSOON_KIND (child)

# duplicate column:RINPERSOON in survey Dataframes
# then name it to RINPERSOON_KIND

survey2016$RINPERSOON_KIND = survey2016$RINPERSOON

survey2017$RINPERSOON_KIND = survey2017$RINPERSOON


# then merge with the microdata, MergeBestand

ch_2016 <- merge(survey2016, MergeBestand, by="RINPERSOON_KIND", type="inner")

ch_2017 <- merge(survey2017, MergeBestand, by="RINPERSOON_KIND", type="inner")


#------------------------------#
###      Data Cleaning       ###
#------------------------------#
# This in the data means "Niet van toepassing" "Not applicable"
# change this to NA so then we can drop it

ch_2016[ch_2016=="Niet van toepassing"] <- NA
ch_2017[ch_2017=="Niet van toepassing"] <- NA

# drop columns where all data is NA

ch_2016 <- ch_2016[, colSums(is.na(ch_2016)) < nrow(ch_2016)]
ch_2017 <- ch_2017[, colSums(is.na(ch_2017)) < nrow(ch_2017)]


# drop columns with more than 50% NA

ch_2016[, -which(colMeans(is.na(ch_2016))> 0.5)]
ch_2017[, -which(colMeans(is.na(ch_2017))> 0.5)]


# drop a useless/dummy column
ch_2016 <- select(ch_2016, -c(GEZVRGAndGebKnd,RINPERSOON.x, RINPERSOONS))
ch_2017 <- select(ch_2017, -c(GEZVRGAndGebKnd,RINPERSOON.x, RINPERSOONS))


#-----------------------------------#
###    Select Xi for modeling     ###
### Here the focus is on children ###
#-----------------------------------#

# make a vector with variable names useful for the analysis
variables_list = c('RINPERSOON_KIND','GEZVRG16CHQ', 'seshoog', 'GEZPUBFruit5Dagen', 'GEZPUBGroente5Dagen',
                   'GEZAFLGewicht', 'GEZVRGGeboorteGewicht', 'GEZVRGDuurZwang', 'GEZHHBHHKern', 
                   'Postcode4cijferig', 'GEZREGProvincie','GEZREGGemeentecode','GEZREGStedgem',
                   'GEZHHBGeslachtOP', 'GEZHHBLeeftijdOP', 'GEZVRGConsultatiebureau', 'GEZVRGSchoolMinutenFietsenKind',
                   'GEZVRGSchoolMinutenBuitenSpelen', 'GEZVRGMinutenFietsenKind', 'GEZPUBAbonnementSport',
                   'GEZPUBLidmaatschapSport', 'GEZVRGRokenEersteVerzorger',
                   'GEZVRG12CHQ','GEZVRG13CHQ', 'GEZVRG14CHQ', 'GEZVRG01CHQ', 'GEZVRG16CHQ', 'GEZVRG17CHQ',
                   'GEZAFLLengte','GEZAFLGeboorteLengte', 'GEZAFLBMI',  'gew_totaal',
                   'gbaherkomstgroepering_MOEDER', 'westers_MOEDER','SOI2006NIVEAU1_MOEDER','INHP100HGEST_MOEDER',
                   'GEZPUBObesitas', 'GEZAFLMatigOvergewicht', 'Filename')

# children variables and ID from GECON2016
table_2016 = ch_2016 [, variables_list] 

# children variables and ID from GECON2017
table_2017 = ch_2017[, variables_list] 


#------------------------------#
###   Merge all children     ###
#------------------------------#

# aim is to make 1 df with all kids from the years of the survey where data is available

# combine all children data for 2016 & 2017
merged_2016_2017 <- rbind(table_2016, table_2017)



#------------------------------#
###    Data Types Issues     ###
#------------------------------#
# see the type of the current age column
typeof(merged_2016_2017$GEZHHBLeeftijdOP) # int but not numeric
merged_2016_2017$GEZHHBLeeftijdOP = as.numeric(as.character(merged_2016_2017$GEZHHBLeeftijdOP))

# change type of current weight
typeof(merged_2016_2017$GEZAFLGewicht) # int 
merged_2016_2017$GEZAFLGewicht = as.numeric(as.character(merged_2016_2017$GEZAFLGewicht))
# see the type of the birth weight column
typeof(merged_2016_2017$GEZVRGGeboorteGewicht) # int
merged_2016_2017$GEZVRGGeboorteGewicht = as.numeric(as.character(merged_2016_2017$GEZVRGGeboorteGewicht))


# another birth weight column
typeof(merged_2016_2017$gew_totaal) # int !! Eline said use this !
merged_2016_2017$gew_totaal = as.numeric(as.character(merged_2016_2017$gew_totaal))


# see the type of the child length column
typeof(merged_2016_2017$GEZAFLLengte)  # int
merged_2016_2017$GEZAFLLengte = as.numeric(as.character(merged_2016_2017$GEZAFLLengte))


# see the type of the child birth length column
typeof(merged_2016_2017$GEZAFLGeboorteLengte)  # int
merged_2016_2017$GEZAFLGeboorteLengte = as.numeric(as.character(merged_2016_2017$GEZAFLGeboorteLengte))

# see the type of the BMI column
typeof(merged_2016_2017$GEZAFLBMI) # int
merged_2016_2017$GEZAFLBMI = as.numeric(as.character(merged_2016_2017$GEZAFLBMI))

# see the type of the 4-digits Postcode column
typeof(merged_2016_2017$Postcode4cijferig) # character
merged_2016_2017$Postcode4cijferig = as.numeric(merged_2016_2017$Postcode4cijferig) #now double

# see the type of the consultation column
typeof(merged_2016_2017$GEZVRGConsultatiebureau) #int
merged_2016_2017$GEZVRGConsultatiebureau = as.numeric(as.character(merged_2016_2017$GEZVRGConsultatiebureau))

# see the type of the pregnancy duration column
typeof(merged_2016_2017$GEZVRGDuurZwang) #int
merged_2016_2017$GEZVRGDuurZwang = as.numeric(as.character(merged_2016_2017$GEZVRGDuurZwang))

# see the type of the income column
typeof(merged_2016_2017$INHP100HGEST_MOEDER)  # int
merged_2016_2017$INHP100HGEST_MOEDER = as.character(merged_2016_2017$INHP100HGEST_MOEDER)


#------------------------------#
###    Data Values Issues     ###
### categorical to numerical ####
#------------------------------#

# replace "Weet niet" first with NA wherever it appears
merged_2016_2017[,][merged_2016_2017[,]=='Weet niet'] = NA


## an IF statement is needed to make a new col, based on the one we have

# Fix GEZVRG01CHQ: health status question
merged_2016_2017$HealthStatus <- ifelse(merged_2016_2017$GEZVRG01CHQ == "Uitstekend,",2,
                                        ifelse(merged_2016_2017$GEZVRG01CHQ == "Heel goed,", 1,
                                               ifelse(merged_2016_2017$GEZVRG01CHQ == "Goed,", 0,
                                                      ifelse(merged_2016_2017$GEZVRG01CHQ == "Matig,", -1,
                                                             ifelse(merged_2016_2017$GEZVRG01CHQ == "Slecht", -2, no = NA)))))
# Fix GEZVRG12CHQ: loneliness question
merged_2016_2017$Loneliness <- ifelse(merged_2016_2017$GEZVRG12CHQ == "Altijd,", -2,
                                        ifelse(merged_2016_2017$GEZVRG12CHQ == "Meestal,", -1,
                                               ifelse(merged_2016_2017$GEZVRG12CHQ == "Soms,", 0,
                                                      ifelse(merged_2016_2017$GEZVRG12CHQ == "Bijna nooit,", 1,
                                                             ifelse(merged_2016_2017$GEZVRG12CHQ == "Nooit", 2, no = NA)))))

# Fix GEZVRG13CHQ: nervousness question
merged_2016_2017$Nervous <- ifelse(merged_2016_2017$GEZVRG13CHQ == "Altijd,", -2,
                                      ifelse(merged_2016_2017$GEZVRG13CHQ == "Meestal,", -1,
                                             ifelse(merged_2016_2017$GEZVRG13CHQ == "Soms,", 0,
                                                    ifelse(merged_2016_2017$GEZVRG13CHQ == "Bijna nooit,", 1,
                                                           ifelse(merged_2016_2017$GEZVRG13CHQ == "Nooit", 2, no = NA)))))

# Fix GEZVRG14CHQ: upsetness question
merged_2016_2017$Upset <- ifelse(merged_2016_2017$GEZVRG14CHQ == "Altijd,", -2,
                                   ifelse(merged_2016_2017$GEZVRG14CHQ == "Meestal,", -1,
                                          ifelse(merged_2016_2017$GEZVRG14CHQ == "Soms,", 0,
                                                 ifelse(merged_2016_2017$GEZVRG14CHQ == "Bijna nooit,", 1,
                                                        ifelse(merged_2016_2017$GEZVRG14CHQ == "Nooit", 2, no = NA)))))

# Fix GEZVRG16CHQ: friendhips question
merged_2016_2017$Friendships <- ifelse(merged_2016_2017$GEZVRG16CHQ == "Heel tevreden,", 2,
                                 ifelse(merged_2016_2017$GEZVRG16CHQ == "Tevreden,", 1,
                                        ifelse(merged_2016_2017$GEZVRG16CHQ == "Niet tevreden en niet ontevreden,", 0,
                                               ifelse(merged_2016_2017$GEZVRG16CHQ == "Ontevreden,", -1,
                                                      ifelse(merged_2016_2017$GEZVRG16CHQ == "Heel ontevreden", -2, no = NA)))))


# Fix GEZVRG17CHQ: satisfaction question
merged_2016_2017$Satisfaction <- ifelse(merged_2016_2017$GEZVRG17CHQ == "Heel tevreden,", 2,
                                  ifelse(merged_2016_2017$GEZVRG17CHQ == "Tevreden,", 1,
                                         ifelse(merged_2016_2017$GEZVRG17CHQ == "Niet tevreden en niet ontevreden,", 0,
                                                ifelse(merged_2016_2017$GEZVRG17CHQ == "Ontevreden,", -1,
                                                       ifelse(merged_2016_2017$GEZVRG17CHQ == "Heel ontevreden", -2, no = NA)))))

# fix the family status column
#merged_2016_2017$GEZHHBHHKern
merged_2016_2017$fam_structure <- ifelse(merged_2016_2017$GEZHHBHHKern == "echtpaar/vaste partners alléén", 1, # married couple, permanent partners
                                      ifelse(merged_2016_2017$GEZHHBHHKern == "echtpaar/vaste partners + kind(eren)", 1, # married couple, permanent partners + kids
                                             ifelse(merged_2016_2017$GEZHHBHHKern == "echtpaar/vaste partners + kind(eren) + ander(en)", 1, # married couple, permanent partners + kids
                                                    ifelse(merged_2016_2017$GEZHHBHHKern == "echtpaar/vaste partners + ander(en)", 1, # married couple, permanent partners
                                                           ifelse(merged_2016_2017$GEZHHBHHKern == "eénouder + kind(eren)", 0, #single parent + kids
                                                                  ifelse(merged_2016_2017$GEZHHBHHKern == "eénouder + kind(eren) + ander(en)", 0, no = NA)))))) # single parent + kids + other

# drop the initial psychological column and the family status
merged_2016_2017 <- select(merged_2016_2017, -c(GEZVRG01CHQ,GEZVRG12CHQ,GEZVRG13CHQ,GEZVRG14CHQ,GEZVRG16CHQ,GEZVRG17CHQ, GEZVRG16CHQ.1, GEZHHBHHKern))



#------------------------------------------#

# replace gender values
merged_2016_2017$GEZHHBGeslachtOP <- ifelse(merged_2016_2017$GEZHHBGeslachtOP == 'Vrouw', 'F', 'M')
merged_2016_2017$Female <- ifelse(merged_2016_2017$GEZHHBGeslachtOP == 'F', 1, 0)

# replace "Ja" first with 1 wherever it appears
# nutrition patterns
merged_2016_2017$GEZPUBFruit5Dagen <- ifelse(merged_2016_2017$GEZPUBFruit5Dagen == 'Ja', 1, 0)
merged_2016_2017$GEZPUBGroente5Dagen <- ifelse(merged_2016_2017$GEZPUBGroente5Dagen == 'Ja', 1, 0)

# physical activity patterns
merged_2016_2017$GEZPUBAbonnementSport <- ifelse(merged_2016_2017$GEZPUBAbonnementSport == 'Ja', 1, 0)
merged_2016_2017$GEZPUBLidmaatschapSport <- ifelse(merged_2016_2017$GEZPUBLidmaatschapSport == 'Ja', 1, 0)

# smoking patterns 1st caregiver
#merged_2016_2017$GEZVRGDagelijksRokenEersteVerzorger <- ifelse(merged_2016_2017$GEZVRGDagelijksRokenEersteVerzorger == 'Ja', 1, 0)
merged_2016_2017$GEZVRGRokenEersteVerzorger <- ifelse(merged_2016_2017$GEZVRGRokenEersteVerzorger == 'Ja', 1, 0)

# obesity label from CBS
merged_2016_2017$GEZPUBObesitas <- ifelse(merged_2016_2017$GEZPUBObesitas == 'Ja', 1, 0)

# overweight label from CBS
merged_2016_2017$GEZAFLMatigOvergewicht <- ifelse(merged_2016_2017$GEZAFLMatigOvergewicht == 'Ja', 1, 0)

# change the income column of the mother, it is int
merged_2016_2017$INHP100HGEST_MOEDER <- sapply(strsplit(merged_2016_2017$INHP100HGEST_MOEDER, "e"), head, 1)
merged_2016_2017$INHP100HGEST_MOEDER = as.numeric(merged_2016_2017$INHP100HGEST_MOEDER)

# make a boolean column with the prematurity of the pregnancy
merged_2016_2017$Premature_preg <- ifelse(merged_2016_2017$GEZVRGDuurZwang < 37 , 1, 0)

# fix the urbanity column
# as of CBS 1: strong urban 5: strong rural
merged_2016_2017$UrbanityIndex <- ifelse(merged_2016_2017$GEZREGStedgem == "zeer sterk stedelijk", 1,
                                        ifelse(merged_2016_2017$GEZREGStedgem == "sterk stedelijk", 2,
                                               ifelse(merged_2016_2017$GEZREGStedgem == "matig stedelijk", 3,
                                                      ifelse(merged_2016_2017$GEZREGStedgem == "weinig stedelijk", 4,
                                                             ifelse(merged_2016_2017$GEZREGStedgem == "niet stedelijk", 5, no = NA)))))
# drop the old urbanity column
merged_2016_2017 <- select(merged_2016_2017, -c(GEZREGStedgem))


#-------------------------------#
###   Cole 2000 Definition    ###
###  Overweight and Obesity  ####
#-------------------------------#

# put the needed columns in a dataframe
# avoid messing with the other dataframe
Coles_list = c('RINPERSOON_KIND','GEZHHBGeslachtOP','GEZHHBLeeftijdOP','GEZAFLLengte','GEZAFLGewicht','fam_structure','seshoog','westers_MOEDER','UrbanityIndex','GEZAFLBMI', 'GEZPUBObesitas', 'GEZAFLMatigOvergewicht')
ColesDF <- data.frame(merged_2016_2017[, Coles_list])

# calculate new BMI since there are NA values for no reason
for (i in 1:nrow(ColesDF)) {
  w <- ColesDF$GEZAFLGewicht[i] # store temp the weight
  h <- ColesDF$GEZAFLLengte[i] / 100 # store temp the height (m)
  ColesDF$BMI[i] <- w / (h)^2
}

# drop rows which have an empty BMI
# since the age and the sex are 100% complete
ColesDF <- ColesDF[-which(is.na(ColesDF$BMI)), ]


## FOR LOOP HERE
for (i in 1:nrow(ColesDF)) {
  sex <- ColesDF$GEZHHBGeslachtOP[i]
  age <- ColesDF$GEZHHBLeeftijdOP[i]
  bmi <- ColesDF$BMI[i] # the new BMI col
  
  if (sex == 'M'){
    if (age>=0 & age<2){
      ColesDF$Cole_Ov[i] <- NA
      ColesDF$Cole_Ob[i] <- NA}
    
    if (age>=2 & age<3){
      if (bmi < 18.5){
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 18.5 & bmi <20){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else{ # (BMI>=20)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}
    
    if (age>=3 & age<4){
      if (bmi < 18){
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 18 & bmi <19.5){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else{ # (BMI>=19.5)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}
    
    if (age>=4 & age<5){
      if (bmi < 17.5){
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 17.5 & bmi <19){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else{ # (BMI>=19)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}
    
    if (age>=5 & age<6){
      if (bmi < 17){
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 17 & bmi <19){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else{ # (BMI >= 19)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}        
    
    if (age>=6){
      if (bmi < 17.5){
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 17.5 & bmi < 20){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else { # (BMI >= 20)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}     
  } # End if for the MALE
  
  else if (sex == 'F'){
    if (age>=0 & age<2){
      ColesDF$Cole_Ov[i] <- NA
      ColesDF$Cole_Ob[i] <- NA}
    
    if (age>=2 & age<3){
      if (bmi < 18){
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 18 & bmi <20){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else { # (BMI>=20)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}
    
    if (age>=3 & age<4){
      if (bmi < 17.5){
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 17.5 & bmi <19.5){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else { # (BMI>=19.5)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}
    
    if (age>=4 & age<5) {
      if (bmi < 17) {
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 17 & bmi <19){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else { # (BMI>=19)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}
    
    if (age>=5 & age<6) {
      if (bmi < 17) {
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 17 & bmi <19){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else { # (BMI >= 19)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}
    
    if (age>=6){
      if (bmi < 17){
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 0}
      else if (bmi >= 17 & bmi < 20){
        ColesDF$Cole_Ov[i] <- 1
        ColesDF$Cole_Ob[i] <- 0}
      else { # (BMI >= 20)
        ColesDF$Cole_Ov[i] <- 0
        ColesDF$Cole_Ob[i] <- 1}}
    
  } # Endif for the female
  
  else{ # in a case of a gender to be NA
    ColesDF$Cole_Ov[i] <- NA
    ColesDF$Cole_Ob[i] <- NA}
  
  
} # End FOR loop


# save the DF with the result locally
write.csv(ColesDF, file="ColesData_Result.csv", row.names = TRUE)


#------------------------------#
###  Data Analysis - Plots   ###
#------------------------------#

# plot birth weight versus length
scatter.smooth(x=merged_2016_2017$gew_totaal, y=merged_2016_2017$GEZAFLGeboorteLengte, 
               main = "GEZAFLGeboorteLengte ~ gew_totaal", xlab = "birth weight", ylab = "birth length")
# it doesn't look very linear & additive, still acceptable

# plot weight versus length
scatter.smooth(x=merged_2016_2017$GEZAFLGewicht, y=merged_2016_2017$GEZAFLLengte, 
               main = "Height ~ weight", xlab = "Current weight(kg)", ylab = "Current length(cm)")
# it doesn't look very linear & additive, still acceptable

qplot(merged_2016_2017$gew_totaal, geom="histogram", 
      main="Histogram birth weight", xlab = "Birth weight (gr)", bins = 40)

qplot(merged_2016_2017$GEZAFLGewicht, geom="histogram", 
      main="Histogram weight", xlab = "Current weight (kg)", bins = 40)

qplot(merged_2016_2017$GEZAFLGeboorteLengte, geom="histogram", 
      main="Histogram birth length", xlab = "Birth length (cm)", bins = 40)

qplot(merged_2016_2017$GEZAFLLengte, geom="histogram", 
      main="Histogram length", xlab = "Current length (cm)", bins = 40) 

#----------------------------------------------------------------------------------

# plot birth weight versus current weight
# the current weight needs to be in grams as well !

# make it under a new col, so they are on the same scale (grams)
merged_2016_2017$GEZAFLGewicht_gr <- merged_2016_2017$GEZAFLGewicht * 1000

scatter.smooth(x=merged_2016_2017$gew_totaal, y=merged_2016_2017$GEZAFLGewicht_gr, 
               main = "birth weight ~ weight", xlab = "Birth weight(gr)", ylab = "Current weight (gr)")

# plot OUTLIERS in pregnancy duration
# outline: is to remove the outliers
boxplot(merged_2016_2017$GEZVRGDuurZwang, main="Pregnancy duration", ylab="weeks", outline = FALSE)

ggplot(merged_2016_2017, aes(y=GEZVRGDuurZwang)) +  
  geom_boxplot(fill='gray', alpha=0.5, outlier.alpha = 0)  + ylab("Pregnancy Duration (weeks)") #hide outlier

ggplot(merged_2016_2017, aes(x = as.factor(seshoog), y=GEZVRGDuurZwang)) + 
  geom_boxplot(fill='gray', alpha=0.5, outlier.alpha = 0) + xlab("SES") #hide outliers


# socioeconomic status high or not based on a SES threshold the CBS defines
# Eline created this variable
qplot(merged_2016_2017$seshoog, geom="histogram", 
      main="Histogram high SES", xlab = "SES",bins=2, col=I("black"))

# age of the children at the time of the survey
qplot(merged_2016_2017$GEZHHBLeeftijdOP, geom="histogram", 
      main="Histogram of age for the whole sample", xlab = "Age (years)",bins=7, col=I("black"))

# BMI histogram of the children at the time of the survey
qplot(merged_2016_2017$GEZAFLBMI, geom="histogram", 
      main="Histogram BMI",bins=30, xlab='BMI')

ggplot(merged_2016_2017, aes(y=GEZAFLBMI)) +  
  geom_boxplot(fill='gray', alpha=0.5, outlier.alpha = 0)  + ylab("BMI values")

# Gender/ GEZHHBGeslachtOP
# trial: 2 with the gender as a factor (tip : from gurru99)
ggplot(data.frame(merged_2016_2017), aes(x=factor(GEZHHBGeslachtOP)))+ geom_bar() + theme_minimal() # this is a histogram



# plot OUTLIERS in BMI & Current Lenght (=height)
# Box plot
par(mfrow=c(1,2))
boxplot(merged_2016_2017$GEZAFLBMI, main="BMI")
boxplot(merged_2016_2017$GEZAFLLengte, main="Current Length")
par(mfrow = c(1,1)) # return the plot space back to 1x1

# plot OUTLIERS in Current weight & Current Lenght (=height)
# Box plot
par(mfrow=c(1,2))
boxplot(merged_2016_2017$GEZAFLGewicht, main="Current Weight")
boxplot(merged_2016_2017$GEZAFLLengte, main="Current Length")
par(mfrow = c(1,1)) # return the plot space back to 1x1


#------------------------------#
###   Plot of location data   ###
#------------------------------#

# prefer the following for provinces
barplot(summary(merged_2016_2017$GEZREGProvincie), ylab= "Occurences", space = 0.2, las=2, cex.names = .7 , ylim=c(0,300))


#------------------------------#
### Write the clean DF to CSV ###
#------------------------------#
write.csv(merged_2016_2017, file="data_ch_1617_11june.csv", row.names = TRUE)