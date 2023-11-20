# Breeding_dispersal

## BREEDING DISPERSAL INSIDE ISLANDS; 2 CONSECUTIVE YEARS

rm(list=ls())

setwd("C:/Users/ihermans/OneDrive - Åbo Akademi O365/Data/Breeding dispersal/R")

getwd()

library (car)
library(lme4)
library(lmerTest)
library(MuMIn)

## SELECTION OF ADULT MORTALITY VARIABLE
## Choosing adult mortality variable: nesting attempts + killed females OR killed females/nesting attempts


adult_mort <- read.csv("Adult_mort_7_11.csv")

# SCALE variables

mort=scale (adult_mort$killed_f)
nest_att=scale (adult_mort$nest_attempts)
mort_att=scale (adult_mort$kill_f_attemps)
mort1=scale (adult_mort$X1_Killed_F)
nest_att1=scale (adult_mort$X1_nest_attempts)
mort_att1=scale (adult_mort$X1_kill_f_attempts)

hist(adult_mort$DISP_DIST)

# Transform response variable
log_dist <- log (adult_mort$DISP_DIST+1)
hist(log_dist)

# Test simple models with alternative variables

mortality1 <-lmer(log_dist ~ mort_att + (1|RING), data= adult_mort)

mortality2 <- lmer(log_dist ~ mort + nest_att +(1|RING), data= adult_mort)

# mortality3 <-lmer(log_dist ~ mort_att1 + (1|RING), data= adult_mort)

# mortality4 <- lmer(log_dist ~ mort1 + nest_att1 +(1|RING), data= adult_mort)

# AIC (mortality1, mortality2, mortality3, mortality4)
AIC (mortality1, mortality2)

# Maybe not necessary to test year n-1 mortality?
## Result: mortality2 is better (delta AIC 4.046)
### ->>> Having the killed females and nesting attempts separate was better,also in the case of mortality from last year (mortality4)


###############
### Hypothesis 1: "Breeding dispersal has changed with the increasing predation pressure"
##############
# New data file with only the needed variables (without empty cells)

# install.packages("MuMIn")


hypo1 <- read.csv ("breed_disp_hypo1.csv")
names(hypo1)


## SCALE variables 
Year=scale (hypo1$YEAR)
early_pred=scale (hypo1$INI_prednest)
mort=scale (hypo1$killed_f)
nest_att=scale (hypo1$nest_attempts)
fin_pred1=scale (hypo1$X1_FIN_prednest)
mort1=scale (hypo1$X1_Killed_F)
nest_att1=scale (hypo1$X1_nest_attempts)

log_dist <- log (hypo1$DISP_DIST+1)

# Full hypothesis 1 model with all variables included

Hypotes1_full <- lmer (log_dist ~ Year + fin_pred1 + early_pred + mort + nest_att + mort1 + nest_att1 + (1|RING), data= hypo1)

summary (Hypotes1_full)

vif(Hypotes1_full)


# Select model
# exclude models containing both mort year n and mort year n-1 at the same time

##FUNKAR INTE
#hypo1_dredge <- dredge(Hypotes1_full, subset = !(mort+ nest_att && mort1+ nest_att1))

#hypo1_dredge <- dredge ( Hypotes1_full, subset =   !(mort && mort1) && !(nest_att && nest_att1) && !(mort && nest_att1) && !(nest_att && mort1))

#hypo1_dredge <- dredge ( Hypotes1_full, subset = dc(nest_att, mort) && dc(mort, nest_att))

#hypo1_dredge <- dredge ( Hypotes1_full, subset = !(dc(nest_att, mort)) && !(dc(mort, nest_att)))

#hypo1_dredge <- dredge ( Hypotes1_full, subset = !(mort+ nest_att && mort1+ nest_att1)) && dc(nest_att, mort) && dc(nest_att1, mort1)

hypo1_dredge <- dredge ( Hypotes1_full, subset = !(mort+ nest_att && mort1+ nest_att1) && dc(nest_att, mort) && dc(nest_att1, mort1) && dc(mort,nest_att) && dc(mort1, nest_att1))

hypo1_dredge

## Best model: dispersal distance ~ final nestpred + mort year n + nest attempts year n 

best_hypo1 <- lmer(log_dist ~ fin_pred1 + mort + nest_att + (1|RING), data= hypo1)

vif(best_hypo1)

summary(best_hypo1)

###### Hypothesis 2:	Dispersal distance is related to breeding experience, condition, 
###### cognitive skills, boldness and nest fate and local nest success the next year 

hypo2 <- read.csv ("Hypo2_data.csv")
names(hypo2)

# Transform dependent variable
log_dist <- log (hypo2$DISP_DIST+1)

hist(log_dist)

# Scale the variables

FID=scale (hypo2$FID)
Head=scale (hypo2$Head)
Year_BC=scale (hypo2$Yearly_BC)
Glob_BC=scale (hypo2$Global_BC)
Experience=scale (hypo2$EXPERIENCE)
early_pred=scale (hypo2$INI_prednest)

# Variables year n-1
cover1=scale (hypo2$X1_COVER)
density1=scale (hypo2$X1_DENSITY)
ind_suc1=scale (hypo2$X1_FATE)
fin_pred1=scale (hypo2$X1_FIN_prednest)



# Choose body condition index
bc_1 <- lmer (log_dist ~ Year_BC +(1|RING), data= hypo2)
bc_2 <- lmer (log_dist ~ Glob_BC +(1|RING), data= hypo2)

AIC(bc_1)
AIC(bc_2)

# Global BC has better fit, delta AIC 1.058


## FULL MODEL Hypothesis 2

Hypotes2_full <- lmer (log_dist ~ FID + Head + Year_BC + Glob_BC + Experience + early_pred  
                       + cover1 + density1 + ind_suc1  + fin_pred1 + (1|RING), data= hypo2, na.action = "na.fail")

summary (Hypotes2_full)
vif (Hypotes2_full)

## MODEL SELECTION

hypo2_dredge <- dredge ( Hypotes2_full)

subset (hypo2_dredge, delta <4)


## Best model: dispersal distance ~ nest cover year n-1 + final nest predation year n-1 + individual nest success year n-1 

best_hypo2 <- lmer(log_dist ~ cover1 + fin_pred1 + ind_suc1+ (1|RING), data= hypo2)

vif(best_hypo2)

summary(best_hypo2)


#########################
## Hypothesis 3: 3.	Island type, breeding density, nest predation and adult mortality affect the breeding dispersal between islands
#########################

## SELECTION OF ADULT MORTALITY VARIABLE
## Choosing adult mortality variable: nesting attempts + killed females OR killed females/nesting attempts


adult_mort <- read.csv("Hypo3.csv")
names(adult_mort)

# SCALE variables

mort=scale (adult_mort$killed_f)
nest_att=scale (adult_mort$nest_attempts)
mort_att=scale (adult_mort$kill_f_attemps)
mort1=scale (adult_mort$X1_killed_f)
nest_att1=scale (adult_mort$X1_nest_attempts)
mort_att1=scale (adult_mort$X1_kill_f_attempts)

hist(adult_mort$DISP_BTW_ISL)

# Transform response variable - unnecessary?
log_disp <- log (adult_mort$DISP_BTW_ISL+1)
hist(log_disp)

# Test simple models with alternative variables

mortality1 <-glmer(log_disp ~ mort_att + (1|RING), data= adult_mort, family= binomial (link="logit"))

mortality2 <- glmer(log_disp ~ mort + nest_att +(1|RING), data= adult_mort, family= binomial(link="logit"))

#mortality3 <-glmer(log_disp ~ mort_att1 + (1|RING), data= adult_mort, family= binomial(link="logit"))

#mortality4 <- glmer(log_disp ~ mort1 + nest_att1 +(1|RING), data= adult_mort, family= binomial(link="logit"))

##AIC (mortality1, mortality2, mortality3, mortality4)
AIC (mortality1, mortality2)

# Maybe not necessary to test year n-1 mortality?
## Result: mortality2 is better (delta AIC 4.789)
### ->>> Having the killed females and nesting attempts separate was better,also in the case of mortality from last obs (mortality4)


######


hypo3 <- read.csv ("Hypo3.csv")
names(hypo3)


# Transform dependent variable - is this necessary for binomial ?
log_disp <- log (hypo3$DISP_BTW_ISL+1)

# Scale the variables
area1=scale(hypo3$X1_AREA)
forest1=scale(hypo3$X1_OF)
density1=scale(hypo3$X1_DENSITY)
ind_suc1=scale(hypo3$X1_fate)
fin_pred1=scale(hypo3$X1_FIN_pred)
mort=scale (hypo3$killed_f)
nest_att=scale (hypo3$nest_attempts)
mort1=scale (hypo3$X1_killed_f)
nest_att1=scale (hypo3$X1_nest_attempts)


# FULL MODEL Hypothesis

Hypotes3_full <- glmer (log_disp ~ area1 + forest1 + density1 + ind_suc1  + fin_pred1 + mort + nest_att + mort1 + nest_att1 +
                          (1|RING), data= hypo3, family= binomial(link="logit"), na.action = "na.fail")

summary (Hypotes3_full)
vif (Hypotes3_full)



## MODEL SELECTION (with either adult mortality from year n or n-1)

hypo3_dredge <- dredge ( Hypotes3_full, subset = !(mort+ nest_att && mort1+ nest_att1) && dc(nest_att, mort) && dc(nest_att1, mort1) &&
                           dc(mort,nest_att) && dc(mort1, nest_att1))

subset (hypo3_dredge, delta <4)


## Best model:

best_hypo3 <- glmer (log_disp ~ density1 + forest1 + ind_suc1 + mort1 + nest_att1 + (1|RING) , data= hypo3, family= binomial(link="logit"))

summary(best_hypo3)

################
## Hypothesis 4: 4.	Direction of dispersal in relation to mainland is affected by island type, breeding density, nest predation and adult mortality.
###############
