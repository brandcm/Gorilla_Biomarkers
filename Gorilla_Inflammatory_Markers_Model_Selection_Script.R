# This script was run with means already imputed for missing data.
# For imputation via hotdeck we used a different csv file where missing values were blank. To run, comment out line 11 and uncomment lines 14-16.

# Load packages
library(lme4)         # used for GLMs
library(MuMIn)        # used for model selection
library(rcompanion)   # used for quantifying model prediction (R^2)
library(VIM)          # used for hotdeck imputation

# Read in Data
data <- read.csv("Gorillas_Inflam.csv", header=TRUE, sep=',')

# Hot deck imputation for missing data
#data <- read.csv("Gorillas_Inflam_NoMeans.csv", header=TRUE, sep=',')
#data <- hotdeck(data, variable = "Alb")
#data <- hotdeck(data, variable = "CRP")


### Cardiovascular Disease ###
CVDsexage <- glm(CVD ~ Age + Sex, data=data, family=binomial(link="logit"))
summary(CVDsexage)

CVDAlb <- glm(CVD ~ Alb + Age + Sex, data=data, family=binomial(link="logit"))
summary(CVDAlb)

CVDCRP <- glm(CVD ~ CRP + Age + Sex, data=data, family=binomial(link="logit"))
summary(CVDCRP)

CVDIL6 <- glm(CVD ~ IL6 + Age + Sex, data=data, family=binomial(link="logit"))
summary(CVDIL6)

CVDTNFa <- glm(CVD ~ TNFa + Age + Sex, data=data, family=binomial(link="logit"))
summary(CVDTNFa)

CVDall <- glm(CVD ~ Alb + CRP + IL6 + TNFa + Age + Sex, data=data, family=binomial(link="logit"))
summary(CVDall)

# Model selection for CVD
model.sel(CVDsexage, CVDAlb, CVDCRP, CVDIL6, CVDTNFa, CVDall,
          rank = NULL, rank.args = NULL,
          beta = c("none", "sd", "partial.sd"))

### Chronic Degenerative Conditions ###
CDCsexage <- glm(CDC ~ Age + Sex, data=data, family=binomial(link="logit"))
summary(CDCsexage)

CDCAlb <- glm(CDC ~ Alb + Age + Sex, data=data, family=binomial(link="logit"))
summary(CDCAlb)

CDCCRP <- glm(CDC ~ CRP + Age + Sex, data=data, family=binomial(link="logit"))
summary(CDCCRP)

CDCIL6 <- glm(CDC ~ IL6 + Age + Sex, data=data, family=binomial(link="logit"))
summary(CDCIL6)

CDCTNFa <- glm(CDC ~ TNFa + Age + Sex, data=data, family=binomial(link="logit"))
summary(CDCTNFa)

CDCall <- glm(CDC ~ Alb + CRP + IL6 + TNFa + Age + Sex, data=data, family=binomial(link="logit"))
summary(CDCall)

# Model Selection for CDC
model.sel(CDCsexage, CDCAlb, CDCCRP, CDCIL6, CDCTNFa, CDCall,
          rank = NULL, rank.args = NULL,
          beta = c("none", "sd", "partial.sd"))

### Mortality Risk ###
DEADsexage <- glm(DEAD ~ Age + Sex, data=data, family=binomial(link="logit"))
summary(DEADsexage)

DEADAlb <- glm(DEAD ~ Alb + Age + Sex, data=data, family=binomial(link="logit"))
summary(DEADAlb)

DEADCRP <- glm(DEAD ~ CRP + Age + Sex, data=data, family=binomial(link="logit"))
summary(DEADCRP)

DEADIL6 <- glm(DEAD ~ IL6 + Age + Sex, data=data, family=binomial(link="logit"))
summary(DEADIL6)

DEADTNFa <- glm(DEAD ~ TNFa + Age + Sex, data=data, family=binomial(link="logit"))
summary(DEADTNFa)

DEADall <- glm(DEAD ~ Alb + CRP + IL6 + TNFa + Age + Sex, data=data, family=binomial(link="logit"))
summary(DEADall)

# Model selection for Mortality Risk
model.sel(DEADsexage, DEADAlb, DEADCRP, DEADIL6, DEADTNFa, DEADall,
          rank = NULL, rank.args = NULL,
          beta = c("none", "sd", "partial.sd"))


# Nagelkerke's R^2
nagelkerke(CDCsexage)
nagelkerke(CDCAlb)
nagelkerke(CDCCRP)
nagelkerke(CDCIL6)
nagelkerke(CDCTNFa)
nagelkerke(CDCall)

nagelkerke(CVDsexage)
nagelkerke(CVDAlb)
nagelkerke(CVDCRP)
nagelkerke(CVDIL6)
nagelkerke(CVDTNFa)
nagelkerke(CVDall)

nagelkerke(DEADsexage)
nagelkerke(DEADAlb)
nagelkerke(DEADCRP)
nagelkerke(DEADIL6)
nagelkerke(DEADTNFa)
nagelkerke(DEADall)
