####################################################
####################################################

## Article 2: Assessing confounding and modification

####################################################
####################################################

## Importing required packages
library(tidyverse)  # a collection of R packages designed for data science
library(skimr)      # provides a strong set of summary statistics 

## Read in clean data set
df_dia <- read_csv("diabetes-data.csv")

## Skim the dataset
skim(df_dia)

## Explore data set
head(df_dia)
glimpse(df_dia)

## Create interaction terms
df_dia <- df_dia %>% 
  mutate(
    padmit = patient_visits*admit_source,
    hadmit = HbA1c*admit_source,
    hpatient = HbA1c*patient_visits,
    hpadmit = HbA1c*patient_visits*admit_source) %>% 
  mutate_at(vars(diabetesMed, HbA1c, admit_source, hadmit), as.factor)

## Check the structure of the data
glimpse(df_dia)

## Descriptive statistics of variables
table(df_dia$diabetesMed)
table(df_dia$HbA1c)
table(df_dia$admit_source)
summary(df_dia$patient_visits)

## Explore diabetesmed by HbA1c levels
table(df_dia$diabetesMed, df_dia$HbA1c)

## Building logistic regression model

## Step 1: Full model
full_model <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
                  + padmit + hadmit + hpatient + hpadmit, 
                  df_dia, family = "binomial")

## Print the summary of the model
summary(full_model)

## Step 2: Remove hpadmit
model1 <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
              + padmit + hadmit + hpatient,
              df_dia, family = "binomial")

## Print the summary of the model
summary(model1)

## Step 2: Remove hpadmit and hpatient
model1a <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
              + padmit + hadmit,
              df_dia, family = "binomial")

## Print the summary of the model
summary(model1a)

## Step 3: Remove hadmit - Start assessing for confounding
model2 <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
              + padmit + hpatient,
              df_dia, family = "binomial")

## Print the coefficients of the model
coef(summary(model2))

## Step 4: Remove padmit
model3 <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
              + hpatient,
              df_dia, family = "binomial")

## Print the coefficients of the model
coef(summary(model3))

## Step 5: Remove admit_source
model4 <- glm(diabetesMed ~ HbA1c + patient_visits + hpatient,
              df_dia, family = "binomial")

## Print the coefficients of the model
coef(summary(model4))

## Get the confidence interval
CI <- confint(model4)
CI

## Find the odds ratio
Odds_ratios <- exp(model4$coefficients)
Odds_ratios


###############################################
###############################################

## Article 3: The dangers of extrapolation

##############################################
##############################################

## Summary statistics of patient visits
summary(df_dia$patient_visits)

## Create a frequency table of patient visits
table(df_dia$patient_visits)

## Create a new data set to include where
## patient_visits <= 20
df_dia20 <- df_dia %>% 
  filter(patient_visits <= 20)

## Explore the patient_visits variable 
table(df_dia20$patient_visits)
summary(df_dia20$patient_visits)

## Step 1: Full model
new_full_model <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
                  + padmit + hadmit + hpatient + hpadmit, 
                  df_dia20, family = "binomial")

## Print the summary of the model
summary(new_full_model)

## Step 2: Remove hpadmit
new_model1 <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
              + padmit + hadmit + hpatient,
              df_dia20, family = "binomial")

## Print the summary of the model
summary(new_model1)

## Step 2: Remove hpadmit and hpatient
new_model1a <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
               + padmit + hadmit,
               df_dia20, family = "binomial")

## Print the summary of the model
summary(new_model1a)

## Step 3: Remove hadmit - Start assessing for confounding
new_model2 <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
              + padmit + hpatient,
              df_dia20, family = "binomial")

## Print the coefficients of the model
coef(summary(new_model2))

## Step 4: Remove padmit
new_model3 <- glm(diabetesMed ~ HbA1c + admit_source + patient_visits 
              + hpatient,
              df_dia20, family = "binomial")

## Print the coefficients of the model
coef(summary(new_model3))

## Step 5: Remove admit_source
new_model4 <- glm(diabetesMed ~ HbA1c + patient_visits + hpatient,
              df_dia20, family = "binomial")

## Print the coefficients of the model
coef(summary(new_model4))

## Get the confidence interval
new_CI <- confint(new_model4)
new_CI

## Find the odds ratio
new_Odds_ratios <- exp(new_model4$coefficients)
new_Odds_ratios
