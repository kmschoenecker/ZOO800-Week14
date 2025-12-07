#### ZOO 800 Week 14 Homework ----

# Kristine Schoenecker

# Model selection homework

#### Load libraries ----

library(tidyverse)
library(lmtest)

#### Objective 1 ----

## (A) Exchange data with a partner 
# using Nathan's simulated tick data from Week 11

# load the data in 

ticks <- read.csv("ticks.csv")

# Ecological scenario:
# For our newly-discovered tick species Tickus bittus, we are interested in 
# seeing if the different life stages (nymphs and adults) have different 
# vulnerability to desiccation, and how this might affects their questing 
# patterns. Our question: How does sensitivity of questing duration to a 
# humidity index differ between tick nymphs and adults? 
# Continuous response variable (questing_duration, in minutes)
# continuous predictor variable (humidity_index, does not exist outside of this exercise), 
# and two-level factor (life_stage).

## (B) Fit two alternative models (full and reduced model) to the data given to you:

### The full model (both X variables and their interaction)

# I am going to do my full model as a linear model with the life stage as a random variable
# save life stage of the ticks as a factor rather than character variable first

ticks$life_stage <- as.factor(ticks$life_stage)

full_model <- lm(questing_duration ~ humidity_index * life_stage, data = ticks)

### A reduced model with no interaction term. But we still want to include life stage
# so we use the + when making the model, right?

reduced_model <- lm(questing_duration ~ humidity_index + life_stage, data = ticks)

# summaries of the models

summary(full_model)
summary(reduced_model)

## (C) Calculate the negative log likelihood (NLL) of each model using the logLik function. 
# Which has the lower NLL?

NLL_full_model <- logLik(full_model)
NLL_full_model # -535.2249

NLL_reduced_model <- logLik(reduced_model)
NLL_reduced_model # -535.2963

## (D) Compare the two models using the likelihood ratio test (lmtest::lrtest) to see whether the NLL of
# the full model is sufficiently lower to justify the additional parameter. Which model is preferred
# and what does that mean ecologically? How does your answer compare to the result you
# obtained when doing backward model selection?

likelihood_ratio_test <- lrtest(full_model, reduced_model)
likelihood_ratio_test 

# According to the likelihood ratio test, there is not a significant degree of difference
# between the models. Following the idea of parsimony, we should then used the reduced model.
# From this result, we would conclude that the questing duration of ticks is not 
# determined by their life stage, but solely by the humidity index.

# This does match the interpretation we would make off our full model summary table.
# That table also noted no significance when including the interaction term.

## Objective 2 - In Objective 1, you determined whether a model with an 
# interaction is better than one without. However, both models could still be bad. 
# A more comprehensive model selection process might involve
# fitting the full set of possible models and comparing their AIC values in a table

## (A) Develop an AIC table (in a data frame) to compare the following models:
### a. The full model – intercept, both X variables and their interaction
### b. Main effects – intercept and both X variables
### c. Single variable 1 – intercept and X1
### d. Single variable 2 – intercept and X2
### e. Intercept only

# with the simulated data I have, there is only one x-variable to compare?
# is it fine to also run the models with just life stage, even though that is a 2-level factor
# and not continous?

# Full model, same as what we used in Objective 1
full_model

# Main effects, same as our reduced model above? Since we are not including the interaction effect?
reduced_model

# Single variable 1, so only model based on questing duration and humidity, do not include age?

single_var1_model <- lm(questing_duration ~ humidity_index, data = ticks)

# Single variable 2, model questing duration based on life stage?
# is this correct to do, with life stage not being continous, in this case?

single_var2_model <- lm(questing_duration ~ life_stage, data = ticks)

# Intercept only? I am a little confused as to what these means
# Nathan mentioned just setting the intercept to 1?

intercept_model <- lm(questing_duration ~ 1, data = ticks) # not sure this is right
# r lets me run the model, but I guess that does not mean it is technically correct

# extract our AIC values, there is a function called AIC() in R to do such

aic_full <- AIC(full_model)
aic_main <- AIC(reduced_model)
aic_var1 <- AIC(single_var1_model)
aic_var2 <- AIC(single_var2_model)
aic_int <- AIC(intercept_model)

# data frame with our AIC values

aic_table <- data.frame(Model = c("Full Model", "Main Effects", "Humidity Only", "Life Stage Only", "Intercept"),
                        AIC = c(aic_full, aic_main, aic_var1, aic_var2, aic_int))

aic_table #view our table

## (B) Which model or models are supported by AIC? What does this mean ecologically?

# The Main Effects and Humidity Only model, have the lowest AIC values.
# with the Full Model still within reason. We should exclude our Life Stage only and Intercept
# models. The "best" models being the Main Effects and Humidity only again suggests
# that the interaction between life stage is not a significant driver of the trend we 
# are seeing.