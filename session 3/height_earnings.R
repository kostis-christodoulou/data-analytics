library(tidyverse)
library(mosaic)
library(broom)
library(skimr)
library(GGally)
library(here)
options(scipen=999, show.signif.stars=FALSE, digits=6)

# read file
height_earnings <- read_csv(here("data", "height_earnings.csv"))

# Quick summary stats
skim(height_earnings)



height_earnings <- height_earnings %>%  
  
  # need  to order education levels from least to most
  # otherwise, they will be in alphabetical order
  mutate(
    ed_level = fct_relevel(ed_level, 
                           "Elementary", 
                           "High School", 
                           "College", 
                           "Some Graduate School", 
                           "Graduate Diploma")
  )



# Do men earn more than women?
favstats(~earn, data=height_earnings)
favstats(earn ~ gender, data=height_earnings)

# R's t.test calculates the t-statistics and also reports
# the 95% Confidence Interval for the difference between means
# if the interval does *not* contain zero, we can reject the null

t.test(earn~gender,data=height_earnings)
confint(t.test(earn~gender,data=height_earnings))

# summary stats on earnings by gender and education level
favstats( earn ~ gender + ed_level, data=height_earnings)


# Regression models
# the bse case model is just the arithmetic mean
model0 <- lm(earn ~ 1, data=height_earnings)

# does height matter? What is the effect of height?
model1 <- lm(earn~height, data=height_earnings)
msummary(model1)
# an extra cm of height increases earning by about 620$

# gender is related to both earnings and height
# Confounding variable bias: when we have a variable Z which is related to both X and Y
model2 <- lm(earn ~ height + gender, data=height_earnings)
msummary(model2)

# let us dra a correlation- scatterplot matrix
height_earnings %>% 
  select(gender, age, height, earn) %>% 
  GGally::ggpairs(aes(colour = gender, alpha = 0.3))+
  theme_bw()

# lets run a model where we try to explain `earn` using all available variables
# the notation "earn ~ ." means exapin 'earn' using all available variables
model3 <- lm(earn ~ ., data=height_earnings)
msummary(model3)

# Since `race` and `hispanic` are not significant, let us remove them from our model
model4 <- lm(earn ~ . -race -hispanic, data=height_earnings)
msummary(model4)

# using this, we get a table with models we have built
huxreg(model0, model1, model2, model3, model4, 
       statistics = c('#observations' = 'nobs', 
                      'R squared' = 'r.squared', 
                      'Adj. R Squared' = 'adj.r.squared', 
                      'Residual SE' = 'sigma'), 
       bold_signif = 0.05, 
       stars = NULL
) %>% 
  set_caption('Comparison of models')

# model 4 rulz... but is it any good when it comes to predictions? 
# using model4, estimate your earnings if you are a man who has
# height = 175 to 185
# gender = Male
# age = 40 years 
# race = "white"
# hispanic = 0
# and lets say he has a Graduate Diploma

# Here is a dataframe with our imaginary man
imaginary_man <- tibble(height = 175:185,
                     gender = "Male",
                     age = 40,
                     race = "white",
                     hispanic = 0,
                     ed_level = "Graduate Diploma")

# When we plug this data frame into predict(), it'll generate a prediction
predict(model4, 
        newdata = imaginary_man, 
        interval = "prediction")

# We can also use broom::augment(). It's  essentially the same thing as predict(), 
# but it adds the predictions and confidence intervals to the imaginary constituency 
model_predictions <- broom::augment(model4, 
                                    newdata = imaginary_man,
                                    interval = "prediction")

# Now we have two new columns named .fitted and .se.fit: .fitted is the
# predicted value and .se.fit is the standard error of the predicted value
model_predictions

