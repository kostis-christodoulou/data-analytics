library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)
library(car)
library(ggfortify)
library(janitor)
library(broom)
library(huxtable)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
uber_surge <- read_csv(here::here('data', 'uber_surge.csv')) %>% 
  janitor::clean_names()


# ---------------------- Have a look at the datafile and calculate summary statistics  ----------------------

uber_surge%>% 
  skim()

uber_surge %>% 
  select(-region,-surge_price) %>% 
  ggpairs()+
  theme_bw()



favstats(~pick_up_time_min, data=uber_surge)

uber_surge %>% 
  ggpairs(aes(colour = region, alpha = 0.5))+
  theme_bw()

model0 <- lm(pick_up_time_min ~ 1, data = uber_surge)
mosaic::msummary(model0)


# ---------------------- 1. Relationship time vs number of drivers ? ----------------------

ggplot(uber_surge, aes(x = number_of_drivers, 
                       y = pick_up_time_min
                       ))+
  geom_point()+
  geom_smooth(method = "loess", color="blue", se=F)+
  #geom_smooth(method = "lm",
  #            se = FALSE)+
  theme_bw()


# regression of pick up time vs number of drivers
model1 <- lm(pick_up_time_min ~ number_of_drivers, data = uber_surge)
mosaic::msummary(model1)


# ---------------------- 2. Use model 1 to predict pick up time  ----------------------
# using broom::augment() see details, fitted values, prediciction interval for each point, etc. 
model1 %>% 
  augment(interval = "prediction")


# ---------------------- 3. pick up time on drivers + surge price ? ----------------------

# regression model
model2 <- lm(pick_up_time_min ~ number_of_drivers + surge_price, data = uber_surge)
mosaic::msummary(model2)
car::vif(model2)


# High colinearity... number of drivers and surge price have a correlation of -0.98

uber_surge %>% 
  select(-region) %>% 
  ggpairs()+
  theme_bw()

# ---------------------- 4. pick up time on  surge price ? ----------------------

# regression model
model3 <- lm(pick_up_time_min ~ surge_price, data = uber_surge)
mosaic::msummary(model3)


# ---------------------- 5. pick up time on drivers + region ? ----------------------

ggplot(uber_surge, aes(x = number_of_drivers, 
                       y = pick_up_time_min, 
                       colour = region))+
  geom_point(alpha = 0.7) +
  theme_minimal()+
  geom_smooth(method = 'lm',
              se = FALSE)


model4 <- lm(pick_up_time_min ~ number_of_drivers + region, data = uber_surge)
mosaic::msummary(model4)


# ---------------------- 5. Comparison of models ----------------------

huxreg(model1, model2, model3, model4,
       statistics = c('#observations' = 'nobs',
                      'R squared' = 'r.squared',
                      'Adj. R Squared' = 'adj.r.squared',
                      'Residual SE' = 'sigma'),
       bold_signif = 0.05,
       stars = NULL
) %>%
  set_caption('Comparison of models')
