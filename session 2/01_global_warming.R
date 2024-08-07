library(tidyverse)
library(GGally)
library(skimr)
library(mosaic)

# read NASA temperature data. The tabular data of temperature anomalies can be found here
# https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt
#  
global_warming_data <- read_csv(here::here('data', 'global_warming_data.csv'))


# summary statistics of delta vs. interval
mosaic::favstats(delta ~ interval, data = global_warming_data)

# hockeystick plot
ggplot(global_warming_data, aes(x=date, y = delta))+
  geom_point()+
  geom_smooth(color="red") +
  theme_bw() +
  geom_hline(yintercept = 0, 
             colour = "orange",
             size = 1.5,
             linetype = "dashed")+
  labs (
    title = "Monthly Temperature Anomalies",
    subtitle = "degrees C compared to the 1951-1980 mean",
    caption = "Source: https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt",
    y     = "Temperature Anomaly (in degrees Celsius)",
    x = NULL
  )

ggplot(global_warming_data, aes(x=delta, fill=interval))+
  geom_density(alpha=0.2) +   #density plot with transparency set to 20%
  theme_bw() +                #theme
  labs (
    title = "Density Plot for Monthly Temperature Anomalies",
    subtitle = "degrees C compared to the 1951-1980 mean",
    caption = "Source: https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt",
    x     = "Temperature Anomaly (in degrees Celsius)",
    y = NULL
  )+
  facet_wrap(~ interval, ncol=1)+
  theme(
      axis.text.y = element_blank(),  # Remove y-axis text labels
      axis.ticks.y = element_blank(),  # Remove y-axis tick marks
      legend.position = "none"  # remove legends
      )+
  NULL

global_warming_data %>% 
  ggplot()+
  aes(x=delta, colour=interval)+
  stat_ecdf()+
  theme_bw() +               
  geom_vline (xintercept = 1.5, 
              colour = "red", 
              size = 2,
              linetype = "dashed")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks = seq(-2, 4, by = 0.5),
                     labels = scales::number)+
  labs (
    title = "Cumulative Density Function for Monthly Temperature Anomalies",
    subtitle = "degrees C compared to the 1951-1980 mean",
    caption = "Source: https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.txt",
    x     = "Temperature Anomaly (in degrees Celsius)",
    y = NULL
  )+
  NULL