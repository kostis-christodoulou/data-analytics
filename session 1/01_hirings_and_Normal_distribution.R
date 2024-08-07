library(tidyverse)
library(mosaic)


# Test scores are normally distributed with a mean of 525 and standard deviation of 55. 
# Automatic accepts have exam score > = 600 and automatic rejects score < = 425
# Calculate the percentage of applicants who are automatic accepted and rejected

# For those automatically accepted, they score > 600

# What is the Z-score, the distance in SD, between 600 (automatic accept point)
# and the mean of 525? = (600 - 525) / 55 = 75/55 = 1.36
xpnorm(600, mean = 525, sd = 55) 




# For those automatically rejected, the score < 425
# What is the Z-score, the distance in SD, between 425 (automatic reject point)
# and the mean of 525? = (425 - 525) / 55 = -100/55 = -1.82

xpnorm(425, mean = 525, sd = 55) 


# If we wanted to automatically accept 15% and automatically reject 10%, 
# we are doing the inverse calculation, namely finding the Z that corresponds 
# to the 85th and 10th percentile, respectively

# Find the Z value that corresponds to the 85th percentile
qnorm(0.85)  

# Find the Z value that corresponds to the 10th percentile
qnorm(0.10)  

# If the mean=525, and the sd=55, the X values can be found as 
qnorm(0.85, mean=525, sd=55) 
qnorm(0.10, mean=525, sd=55) 
