# Day_3
# 17th April 2018
# Distributions
# Tests
# Normal population
# mean is the same as median
# most measurements are clustard around the mean- central limit theorum
# normal dis. the concept of mean and sd carries the weight'
# as distribution deviate
# T distribution is representation of many means 
# Poisson_ fixed number of time - number of occurances at a specific unit of time
# exponential distribution-

# Generate a cullen and Frey graph
library(tidyverse)
library(fitdistrplus)
library(logspline)
library(ggpubr)

r_norm <- rnorm(n = 1000, mean = 13, sd = 1) 
# run a fit distibution
# create histogram
hist(r_norm)
descdist(r_norm, discrete = FALSE, boot = 100)

# uniform data
y <- runif(100)
par(mfrow = c(1, 1))
plot(x = c(1:100), y  = y)
hist(y)
descdist(y)

# t-test ------------------------------------------------------------------

library(tidyverse)
library(plotly)

# Difference in t test and anova- t test: comparig two things, anova- more than 2 things
# Compare three variables or more than two use anova
# Assumptions, 

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Check assumptions -------------------------------------------------------

# Normality
# for this use the Shapiro- wilk test
# returns w value and P values
shapiro.test(r_dat$dat)

# but this is testing all of the data together
# tell r to run dataset on specific groups and not only one column

r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))
# data are normal when p > 0.05
# the data is not normal when p>= 0.05

# check homoscedasticity --------------------------------------------------
# There are many ways to check for homoscedasticity
# which is thesimilarity of variance between sample sets
# for now we will simply say that this assumptions is met when
# the variance of the samples are not more than 2 - 4 times greater
# then one another

# checck variance for entire dataset
var(r_dat$dat)

#
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))
# inependant of eachther- paired t-test
# Independant variables must b continous
# what are continues data - 
# t-test: testing if continues values are different


# A one sample t test -----------------------------------------------------
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")


# Bonus ques --------------------------------------------------------------
# Bonus ques:
# Visualisation (Histogram)
ggplot(data = r_one, aes(x = dat)) +
  geom_histogram(aes(fill = sample), position = "dodge") +
  labs(x = "Data", y = "Count")


# Visualisation( density plot)
ggplot(data = r_one, aes(x = dat)) +
  geom_density(aes(fill = sample)) +
  labs(x = "Data", y = "Count")
# right skewness is when its laying more to the left(Density plots)

# Run the test
t.test(r_one$dat, mu = 20)

# Run a test we know will produce a significant result
t.test(r_one$dat, mu = 30)


# Pick a side -------------------------------------------------------------
# Are these data smaller/ less than the population mean
# Alternative- specific, only either looks at the smaller or bigger tail


# less than
t.test(r_one$dat, mu = 20, alternative = "less")
# or greater
t.test(r_one$dat, mu = 20, alternative = "greater") 

# But what about for the largest population mean?
# Are the samples less than a population of 30?
t.test(r_one$dat, mu = 30, alternative = "less")
# what about the greater than
t.test(r_one$dat, mu = 30, alternative = "greater")


# Two sample t test -------------------------------------------------------
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))



# Bonus : Visualisation (Histogram)
ggplot(data = r_two, aes(x = dat)) +
  geom_histogram(aes(fill = sample), position = "dodge") +
  labs(x = "Data", y = "Count")

# Run a default/basic test
# ~ what are the data that you are comparing
# check if the data is normally distributed and is the variancce diff
t.test(dat ~ sample, data = r_two, var.equal = TRUE)

# one sided two sample t test
# is A less than B
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
# or greater
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")

# RWS: SJog! Flippin nice one!
theme1 <- function(base_size = 10, base_family = "serif"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.title = element_text(size = 10),
          legend.key=element_rect(colour=NA, fill =NA),
          panel.grid = element_blank(),   
          panel.border = element_rect(fill = NA, colour = "black", size = 1),
          panel.background = element_rect(fill = "white", colour = "black"), 
          strip.background = element_rect(fill = NA)
    )
}



# Analysis ----------------------------------------------------------------



# Exercise(working with the ecklonia dataset)
ecklonia_2 <- read_csv("Day3/ecklonia_2.csv")%>% 
  gather(key = "variable", value = "value", -species, -site, -ID)


ggplot(data = ecklonia_2, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  theme1()
# H0: Frond length at Batsata Rock is not greater than at Boulders Beach.
# H1: Frond length at Batsata Rock is greater than at Boulders Beach
ecklonia_sub <- ecklonia_2 %>% 
  filter(variable == "frond_length")

frond_length <- ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Frond length (m)", x = "") +
  theme1()
frond_length  

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(frond_length_var = var(value)[1],
            frond_length_norm = as.numeric(value)[2])
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")
# The frond length (m) of the kelp Ecklonia maxima was found to be significantly 
# greater at Batsata Rock than at Boulders Beach (p = 1.93e-06, t = 5.9494, df = 24).

# Exercise 1

# There is a significant difference in the pH of the water upstream and downstream of the river  
pH <- read_csv("pH.csv")
graph1 <- ggplot(data = pH, aes(x = pH, fill = Site)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = Site), colour = NA, alpha = 0.4) +
  labs(x = "value") +
  theme1()
graph1
ggplotly(graph1)

pH %>% 
  group_by(Site) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(pH)[2]))


t.test(pH ~ Site, data = pH, var.equal = TRUE)
#Two Sample t-test

# data:  pH by Site
# t = -1.3981, df = 18, p-value = 0.1791
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -2.4120496  0.4845072
#sample estimates:
# mean in group down-stream   mean in group up-stream 
# 5.620889                  6.584660 

# Using a T-test, there is no significant difference 
# in pH at the different levels of the river
# T = -1.3981; df = 18;P = 0.1791

ggplot(data = pH, aes(x = Site, y = pH)) + 
  geom_boxplot(aes(fill = Site), outlier.colour = "red") + 
  labs(y = "pH") +
  theme1()



# library(plyr)
# pHStats <- ddply(pH, .(Site), summarize, 
#                  pH_mean = mean(pH, na.rm = TRUE),
#                  pH_sd = sd(pH, na.rm = TRUE))
# RWS: The use of the "plyr" package is discouraged 
# because it loads a lot of functions that change the way R works
# in unexpected ways that also interferes with other packages.
# This is why "dplyr" was created.
# It performs the same calculations but in a different way
# that plays more nicely with R more broadly.
# Here is how to perform the same task:
pHStats <- pH %>% 
  group_by(Site) %>% 
  summarise(pH_mean = mean(pH, na.rm = TRUE),
            pH_sd = sd(pH, na.rm = TRUE))

ggplot(data = pHStats, aes(x = Site, y = pH_mean)) +
  geom_bar(stat="identity", aes (fill = Site)) +
  geom_errorbar(aes(ymin=pH_mean-pH_sd, ymax=pH_mean+pH_sd), width=.1) +
  labs(x = "Site", y = "Average pH") +
  theme1()

