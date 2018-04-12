# Day_1.R 
# Purpose to practice some of the concepts that we will encounter
# 12 April 2018


# Loading packages --------------------------------------------------------
library(tidyverse)


# Integers ----------------------------------------------------------------
# generate integers
# seq - create a sequence of number that starts at 5 up to 22 and the difference between #is 1

integer_r <- as.integer(seq(5, 14, by = 1))
integer_r
summary(integer_r) #Gives a summary of the data

# Continuous --------------------------------------------------------------
# length.out - length is a function of itself

numeric_r <- seq(23, 43, length.out = 10)

# Dates -------------------------------------------------------------------
# dates between steps
# an example of an arithmetic function
# what is the difference between 12 december 2005

as.Date("2005-12-31") - as.Date("2005-12-12")
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")

# dates integers and numbers can be combined into a data frame if they are of the same length

# Create a dataframe
df_r <- data.frame(integers = integer_r,
                  numeric = numeric_r,
                  dates  = dates_r)

df_r <- as_tibble(df_r) # because i like tibbles
summary(df_r)

# Categories --------------------------------------------------------------
# Electronics
elect_r <- as.factor(c("laptops","desktops", "cell phones"))

# People
people_r <- as.factor(c("funny hair", "beautiful", "beanies"))

# colours
colour_r <- as.factor(c("red","blue"))

# factor variables used to create buckets in order to place certain number of variables
# creates a factor


# Ordinal data ------------------------------------------------------------
# Ranks
# Still have qualitative data but ranking the data
# but with some sort of order
# ordering the colours and the levels inside the order provides the info
# order of preference
colour_qual <- ordered(c("blue", "green", "yellow", "orange", "red"),
                          levels = c("blue", "green","yellow", "orange", "red"))

# Binary data -------------------------------------------------------------
# take on one or two binary measurements
# True and False, dead or alive, black or white.
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
Summary(binary_r)


# Characters --------------------------------------------------------------
# bunch of characters
# can apply various stats functions on the data
# create a vector of characters by using "
# May have multi words
sites_r <- c("Yztervarkpunt", "Betty's Bay", "Gansbaai", " Sea Point")
summary(sites_r)


# Missing values ----------------------------------------------------------
# how to record  a missing value by using NA
# zero is a value and not a "missing value"
chicks_nest <- c(3, 2, 10, 0, 5, 6, 8, 2, 4, NA)
summary(chicks_nest)
 
# The mean
mean(chicks_nest)
# The standard deviation
sd(chicks_nest)
# Rob missing value paper
# Complex numbers ---------------------------------------------------------

# Loading new data --------------------------------------------------------
# Looking at data
ChickWeight
summary(ChickWeight)

# bringing chicks into environment
chick <- ChickWeight

# head of the first 7 rows
head(chick, 7)

# Way to look at the data- shows first the rows and then the columns
ChickWeight[c(1, 54, 61, 12),]

# Descriptive stats -------------------------------------------------------
# Create a dataframe
# %>% - allows to set in a logical order

chicks <- as_tibble(ChickWeight)

# n to get count
chicks %>%
  summarise(chicken_count = n())

# or
nrow(chicks)

# measures central tendency -----------------------------------------------
# calculating the mean weight

# calculating the mean of the weight
chicks %>% 
  summarise(mean_wt = mean(weight)) 
# being more specific on what we are working in

  chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight))

# If we have a lot of outliers calculate the median as also central tendency
  
  chicks %>% 
    filter(Time == 21) %>% 
    group_by(Diet) %>% 
    summarise(mean_wt = mean(weight),
              median_wt = median(weight))
# visualise the density of the data

ggplot(data = filter(central_chicks, Time == 21), aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4) 
# Skewness ----------------------------------------------------------------
# Data may be smaller or not
# right skewness is when its laying more to the left(Density plots)
library(e1071)

# Calculate the number
# compare the difference in mean and median against skewness

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight))
# 2 and 3 are negativley skewed
# mean is where the tip is
# left skewed

# Kurtosis ----------------------------------------------------------------
# fat tail- bell curve is squashed: look like outliers but the data isnt distributed centrally
# Calculate the kurtosis of the tails of the distribution
kurt <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight),
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurt_wt = kurtosis(weight))
# kurtosis has no tails thus it does not show on the graph


exp_r <- data.frame(dat = rexp(n = 500),
                    sample = "A")

ggplot(data = exp_r)



# Measure of variability --------------------------------------------------
# variance firstly finds the mean
# find mean chickweight then take that value minus mean take that value and square it and divide by the no. of observation
# variance not used as much but rather sd, sd is the squareroot of variance
# variance is weight squared
# sd is just the weight
# proposal- show mean +- standard deviation
# variance is the actual measurement

# Bellow is a summary of many different statistical properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.50),
            wt_quart3 = quantile(weight, 0.75))
wt_summary







