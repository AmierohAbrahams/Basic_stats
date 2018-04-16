# Day_2
# 13 April 2018
# Day 2
# Data visualisations and distributions

# Loading libraries -------------------------------------------------------
library(tidyverse)

# How to calculate mean/ median/ sd/var

# Manual Calculations -----------------------------------------------------
# Making dataframes

## The mean
#Random data: rnorm()- number of samples; mean;
r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50), sample = "A")

# Quick visualisation
ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

## The mean
# sum of all the values
# divide by
# number of all the pointd

r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))

# Brute force with baseR
r_dat$dat[(length(r_dat$dat)+1)/2]

# tidy way of life
r_dat %>% 
  summarise(r_median = median(dat))

r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)

# Variance
# The sum of
# Each value minus the mean
# Squared

# Divide by the count of the samples minus one
# mutate is to create a new column
r_dat %>%
  mutate(r_error = dat - mean(dat),
         r_error_square = r_error * r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/ (n()-1),
            r_var_func = var(dat))

# The standard deviation

r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))

## Exercise
# what does summary return when applied to chicken weights

summary(ChickWeight$weight)
chick <- ChickWeight
chick %>%
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))

ggplot(chick, aes(x = Time, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm")

# Visualisation -----------------------------------------------------------
# Load our libraries
# These few packages contain most functions necessary
# To make publication ready figures
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

# Qualitative -------------------------------------------------------------
# Stacked bar graph

# Create the count of qualitative data

iris_count <- iris %>%
  count(Species) %>% 
  mutate(prop = n/sum(n))

# Load SA time data
# Use tab in the " after read.csv
sa_time <- read.csv("SA_time.csv")

# Edit our time
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1))
         #geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                #rep("Joburg", 2)))

sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human)
# overwriting the raw data with a copy that has an extra column that we created

sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n/sum(n))

ggplot(data = sa_count, aes(x = "", y = n, fill= time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumalitive sum",
       x = NULL, y = "Count") +
  theme_minimal()


# Making a pie chart

ggplot(data = sa_count, aes(x = "", y = n, fill= time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  scale_fill_brewer() #### Using a different palette

# Histogram
ggplot(data = sa_long, aes(x = minutes)) +
  geom_histogram()

# Get rid of the 1 value
sa_clean <- sa_long %>% 
  filter(minutes < 300)

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

ggplot(sa_clean, aes(x = minutes, y = human, colour = time_type)) +
  geom_point() +
  geom_smooth()


# Realtive proportion Histogram
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Boxplot
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))

# Anatomy of a box plot
# Dot - max
# between 1st and 3rd quartile its the inter quartile range 
# the smaller the space the more central the data
# take interquartile range and find the tail by the up and down and multiplying it by 1.5
# Why are the tails not the same length? - 

# Notched boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)

# notch- symmetrical to top and bottom- the areas that overlap in the notch have no statistical difference
# because the notches ate overlapping
# now - now now and just now looks the same.  but 2 people have up to 10mins. 
# help file: notch- explains how width is calculated
# normally distributed data - use box plot rather than bar graph
# box plots can layer


# Calculate sumarry stats for plotting over the box plots

sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

# Plot these
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(date = sa_summary_stats,  size = 6, shape = 18,
             aes(y = time_type_mean, colour = "goldenrod"))


# Relationships -----------------------------------------------------------
# A basic scatter plot

sa_time_clean <- sa_time %>% 
  filter(just_now < 300)

ggplot(data = sa_time_clean, aes(y = now_now, x = just_now)) +
  geom_point()

# limit the axis
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0, 60), ylim = c(0,60))

# Areas and adding trend
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0,60))
# Gray shade is the standard error
# angle of line shows the relationship
# cape town- the more they think just now is the less they have now now
# JH and PE similar relationships
# George is just the same

################################## Additionl analysis############################

sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                 rep("Joburg", 2)))

stat_data <- sa_time %>%
  group_by(now_now, geo) %>%
  summarise()

ggplot(stat_data, aes(x = geo, y = now_now, fill = geo)) +
  geom_col(aes(fill = geo), position = "dodge", width = 0.10) +
  labs(x = "Location",y = "just now",title = "Relationship between location and just now")
# People in JHB take the least time 



# Additional analysis
# graphs'
# Correlations

ggplot(data = r_dat, aes(x = dat), fill= sample) +
  geom_histogram(aes(fill = sample), position = "dodge", binwidth = 100) +
  labs(title = "Histogram of  the data")


#kurtosis of the tails of the distribution
library(e1071)
stats_summary <- r_dat %>% 
  summarise(mean_dt = mean(dat),
            median_dt = median(dat),
            skew_dt = skewness(dat),
            kurt_dt = kurtosis(dat))

# Stats analysis
dt_summary <- r_dat %>% 
  summarise(dt_mean = mean(dat),
            dt_median = median(dat),
            dt_var = var(dat),
            dt_sd = sd(dat),
            dt_min = min(dat),
            dt_quart1 = quantile(dat, 0.25),
            dt_quart2 = quantile(dat, 0.50),
            dt_quart3 = quantile(dat, 0.75))
dt_summary

iris_graph <- iris
ggplot(data = iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot(aes(fill = Species))

ggplot(sa_clean, aes(x = minutes, colour = time_type)) +
  geom_bar(aes(fill = time_type), position = "dodge", binwidth = 100) 

#sa_clean
mean_sd <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(mn.min = mean(minutes),
            sd.min = sd(minutes))

ggplot(mean_sd, aes(x = time_type, y = mn.min)) +
  geom_col(aes(fill = time_type)) +
  geom_errorbar(aes(ymin = mn.min - sd.min, 
                    ymax = mn.min + sd.min))+
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Time type", y = "Average minutes") 


# Pearsons correlations
 cor.test(sa_time$now_now, sa_time$just_now, method = "pearson",
          conf.level = 0.95)
 
# Pearson's product-moment correlation

# data:  sa_time$now_now and sa_time$just_now
# t = -0.046516, df = 18, p-value = 0.9634
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.4512947  0.4336614
# sample estimates:
# cor 
# -0.01096334 
 