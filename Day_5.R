# An0va exercise
# Some more exercise
# Loading Library ---------------------------------------------------------

library(tidyverse)
library(Rmisc)

# Load the data -----------------------------------------------------------
snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))


# Manipulate the data -----------------------------------------------------
snakes$day = as.factor(snakes$day)

# Summarise the data ------------------------------------------------------
snakes_summary <- snakes %>%
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings))

# Formulate the hypothesis ------------------------------------------------
# Ho- precise expcttion, stating no difference
# H1 - if show a difference 


# hypothesis --------------------------------------------------------------

# There is NO difference in the number of openings in day to day
# There is a difference in number of openings in day to day

# Probability -  there is a chance that they may not be , 95% cetaintly that the null-hypothesis is true

# Testing an hypothesis ---------------------------------------------------
# Visualising the data
# Creating SE and Ci

snakes.summary2 <- summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)
# dots not clumped, evenly spread, data isnt normal in terms of kurtosis
# where is there a significant difference, is there a 95% probability that they are real 
# or is it just by chance
# because small sample could be because of chance.


# H0: There is no difference between snakes with respect to the number of openings at which they habituate.
# H0: There is no difference between days in terms of the number of openings at which the snakes habituate.
snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)
# less 0.05- there is a significnt difference, so we reject null hypoth
# sum of squares- varience
# still large amount of variation to reduce residual sum of squars than can do tukey
# 
TukeyHSD()


snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)

# analysis of varience of openings and closing as a function of day and snake using
# the data snakes
# Testing assumptions -----------------------------------------------------
snakes.res <- residuals(snakes.aov)
hist(snakes.res)
plot(fitted(snakes.aov), residuals(snakes.aov))
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)

# Exercise

# fitted values - something that we can predict
# plot predicted values relative to the error values'
# if data was heteroscedastic, the number of dots will be more orless even and the patter
# will be consistant, not clumped up
library(ggpubr)
# moth data backgroup

moths <- read_csv("moth_traps.csv") %>% 
  gather(key = "trap", value = "count", - Location)

# Summarise the data ------------------------------------------------------

moth_loc_summary <- moths %>%
  group_by(Location) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

moth_trap_summary <- moths %>%
  group_by(trap) %>% 
  summarise(moth_mean = mean(count),
            moth_sd = sd(count))

# Formulate the hypotheses --------------------------------------------------

# HO: There is no difference in the count depending of different locations
# HO: There is no difference in count depending on the different trap types

#  Calculate SE & CI ------------------------------------------------------

moth_loc_summary_2 <- summarySE(data = moths,
                                measurevar = "count",
                                groupvars = c("Location"))

moth_trap_summary_2 <- summarySE(data = moths,
                                 measurevar = "count",
                                 groupvars = c("trap"))

# Visualise the data ------------------------------------------------------

Location <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_segment(data = moth_loc_summary_2, aes(x = Location, xend = Location, y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Trap <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_segment(data = moth_trap_summary_2, aes(x = trap, xend = trap, y = count - ci, yend = count + ci, colour = trap),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = trap), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

Final <- ggarrange(Location, Trap,
                   ncol = 2, nrow = 1,
                   labels = c("Location", "Trap"),
                   common.legend = TRUE)
Final
# Test the hypothesis -----------------------------------------------------

moth.loc.aov <- aov(count ~ Location, data = moths)
summary(moth.loc.aov)

moth.trap.aov <- aov(count ~ trap, data = moths)
summary(moth.trap.aov)

# Test both hypotheses

moths.all.aov <- aov(count ~ Location + trap, data = moths)
summary(moths.all.aov)

# Testing assumptions 

# First visualise normality of data

moths.residuals <- residuals(moths.all.aov)
hist(moths.residuals)

# Visualise homoscedasticity

plot(fitted(moths.all.aov), residuals(moths.all.aov))

# Apply tukey test 

moths.loc.tukey <- TukeyHSD(moths.all.aov, which = "Location")
plot(moths.loc.tukey)

moths.trap.tukey <- TukeyHSD(moths.all.aov, which = "trap")
plot(moths.trap.tukey)

# Linear regression -------------------------------------------------------
# Scatter plot on x axis and y axis
# residual- diffrence from observed value and pridicted value
# what is the linear regression:find out the relationship/ slope
# y =mx+c
# linear regression minimise the sum of squares
# For the explanation of this statistical anakysis
# We are going to use eruption data from 01' faithful

# Loading the data --------------------------------------------------------
faithful <- as_tibble(faithful)

head(faithful)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")
# hypoth: there is a significant linear relationship between waiting time and eruptions
# null: waiting time does not influence the duration of an eruption
# altern:waiting time does influence 
# Ho: waiting time does not influence the duration of erption
# h1: waiting time does influence the duration of erption

# Test the hypothesis -----------------------------------------------------

faithful_lm <- lm(eruptions ~ waiting, data = faithful)
summary(faithful_lm)

# testing if the estimate(y -intercep) is significantly different from 0. the estimate is present
# in the regression table
# 
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.874016   0.160143  -11.70   <2e-16 ***
#   waiting      0.075628   0.002219   34.09   <2e-16 ***

# No signifcant difference in y intercept fronm o
# alternative is accepted i this case

slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")
# Correlation -------------------------------------------------------------
# Loading libraries -------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(corrplot)

# Loading the data --------------------------------------------------------

ecklonia <- read_csv("ecklonia_2.csv")
# Formulate an hypothesis -------------------------------------------------

# Ho: there is no relationship between stipe mass and frond mass
# for the kelp Ecklonia maxima
# Ho: there is a relationship between stipe mass and frond mass
# for kelp ecklonia maxima
# Test a hypothesis -------------------------------------------------------

cor.test(x = ecklonia$stipe_mass, ecklonia$frond_mass)
# Pearson's product-moment correlation
# 
# data:  ecklonia$stipe_mass and ecklonia$frond_mass
# t = 2.6225, df = 24, p-value = 0.01492
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.1035249 0.7264923
# sample estimates:
# cor 
# 0.4719513 
#significants- is bellow 0.05

# Visualising the data ----------------------------------------------------

ggplot(data = ecklonia, aes(x = stipe_mass, y = frond_mass))+
  geom_point() +
  geom_smooth(method = "lm", SE = TRUE)

ggplot(data = ecklonia, aes(x = stipe_mass)) +
  geom_density(aes(fill = site)) +
  labs(x = "Data", y = "count")


# Run hecka test ----------------------------------------------------------

ecklonia_sub <- ecklonia %>% 
  select(stipe_length:stipe_mass)

ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor

ggplot(data = ecklonia, aes(x = stipe_mass, y = frond_mass)) +
  geom_boxplot(aes(fill = site), notch = TRUE)
try1<- aov(stipe_mass ~ frond_mass, data = ecklonia)
try1
summary(try1)

# Spearman rank test ------------------------------------------------------
# uses ordinal data
# making ordinal values
# 
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), 3))
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "Spearman")
cor.test(ecklonia$length, ecklonia$digits)

# Kendall experiment ------------------------------------------------------

cor.test(ecklonia$primary_blade_width, ecklonia$primary_blade_width, method = "kendall")

# normal spearman
# not normal kendall
# Visualising all things --------------------------------------------------
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson
corrplot(ecklonia_pearson, method = "circle")
# Diagnol blue line- on iether side of the line the data are the same
# what is it showing? 
# all of the relaionships are iether positive or negative
# negative corealtion- dot will be red(one variable increaseand another decrease)

# Using pearson
library(plotly)
try1 <- plot_ly(z = ecklonia_pearson, type = "heatmap", col = brewer.pal(9, "Blues"),
                        x = c("D", "FM", "FL","PBL","SM", "PBW"),
                        y = c("PBW", "SM", "PBL", "FL", "FM", "D"))
try1

library("RColorBrewer")
my_palette <- colorRampPalette(c("Red", "yellow","green"))(n = 226)
ecklonia_matrix <- data.matrix(ecklonia)
ecklonia_heatmap <- heatmap(ecklonia_matrix, Rowv=NA, Colv=NA, col = my_palette, scale="column", margins=c(5,10))


# This example is already ina data format and thus can be used
# Ecklonia heatmap using the pearson data
ecklonia_heatmap <- heatmap(ecklonia_pearson,Rowv=NA, Colv=NA, col = my_palette, scale="column", margins=c(5,10), main = "Correlation Matrix")

