# Day_4
# Anova tests
# Intrested in calculatng the var.
# Have to be indpendant
# many t tests increase the errors
# t-test/ null hypothesis 
# compare mass of chicks at day21 between diet 1 and 2

# Loading packages --------------------------------------------------------
library(tidyverse)

# t-test ------------------------------------------------------------------

chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

t.test(weight ~ Diet, data = chicks_sub)
# Welch Two Sample t-test
# 
# data:  weight by Diet
# t = -1.2857, df = 15.325, p-value = 0.2176
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -98.09263  24.19263
# sample estimates:
#   mean in group 1 mean in group 2 
# 177.75          214.70

# We dont reject the null hypothesis
# testing is p value is greater than or less than 0.05


# Anova -------------------------------------------------------------------
# 1 factor has four levels
# Research question
# at day 21 is there a change in the mass of the chicks- as a result of having being fed
# by four different diets
# is there a difference in chicken mass attained
# after 21days of being fed 4different diets

# Null hypothesis: there is no difference in the chicken mass at 21days after being fed
# the four different diets


chicks <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks)

chicks_21 <- chicks %>% 
  filter(Time == 21)
chicks<- aov(weight ~ Diet, data = chicks_21)
summary(chicks)

# Box 

ggplot(data = chicks, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)


# they dont overlap
# if boxes overlap then mean of the boxes are similar if the means are significantly differet
# if the notches overlap then the mean of the
# notches are the indented lines

# Tukey HSD ---------------------------------------------------------------
tukey <- TukeyHSD(chicks) 
# discuss the output:
# comparing each of the diets
# diff- difference between the means
# null hyp: 2-1 there is no significant difference between the mean, p shows to reject or not reject the null hypothesis
#          diff        lwr       upr     p adj
# 2-1  36.95000  -32.11064 106.01064 0.4868095
# 3-1  92.55000   23.48936 161.61064 0.0046959
# 4-1  60.80556  -10.57710 132.18821 0.1192661
# 3-2  55.60000  -21.01591 132.21591 0.2263918
# 4-2  23.85556  -54.85981 102.57092 0.8486781
# 4-3 -31.74444 -110.45981  46.97092 0.7036249
# Test for sig difference between 3-1
# use function called geom_segment, give the co ordinates of the begining and ending of the segement

ggplot(data = chicks, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE) +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

# show lower and upper value as a bar using geom_seg
# x= categories
# y= yend: lower and upper

# Segment showing

chicks_tukey <- as.data.frame(TukeyHSD(aov(weight ~Diet, data = chicks_21))$Diet)
chicks_tukey$pairs <- as.factor(row.names(chicks_tukey))

ggplot(data = chicks_tukey, aes(x = pairs, y = diff)) +
  geom_segment(aes(x = lwr, xend = upr, y = pairs, yend = pairs)) +
  geom_vline(xintercept = 0,linetype="dotted", 
             color = "blue", size=1.5) 
# More samples the results are most likely to be accurate
# the variance = TRUE
# var.  false
# anova and tukey-why the shape matters
# group2 - wht does it differ so much compared to the others

# Multiple factor Anova ---------------------------------------------------
# Ho: there is no change in chicken mass(kg) from day 0 to dy 21

chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0, 2, 21))
ggplot(data = chicks_0_21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))
# Run an anova
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))
TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21)))
plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))

# What is the effort of time on rate and what is the effect of diet on rate
# look only at day 0 and 21 for both Time and Diet
# We can see that chicken got fatter
# but what is the relationship between diet and time

summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# or simply look at all of the time
# This is not the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
# Note the increase in the degrees of freedom for the time factor
# but no increase for the d.f diet
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

## lets look at tukey 
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))
plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

     
# Create the mean values by Time and Diet
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = TRUE))

# time and diet influence the final weight
# effect of diet is not consistant all the time
# when in the chicken life cycle is it looking at the effect

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_line(size = 2) +
  geom_point(shape = 15, size = 5)

# Non-parametric test -----------------------------------------------------
# what if we do not have normal data?

# for a t-test we rather will use Wilcox rank sum test
wilcox.test() # this is then filled with the same as for the t-test

# and now for the Kruskall-Wallis
# same as t-test but data is not normally distributed
kruskal.test(weight ~ Diet, data = chicks_0_21)
# summary(kruskal.test(weight ~ Diet, data = chicks_0_21)) - try    

# test: no significant difference
library(pgirmess)
kruskalmc(weight ~Diet, data = chicks_0_21)

# Exercise 1

feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# Making the dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

# Does feed type have an effect on the mass
# of pigs at the end of the experiment?
# Hypothesis: 
# h0: Feed type does not have an effect on thee mass of the pigs
# H1: Feed type does have an effect on the mass of the pigs
Bac <- as_tibble(bacon)
t.test(mass ~ feed, data = Bac) # Cannot work because has more than two levels
Bac_1<- aov(mass ~ feed, data = Bac)
Bac_1
summary(Bac_1)

ggplot(data = Bac_1, aes(x = feed, y = mass)) +
  geom_boxplot(aes(fill = feed), notch = TRUE)
# There is a signifcant difference
# The feed type does have an effect on the mass of the pigs

TukeyHSD(Bac_1)

# 95% family-wise confidence level
# 
# Fit: aov(formula = mass ~ feed, data = Bac)
# 
# $feed
# diff        lwr       upr     p adj
# Feed 2-Feed 1   8.68   3.347895 14.012105 0.0014725
# Feed 3-Feed 1  39.73  34.074449 45.385551 0.0000000
# Feed 4-Feed 1  25.62  20.287895 30.952105 0.0000000
# Feed 3-Feed 2  31.05  25.394449 36.705551 0.0000000
# Feed 4-Feed 2  16.94  11.607895 22.272105 0.0000009
# Feed 4-Feed 3 -14.11 -19.765551 -8.454449 0.0000168

# Exercise 2
teeth <- datasets::ToothGrowth
teeth.anov <- aov(len ~ supp, data = teeth)
summary(teeth.anov)

ggplot(data = teeth.anov, aes(x = supp, y = len)) +
  geom_boxplot(aes(fill = supp), notch = TRUE)
# ANSWER: The type of supplement  have no significant impact on tooth length.
# Ho: Tooth length is not affected by supplements
# H1: Tooth length is affected by supplements

