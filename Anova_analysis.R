library(tidyverse)

# Anova exercise
# Additional analysis

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
# We reject the null hypothesis

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

# H0: There is no difference between the length of the tooth and the supplents taken
# H1: There is a difference between the length of the tooth and the supplents taken
teeth <- datasets::ToothGrowth
teeth.anov <- aov(len ~ supp, data = teeth)
summary(teeth.anov)

ggplot(data = teeth.anov, aes(x = supp, y = len)) +
  geom_boxplot(aes(fill = supp), notch = TRUE)
# ANSWER: The type of supplement  have no significant impact on tooth length.
# We accept the null hypothesis as p>0.05

# Own Dataset 

# H0: There is no difference between pH at the different sites on the same day
# H1: There is a difference between pH at the different sites on the same day
pH_day.aov <- aov(pH ~ Day + Site, data = pH_day)
summary(pH_day.aov)

pH_day.res <- residuals(pH_day.aov)
hist(pH_day.res, col = "red")

plot(fitted(pH_day.aov), residuals(pH_day.aov), col = "red")

# More than 0.05 so there is no significant difference in the pH at the different sites at the same day
# We accept the null hypothesis

pH_day.summary2 <- Rmisc::summarySE(data = pH_day,
                                    measurevar = "pH",
                                    groupvars = c("Day"))


#Built in datset for R

# H0: There is no difference between species and petal length
# H1: There is a difference between species and petal length
Iris <- iris
library(mosaic)
favstats(data=Iris, Petal.Length ~ Species)
Iris.ano <- aov( data=Iris, Petal.Length ~ Species )
summary(Iris.ano)

TukeyHSD(Iris.ano, conf.level = 0.95)

# THere is a difference thua reject null hypoth