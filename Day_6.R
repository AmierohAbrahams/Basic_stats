# Day 6
# Confidence Intervals
# 26 April 2018


# Confidence interval is the range of estimates of the mean
# that cover and represent 95% of the mean is within the range and 5% will lie outside


# Load library ------------------------------------------------------------

library(tidyverse)
install.packages("rcompanion")
library(rcompanion)

Input <- ("
          Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")

data <- read.table(textConnection(Input),header = TRUE)
summary(data)
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

# One-way data
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# Two-way data
groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

# Plot graph showing mean and confidence intervals
library(rcompanion)
dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

# Density plot ------------------------------------------------------------

# The difference in ratings and the sex of the teacher

ggplot(data = data, aes(x = Rating)) +
  geom_density(aes(fill = Sex)) +
  labs(x = "Data", y = "Count")




# Create the graph --------------------------------------------------------
library(ggplot2)

ggplot(data = dat1, aes(y = Mean, x = Sex)) +
  geom_point(aes(colour = Teacher))+
  geom_errorbar(aes(ymin = Mean - Trad.lower, 
                    ymax = Mean + Trad.upper,
                    colour = Teacher))+
  facet_wrap(~Teacher)

# Bootstrapping
groupwiseMean(Steps ~ Sex,
              data = data,
              conf = 0.95,
              digits = 3,
              R = 10000,
              boot = TRUE,
              traditional = FALSE,
              normal = FALSE,
              basic = FALSE,
              percentile = FALSE,
              bca = TRUE) 
# Histogram, visualising the data

ggplot(data = data, aes(x = Rating)) +
  geom_histogram(aes(fill = Teacher), position = "dodge") +
  labs(x = "Ratings", y = "Count")

ggplot(data = data, aes(x = Teacher, y = Rating)) +
  geom_boxplot(aes(fill = Sex), notch = TRUE)


# Chickweight -------------------------------------------------------------

library(tidyverse)
chicks <- as_tibble(ChickWeight)
shapiro.test(chicks$weight)

chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))

# If the p-value is < 0.05 then data is considered non-Normal
# If the p-value is > 0.05 then data is considered Normal
# differ from normal do the KWallace test
library(tidyverse)
# Log Transform 
log_data <- data %>%
  mutate(log10 = log10(Steps)) %>% 
  mutate(log = log(Steps)) %>%
  mutate(cuberoot = (Steps)) %>% 
  mutate(sqrt = sqrt(Steps)) 

# Plot histogram of all of the columns ------------------------------------

hist1 <- ggplot(data = log_data, aes(x = log10, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist2 <- ggplot(data = log_data, aes(x = log, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist3 <- ggplot(data = log_data, aes(x = cuberoot, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")

hist4 <- ggplot(data = log_data, aes(x = sqrt, fill = Teacher))+
  geom_histogram(aes(fill = Teacher), position = "dodge")
library(ggpubr)
final_hist <- ggarrange(hist1, hist2, hist3, hist4)
final_hist

# Smits way
dat2 <- data %>% 
  mutate(log10 = log10(Steps)) %>% 
  mutate(log = log(Steps)) %>%
  mutate(cuberoot = (Steps)) %>% 
  mutate(sqrt = sqrt(Steps)) %>%
  select(-Student, -Rating) %>% 
  gather(key = "dat.type", value = "trans.data", -Sex, - Teacher)


ggplot(data = dat2, aes( x = trans.dat)) +
  geom_histogram(binwidth = 1000, aes(fill = Sex), position = "dodge") +
  facet_grid(Sex ~ Teachers)


iris <- iris
iris.aov <- aov(data= iris, Petal.Length~Species)
summary(iris.aov)


# Visualising the data ----------------------------------------------------
#Visualising the iris data


library(gridExtra)
library(grid)
library(plyr)
# histogram
graph1 <- ggplot(data=iris, aes(x= Petal.Length))+
  geom_histogram(binwidth=0.2, color="black", aes(fill= Species)) + 
  xlab("Petal Length (cm)") +  
  ylab("Frequency") + 
  ggtitle("Histogram of Petal Length")+
  geom_vline(data=iris, aes(xintercept = mean(Petal.Length)),linetype = "dashed",color= "salmon")

graph1

# H0:There is no significant difference in petal width between the three species.
# H1:There is a difference in petal width between the three species.

shapiro.test(iris$Petal.Width)
# If the data is not normal then the p value is less than 0.05
# States if there is or isnt a significant difference

iris.dat <- as.tibble(iris)

iris %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2]))


# we find that some of the species have non-normal data -------------------

# do a Kruskal-Wallis test in stead o an ANOVA

kruskal.test(Petal.Width ~ Species, data = iris)




