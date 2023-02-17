#________________----
#PACKAGES ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

#__________________----
#VISUALISATION----
lsmodel0 <- lm(formula = height ~ 1, data = darwin)
summary(lsmodel0)

#mean of height
mean(darwin$height)
#COMPARING MEAN====
lsmodel1 <- lm(height ~ type, data=darwin)
#summarizes information about model components
broom::tidy(lsmodel1)

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))
