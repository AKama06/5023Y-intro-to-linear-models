#_____________----
#PACKAGES ----
library(tidyverse)
library(rstatix)
library(performance)
library(here)
library(janitor)

#_____________----
#ACTIVITY 1 ----
#import data
janka <- read_csv(here("data", "janka.csv"))

#clean data
head(janka)
janka <- janitor::clean_names(janka)
glimpse(janka)
summary(is.na(janka))#check for any NA values

#plot
janka %>%
  ggplot(aes(x=dens,
       y=hardness))+
  geom_point()

#pearson's r
janka_correlation <-
  cor.test(x= janka$dens, y= janka$hardness, method = "pearson")
janka_correlation

#person's r using rstatix package
#base r
with(janka, cor(dens, hardness))
#tidyverse
janka %>% 
  cor_test(dens, hardness)

#regression model ----
janka_ls1 <- lm(hardness ~ dens, data = janka) 
janka_ls1

#linear model plot
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")
# specify linear model method for line fitting
