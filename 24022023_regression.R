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
janka %>%
  ggplot(aes(x=dens,
       y=hardness))+
  geom_point()
