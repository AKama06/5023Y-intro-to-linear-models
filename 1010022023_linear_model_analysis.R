#________________----
#PACKAGES ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

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

summary(lsmodel1)

#plot of model ---- 
darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()
#confidence interval
confint(lsmodel1)

#broom.helpers package installed and added to library
#install.packages("broom.helpers")

#GGally Package ----
#graph of the estimated mean difference with an approx 95% CI
GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

#99%CI added to lsmodel1 model summary
broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

#calculating other treatment mean and SE
darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()
#after relevelling, the self treatment is now taken as the intercept
#we get the estimate for it's mean and standard error

#emmeans package ----
#provides the mean, standard error and 95% confidence interval estimates of all levels from the model at once 
means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means
#data visuals that combine raw data and statistical inferences
means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

#check assumption of linear model
performance::check_model(lsmodel1)

#normal distribution ----
#check if model follows a normal distribution visually
performance::check_model(lsmodel1, check=c("normality","qq"))

#quantile-quantile plot
#checks whether a sample distribution is the same as theoretical distribution
plot(lsmodel1, which=c(2,2))

#equal variance ----
performance::check_model(lsmodel1, check="homogeneity")
plot(lsmodel1, which=c(1,3))

#outliers ----
performance::check_model(lsmodel1, check="outliers")
plot(lsmodel1, which=c(4,4))

#visual t-test
darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)