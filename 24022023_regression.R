#_____________----
#PACKAGES ----
library(tidyverse)
library(rstatix)
library(performance)
library(here)
library(janitor)

#_____________----
#ACTIVITY 1: Exploratory data analysis ----
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

#Regression model ----
janka_ls1 <- lm(hardness ~ dens, data = janka) 
janka_ls1

#linear model plot
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")
# specify linear model method for line fitting
#blue linear line represents regression line
#shaded area is 95% CI band

#CI band width difference ----
#The 95% confidence interval band is narrowest in the middle 
#and widest at either end of the regression line. 
#When performing a linear regression,
#there are two types of uncertainty in the prediction.

#First is the prediction of the overall mean of the estimate 
#(ie the center of the fit). 
#The second is the uncertainly in the estimate calculating the slope.

#So when you combine both uncertainties of the prediction 
#there is a spread between the high and low estimates. 
#The further away from the center of the data you get (in either direction), 
#the uncertainty of the slope becomes a large and more noticeable factor, 
#thus the limits widen.

#Linear model summary ---- 
#tidyverse
janka_ls1 %>% 
  broom::tidy()
#base r
summary(janka_ls1)

#__________________----
#ACTIVITY 3: Mean centered regression ----

#scale(janka$dens, scale = F)

#centre dens values and fit a new linear model
dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

#Confidence Intervals ----
#base r
confint(janka_ls1)

#tidyverse
broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)

#Effect size ----
janka_ls1 %>% 
  broom::glance()

#Assumptions ----
#data point residuals
janka_ls1 %>% 
  broom::augment() %>% 
  head()
#plot with residuals
augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")

# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")


library(patchwork)
p1+p2+p3

#less repetitive plot
model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

#Normal distribution ----
#base r
plot(janka_ls1, which=c(2,2))

#tidyverse
performance::check_model(janka_ls1, check=c("normality","qq"))

#Equal variance ----
#base r
plot(janka_ls1, which=c(1,3))

#tidyverse
performance::check_model(janka_ls1, check="homogeneity")

#Outliers ----
performance::check_model(janka_ls1, check="outliers")

#Predictions ----
#Using the coefficients of the intercept and the slope,
#we can make predictions on new data
coef(janka_ls1)

#prediction for new wood sample with a density of 65
coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65
#(Intercept) 
#2577.434 

#Adding confidence intervals ----
#standard error
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)

#95% CI
broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

#________________----
#ACTIVITY 4- Predictions
pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))