### Home Assignment Part III ZK HT21 ###

library(cAIC4) #first, make sure all of the following packages are installed and ready to use 
library(car)
library(carData)
library(dplyr) 
library(ggplot2)
library(gridExtra)
library(lm.beta)
library(lme4)
library(lmerTest)
library(lmtest)
library(MuMIn)
library(psych)
library(r2glmm)
library(tidyverse)

data_sample_3 = read.csv("https://tinyurl.com/b385chpu") #let's view the data sets
view(data_sample_3)

data_sample_4 = read.csv("https://tinyurl.com/4f8thztv")
view(data_sample_4)

summary(data_sample_3) #let's analyze. Right off the bat, I can see that there is a minimum household_income of -7884. 
summary(data_sample_4) #summary statistics for data_sample_4 looks good
str(data_sample_3) #let's me know that there is a 'hospital' variable
str(data_sample_4)

data_sample_3%>% #I can run a few variables to make sure nothing stands out. I notice that #25 is labeled as 'woman'. We will change this to 'female' later. 
  mutate(rownum=row.names(data_sample_3))%>%
  ggplot()+
  aes(x=sex, y=pain, label=rownum)+
  geom_label()

data_sample_3%>% #looks good
  mutate(rownum=row.names(data_sample_3))%>%
  ggplot()+
  aes(x=STAI_trait, y=pain, label=rownum)+
  geom_label()

data_sample_3%>% #household_income for #2 is a negative value. I believe this is a coding error and I will change it to a positive value. 
  mutate(rownum=row.names(data_sample_3))%>%
  ggplot()+
  aes(x=household_income, y=pain, label=rownum)+
  geom_label()

data_sample_3 <- data_sample_3 %>% #I change 'woman' to 'female' for #25. This time, I'll keep the object name as 'data_sample_3' to avoid confusion. 
  mutate(sex = replace(sex, sex == "woman","female"))

data_sample_3 <- data_sample_3 %>% #Change '-7884' to '7884' in #2. 
  mutate(household_income = replace(household_income, household_income == "-7884", "7884")) 

data_sample_3 %>% #let's visualize. Scatterplot is clustered vertically. 'Hospital 6' seems to have the highest pain level. 
  ggplot()+
  aes(y=pain, x=hospital)+
  geom_point(aes(color=hospital), size=5)+
  geom_smooth(method="lm", se=F)

mod_rnd_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva + (1|hospital), data = data_sample_3) #First, let's check all variables as the fixed effect predictors. 
mod_rnd_int #Let's compare with model_2. 'Age' has a .115 decrease in pain for mod_rnd_int vs. only a .02 decrease in model_2. 
#Mod_rnd_int has a slight increase in pain for 'sex' variable. 
#Mod_rnd_int has an almost 1-pt increase in pain fpr STAI_trait
#Mod_rnd_int has a slight increase in pain compared to model_2 for pain_cat 
#Mod_rnd_int has a higher pain decrease for model_2 in pain_cat
#Mod_rnd_int has a higher pain level in cortisol_serum
#Mod_rnd_int has a higher pain level in cortisol_saliva

summary(mod_rnd_int) #get summary statistics

mod_fixed = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_3)
mod_fixed #Now, let's take a look at fixed effect (same as simple regression, but we'll save it as mod_fixed)

summary(mod_fixed) #look at summary statistics 

sum(residuals(mod_rnd_int)^2) #Let's check RSS to see how much error is left.
sum(residuals(mod_fixed)^2) #tells me that mod_rnd_int has less error because RSS is smaller, which means less error. 

cAIC(mod_rnd_int)$caic #Let's compare using AIC/cAIC
AIC(mod_fixed) #Again, mod_rnd_int is smaller, so it is the better model. 

anova(mod_rnd_int, mod_fixed) #Let's compare using ANOVA. mod_rnd_int has a signifiant P-value(.001)

r2beta(mod_fixed, method = "nsj", data = data_sample_3) #I run a marginal R^2 for mod_fixed
#We can see that the upper and lower confidence levels do not include 0, so there is a significant effect. 
#The variance can still be explained under R^2. 

r.squaredGLMM(mod_fixed) #I get a revised statistic for marginal and conditional R^2 for mod_fixed
r.squaredGLMM(mod_rnd_int) #mod_fixed explains 40.0% of variance of pain while mod_rnd_int explains 38.3%. 

confint(mod_fixed) #Let's check confidence intervals. 
confint(mod_rnd_int) #mod_fixed has a slightly smaller CI, which makes it more reliable. 

coefTable(mod_rnd_int)
view(coefTable(mod_rnd_int))

data_sample_4%>% #Now let's run the variables in data_sample_4 to make sure nothing stands out.
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=sex, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=age, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=STAI_trait, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=pain_cat, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=cortisol_serum, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=cortisol_saliva, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=mindfulness, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=weight, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=IQ, y=pain, label=rownum)+
  geom_label()

data_sample_4%>% 
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=household_income, y=pain, label=rownum)+
  geom_label()

data_sample_4%>%
  mutate(rownum=row.names(data_sample_4))%>%
  ggplot()+
  aes(x=hospital, y=pain, label=rownum)+
  geom_label()

###Regression Equation mod_rnd_int###
Y=0.42+(-.06)*age+0.21*sex+(-0.03)*STAI_trait+0.09*pain_cat+(-0.21)*mindfulness+0.42*cortisol_serum+0.12*cortisol_saliva
#Where Y=pain(prediction value), b0=intercept(-.06), b1=0.21, x1=age, b2=-0.03, x2=STAI_trait, b3=0.09, x3=pain_cat, b4=-0.21, x4=mindfulness, b5=0.42, x5=cortisol_serum, b6=0.12, x6=cortisol_saliva

pain = c(2,4,6,9,10) #predictions for data_sample_4
pain
pain_df = 'as.tibble(pain)'
pain_df

predictions = predict(mod_fixed, data = pain_df) #fitting our predictions in

pain_df_with_predicted=cbind(pain_df, predictions)
pain_df_with_predicted

RSS = sum((data_sample_4$pain - predict(mod_fixed))^2) #Run RSS
RSS #this gives you how much error

mod_mean <- lm(pain ~ 1, data = data_sample_4) #find the mean
mod_mean #we insert the value in TSS

TSS = sum((data_sample_4$pain - predict(mod_mean))^2) #Find the Total Sum of Squares 
TSS #insert the numbers into the 1-(RSS/TSS) formula to find out how much variability we can explain compared to the population mean. 

Rsq=1-(644.47/495.5)
Rsq #We can explain 30% variability of the data compared to the population mean. 
#compared to the marginal and conditional R^2 of mod_fixed and mod_rnd_int, mod_fixed explains the most variance at 40% while mod_rnt_int explains 39%. 

new_rnd_int_mod = lmer (pain ~ cortisol_serum + sex + (1|hospital), data = data_sample_3) #My most influential predictors are cortisol_serum and sex
new_rnd_int_mod #this is my random intercept model

rnd_slope_mod_opt = lmer(pain~cortisol_serum+sex+(cortisol_serum|hospital)) #Next, we make our random slope model
                         control = lmerControl(check.conv.singular = .makeCC(action = "ignore", tol = 1e-4)) #I kept getting an error message, so I used this code I found online to ignore it in order to obtain my model. 
rnd_slope_mod_opt

summary(rnd_slope_mod)
confint(rnd_slope_mod)
r.squaredGLMM(rnd_slope_mod)
cAIC(rnd_slope_mod)
coefTable(rnd_slope_mod)
view(coefTable(rnd_slope_mod))

data_sample_3 = data_sample_3%>% #Time to visualize the two mixed models. Saves the predictions of the models into a variable. 
  mutate(pred_int=predict(new_rnd_int_mod),pred_slope=predict(rnd_slope_mod_opt))

data_sample_3%>% #Random Intercept Model Plot. Allows us to see each hospital separately with a regression line and a comparison of male and female pain levels.
  ggplot()+
  aes(y=pain, x=cortisol_serum, group=hospital)+
  geom_point(aes(color=sex))+
  geom_line(color="black", aes(y=pred_int, x=cortisol_serum))+
  facet_wrap(~hospital, ncol=2)

data_sample_3%>% #Random Slope Model Plot. *Very* slight difference in comparison, but we know that random slope model is always better. 
  ggplot()+
  aes(y=pain, x=cortisol_serum, group=hospital)+
  geom_point(aes(color=sex, size=1))+
  geom_line(color="black", aes(y=pred_slope, x=cortisol_serum))+
  facet_wrap(~hospital, ncol=2)
