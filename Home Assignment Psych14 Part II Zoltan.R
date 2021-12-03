### Home Assignment Part II ZK HT21 ###

library(car) #first, make sure all of the following packages are installed and ready to use 
library(carData)
library(dplyr) 
library(ggplot2)
library(gridExtra)
library(lm.beta)
library(lmtest)
library(psych)
library(sandwich)
library(tidyverse)

###The following is a custom code copy/pasted from class to run the error_plotter()###
error_plotter <- function(mod, col = "black", x_var = NULL) {
  mod_vars = as.character(mod$call[2])
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern = "~",
                                              mod_vars)) - 2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern = "~", mod_vars)) +
               2, nchar(mod_vars))
  data$pred = predict(mod)
  if (x == "1" & is.null(x_var)) {
    x = "response_ID"
    data$response_ID = 1:nrow(data)
  } else if (x == "1") {
    x = x_var
  }
  plot(data[, y] ~ data[, x], ylab = y, xlab = x)
  abline(mod)
  for (i in 1:nrow(data)) {
    clip(min(data[, x]), max(data[, x]), min(data[i, c(y,
                                                       "pred")]), max(data[i, c(y, "pred")]))
    abline(v = data[i, x], lty = 2, col = col)
  }
}
###custom code ends here###

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") #then, upload the original data set

View(data_sample_1) #view data.
data_sample_1%>% #look at general info. 
  summary()

str(data_sample_1) #further analyze the data
describe(data_sample_1)

updated_data_sample_1 <- data_sample_1[-c(34, 88), ] #I already know #34 and #88 are coding errors, we will again remove them

View(updated_data_sample_1) #view the data set in a script
describe(updated_data_sample_1) #get descriptive data
summary(updated_data_sample_1) #get summary statistics

updated_data_sample_1%>% #Visualize variable, sex 
  ggplot()+ 
  aes(x=sex, y=pain)+
  geom_point()+
  geom_smooth(method="lm", se=F)

updated_data_sample_1%>% #Visualize variable, age (it's nice to see the contrast in sex as well)
  ggplot()+ 
  aes(x=age, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

updated_data_sample_1%>% #Visualize the variable, STAI_trait. 
  ggplot()+ 
  aes(x=STAI_trait, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

updated_data_sample_1%>% #Visualize the variable, pain_cat. 
  ggplot()+
  aes(x=pain_cat, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

updated_data_sample_1%>% #Visualize the variable, mindfulness. 
  ggplot()+
  aes(x=mindfulness, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

updated_data_sample_1%>% #Visualize the variable, cortisol_serum.
  ggplot()+
  aes(x=cortisol_serum, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

updated_data_sample_1%>% #Visualize the variable, weight. The regression line for males and females have different slopes. 
  ggplot()+
  aes(x=weight, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

updated_data_sample_1%>% #Visualize the variable, IQ. 
  ggplot()+ 
  aes(x=IQ, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

updated_data_sample_1%>% #Visualize the variable, household_income. 
  ggplot()+ 
  aes(x=household_income, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

no_saliva <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = updated_data_sample_1)
no_saliva #I notice new variable, weight, has a negative effect on pain

no_saliva%>% plot(which=4) #I run Cook's Distance for any possible outliers. Here, I find #'s, 47, 85, and 86. 
#They don't deviate too much from the other observations. It is not good to remove too many cases so for that reason, they will remain in my data set. 

no_saliva%>% plot(which=2) #To check for violation of normality assumption, I run argument 2. #'s 85, 86, and 104 don't seem to deviate too much. 
describe(residuals(no_saliva)) #Let's check skewness and kurtosis to make sure. They fall between -1 and 1 for all so normality assumption is not violated. 

no_saliva%>% residualPlots() #I check for linearity. Plots show slight curvature. Not enough for me think that the linearity assumption is violated.
#T-test tell me that there isn't a strong significance either. 

no_saliva%>% plot(which=3) #I run argument 3 to check for homoscedasticity. Data appear to be evenly distributed. 
no_saliva%>% ncvTest() #I run a ncv test to make sure. P-value is greater than .005 (p=.87). 
no_saliva_sandwich_test=coeftest(no_saliva, vcov=vcovHC, type="HC") #I use robust estimators to solve the homoscedasticity assumption. 
no_saliva_sandwich_test #Huber-White Sandwich Estimator gives us a list of std. errors
no_saliva_sandwich_test_se=unclass(no_saliva_sandwich_test)[,2] #obtaining a standard error
no_saliva_sandwich_test_se

CI95_lb_robust=coef(no_saliva)-1.96*no_saliva_sandwich_test_se #gives me a more reliable CI
CI95_ub_robust=coef(no_saliva)+1.96*no_saliva_sandwich_test_se
cbind(no_saliva_sandwich_test, CI95_lb_robust, CI95_ub_robust) #now it looks better

no_saliva%>% vif() #lastly, to check for multicollinearity, I use the Variance Inflation Factor. Every variable falls below 3. 

back_reg=step(no_saliva, direction="backward")

backward_model <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = updated_data_sample_1) #model w/predictors left over after backwards regression
backward_model

theory_based_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = final_data_sample_1) #final model from part 1 (please note that I removed predictor 'cortisol_saliva' after the violation of Data Multicollinearity, so final_data_set_1 does not have that predictor)
theory_based_model

summary(backward_model) #let's me look at summary statistics
confint(backward_model)
lm.beta(backward_model)
coefTable(backward_model)
view(coefTable(backward_model))

summary(theory_based_model)
confint(theory_based_model)
lm.beta(theory_based_model)
coefTable(theory_based_model)
view(coefTable(theory_based_model))

AIC(backward_model) #run AIC to compare. Wow, we get a difference of only 3! Backward_model is better with the lower AIC of 476.30. 
AIC(theory_based_model)

anova(backward_model, theory_based_model) #let's compare using ANOVA. It looks like the p-val is not significant (p=.6). Theory_based_model has a lower RSS, which theoretically makes it the better model. 
#This is interesting because we are getting conflicting results in which model is better, but since the p-val is insignificant, we should take the ANOVA test with a grain of salt. 

summary(backward_model)$adj.r.squared #let's run adj. R^2
summary(theory_based_model)$adj.r.squared #adj R^2 gives us *extremely* close values. If we were to round it, backward_model has a *slightly* higher percentage in predicting pain (51% vs. 50%)
#This leads me to believe backward_model is better

data_sample_2 = read.csv("https://tinyurl.com/87v6emky") #Let's move on to data sample 2
View(data_sample_2)

summary(data_sample_2) #analyze the data set for irregularities
describe(data_sample_2)
str(data_sample_2) #everything looks fine

data_sample_2%>% #I run a few variables to make sure nothing stands out
  mutate(rownum=row.names(data_sample_2))%>%
  ggplot()+
  aes(x=STAI_trait, y=pain, label=rownum)+
  geom_label()

data_sample_2%>%
  mutate(rownum=row.names(data_sample_2))%>%
  ggplot()+
  aes(x=pain_cat, y=pain, label=rownum)+
  geom_label()

pain=c(1,3,5,7,8) #Now I predict. Various predicted pain levels in c()
pain_df = 'as.tibble(pain)'

predictions=predict(backward_model, data=pain_df)
pain_df_with_predicted=cbind(pain_df, predictions)
pain_df_with_predicted

error_plotter(backward_model, col="blue") #I can't seem to get the custom function, error_plotter to work so we won't be able to visualize the errors

RSS=sum((data_sample_2$pain - predict(backward_model))^2) #I use RSS to find the difference between the predicted value and actual value
RSS

RSS=sum((data_sample_2$pain - predict(theory_based_model))^2) #both of these models seem to give me a high value of error, but backward_model is slightly better
RSS

mod_mean <- lm(pain ~ 1, data = data_sample_2)
mod_mean

TSS=sum((data_sample_2$pain - predict(mod_mean))^2) #a relatively big difference between TSS and RSS
TSS

Rsq=1-(557.47/581.9)
Rsq #R^2. Backward_model explains variance slightly better (4.2% compared to theory based's 4.0%). 

Rsq_mod2=1-(558.58/581.9)
Rsq_mod2

#Regression Equation of backward_model
Y=1.28+(-.4)*age+0.11*pain_cat+(-.27)*mindfulness+.53*cortisol_serum
#Where Y=pain(prediction value), b0=1.28(the intercept), b1=-.4, x1=age, b2=.11, x2=pain_cat, b3=-.27, x3=mindfulness, b4=.53, and x4=cortisol_serum
#Run the equation and the prediction for pain is 1.25.
