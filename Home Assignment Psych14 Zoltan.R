## Home Assignment Part 1 ZK ##

library(dplyr) #make sure all of the following packages are installed and ready to use 
library(ggplot2)
library(gridExtra)
library(gtools)
library(lmtest)
library(psych)
library(tidyverse)

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") #to view the data set 

View(data_sample_1) #view data and checking the data for errors.
data_sample_1%>% #look at general info. 
  summary()

data_sample_1%>% #I will looks for outliers for each variable by graph.
  mutate(rownum=row.names(data_sample_1))%>%
  ggplot()+
  aes(x=age, y=pain, label=rownum)+
  geom_label() #for age, everything looks fine. 

data_sample_1%>%
  mutate(rownum=row.names(data_sample_1))%>%
  ggplot()+
  aes(x=sex, y=pain, label=rownum)+
  geom_label() #sex looks fine. 

data_sample_1%>%
  mutate(rownum=row.names(data_sample_1))%>%
  ggplot()+
  aes(x=STAI_trait, y=pain, label=rownum)+
  geom_label() #34 and #88 stand out. Looks like #88 rates '55' for pain scale. #34 rated STAI_trait as only 4.2. These are believed to be coding errors. 

data_sample_1%>%
  mutate(rownum=row.names(data_sample_1))%>%
  ggplot()+
  aes(x=pain_cat, y=pain, label=rownum)+
  geom_label() #88 stands out again. 

data_sample_1%>%
  mutate(rownum=row.names(data_sample_1))%>%
  ggplot()+
  aes(x=mindfulness, y=pain, label=rownum)+
  geom_label() #88, once again. 

data_sample_1%>%
  mutate(rownum=row.names(data_sample_1))%>%
  ggplot()+
  aes(x=cortisol_serum, y=pain, label=rownum)+
  geom_label() #nothing stands out except #88

data_sample_1%>%
  mutate(rownum=row.names(data_sample_1))%>%
  ggplot()+
  aes(x=cortisol_saliva, y=pain, label=rownum)+
  geom_label() #for cortisol_saliva, we can see that 30 rated it very low, at 2.91. Similarly, cortisol_serum was also low. I'll leave #30 in the dataset.  

str(data_sample_1) #structure function to further look at observations of each variable

describe(data_sample_1) #to get descriptive data

new_data_sample_1 <- data_sample_1[-c(34, 88), ] #to remove outliers, we run this. I now have a new data set. 

new_data_sample_1%>%
  ggplot()+
  aes(x=age)+
  geom_histogram(bins=50) #how many people in each age group participated

new_data_sample_1%>%
  ggplot()+
  aes(x=sex)+
  geom_bar() #slightly more women than men

model_1 = lm(pain ~ age + sex, data = new_data_sample_1) #look at model_1 first
model_1

plot1=new_data_sample_1%>% #to help me with graph comparison
  ggplot()+
  aes(x=age, y=pain)+
  geom_point()+
  geom_smooth(method = "lm")

plot2=new_data_sample_1%>% #also to help me with graph comparison
  ggplot()+
  aes(x=sex, y=pain)+
  geom_point()+
  geom_smooth(method = "lm")

grid.arrange(plot1, plot2, nrow = 1) #gives me two graphs to compare the variables, age & sex

model_1 %>%
  ggplot()+
  aes(x=age, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F) #color-codes and compares males and females in terms of age in one graph

model_1%>% plot(which=4) #to get Cook's Distance to verify outliers. It gave me a set of new outliers, #8, #23, and #47. Reviewing these participants, I will leave them in. 

summary(model_1)  #age has a negative correlation. P-value of sex is not significant. 
anova(model_1) #ANOVA because model_1 is a subset of model_2. Age is statistically significant (there is a slope), while sex is not (p=0.208). 
confint(model_1)
lm.beta(model_1)
coefTable(model_1)
view(coefTable(model_1))

model_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = new_data_sample_1)
model_2 #now I do model_2 the same way, but we will graph them separately. 

model_2%>% #plot3: STAI_trait. The higher the anxiety trait, the more pain people reported experiencing. 
  ggplot()+ 
  aes(x=STAI_trait, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

model_2%>% #plot4: pain_cat. Unsurprisingly, the more people felt helpless, the more pain they reported. 
  ggplot()+
  aes(x=pain_cat, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

model_2%>% #plot5: mindfulness. Mindfulness helped people report less pain. 
  ggplot()+
  aes(x=mindfulness, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

model_2%>% #plot6: cortisol_serum. Shows upward trend. 
  ggplot()+
  aes(x=cortisol_serum, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

model_2%>% #plot7: cortisol_saliva. Also shows upward trend. 
  ggplot()+
  aes(x=cortisol_saliva, y=pain, color=sex)+
  geom_point()+
  geom_smooth(method="lm", se=F)

model_2%>% plot(which=5) #to get a residual error plot

model_2%>% plot(which=4) #run Cook's distance. Outliers appear to be #47, #74, and #86. Check these participants and the only thing that stands out is they have high household incomes. 

summary(model_2) #pain_cat, mindfulness, and cortisol_saliva are significant. 
confint(model_2)
lm.beta(model_2)
coefTable(model_2)
view(coefTable(model_2))

AIC(model_2) #I choose AIC because model_2 is not a subset. 
AIC(model_1) #I run AIC for model_1 to compare. Confirms model_2 is better because the AIC is lower.  

anova(model_1, model_2) #p-val (2.2e-16) is significant because it's smaller than 0.05. Model_2 is better because the RSS is smaller. 

summary(model_1)$adj.r.squared #there is only 7% 
summary(model_2)$adj.r.squared #there is an increase in predicting the pain in model_2 (7% vs. almost 50% (49%))

#I will now run model diagnostics to find out if the final model violate any assumptions. Only model_2 because model_1 is a subset of model_2.

model_2%>% plot(which=2) #to check the normality assumption. #74, #85, and #104 are outside the diagonal line. 

describe(residuals(model_2)) #to make sure, we check the skewness and kurtosis. -0.15 and -0.03 lies between -1 and 1, so normality assumption is not violated

model_2%>% residualPlots() #Now checking for the violation of linearity assumption. The scatterplots show a slight curvature, but not enough to violate. 
#Tukey test and all variables are non-significant, as they're higher than 0.05. 

model_2%>% plot(which=3) #homoscedasticity function. Scatterplot does not appear in a funnel-shape, which makes me believe it does not violate homoscedasticity

model_2%>% ncvTest() #to make sure, let's run a NCV test. P=val. is 0.87. It's insignificant, so heteroscadicity is not significant. 

model_2%>% vif() #last but not least, we're testing for multicollinearity with the vif() function. Both cortisol variables are giving me a number greater than 3 (cortisol_serum: 4.79 and cortisol_saliva: 5.07)
#this means we have to treat it because the coefficients, confidence intervals, and point estimates are less reliable.
#I'm going to infer that the cortisol variables are examples of data multicollinearity, highly correlated variables within the data set naturally. 
#To solve the issue of multicollinearity, I will delete the 'cortisol_saliva' variable because it explicitly states that serum cortisol is more reliable in relation to stress when it comes to medical research, as serum is a component of the blood plasma. 

final_data_sample_1 <- new_data_sample_1[-(8)] #here, we delete column 8, which was variable 'cortisol_saliva'. The decision is based on theoretical grounds

model_3 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = final_data_sample_1)
model_3 #Because we now have a new data set, we will plot our final model, which is model_3, without the variable, 'cortisol_saliva'.
#Looking at the model coeffs, I can see that the intercept is now positive, age has a even higher negative correlation, whereas STAI_trait has a smaller negative correlation, cortisol_serum now has a higher correlation. 
#Interestingly enough, mindfulness has a higher negative correlation and the sex variable incresed ever so slightly. 

#we won't plot model_3 because it will look identical as model_2, only we take out the variable, 'cortisol_saliva'. 
model_3%>% plot(which=4) #we can, however, run Cook's Distance. Model_3 gives me the same outliers, except for #65. 

summary(model_3) #age, pain_cat, mindfulness, and cortisol_serum are significant

AIC(model_1) #Numbers differ significantly. Model_3 is sig. better than model_1.
AIC(model_3) #It can be interesting to note that model_2 had a lower AIC than the respective models, but because it violated the multicollinearity assumption, we can't trust it since the outputs are non-reliable. 

anova(model_1, model_3) #p-val (2.2e-16) is significant because it's smaller than 0.05. Model_2 is better because the RSS is smaller. 

summary(model_1)$adj.r.squared #there is only 7% accuracy in model_1 
summary(model_3)$adj.r.squared #there is 50% accuracy in model_3! An increase even compared to model_2. 

model_3%>% plot(which=2) #we now check assumptions for model_3. We begin with the normality assumption. #85, #86, and #104 are outside the diagonal line. 

describe(residuals(model_3)) #to make sure, I check the skewness and kurtosis. -0.15 and 0.02 lie between -1 and 1, so normality assumption is not violated

model_3%>% residualPlots() #I'm now checking for the violation of linearity assumption. The scatterplots show an ever so slight curvature, but not enough to violate. 
#Tukey test and all variables are non-significant, as they're higher than 0.05. 

model_3%>% plot(which=3) #homoscedasticity function. Scatterplot does not appear in a funnel-shape, which makes me believe it does not violate homoscedasticity
model_3%>% ncvTest() #to make sure, let's run a NCV test. P=val. is 0.80. It's insignificant, so heteroscadicity is not significant. 

model_3%>% vif() #lastly, I run the multicollinearity test and all variables fall below 3, so model_3 looks good. 

#Regression Equation of Model_3
Y=1.48+0.16*sex+(-0.04)*age+(-0.01)*STAI_trait+0.11*pain_cat+(0.28)*mindfulness+0.57*cortisol_serum
#where Y=pain(prediction value), b0=1.48(the intercept). b1=-0.04, x1=age, b2=0.16, x2=sex, b3=-0.01, x3=STAI_trait, b4=0.11, x4=pain_cat, b5=0.28, x5=mindfulness, b6=0.57, and x6=cortisol_serum   
#Run the equation and the prediction for pain is 2.55. 

##*Please refer to codes 179-185 for ANOVA, adjusted R^2, and AIC tests to see how these two models compare. 
