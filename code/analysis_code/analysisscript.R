###############################
# analysis script

#load needed packages. 
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidymodels)
library(dotwhisker)

#path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Model Fitting
######################################

#We are making a path to use for linear regression. This will be used to create my linear models using tidymodels
lm_mod <- linear_reg() %>% set_engine("lm")

#Linear regression model for body temperature with only main predictor runnynose
lm_fit1 <- lm_mod %>% fit(BodyTemp ~ RunnyNose, data = mydata)

#This creates a clean table describing the runnynose model on body temperature.
#We can see that the runny nose is significant in predicting body temperature. If someone has a runny nose, then the
#person's body temperature will decrease in 0.29 degrees Fahrenheit.
lmtab1 = tidy(lm_fit1)
lmtab1

#Glance is looking at some more statistics on the linear model.
#Although runny nose is significant, the model does not fit the data well.
glance(lm_fit1)

#Linear regression model for body temperature with all predictors
lm_fit2 <- lm_mod %>% fit(BodyTemp ~ ., data = mydata)

#This is a clean table looking at the model for body temperature with all predictors. 
lmtab2 = tidy(lm_fit2)
lmtab2

#This is statistics of the overall model.
#Although we have all predictors, the model fit is still not good
glance(lm_fit2)

#This is a dot and whisker plot which shows the estimates for each predictor better than the table does especially
#Since there are so many variables within this model. There is nor many variables that arent in the range of 0.
plot1 = tidy(lm_fit2) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, linetype = 2))
plot1

#Anova is used to compare the two linear models for predicting body temperature. 
#The model with all predictors is significantly better than the model with only runny nose.
CompMod1 = anova(lm_fit1$fit,lm_fit2$fit, data = mydata, test= "Chisq")
CompMod1

#Creating a path for making logistic models through tidymodels
log_mod <- logistic_reg() %>% set_engine("glm")

#A logistic model for nausea with main predictor runny nose
log_fit1 <- log_mod %>% fit(Nausea ~ RunnyNose, data = mydata)

#A table showing how runny nose affects nausea.
#We can see that runny nose is not a good predictor for nausea at least not significant.
logtab1 = tidy(log_fit1)
logtab1

#Statistics over the logistic regression model based on only runny nose. (Fitting of model)
glance(log_fit1)

#Logistic regression model of nausea with all predictors
log_fit2 <- log_mod %>% fit(Nausea ~ ., data = mydata)

#A table showing information of the nausea vs all predictors model.
logtab2 = tidy(log_fit2)
logtab2

#Statistics of logistic regression model (fitting of model on data)
glance(log_fit2)

#This is a dot and whisker plot which shows the estimates for each predictor better than the table does especially
#Since there are so many variables within this model. We can see that vomit is significantly different from
#an estimate of 0. This variable has a large effect on nausea than any other predictor.
plot2 = tidy(log_fit2) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, linetype = 2))
plot2


#Anova to compare the two logistic models with the test of chi-square
#We can see that the full model is significantly different from just the model with the main predictor runny nose
CompMod2 = anova(log_fit1$fit,log_fit2$fit, data = mydata, test = "Chisq")
CompMod2

#Saving the tables and plots for future reference
lmtable1 = here::here("results",  "LinearTable1.rds")
saveRDS(lmtab1, file = lmtable1)

lmtable2 = here::here("results", "LinearTable2.rds")
saveRDS(lmtab2, file = lmtable2)

logtable1 = here::here("results", "LogisticTable1.rds")
saveRDS(logtab1, file = logtable1)

logtable2 = here::here("results", "LogisticTable2.rds")
saveRDS(logtab2, file = logtable2)

comtable1 = here::here("results", "ComparelinearTable.rds")
saveRDS(CompMod1, file = comtable1)

comtable2 = here::here("results", "ComparelogisticTable.rds")
saveRDS(CompMod2, file = comtable2)

figure1 = here("results","dotwhisklinear.png")
ggsave(filename = figure1, plot=plot1) 

figure2 = here("results","dotwhisklogistic.png")
ggsave(filename = figure2, plot=plot2) 
