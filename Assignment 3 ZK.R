rm(list=ls(all=TRUE))# clears the workspace
graphics.off()# clears graphics

data_sample_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")

data_sample_4 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")


library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM	


# This is a function to extract standardized beta coefficients from linear mixed models.

stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

## check data sets

View(data_sample_3)
View(data_sample_4)

str(data_sample_3)
str(data_sample_4)

describe(data_sample_3)
describe(data_sample_4)

plot(data_sample_3)
plot(data_sample_4)


## clean up data 

data_sample_3$sex <- replace(data_sample_3$sex, data_sample_3$sex=="Female", "female")
data_sample_3$sex <- factor(data_sample_3$sex)   # 2 instead of 3 levels in sex
str(data_sample_3)

data_sample_3 = data_sample_3 %>% mutate(hospital = recode(hospital,
              hospital_1 = 1, hospital_2 = 2, hospital_3 = 3, hospital_4 = 4, hospital_5 = 5, hospital_6 = 6,
              hospital_7 = 7, hospital_8 = 8, hospital_9 = 9, hospital_10 = 10))


## build linear mixed model

mod_rnd_int = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = data_sample_3)


## check model for outliers and assumptions

library(psych) # for pairs.panels
library(tidyverse) # for tidy code and ggplot\t
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath
library(lme4) # for mixed models
library(lmerTest) # for significance test on lmer() mixed models


# check for influential outliers
  
library(lme4)
library(influence.ME)
inf=influence(mod_rnd_int,obs=T)
plot(inf,which="cook")

????????????????
  

# check for normality
  
qqmath(mod_rnd_int)
qqmath(ranef(mod_rnd_int))


# check for linearity

plot(mod_rnd_int, arg = "pearson")

?ggplot

?????????????
  

# homoscedasticity

plot(mod_rnd_int, arg = "pearson") # if funnel shape then not good 

homosced_mod = lm(resid^2 ~ hospital, data = data_sample_3)
summary(homosced_mod)    

???????????????


# multicollinearity 

data_sample_3 %>% select(pain, sex, age, STAI_trait, pain_cat, cortisol_serum, mindfulness) %>%
  pairs.panels(col = "red", lm = T)


## run the model on data_sample_3

summary(mod_rnd_int)           # unstandardized coefficients
confint(mod_rnd_int)           # confidence intervals
stdCoef.merMod(mod_rnd_int)    # standard. coefficients


# marginal and conditional R squared values

r.squaredGLMM(mod_rnd_int)

# variance for fixed, residual, intercept

library(insight)
get_variance_fixed(mod_rnd_int)
get_variance_residual(mod_rnd_int)
get_variance_intercept(mod_rnd_int)


require(apaTables)
require(MBESS)
apa.reg.table(mod_rnd_int, filename = "Tableintercept_APA.doc", table.number = 2)




## run the model on data_sample_4

mod_mean <- lm(pain ~ 1, data = data_sample_4)
mod_rnd_int_4 <- predict(mod_rnd_int, newdata = data_sample_4, allow.new.levels = TRUE)
null_model_4 <- predict(mod_mean, newdata = data_sample_4, allow.new.levels = TRUE)


# calculate explained variance, R^2, with RSS and TSS

RSS = sum((data_sample_4$pain - mod_rnd_int_4)^2) # residual ss
RSS
TSS = sum((data_sample_4$pain - null_model_4)^2)
TSS

R2 = 1 - RSS/TSS

##### build a linear mixed effects model on data_sample_3 but only with the most influential predictor

mod_rnd_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital),
                     data = data_sample_3)


# visualizing fitted regression line for each hospital separately 

data_sample_3 = data_sample_3 %>% mutate(pred_int = predict(mod_rnd_int),
                                               pred_slope = predict(mod_rnd_slope))

data_sample_3 %>% ggplot() + aes(y = pain, x = cortisol_serum,
                                    group = hospital) + geom_point(aes(color = hospital), size = 4) +
  geom_line(color = "red", aes(y = pred_slope, x = cortisol_serum)) +
  facet_wrap(~hospital, ncol = 2) 




