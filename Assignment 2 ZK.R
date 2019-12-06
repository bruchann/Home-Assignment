rm(list=ls(all=TRUE)) ## clears the workspace
graphics.off()        ## clears graphics

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")


## Zoltan's function for coefficient table

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}


## check data set

View(data_sample_1)

str(data_sample_1)



## clean data set 

data_cleaned <- data_sample_1


## remove subject 18 to get mean of STAI_trait without subject 18
data_cleaned1 <- data_cleaned[!data_cleaned$STAI_trait <20, ]

mean(data_cleaned1$STAI_trait)


## replace STAI_trait value of sj 18 with mean of STAI_trait

data_cleaned[18,5] <- mean(data_cleaned1$STAI_trait)

# remove subject with negative income

data_cleaned <- data_cleaned[-c(49),]  


## theory_based model

theory_based <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_cleaned)



## creating new model 

full.model <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = data_cleaned)


## check model for outliers


plot(full.model, which=4)
plot(full.model, which=5)


## look at adjusted R-squared statistics to see how much variance the models explain

summary(full.model)$adj.r.squared



## check normality assumption for residuals

hist( x = residuals(full.model),         # data are the residuals
      xlab = "Value of residual",        # x-axis label
      main = "",                         # no title
      breaks = 20                        # lots of breaks
)

shapiro.test(full.model$residuals)

plot(full.model, which=2)


## check linearity assumption of relationship between predictors and outcomes

yhat.2 <- fitted.values( object = full.model )
plot( x = yhat.2,
      y = data_cleaned$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
)

plot(x = full.model, which = 1)

require(car)
residualPlots( model = full.model ) ## if Tukey insignificant, then no violation


## check homogeneity of variance

plot(full.model, which=3)

ncvTest(full.model) ## if insignificant, then no violation


##check multicollinearity, if predictors in model are too highly correlated with each other

vif(full.model) ## values over 10 may indicate multicollinearity



## stepwise backward regression 

step( object = full.model, # start at the full model
    direction = "backward" # allow it remove predictors but not add them
         )

## model after backward regression

backward_model <- lm(formula = pain ~ sex + age + pain_cat + cortisol_serum + mindfulness + 
                   weight, data = data_cleaned)




## check backward_model for outliers


plot(backward_model, which=4)
plot(backward_model, which=5)



## check normality assumption for residuals

hist( x = residuals(backward_model),         # data are the residuals
      xlab = "Value of residual",        # x-axis label
      main = "",                         # no title
      breaks = 20                        # lots of breaks
)

shapiro.test(backward_model$residuals)

plot(backward_model, which=2)


## check linearity assumption of relationship between predictors and outcomes

yhat.2 <- fitted.values( object = backward_model )
plot( x = yhat.2,
      y = data_cleaned$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
)

plot(x = backward_model, which = 1)

require(car)
residualPlots( model = backward_model ) ## if Tukey insignificant, then no violation


## check homogeneity of variance

plot(backward_model, which=3)

ncvTest(backward_model) ## if insignificant, then no violation


##check multicollinearity, if predictors in model are too highly correlated with each other

vif(backward_model) ## values over 10 may indicate multicollinearity


summary(backward_model)
AIC(backward_model)
## look at adjusted R-squared statistics to see how much variance the models explain
summary(backward_model)$adj.r.squared


summary(full.model)
AIC(full.model)
## look at adjusted R-squared statistics to see how much variance the models explain
summary(full.model)$adj.r.squared

anova(backward_model, full.model)
anova(full.model, backward_model)

library(apaTables)
apa.reg.table(backward_model, filename = "Table2_APA.doc", table.number = 2)
coef_table(backward_model)



##### comparison of theory_based and backward_model (data_sample_1)

anova(backward_model, theory_based)




#######################################################



data_sample_2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")



str(data_sample_2)
describe(data_sample_2)

backward_2 <- predict(backward_model, newdata = data_sample_2)



###### theory-based_2 model 

theory_based_2 <- predict(theory_based, newdata = data_sample_2)




##### comparison of theory_based and backward_model2 (data_sample_2)



SSR_back_2 <- sum((data_sample_2$pain - predict(backward_model, newdata = data_sample_2))^2) # residual sum of squares
SSR_theory_2 <- sum((data_sample_2$pain - predict(theory_based, newdata = data_sample_2))^2)



