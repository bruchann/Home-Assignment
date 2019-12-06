rm(list=ls(all=TRUE))# clears the workspace
graphics.off()# clears graphics



############## Zoltan Assignment 1 ################




data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")


# Zoltan's function for coefficient table

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



# check data set

View(data_sample_1)

str(data_sample_1)


# clean data set 

data_cleaned <- data_sample_1

# remove subject 18 to get mean of STAI_trait without subject 18
data_cleaned1 <- data_cleaned[!data_cleaned$STAI_trait <20, ]

mean(data_cleaned1$STAI_trait)

# replace STAI_trait value of sj 18 with mean of STAI_trait

data_cleaned[18,5] <- mean(data_cleaned1$STAI_trait)
data_cleaned <- data_cleaned[-c(49),]   # remove subject with negative income


# build hierarchical model1 with sex and age as predictors of pain

model1 <- lm(pain ~ sex + age, data = data_cleaned)


# build hierarchical model2 with sex, age, STAI, pain catastrophizing, mindfulness, and cortisol measures as predictors

model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_cleaned)


# check model for outliers

plot(model2, which=4)   # Cook's Distance
plot(model2, which=5)   # leverage 



# check normality assumption for residuals

hist( x = residuals(model2),         # data are the residuals
       xlab = "Value of residual",   # x-axis label
       main = "",                    # no title
       breaks = 20                   # lots of breaks
       )

shapiro.test(model2$residuals)

plot(model2, which=2)


# skew and kurtosis (part of normality)

describe(residuals(model2))


# check linearity assumption of relationship between predictors and outcomes

yhat.2 <- fitted.values( object = model2 )
plot( x = yhat.2,
         y = data_cleaned$pain,
         xlab = "Fitted Values",
         ylab = "Observed Values"
         )

plot(x = model2, which = 1)

require(car)
residualPlots( model = model2 ) # if Tukey insignificant, then no violation


# check homogeneity of variance

plot(model2, which=3)

ncvTest(model2)  # if insignificant, then no violation

bptest(model2)   # Breush-Pagan test


#check multicollinearity, if predictors in model are too highly correlated with each other

vif(model2) # values over 3 may indicate multicollinearity

data_cleaned %>% select(pain, sex, age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva) %>%
  pairs.panels(col = "red", lm = T)  # cortisol_serum and cortisol_saliva are highly correlated -> keep just one of them
# keep cortisol_serum because it is more robust to stress (see description home assignment)


# re-run check of assumptions for new model 

model3 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_cleaned)


# check model for outliers

plot(model3, which=4)   # Cook's Distance
plot(model3, which=5)   # leverage 


# check normality assumption for residuals

hist( x = residuals(model3),         # data are the residuals
      xlab = "Value of residual",   # x-axis label
      main = "",                    # no title
      breaks = 20                   # lots of breaks
)

shapiro.test(model3$residuals)

plot(model3, which=2)


# skew and kurtosis (part of normality)

describe(residuals(model3))


# check linearity assumption of relationship between predictors and outcomes

yhat.2 <- fitted.values( object = model3 )
plot( x = yhat.2,
      y = data_cleaned$pain,
      xlab = "Fitted Values",
      ylab = "Observed Values"
)

plot(x = model3, which = 1)

require(car)
residualPlots( model = model3 ) # if Tukey insignificant, then no violation


# check homogeneity of variance

plot(model3, which=3)

ncvTest(model3)  # if insignificant, then no violation

bptest(model3)   # Breush-Pagan test


#check multicollinearity, if predictors in model are too highly correlated with each other

vif(model3) # values over 3 may indicate multicollinearity




# look at adjusted R-squared statistics to see how much variance the models explain

summary(model1)$adj.r.squared

summary(model3)$adj.r.squared


# report statistics R2, F, df, and p value, 
# unstandardized regression coefficients and 95% confidence intervals, 
# standardized regression coefficients (B and Beta values), and p values

summary(model1)
summary(model3)

confint(model1)
confint(model3)


require(lm.beta)

lm.beta(model1)
lm.beta(model3)

lm(model1)
lm(model3)

coef_table(model1)
coef_table(model3)


# comparing the two models to see which one has more predictive power

anova(model1, model3)

AIC(model1)

AIC(model3) 


# creating tables

require(apaTables)

require(MBESS)


block1 <- model1
block2 <- model3
apa.reg.table(block1, block2, filename = "Tablefinal_APA.doc", table.number = 3)

