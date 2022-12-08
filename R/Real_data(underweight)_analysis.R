#=================================================================================
#Non-parametric model
#=================================================================================
rm(list = ls())
setwd("E:\\Study\\MS\\Thesis_Article")

data<-read.csv("underweight1.csv", header = T)
str(data)
## Load empirical data and model parameters
dfName = c('underweight1')
yName = c('l.underweight')
zName = c('s.child')
load(file = paste(dfName, '_outcome_model.RData', sep=''))
df = obj[['df']]
po = obj[['po']]
xNames = setdiff(names(df), yName)
rm(obj)

## Define treatment effect funciton on linear predictor scale
t_tau = qlogis(po[,2]) - qlogis(po[,1])

## Specify marginal effect based on real study
t_tau_2 = t_tau
t_tau_2 = t_tau + (log(0.4112343)*2-mean(t_tau)) #child size at birth


po[,2] = plogis(qlogis(po[,1]) + t_tau_2)
tau = po[,2] - po[,1]

## Sample factual outcome
pi_fact = ifelse(df[,zName]==0, po[,1], po[,2])
y_fact = data$l.underweight

## Bind empirical and simulated data
df = cbind(df[,xNames], y_fact, pi_fact, tau)

str(df)

library(glmnet)
library(BART)
library(grf)
library(causalLearning)  # Package for causal boosting and causal MARS -- under development
library(causalTree)
memory.limit(500000)

#-----------------------------------------------------------------------------------------------------
# Simulate and format data
#-----------------------------------------------------------------------------------------------------
df = df[complete.cases(df),]
zName = c('s.child')
xNames = setdiff(names(df), c(zName, 'y_fact', 'tau', 'pi_fact'))
n_samp = nrow(df)

#-----------------------------------------------------------------------------------------------------
# Null estimator
#-----------------------------------------------------------------------------------------------------
ATE_naive = mean(df$pi_fact[df[,zName]==1]) - mean(df$pi_fact[df[,zName]==0])
CATE_true = df$tau
ATE_true = mean(CATE_true)
ATE_bias_null = abs( (ATE_naive- ATE_true) / ATE_true * 100 )
CATE_RMSE_null = sqrt(mean((0 - CATE_true)^2))

#-----------------------------------------------------------------------------------------------------
# Logistic (with and without PS-adjustment)
#-----------------------------------------------------------------------------------------------------
## Logistic model for Z 
RHS0 <- "c.age + c.sex + deliv + interval + bord + age.marri + age.dif + age.1birth + height + reli + p.occup + h.occup + h.edu + peduc + l.child + wealth + s.water + resid + div + h.sex + h.memb +  health + clinic + f.plan + polio + b.f + ante.care"
formula_stu <- as.formula(paste("s.child~", RHS0))
log_fit_Z<-glm(formula_stu,df, family = binomial())
PS_log<-predict(log_fit_Z, type = "response")

## Logistic model for Y
RHS1 <- "c.age + c.sex + s.child + deliv + interval + bord + age.marri + age.dif + age.1birth + height + reli + p.occup + h.occup + h.edu + peduc + l.child + wealth + s.water + resid + div + h.sex + h.memb +  health + clinic + f.plan + polio + b.f + ante.care"
formula_stu1 <- as.formula(paste("y_fact~", RHS1))
log_fit_Y<-glm(formula_stu1,df, family = binomial())

## Logistic model for Y (PS adjusted)
df1<-cbind(df,PS_log)
RHS11 <- "c.age + c.sex + s.child + deliv + interval + bord + age.marri + age.dif + age.1birth + height + reli + p.occup + h.occup + h.edu + peduc + l.child + wealth + s.water + resid + div + h.sex + h.memb +  health + clinic + f.plan + polio + b.f + ante.care + PS_log"
formula_stu1 <- as.formula(paste("y_fact~", RHS11))
log_fit_Y_PS<-glm(formula_stu1,df1, family = binomial())

## CATE estimates with logistic
CATE_hat<-predict(log_fit_Y, type = "response")
CATE_RMSE_log = as.vector(sqrt(mean((CATE_hat - CATE_true)^2)))
pi_Y<-pnorm(log_fit_Y$fitted.values)
ATE_hat<-mean(pi_Y[df1[,zName]==1])-mean(pi_Y[df1[,zName]==0])
ATE_bias_log = as.numeric( abs( (ATE_hat - ATE_true) / ATE_true * 100 ) )
ATE_CI = quantile(ATE_hat, c(0.025, 0.975))
ATE_IL_log = ATE_CI[2] - ATE_CI[1]
ATE_coverage_log = as.numeric(ifelse(ATE_true>=ATE_CI[1] & ATE_true<=ATE_CI[2], 1, 0))

## CATE estimates with logistic (PS adjusted)
CATE_hat_PS<-predict(log_fit_Y_PS, type = "response")
CATE_RMSE_log_PS = as.vector(sqrt(mean((CATE_hat_PS - CATE_true)^2)))
pi_Y_PS<-pnorm(log_fit_Y_PS$fitted.values)
ATE_hat_PS<-mean(pi_Y_PS[df1[,zName]==1])-mean(pi_Y_PS[df1[,zName]==0])
ATE_bias_log_PS = as.numeric( abs( (ATE_hat_PS - ATE_true) / ATE_true * 100 ) )
ATE_CI_PS = quantile(ATE_hat_PS, c(0.025, 0.975))
ATE_IL_log_PS = ATE_CI_PS[2] - ATE_CI_PS[1]
ATE_coverage_log_PS = as.numeric(ifelse(ATE_true>=ATE_CI_PS[1] & ATE_true<=ATE_CI_PS[2], 1, 0))

#-----------------------------------------------------------------------------------------------------
# BART (with and without PS-adjustment)
#-----------------------------------------------------------------------------------------------------
## Hyperparameters -- defaults except the following MCMC parameters:
nskip = round(nrow(df)/2)
ndpost = nrow(df)

## BART model for Z
X = df[,xNames]
y = df[,zName]
BART_fit_Z = pbart(X, y,
                   nskip = nskip, ndpost = ndpost)
PS = pnorm(BART_fit_Z$yhat.train)

## BART model for Y
X_fact = cbind(df[,xNames], Z = df[,zName])
X_count = X_fact
X_count$Z = abs(X_fact$Z - 1)
y = df$y_fact

BART_fit_Y = pbart(x.train = X_fact, y.train = y, x.test = X_count,
                   nskip = nskip, ndpost = ndpost)
PS1 = pnorm(BART_fit_Y$yhat.train)

## BART model for Y (PS adjusted)
X_fact = cbind(df[,xNames], Z = df[,zName], PS)
X_count = X_fact
X_count$Z = abs(X_fact$Z - 1)
y = as.numeric(df$y_fact)
psBART_fit_Y = pbart(x.train = X_fact, y.train = y, x.test = X_count,
                     nskip = nskip, ndpost = ndpost)

## CATE estimates with BART
pi_hat_Z0 = pnorm(BART_fit_Y$yhat.train)
pi_hat_Z0[,X_fact$Z==1] = pnorm(BART_fit_Y$yhat.test)[ ,X_fact$Z==1]
pi_hat_Z1 = pnorm(BART_fit_Y$yhat.train)
pi_hat_Z1[,X_fact$Z==0] = pnorm(BART_fit_Y$yhat.test)[ ,X_fact$Z==0]
CATE_post = pi_hat_Z1 - pi_hat_Z0
ATE_post  = apply(CATE_post, 1, mean)
ATE_bias_post = abs( (ATE_post - ATE_true) / ATE_true * 100 )
ATE_bias_BART0 = mean(ATE_bias_post)
ATE_CI = quantile(ATE_post, c(0.025, 0.975))
ATE_IL_BART0 = ATE_CI[2] - ATE_CI[1]
ATE_coverage_BART0 = as.numeric(ifelse(ATE_true>=ATE_CI[1] & ATE_true<=ATE_CI[2], 1, 0))
CATE_hat = apply(CATE_post, 2, mean)
CATE_RMSE_BART0 = sqrt(mean((CATE_hat - CATE_true)^2))

## CATE estimates with PS-BART
pi_hat_Z0 = pnorm(psBART_fit_Y$yhat.train)
pi_hat_Z0[,X_fact$Z==1] = pnorm(psBART_fit_Y$yhat.test)[ ,X_fact$Z==1]
pi_hat_Z1 = pnorm(psBART_fit_Y$yhat.train)
pi_hat_Z1[,X_fact$Z==0] = pnorm(psBART_fit_Y$yhat.test)[ ,X_fact$Z==0]
CATE_post = pi_hat_Z1 - pi_hat_Z0
ATE_post  = apply(CATE_post, 1, mean)
ATE_bias_post = abs( (ATE_post - ATE_true) / ATE_true * 100 )
ATE_bias_BART1 = mean(ATE_bias_post)
ATE_CI = quantile(ATE_post, c(0.025, 0.975))
ATE_IL_BART1 = ATE_CI[2] - ATE_CI[1]
ATE_coverage_BART1 = as.numeric(ifelse(ATE_true>=ATE_CI[1] & ATE_true<=ATE_CI[2], 1, 0))
CATE_hat = apply(CATE_post, 2, mean)
CATE_RMSE_BART1 = sqrt(mean((CATE_hat - CATE_true)^2))

#-----------------------------------------------------------------------------------------------------
# Causal Forest
#-----------------------------------------------------------------------------------------------------
X = as.matrix(df[,xNames])
W = df[,zName]
Y = df$y_fact
CF_fit = causal_forest(X, Y, W)
CATE_hat = as.vector(predict(CF_fit)$predictions)
CATE_RMSE_CF = sqrt(mean((CATE_hat - CATE_true)^2))
ATE_estimates = average_treatment_effect(CF_fit, target.sample = 'all', method = 'AIPW')
ATE_hat = ATE_estimates[1]
ATE_bias_CF = as.numeric( abs( (ATE_hat - ATE_true) / ATE_true * 100 ) )
ATE_CI = c(ATE_hat-1.96*ATE_estimates[2], ATE_hat+1.96*ATE_estimates[2])
ATE_IL_CF = ATE_CI[2] - ATE_CI[1]
ATE_coverage_CF = as.numeric(ifelse(ATE_true>=ATE_CI[1] & ATE_true<=ATE_CI[2], 1, 0))

#-----------------------------------------------------------------------------------------------
##Causal tree   
#-----------------------------------------------------------------------------------------------
W = df[,zName]
Y = df$y_fact
CT_fit <- causalTree(Y ~ X + c.age + c.sex + s.child + deliv + interval + bord + age.marri + age.dif 
                     + age.1birth + height + reli + p.occup + h.occup + h.edu + peduc + l.child + wealth 
                     + s.water + resid + div + h.sex + h.memb +  health + clinic + f.plan + polio + b.f + ante.care,
                     data = df, treatment = W, split.Rule = "CT",
                     split.Honest = T, cv.option = "matching", 
                     cv.Honest = F, split.Bucket = F, xval = 10)

pi_Y<-pnorm(predict(CT_fit))
ATE_hat_CT<-mean(pi_Y[df[,zName]==1])-mean(pi_Y[df[,zName]==0])
ATE_bias_CT = as.numeric( abs( (ATE_hat_CT - ATE_true) / ATE_true * 100 ) )
CATE_hat = as.vector(predict(CT_fit))
CATE_RMSE_CT = as.vector(sqrt(mean((CATE_hat - CATE_true)^2)))

#CT_fit1 <- causalTree(Y ~ X+ c.age + c.sex + b.feeding + resid + m.edu + h.occup 
#                     + BMI + age.1birth + f.edu + p.occup + wealth, data = df,
#                     treatment = W, split.Rule = "CT", weights = (1/PS),
#                     split.Honest = T, cv.option = "matching", 
#                     cv.Honest = F, split.Bucket = F, xval = 10)
#CATE_hat1 = as.vector(predict(CT_fit1))
#CATE_RMSE_CT1 = sqrt(mean((CATE_hat1 - CATE_true)^2))

#-----------------------------------------------------------------------------------------------
##Causal transformed outcome tree   
#-----------------------------------------------------------------------------------------------
CTOT_fit <- causalTree(Y ~ X + c.age + c.sex + s.child + deliv + interval + bord + age.marri + age.dif 
                       + age.1birth + height + reli + p.occup + h.occup + h.edu + peduc + l.child + wealth 
                       + s.water + resid + div + h.sex + h.memb +  health + clinic + f.plan + polio + b.f + ante.care,
                       data = df, treatment = W, split.Rule = "TOT", cv.option = "matching", 
                       cv.Honest = F, split.Bucket = F, xval = 10)
pi_Y<-pnorm(predict(CTOT_fit))
ATE_hat_TOT<-mean(pi_Y[df[,zName]==1])-mean(pi_Y[df[,zName]==0])
ATE_bias_TOT = as.numeric( abs( (ATE_hat_TOT - ATE_true) / ATE_true * 100 ) )
CATE_hat = as.vector(predict(CTOT_fit))
CATE_RMSE_CTOT = sqrt(mean((CATE_hat - CATE_true)^2))

#CTOT_fit1 <- causalTree(Y ~ X+ c.age + c.sex + b.feeding + resid + m.edu + h.occup
#                       + BMI + age.1birth + f.edu + p.occup + wealth, data = df,
#                       treatment = W, split.Rule = "TOT", weights = (1/PS),
#                       cv.option = "matching", 
#                       cv.Honest = F, split.Bucket = F, xval = 10)
#CATE_hat1 = as.vector(predict(CTOT_fit1))
#CATE_RMSE_CTOT1 = sqrt(mean((CATE_hat1 - CATE_true)^2))

#-----------------------------------------------------------------------------------------------------
# Causal Boosting (with and without PS-adjustment).
# The following code was written for a rough beta version of the 'causalLearning' package and might have 
# to be modified for the published version.
#-----------------------------------------------------------------------------------------------------
## CB
X = as.matrix(df[,xNames])
Z = df[,zName]
Y = df$y_fact
ps_adjus = FALSE
CB_fit = causalLearning::cv.causalBoosting(X, Z, Y,
                                           num.trees = 300, maxleaves = 4, splitSpread = 0.2,
                                           type.measure = 'effect', nfolds = 3,
                                           propensity = ps_adjus)
CATE_hat = predict(CB_fit, X)
ATE_hat = mean(CATE_hat)
ATE_bias_CB0 = as.numeric( abs( (ATE_hat - ATE_true) / ATE_true * 100 ) )
CATE_RMSE_CB0 = sqrt(mean((CATE_hat - CATE_true)^2))

out = list(n_samp=n_samp,
           ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
           ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF,
           ATE_bias_CB0=ATE_bias_CB0,  
           ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, ATE_coverage_CF=ATE_coverage_CF,
           ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
           CATE_RMSE_null=CATE_RMSE_null, 
           CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
           CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB0=CATE_RMSE_CB0
)

data.matrix(out)

##Export result##
fname= paste('realdata_underweight_nonpara_result', '_outcome.Rdata', sep = '')
object= list(n_samp=n_samp,
             ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
             ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF,
             ATE_bias_CB0=ATE_bias_CB0, #ATE_bias_CB1=ATE_bias_CB1, 
             ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, ATE_coverage_CF=ATE_coverage_CF,
             ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
             CATE_RMSE_null=CATE_RMSE_null, 
             CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
             CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB0=CATE_RMSE_CB0,
             CATE_RMSE_CT=CATE_RMSE_CT, CATE_RMSE_CTOT=CATE_RMSE_CTOT
             #CATE_RMSE_CB1=CATE_RMSE_CB1 
)
save(object, file = fname)

load(file = paste('realdata_underweight_nonpara_result', '_outcome.Rdata', sep=''))
attach(object)
ATE<-data.frame(ATE_bias_BART0, ATE_bias_BART1, ATE_bias_CF, ATE_bias_CB0)
CATE<-data.frame(CATE_RMSE_BART0, CATE_RMSE_BART1, CATE_RMSE_CF, CATE_RMSE_CB0, CATE_RMSE_CT, CATE_RMSE_CTOT)

#=================================================================================
#Parametric model
#=================================================================================

rm(list = ls())
setwd("E:\\Study\\MS\\Thesis")

data<-read.csv("underweight1.csv", header = T)
str(data)
## Load empirical data and model parameters
dfName = c('underweight1')
yName = c('l.underweight')
zName = c('s.child')
load(file = paste(dfName, '_outcome_parametric_model1.RData', sep=''))
df1 = obj[['df1']]
po = obj[['po']]
xNames = setdiff(names(df1), yName)
rm(obj)

## Define treatment effect funciton on linear predictor scale
t_tau = qlogis(po[,2]) - qlogis(po[,1])

## Specify marginal effect based on real study
t_tau_2 = t_tau
t_tau_2 = t_tau + (log(0.4112343)*2-mean(t_tau)) #child size at birth


po[,2] = plogis(qlogis(po[,1]) + t_tau_2)
tau = po[,2] - po[,1]

## Sample factual outcome
pi_fact = ifelse(df1[,zName]==0, po[,1], po[,2])
y_fact = data$l.underweight

## Bind empirical and simulated data
df = cbind(df1[,xNames], y_fact, pi_fact, tau)

str(df)

library(glmnet)
library(BART)
library(grf)
library(causalLearning)  # Package for causal boosting and causal MARS -- under development
library(causalTree)
memory.limit(500000)

#-----------------------------------------------------------------------------------------------------
# Simulate and format data
#-----------------------------------------------------------------------------------------------------
df = df[complete.cases(df),]
zName = c('s.child')
xNames = setdiff(names(df), c(zName, 'y_fact', 'tau', 'pi_fact'))
n_samp = nrow(df)

#-----------------------------------------------------------------------------------------------------
# Null estimator
#-----------------------------------------------------------------------------------------------------
ATE_naive = mean(df$pi_fact[df[,zName]==1]) - mean(df$pi_fact[df[,zName]==0])
CATE_true = df$tau
ATE_true = mean(CATE_true)
ATE_bias_null = abs( (ATE_naive- ATE_true) / ATE_true * 100 )
CATE_RMSE_null = sqrt(mean((0 - CATE_true)^2))

#-----------------------------------------------------------------------------------------------------
# Logistic (with and without PS-adjustment)
#-----------------------------------------------------------------------------------------------------
## Logistic model for Z 
RHS1 <- "c.age + c.sex  + b.feeding + m.edu + h.occup + BMI + age.1birth + f.edu + p.occup + wealth"
formula_stu <- as.formula(paste("s.child~", RHS1))
log_fit_Z<-glm(formula_stu,df, family = binomial())
PS_log<-predict(log_fit_Z, type = "response")

## Logistic model for Y
RHS1 <- "c.age + s.child + c.sex  + b.feeding + m.edu + h.occup + BMI + age.1birth + f.edu + p.occup + wealth"
formula_stu1 <- as.formula(paste("y_fact~", RHS1))
log_fit_Y<-glm(formula_stu1,df, family = binomial())

## Logistic model for Y (PS adjusted)
df1<-cbind(df,PS_log)
RHS11 <- "c.age + s.child + c.sex  + b.feeding + m.edu + h.occup + BMI + age.1birth + f.edu + p.occup + wealth + PS_log"
formula_stu1 <- as.formula(paste("y_fact~", RHS11))
log_fit_Y_PS<-glm(formula_stu1,df1, family = binomial())

## CATE estimates with logistic
CATE_hat<-predict(log_fit_Y, type = "response")
CATE_RMSE_log = as.vector(sqrt(mean((CATE_hat - CATE_true)^2)))
pi_Y<-pnorm(log_fit_Y$fitted.values)
ATE_hat<-mean(pi_Y[df1[,zName]==1])-mean(pi_Y[df1[,zName]==0])
ATE_bias_log = as.numeric( abs( (ATE_hat - ATE_true) / ATE_true * 100 ) )
ATE_CI = quantile(ATE_hat, c(0.025, 0.975))
ATE_IL_log = ATE_CI[2] - ATE_CI[1]
ATE_coverage_log = as.numeric(ifelse(ATE_true>=ATE_CI[1] & ATE_true<=ATE_CI[2], 1, 0))

## CATE estimates with logistic (PS adjusted)
CATE_hat_PS<-predict(log_fit_Y_PS, type = "response")
CATE_RMSE_log_PS = as.vector(sqrt(mean((CATE_hat_PS - CATE_true)^2)))
pi_Y_PS<-pnorm(log_fit_Y_PS$fitted.values)
ATE_hat_PS<-mean(pi_Y_PS[df1[,zName]==1])-mean(pi_Y_PS[df1[,zName]==0])
ATE_bias_log_PS = as.numeric( abs( (ATE_hat_PS - ATE_true) / ATE_true * 100 ) )
ATE_CI_PS = quantile(ATE_hat_PS, c(0.025, 0.975))
ATE_IL_log_PS = ATE_CI_PS[2] - ATE_CI_PS[1]
ATE_coverage_log_PS = as.numeric(ifelse(ATE_true>=ATE_CI_PS[1] & ATE_true<=ATE_CI_PS[2], 1, 0))

#-----------------------------------------------------------------------------------------------------
# BART (with and without PS-adjustment)
#-----------------------------------------------------------------------------------------------------
## Hyperparameters -- defaults except the following MCMC parameters:
nskip = round(nrow(df)/2)
ndpost = nrow(df)

## BART model for Z
X = df[,xNames]
y = df[,zName]
BART_fit_Z = pbart(X, y,
                   nskip = nskip, ndpost = ndpost)
PS = pnorm(BART_fit_Z$yhat.train)

## BART model for Y
X_fact = cbind(df[,xNames], Z = df[,zName])
X_count = X_fact
X_count$Z = abs(X_fact$Z - 1)
y = df$y_fact
BART_fit_Y = pbart(x.train = X_fact, y.train = y, x.test = X_count,
                   nskip = nskip, ndpost = ndpost)

## BART model for Y (PS adjusted)
X_fact = cbind(df[,xNames], Z = df[,zName], PS)
X_count = X_fact
X_count$Z = abs(X_fact$Z - 1)
y = df$y_fact
psBART_fit_Y = pbart(x.train = X_fact, y.train = y, x.test = X_count,
                     nskip = nskip, ndpost = ndpost)

## CATE estimates with BART
pi_hat_Z0 = pnorm(BART_fit_Y$yhat.train)
pi_hat_Z0[,X_fact$Z==1] = pnorm(BART_fit_Y$yhat.test)[ ,X_fact$Z==1]
pi_hat_Z1 = pnorm(BART_fit_Y$yhat.train)
pi_hat_Z1[,X_fact$Z==0] = pnorm(BART_fit_Y$yhat.test)[ ,X_fact$Z==0]
CATE_post = pi_hat_Z1 - pi_hat_Z0
ATE_post  = apply(CATE_post, 1, mean)
ATE_bias_post = abs( (ATE_post - ATE_true) / ATE_true * 100 )
ATE_bias_BART0 = mean(ATE_bias_post)
ATE_CI = quantile(ATE_post, c(0.025, 0.975))
ATE_IL_BART0 = ATE_CI[2] - ATE_CI[1]
ATE_coverage_BART0 = as.numeric(ifelse(ATE_true>=ATE_CI[1] & ATE_true<=ATE_CI[2], 1, 0))
CATE_hat = apply(CATE_post, 2, mean)
CATE_RMSE_BART0 = sqrt(mean((CATE_hat - CATE_true)^2))

## CATE estimates with PS-BART
pi_hat_Z0 = pnorm(psBART_fit_Y$yhat.train)
pi_hat_Z0[,X_fact$Z==1] = pnorm(psBART_fit_Y$yhat.test)[ ,X_fact$Z==1]
pi_hat_Z1 = pnorm(psBART_fit_Y$yhat.train)
pi_hat_Z1[,X_fact$Z==0] = pnorm(psBART_fit_Y$yhat.test)[ ,X_fact$Z==0]
CATE_post = pi_hat_Z1 - pi_hat_Z0
ATE_post  = apply(CATE_post, 1, mean)
ATE_bias_post = abs( (ATE_post - ATE_true) / ATE_true * 100 )
ATE_bias_BART1 = mean(ATE_bias_post)
ATE_CI = quantile(ATE_post, c(0.025, 0.975))
ATE_IL_BART1 = ATE_CI[2] - ATE_CI[1]
ATE_coverage_BART1 = as.numeric(ifelse(ATE_true>=ATE_CI[1] & ATE_true<=ATE_CI[2], 1, 0))
CATE_hat = apply(CATE_post, 2, mean)
CATE_RMSE_BART1 = sqrt(mean((CATE_hat - CATE_true)^2))

#-----------------------------------------------------------------------------------------------------
# Causal Forest
#-----------------------------------------------------------------------------------------------------
X = as.matrix(df[,xNames])
W = df[,zName]
Y = df$y_fact
CF_fit = causal_forest(X, Y, W)
CATE_hat = as.vector(predict(CF_fit)$predictions)
CATE_RMSE_CF = sqrt(mean((CATE_hat - CATE_true)^2))
ATE_estimates = average_treatment_effect(CF_fit, target.sample = 'all', method = 'AIPW')
ATE_hat = ATE_estimates[1]
ATE_bias_CF = as.numeric( abs( (ATE_hat - ATE_true) / ATE_true * 100 ) )
ATE_CI = c(ATE_hat-1.96*ATE_estimates[2], ATE_hat+1.96*ATE_estimates[2])
ATE_IL_CF = ATE_CI[2] - ATE_CI[1]
ATE_coverage_CF = as.numeric(ifelse(ATE_true>=ATE_CI[1] & ATE_true<=ATE_CI[2], 1, 0))

#-----------------------------------------------------------------------------------------------
##Causal tree   
#-----------------------------------------------------------------------------------------------
W = df[,zName]
Y = df$y_fact
CT_fit <- causalTree(Y ~ X+ c.age + c.sex + b.feeding + resid + m.edu + h.occup 
                     + BMI + age.1birth + f.edu + p.occup + wealth, data = df,
                     treatment = W, split.Rule = "CT",
                     split.Honest = T, cv.option = "matching", 
                     cv.Honest = F, split.Bucket = F, xval = 10)

pi_Y<-pnorm(predict(CT_fit))
ATE_hat_CT<-mean(pi_Y[df[,zName]==1])-mean(pi_Y[df[,zName]==0])
ATE_bias_CT = as.numeric( abs( (ATE_hat_CT - ATE_true) / ATE_true * 100 ) )
CATE_hat = as.vector(predict(CT_fit))
CATE_RMSE_CT = as.vector(sqrt(mean((CATE_hat - CATE_true)^2)))

#-----------------------------------------------------------------------------------------------
##Causal transformed outcome tree   
#-----------------------------------------------------------------------------------------------
CTOT_fit <- causalTree(Y ~ X+ c.age + c.sex + b.feeding + resid + m.edu + h.occup
                       + BMI + age.1birth + f.edu + p.occup + wealth, data = df,
                       treatment = W, split.Rule = "TOT", cv.option = "matching", 
                       cv.Honest = F, split.Bucket = F, xval = 10)
pi_Y<-pnorm(predict(CTOT_fit))
ATE_hat_TOT<-mean(pi_Y[df[,zName]==1])-mean(pi_Y[df[,zName]==0])
ATE_bias_TOT = as.numeric( abs( (ATE_hat_TOT - ATE_true) / ATE_true * 100 ) )
CATE_hat = as.vector(predict(CTOT_fit))
CATE_RMSE_CTOT = sqrt(mean((CATE_hat - CATE_true)^2))

#-----------------------------------------------------------------------------------------------------
# Causal Boosting (with and without PS-adjustment).
# The following code was written for a rough beta version of the 'causalLearning' package and might have 
# to be modified for the published version.
#-----------------------------------------------------------------------------------------------------
## CB
X = as.matrix(df[,xNames])
Z = df[,zName]
Y = df$y_fact
ps_adjus = FALSE
CB_fit = causalLearning::cv.causalBoosting(X, Z, Y,
                                           num.trees = 300, maxleaves = 4, splitSpread = 0.2,
                                           type.measure = 'effect', nfolds = 3,
                                           propensity = ps_adjus)
CATE_hat = predict(CB_fit, X)
ATE_hat = mean(CATE_hat)
ATE_bias_CB0 = as.numeric( abs( (ATE_hat - ATE_true) / ATE_true * 100 ) )
CATE_RMSE_CB0 = sqrt(mean((CATE_hat - CATE_true)^2))

out = list(n_samp=n_samp,
           ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
           ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF,
           ATE_bias_CB0=ATE_bias_CB0,  
           ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, ATE_coverage_CF=ATE_coverage_CF,
           ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
           CATE_RMSE_null=CATE_RMSE_null, 
           CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
           CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB0=CATE_RMSE_CB0
)

data.matrix(out)

##Export result##
fname= paste('realdata_underweight_para_result', '_outcome.Rdata', sep = '')
object= list(n_samp=n_samp,
             ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
             ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF,
             ATE_bias_CB0=ATE_bias_CB0, #ATE_bias_CB1=ATE_bias_CB1, 
             ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, ATE_coverage_CF=ATE_coverage_CF,
             ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
             CATE_RMSE_null=CATE_RMSE_null, 
             CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
             CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB0=CATE_RMSE_CB0,
             CATE_RMSE_CT=CATE_RMSE_CT, CATE_RMSE_CTOT=CATE_RMSE_CTOT
             #CATE_RMSE_CB1=CATE_RMSE_CB1 
)
save(object, file = fname)

load(file = paste('realdata_underweight_para_result', '_outcome.Rdata', sep=''))
attach(object)
ATE<-data.frame(ATE_bias_BART0, ATE_bias_BART1, ATE_bias_CF, ATE_bias_CB0)
CATE<-data.frame(CATE_RMSE_BART0, CATE_RMSE_BART1, CATE_RMSE_CF, CATE_RMSE_CB0, CATE_RMSE_CT, CATE_RMSE_CTOT)

(length(which(y_fact==1))/length(y_fact))*100
(length(which(data$s.child==0))/length(data$s.child))*100

PS1<-data.frame(x=PS[which(df[,zName]==0)])
PS2<-data.frame(x=PS[which(df[,zName]==1)])
length(PS2$x)
hist(PS2$x)
hist(PS1$x)

length(PS[df[,zName]==1])
length(PS[df[,zName]==0])
PS1<-data.frame(x=PS[df[,zName]==0])
PS2<-data.frame(x=PS[df[,zName]==1])

require(tidyverse)
options(scipen = 999)
d<-rbind(PS1 %>% 
           mutate(var = "Control"),
         PS2 %>% 
           mutate(var = "Treatment"))

ggplot(d,aes(x, group = var, color = var, fill = var, alpha = 0.2))+ 
  geom_histogram(breaks=seq(0.4,1,by=0.01), 
                 col="red",
                 alpha=0.2)+
  labs(title = "Histogram for Propensity scores", x="Propensity", y="Density")

#--------------------------------------------------------------------------------------
#Propensity_histogram
#--------------------------------------------------------------------------------------
prs_df <- data.frame(pr_score = predict(log_fit_Z, type = "response"),
                     s.child = log_fit_Z$model$s.child)
prs_df1 <- data.frame(pr_score = PS,
                      treat = BART_fit_Z$yhat.train)
library(tidyverse)
labs <- paste("Child_size:", c("Treatment", "Control"))
prs_df1 %>%
  mutate(s.child = ifelse(s.child == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "black",fill="blue", alpha=0.2) +
  facet_wrap(~s.child) +
  xlab("Propensity_Score")
