rm(list=ls())
setwd("E:\\Study\\MS\\Thesis_Article")
library(dplyr)
library(foreign)
library(car)

data<-read.csv("Child_fin.csv", header = T)
stunting11<-data[complete.cases(data$HW70,data$HW71,data$HW72),]
str(stunting11)
### Discard unusual/coded missing values from analysis (on each dependent variable)
stunting11<-stunting11[stunting11$HW70<=1000,] 
stunting11<-stunting11[stunting11$HW71<=1000,]
stunting11<-stunting11[stunting11$HW72<=1000,]

### Create binary variables for each malnutrition scores
l.stunting<-ifelse(stunting11$HW70<= -200,"1","0")
l.underweight<-ifelse(stunting11$HW71<= -200,"1","0")
l.wasting<-ifelse(stunting11$HW72<= -200,"1","0")

l.stunting<-factor(l.stunting, levels=c(0,1), labels=c("No-stunt", "Stunt"))
l.underweight<-factor(l.underweight, levels=c(0,1), labels=c("No-under", "Under"))
l.wasting<-factor(l.wasting, levels=c(0,1), labels=c("No-wast", "Wast"))


### Add binary dependent variables in the main dataset
stunting11<-data.frame(stunting11,l.stunting,l.underweight,l.wasting)

### Summary of dependent variables
summary(stunting11$l.stunting)
summary(stunting11$l.underweight)
summary(stunting11$l.wasting)

### Create a shorter dataset with required variables
stunting <- stunting11%>%
  select(l.underweight, c.age, c.sex, s.child, deliv, interval, bord, m.age, f.age, age.marri, 
         age.1birth, height, reli, p.occup, h.occup, h.edu, peduc, l.child, wealth, s.water, 
         resid, div, h.sex, h.memb,  health, clinic, f.plan, polio, b.f, ante.care)

### Delete missing values on bmi and peduc
stunting_22<-stunting[complete.cases(stunting$c.age),] 
stunting_22<-stunting[complete.cases(stunting_22$s.child),] 
stunting_22<-stunting[complete.cases(stunting_22$deliv),] 
stunting_22<-stunting_22[complete.cases(stunting_22$interval),] 
stunting_22<-stunting_22[complete.cases(stunting_22$f.age),]
stunting_22<-stunting_22[complete.cases(stunting_22$health),]
stunting_22<-stunting_22[complete.cases(stunting_22$p.occup),]
stunting_22<-stunting_22[complete.cases(stunting_22$h.occup),]
stunting_22<-stunting_22[complete.cases(stunting_22$peduc),]
stunting_22<-stunting_22[complete.cases(stunting_22$s.water),]
stunting_22<-stunting_22[complete.cases(stunting_22$health),]
stunting_22<-stunting_22[complete.cases(stunting_22$polio),]
stunting_22<-stunting_22[complete.cases(stunting_22$ante.care),]
stunting_22$age.dif<-stunting_22$f.age-stunting_22$m.age
stunting_22<-stunting_22[,-c(8,9)]
str(stunting_22)
write.csv(stunting_22, "underweight1.csv")
summary(stunting_22)

stunting_22<-read.csv("underweight1.csv")
RHS1 <- "c.age + c.sex + s.child + deliv + interval + bord + age.marri + age.dif + age.1birth + height + reli + p.occup + h.occup + h.edu + peduc + l.child + wealth + s.water + resid + div + h.sex + h.memb +  health + clinic + f.plan + polio + b.f + ante.care"
formula_stu <- as.formula(paste("l.underweight~", RHS1))
stunting_mod1<-glm(formula_stu,stunting_22, family = binomial())
summary(stunting_mod1)
dat<-round(summary(stunting_mod1)$coefficients,5)
write.csv(dat, "Underweight_estimate.csv")


## Load dataset
scenario = 1
dfName = c('underweight1')[scenario]
yName = c('l.underweight')[scenario]
zName = c('s.child')[scenario]
data = read.csv(paste(dfName, '.csv', sep=''), header = T)
xNames = setdiff(names(data), yName)
data[,yName] <- as.factor(data[,yName])
unique(data[,yName])

## Deep learning with H2O
library(h2o)
localH2O = h2o.init(nthreads=-1)
df = as.h2o(data)
splits = h2o.splitFrame(df, c(0.8), seed=5)
train  = h2o.assign(splits[[1]], "train.hex") # 80%
valid  = h2o.assign(splits[[2]], "valid.hex") # 20%

## Grid search of optimal hyperparameters
hyper_params = list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(rep(50,2),rep(50,3),rep(100,2),rep(100,3),c(50,100)),
  input_dropout_ratio=c(0.2,0.5),
  l2=c(0,1e-3)
)
hyper_params

## Fit best model to the training dataset (update the hyperparemeter values based on the output of 'grid')
dff.fit = h2o.deeplearning(x = xNames, y = yName, training_frame = train, validation_frame = valid,
                           hidden = c(50, 100),
                           activation = 'RectifierWithDropout',
                           input_dropout_ratio = 0.2,
                           l2 = 0.00,
                           epochs = 10000,
                           score_training_samples = 0,
                           stopping_metric = "logloss",
                           stopping_tolerance = 1e-3,
                           nfolds = 0
)
h2o.performance(dff.fit, valid = T)

## Get estimated probabilities for the potential outcomes
df_Z0 = df_Z1 = df
df_Z0[,zName] = 0
df_Z1[,zName] = 1
po = array(0, c(nrow(data), 2))
colnames(po) = c('Z0', 'Z1')
po[,1] = as.vector(predict(dff.fit, df_Z0)[,3])
po[,2] = as.vector(predict(dff.fit, df_Z1)[,3])
h2o.shutdown(prompt=FALSE)

## Export results
fName = paste(dfName, '_outcome_model.RData', sep='')
obj = list(df=data, po=po)
save(obj, file = fName)

exp(0.562471)

####NOT ready yet####
#Scenario 2
scenario = 2
dfName = c('stunting2')[scenario]
yName = c('l.stunting')[scenario]
zName = c('ante.care')[scenario]
data = read.csv(paste(dfName, '.csv', sep=''), header = T)
xNames = setdiff(names(data), yName)
data[,yName] <- as.factor(data[,yName])
unique(data[,yName])

## Deep learning with H2O ##
library(h2o)
localH2O = h2o.init(nthreads=-1)
df = as.h2o(data)
splits = h2o.splitFrame(df, c(0.8), seed=5)
train  = h2o.assign(splits[[1]], "train.hex") # 80%
valid  = h2o.assign(splits[[2]], "valid.hex") # 20%

## Grid search of optimal hyperparameters
hyper_params = list(
  activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden=list(rep(50,2),rep(50,3),rep(100,2),rep(100,3),c(50,100)),
  input_dropout_ratio=c(0.2,0.5),
  l2=c(0,1e-3)
)
hyper_params

## Fit best model to the training dataset (update the hyperparemeter values based on the output of 'grid')
dff.fit = h2o.deeplearning(x = xNames, y = yName, training_frame = train, validation_frame = valid,
                           hidden = c(50, 100),
                           activation = 'RectifierWithDropout',
                           input_dropout_ratio = 0.2,
                           l2 = 0.00,
                           epochs = 10000,
                           score_training_samples = 0,
                           stopping_metric = "logloss",
                           stopping_tolerance = 1e-3,
                           nfolds = 0
)
h2o.performance(dff.fit, valid = T)

## Get estimated probabilities for the potential outcomes
df_Z0 = df_Z1 = df
df_Z0[,zName] = 0
df_Z1[,zName] = 1
po = array(0, c(nrow(data), 2))
colnames(po) = c('Z0', 'Z1')
po[,1] = as.vector(predict(dff.fit, df_Z0)[,3])
po[,2] = as.vector(predict(dff.fit, df_Z1)[,3])
h2o.shutdown(prompt=FALSE)

## Export results
fName = paste(dfName, '_outcome_model.RData', sep='')
obj = list(df=data, po=po)
save(obj, file = fName)
