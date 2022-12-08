## Overview
<div align= "justify">This project was done while I was doing my Master's degree at ISRT of the University of Dhaka. I have used only R language for this project.
  
## Introduction
<div align= "justify">Quantifying the causal effect of a treatment variable in the socio-demographic study is of growing interest but is particularly challenging because of the observational nature of the data and the possibility of the presence of confounding variables. While exciting developments have been made in the machine learning field in devising methods for estimating causal impacts, their use in demographic survey data is very limited. The aim of this study is to make a systematic comparison of cutting-edge machine-learning methods using Bangladesh Demographic Health Survey (BDHS) data. The research question of particular interest is whether the size at birth of under-five children in Bangladesh has a causal influence on children’s current BMI after controlling for potential confounders including children’s age, gender, parent’s education, and family wealth. The competing causal estimation models, namely causal tree, causal forest, and Bayesian Additive Regression Trees (BART) are different variants of standard classification trees specifically modified to perform causal inference and estimated either with classical or Bayesian approach. Simulation experiments are designed to emulate response and explanatory variables similar to those in the real survey data and preliminary results based on different scenarios and replications suggest the superior performance of causal forest and BART in terms of low bias in conditional risk difference. These methods are then applied to the actual survey data to measure the causal effect of size at birth on children’s BMI. The estimated causal relationship highlights the importance of precautionary measures as early as the mother’s pregnancy period rather than an intervention following the child’s birth. This is a crucial policy decision for a country desperately trying to reduce its concerningly high prevalence of childhood malnutrition.</div> 

The methods used in this paper are:
*	Bayesian Additive Regression trees (BART)
*	Causal Tree and Transform Outcome Tree
*	Causal Forest
*	Causal Boosting

##	Bayesian Additive Regression trees (BART): 
<div align= "justify">Bayesian Additive Regression Trees (BART) is a sum-of-trees model for approximating an unknown function ff. Like other ensemble methods, every tree acts as a weak learner, explaining only part of the result. All these trees are of a particular kind called decision trees. The decision tree is a very interpretable and flexible model but it is also prone to overfitting. To avoid overfitting, BART uses a regularization prior that forces each tree to be able to explain only a limited subset of the relationships between the covariates and the predictor variable.</div> 

##	Causal Tree and Transform Outcome Tree: 
<div align= "justify">Causal Tree is a machine learning technique developed by the economists Susan Athey and Guido Imbens for automatically estimating heterogeneous treatment effects conditional on a large number of confounding variables. Causal Tree Learning leverages a machine learning algorithm known as decision tree learning to identify an optimal strategy for splitting observed individuals into groups to estimate heterogeneous treatment effects. The formulation of Causal Tree Learning by Athey and Imbens utilizes two foundational concepts in heterogeneous treatment effects and decision tree learning.</div> 


## Causal Forest
<div align= "justify">Causal forests are the extension of Breiman’s widely used random forest algorithm (Breiman, 2001) for the purpose of estimating heterogeneous treatment effects. They can be implemented in different ways depending on which specific algorithm is used to grow a forest and which splitting rule is applied to maximize heterogeneity in the treatment effect. Here, we use the causal forest implementation based on the generalized random forests (GRF) method recently proposed by Athey et al. (2019). Their resulting framework presents a flexible method for non-parametric statistical estimation and inference with formal asymptotic guarantees. Generalized random forest is a practical and computationally efficient way of growing forest-based weighting functions that express heterogeneity in a key parameter of interest.</div> 

##	Causal Boosting: 
<div align= "justify">As we know, an alternative to a random forest for least squares regression is boosted trees, and boosting builds up a function approximation by successively fitting weak learners to the residuals of the model at each step, Powers et al. (2018) proposed causal boosting recently as an alternative to causal forests that adapts least squares boosting for regression (Friedman, 2001) to the problem of heterogeneous treatment effects estimation.</div> 


## Findings: 
* <div align= "justify">From simulation studies we found that BART, causal forest and causal boosting perform consistently well for estimating child-specific CATEs (conditional risk differences) and overall significantly better than logistic regression, causal tree and causal transformed outcome tree.</div> 
* <div align= "justify">When applied to observed data, these methods predicted sizeable treatment effect (risk difference), particularly for stunting and underweight.</div>
* <div align= "justify">Findings suggest formulation of policies/interventions designed to effectively reduce low birth weight, possibly through effective antenatal care and supply of nutritional food for pregnant mothers.</div>
