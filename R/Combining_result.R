################################################################################################
################################### NON-Parametric Estimation ##################################
################################################################################################

load(file = paste('stunting_result', '_outcome.Rdata', sep=''))
attach(object)
names(object)
fname= paste('Non_Para_Stunt_result', '_outcome.Rdata', sep = '')
para<-list(n_samp=n_samp,
           ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
           ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF, ATE_bias_CB=ATE_bias_CB, 
           ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, 
           ATE_coverage_CF=ATE_coverage_CF,
           ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
           CATE_RMSE_null=CATE_RMSE_null, 
           CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
           CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB=CATE_RMSE_CB, 
           CATE_RMSE_CT = CATE_RMSE_CT, CATE_RMSE_CTOT = CATE_RMSE_CTOT,
           ATE_bias_log = ATE_bias_log, ATE_bias_log_PS = ATE_bias_log_PS, 
           ATE_coverage_log = ATE_coverage_log, ATE_coverage_log_PS = ATE_coverage_log_PS, 
           ATE_IL_log = ATE_IL_log, ATE_IL_log_PS = ATE_IL_log_PS, 
           CATE_RMSE_log = CATE_RMSE_log, CATE_RMSE_log_PS = CATE_RMSE_log_PS)
save(para, file = fname)

names(para)

load(file = paste('underweight_result', '_outcome.Rdata', sep=''))
attach(object)
names(object)

fname= paste('Non_Para_Underweight_result', '_outcome.Rdata', sep = '')
para<-list(n_samp=n_samp,
           ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
           ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF, ATE_bias_CB=ATE_bias_CB, 
           ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, 
           ATE_coverage_CF=ATE_coverage_CF,
           ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
           CATE_RMSE_null=CATE_RMSE_null, 
           CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
           CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB=CATE_RMSE_CB, 
           CATE_RMSE_CT = CATE_RMSE_CT, CATE_RMSE_CTOT = CATE_RMSE_CTOT,
           ATE_bias_log = ATE_bias_log, ATE_bias_log_PS = ATE_bias_log_PS, 
           ATE_coverage_log = ATE_coverage_log, ATE_coverage_log_PS = ATE_coverage_log_PS, 
           ATE_IL_log = ATE_IL_log, ATE_IL_log_PS = ATE_IL_log_PS, 
           CATE_RMSE_log = CATE_RMSE_log, CATE_RMSE_log_PS = CATE_RMSE_log_PS)
save(para, file = fname)


names(para)

load(file = paste('wasting_result', '_outcome.Rdata', sep=''))
attach(object)
names(object)

fname= paste('Non_Para_Wasting_result', '_outcome.Rdata', sep = '')
para<-list(n_samp=n_samp,
           ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
           ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF, ATE_bias_CB=ATE_bias_CB, 
           ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, 
           ATE_coverage_CF=ATE_coverage_CF,
           ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
           CATE_RMSE_null=CATE_RMSE_null, 
           CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
           CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB=CATE_RMSE_CB, 
           CATE_RMSE_CT = CATE_RMSE_CT, CATE_RMSE_CTOT = CATE_RMSE_CTOT,
           ATE_bias_log = ATE_bias_log, ATE_bias_log_PS = ATE_bias_log_PS, 
           ATE_coverage_log = ATE_coverage_log, ATE_coverage_log_PS = ATE_coverage_log_PS, 
           ATE_IL_log = ATE_IL_log, ATE_IL_log_PS = ATE_IL_log_PS, 
           CATE_RMSE_log = CATE_RMSE_log, CATE_RMSE_log_PS = CATE_RMSE_log_PS)
save(para, file = fname)

names(para)

################################################################################################
################################### Parametric Estimation ######################################
################################################################################################

load(file = paste('Stunting_Para_result', '_outcome.Rdata', sep=''))
attach(object)
names(object)
fname= paste('Para_Stunt_result', '_outcome.Rdata', sep = '')
para<-list(n_samp=n_samp,
           ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
           ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF, ATE_bias_CB=ATE_bias_CB, 
           ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, 
           ATE_coverage_CF=ATE_coverage_CF,
           ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
           CATE_RMSE_null=CATE_RMSE_null, 
           CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
           CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB=CATE_RMSE_CB, 
           CATE_RMSE_CT = CATE_RMSE_CT, CATE_RMSE_CTOT = CATE_RMSE_CTOT,
           ATE_bias_log = ATE_bias_log, ATE_bias_log_PS = ATE_bias_log_PS, 
           ATE_coverage_log = ATE_coverage_log, ATE_coverage_log_PS = ATE_coverage_log_PS, 
           ATE_IL_log = ATE_IL_log, ATE_IL_log_PS = ATE_IL_log_PS, 
           CATE_RMSE_log = CATE_RMSE_log, CATE_RMSE_log_PS = CATE_RMSE_log_PS)
save(para, file = fname)

names(para)

load(file = paste('Underweight_Para_result', '_outcome.Rdata', sep=''))
attach(object)
names(object)

fname= paste('Para_Underweight_result', '_outcome.Rdata', sep = '')
para<-list(n_samp=n_samp,
           ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
           ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF, ATE_bias_CB=ATE_bias_CB, 
           ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, 
           ATE_coverage_CF=ATE_coverage_CF,
           ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
           CATE_RMSE_null=CATE_RMSE_null, 
           CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
           CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB=CATE_RMSE_CB, 
           CATE_RMSE_CT = CATE_RMSE_CT, CATE_RMSE_CTOT = CATE_RMSE_CTOT,
           ATE_bias_log = ATE_bias_log, ATE_bias_log_PS = ATE_bias_log_PS, 
           ATE_coverage_log = ATE_coverage_log, ATE_coverage_log_PS = ATE_coverage_log_PS, 
           ATE_IL_log = ATE_IL_log, ATE_IL_log_PS = ATE_IL_log_PS, 
           CATE_RMSE_log = CATE_RMSE_log, CATE_RMSE_log_PS = CATE_RMSE_log_PS)
save(para, file = fname)

names(para)

load(file = paste('Wasting_Para_result', '_outcome.Rdata', sep=''))
attach(object)
names(object)

fname= paste('Para_Wasting_result', '_outcome.Rdata', sep = '')
para<-list(n_samp=n_samp,
           ATE_bias_null=ATE_bias_null, ATE_bias_BART0=ATE_bias_BART0,
           ATE_bias_BART1=ATE_bias_BART1, ATE_bias_CF=ATE_bias_CF, ATE_bias_CB=ATE_bias_CB, 
           ATE_coverage_BART0=ATE_coverage_BART0, ATE_coverage_BART1=ATE_coverage_BART1, 
           ATE_coverage_CF=ATE_coverage_CF,
           ATE_IL_BART0=ATE_IL_BART0, ATE_IL_BART1=ATE_IL_BART1, ATE_IL_CF=ATE_IL_CF,
           CATE_RMSE_null=CATE_RMSE_null, 
           CATE_RMSE_BART0=CATE_RMSE_BART0, CATE_RMSE_BART1=CATE_RMSE_BART1,
           CATE_RMSE_CF=CATE_RMSE_CF, CATE_RMSE_CB=CATE_RMSE_CB, 
           CATE_RMSE_CT = CATE_RMSE_CT, CATE_RMSE_CTOT = CATE_RMSE_CTOT,
           ATE_bias_log = ATE_bias_log, ATE_bias_log_PS = ATE_bias_log_PS, 
           ATE_coverage_log = ATE_coverage_log, ATE_coverage_log_PS = ATE_coverage_log_PS, 
           ATE_IL_log = ATE_IL_log, ATE_IL_log_PS = ATE_IL_log_PS, 
           CATE_RMSE_log = CATE_RMSE_log, CATE_RMSE_log_PS = CATE_RMSE_log_PS)
save(para, file = fname)

names(para)
