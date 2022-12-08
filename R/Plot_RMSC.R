rm(list=ls())
setwd("E:\\Study\\MS\\Thesis_Article")

load(file = paste('Non_Para_Stunt_result', '_outcome.Rdata', sep=''))
attach(para)

CATE_RMSE_null<-rep(CATE_RMSE_null,50)
a1<-data.frame(group="NULL", RMSE= CATE_RMSE_null)
a2<-data.frame(group="BART0", RMSE= CATE_RMSE_BART0)
a3<-data.frame(group="BART1", RMSE= CATE_RMSE_BART1)
a4<-data.frame(group="CF", RMSE= CATE_RMSE_CF)
a5<-data.frame(group="CB", RMSE= CATE_RMSE_CB)
a6<-data.frame(group="LR0", RMSE= CATE_RMSE_log)
#a61<-data.frame(group="LR01", RMSE= CATE_RMSE_log1)
a7<-data.frame(group="LR1", RMSE= CATE_RMSE_log_PS)
#a71<-data.frame(group="LR11", RMSE= CATE_RMSE_log_PS1)
a8<-data.frame(group="CT", RMSE= CATE_RMSE_CT)
a9<-data.frame(group="CTOT", RMSE= CATE_RMSE_CTOT)

plot.data<-rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9)

library(ggplot2)
pl1<-ggplot(plot.data, aes(x=group, y=RMSE, fill=group)) + 
  geom_boxplot()+ylim(-0.1,0.6)+
  theme(legend.position = "none", text = element_text(size=15), axis.text.x = element_text(angle=0, hjust=0.5))
pl1

load(file = paste('Para_Stunt_result', '_outcome.Rdata', sep=''))
attach(para)

CATE_RMSE_null<-rep(CATE_RMSE_null,50)
a1<-data.frame(group="NULL", RMSE= CATE_RMSE_null)
a2<-data.frame(group="BART0", RMSE= CATE_RMSE_BART0)
a3<-data.frame(group="BART1", RMSE= CATE_RMSE_BART1)
a4<-data.frame(group="CF", RMSE= CATE_RMSE_CF)
a5<-data.frame(group="CB", RMSE= CATE_RMSE_CB)
a6<-data.frame(group="LR0", RMSE= CATE_RMSE_log)
a7<-data.frame(group="LR1", RMSE= CATE_RMSE_log_PS)
a8<-data.frame(group="CT", RMSE= CATE_RMSE_CT)
a9<-data.frame(group="CTOT", RMSE= CATE_RMSE_CTOT)

plot.data<-rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9)

library(ggplot2)
pl2<-ggplot(plot.data, aes(x=group, y=RMSE, fill=group)) + 
  geom_boxplot()+ylim(-0.1,0.6)+
  theme(legend.position = "none", text = element_text(size=15), axis.text.x = element_text(angle=0, hjust=0.5))
pl2
gridExtra::grid.arrange(pl1, pl2, ncol=2)

CATE_RMSE<-c(0.25015, 0.12531, 0.12294, 0.131, 0.14600, 0.61568, 0.61577, 0.56279, 0.48945)
CATE_RMSE<-c(0.25064, 0.12056, 0.12193, 0.130, 0.16329, 0.61675, 0.61679, 0.56423, 0.49361)
CATE_RMSE<-c(0.32129, 0.15450, 0.15466, 0.160, 0.18128, 0.64682, 0.64692, 0.54058, 0.50670)
CATE_RMSE<-c(0.31641, 0.14564, 0.14534, 0.151, 0.18645, 0.64395, 0.64412, 0.54266, 0.50690)
CATE_RMSE<-c(0.14041, 0.06954, 0.07476, 0.073, 0.09240, 0.30091, 0.30171, 0.46369, 0.35496)
CATE_RMSE<-c(0.13154, 0.06007, 0.06892, 0.068, 0.09307, 0.29438, 0.29493, 0.46768, 0.35704)
a1<-data.frame(group="NULL", RMSE= CATE_RMSE[1])
a2<-data.frame(group="BART0", RMSE= CATE_RMSE[2])
a3<-data.frame(group="BART1", RMSE= CATE_RMSE[3])
a4<-data.frame(group="CF", RMSE= CATE_RMSE[4])
a5<-data.frame(group="CB", RMSE= CATE_RMSE[5])
a6<-data.frame(group="LR0", RMSE= CATE_RMSE[6])
a7<-data.frame(group="LR1", RMSE= CATE_RMSE[7])
a8<-data.frame(group="CT", RMSE= CATE_RMSE[8])
a9<-data.frame(group="CTOT", RMSE= CATE_RMSE[9])

plot.data<-rbind(a1, a2, a3, a4, a5, a6, a7, a8, a9)

ggplot(plot.data, aes(x=group, y=RMSE)) + 
  geom_point(shape = 23, fill = "maroon", size = 4)+
  theme(legend.position = "none", text = element_text(size=15), 
        axis.text.x = element_text(angle=0, hjust=0.5))
