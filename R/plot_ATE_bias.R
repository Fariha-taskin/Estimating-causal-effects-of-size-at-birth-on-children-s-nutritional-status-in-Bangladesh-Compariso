rm(list=ls())
setwd("E:\\Study\\MS\\Thesis_Article")

load(file = paste('Non_Para_Wasting_result', '_outcome.Rdata', sep=''))
attach(para)

ATE_bias_null<-rep(ATE_bias_null,50)
a1<-data.frame(group="NULL", absolute_bias= ATE_bias_null)
a2<-data.frame(group="BART0", absolute_bias= ATE_bias_BART0)
a3<-data.frame(group="BART1", absolute_bias= ATE_bias_BART1)
a4<-data.frame(group="CF", absolute_bias=  ATE_bias_CF)
a5<-data.frame(group="CB", absolute_bias= ATE_bias_CB)
a6<-data.frame(group="LR0", absolute_bias= ATE_bias_log)
a7<-data.frame(group="LR1", absolute_bias= ATE_bias_log_PS)

plot.data<-rbind(a1,a2,a3,a4,a5,a6, a7)
library(ggplot2)
pl1<-ggplot(plot.data, aes(x=group, y=absolute_bias, fill=group)) + geom_boxplot()+
  theme(legend.position = "none", text = element_text(size=15), axis.text.x = element_text(angle=0, hjust=0.5))
pl1

load(file = paste('Para_Wasting_result', '_outcome.Rdata', sep=''))
attach(para)
ATE_bias_null<-rep(ATE_bias_null,50)
a1<-data.frame(group="NULL", absolute_bias= ATE_bias_null)
a2<-data.frame(group="BART0", absolute_bias= ATE_bias_BART0)
a3<-data.frame(group="BART1", absolute_bias= ATE_bias_BART1)
a4<-data.frame(group="CF", absolute_bias=  ATE_bias_CF)
a5<-data.frame(group="CB", absolute_bias= ATE_bias_CB)
a6<-data.frame(group="LR0", absolute_bias= ATE_bias_log)
a7<-data.frame(group="LR1", absolute_bias= ATE_bias_log_PS)
plot.data<-rbind(a1,a2,a3,a4,a5,a6,a7)

pl2<-ggplot(plot.data, aes(x=group, y=absolute_bias, fill=group)) + geom_boxplot()+
  theme(legend.position = "none", text = element_text(size=15), axis.text.x = element_text(angle=0, hjust=0.5))
pl2

gridExtra::grid.arrange(pl1, pl2, ncol=2)

ATE_bias<-c(8.06836, 45.34189, 43.16246, 46.584, 53.00482, 75.99253, 75.98610)
ATE_bias<-c(8.98464, 46.36539, 46.12229, 47.166, 61.97801, 76.99253, 76.60832)
ATE_bias<-c(8.57276, 42.19706, 39.65205, 40.885, 49.10631, 74.95679, 74.97424)
ATE_bias<-c(8.57930, 43.15128, 42.09745, 41.594, 53.54993, 75.10577, 75.12311)
ATE_bias<-c(7.52306, 41.99193, 41.39693, 43.227, 62.02407, 74.86885, 74.89316)
ATE_bias<-c(5.76991, 42.19057, 48.10840, 41.369, 66.23356, 74.44698, 74.47170)

a1<-data.frame(group="NULL", absolute_bias= ATE_bias[1])
a2<-data.frame(group="BART0", absolute_bias= ATE_bias[2])
a3<-data.frame(group="BART1", absolute_bias= ATE_bias[3])
a4<-data.frame(group="CF", absolute_bias=  ATE_bias[4])
a5<-data.frame(group="CB", absolute_bias= ATE_bias[5])
a6<-data.frame(group="LR0", absolute_bias= ATE_bias[6])
a7<-data.frame(group="LR1", absolute_bias= ATE_bias[7])
plot.data<-rbind(a1,a2,a3,a4,a5,a6,a7)

ggplot(plot.data, aes(x=group, y=absolute_bias)) + 
  geom_point(shape = 23, fill = "maroon", size = 4)+
  theme(legend.position = "none", text = element_text(size=15), 
        axis.text.x = element_text(angle=0, hjust=0.5))


ggplot(plot.data, aes(x=group, y=absolute_bias)) + 
  geom_point(shape = 23, aes(fill = group), size = 5)+
  theme(legend.position = "none", text = element_text(size=15), 
        axis.text.x = element_text(angle=0, hjust=0.5))

ggplot(plot.data, aes(x=group, y=absolute_bias)) + geom_point(aes(shape = group,  fill = "maroon"), size = 4)+
  scale_shape_manual(values=c(21:25, 8, 11))+
  theme(legend.position = "none", text = element_text(size=15), axis.text.x = element_text(angle=0, hjust=0.5))



