
# install.packages("locfit")
library(locfit)
library(dplyr)
library(tidyr)
library(ggplot2)
library(doBy)
# code to load data from the cluster computation
setwd('/Users/xiangyijin/Desktop/Machine-learning/interaction_project/code_for_cluster/add_int/result_add')

results_temp<-matrix(NA, ncol = 13)
for(i in c(seq(from = -0.5, to =1,by=0.1))){
  load(paste("/Users/xiangyijin/Desktop/Machine-learning/interaction_project/code_for_cluster/add_int/result_add/add_int",i,"_0221",".rda",sep=""))
  results_temp = rbind(results_temp,as.matrix(result_add_final))
}
result_add_final<- as.data.frame(results_temp[-1,])
names(result_add_final) <- c("seed","pi","marginal_y_value",
                             "marginal_x_value","marginal_m_value",
                             "RR_x1_m1","RR_x1_m0","RR_x0_m1",
                             "tX","tXM","add_interaction_estimated",
                             "mult_interaction_true","mult_interaction_estimated")

# Gain the mean of estimates 
result_add_final_summary <-result_add_final%>%group_by(pi,tX, tXM)%>%
  summarise(mean_RR_x1_m1=mean(RR_x1_m1),
            mean_RR_x1_m0=mean(RR_x1_m0),
            mean_RR_x0_m1 =mean(RR_x0_m1),
            mean_add_interaction_true =mean(tXM,na.rm =TRUE),
            mean_add_interaction_estimated = mean(add_interaction_estimated,na.rm = TRUE),
            sd_add_interaction_estimated = sd(add_interaction_estimated,na.rm = TRUE),
            mean_mult_interaction_true = mean(mult_interaction_true,na.rm = TRUE),
            mean_mult_interaction_estimated= mean(mult_interaction_estimated,na.rm = TRUE),
            sd_mult_interaction_estimated = sd(mult_interaction_estimated,na.rm = TRUE),
            mean_log_mult_interaction_true = mean(log(mult_interaction_true),na.rm = TRUE),
            mean_log_mult_interaction_estimated= mean(log(mult_interaction_estimated),na.rm = TRUE),
            sd_log_mult_interaction_estimated = sd(log(mult_interaction_estimated),na.rm = TRUE))
# remove those with estimated multiplicative interaction of 0
result_add_final_summary <-result_add_final%>%
  filter(!is.na(add_interaction_estimated)&
           mult_interaction_estimated !=0)%>%
  group_by(pi,tX, tXM)%>%
  summarise(mean_RR_x1_m1=mean(RR_x1_m1),
            mean_RR_x1_m0=mean(RR_x1_m0),
            mean_RR_x0_m1 =mean(RR_x0_m1),
            mean_add_interaction_true =mean(tXM,na.rm =TRUE),
            mean_add_interaction_estimated = mean(add_interaction_estimated,na.rm = TRUE),
            sd_add_interaction_estimated = sd(add_interaction_estimated,na.rm = TRUE),
            mean_mult_interaction_true = mean(mult_interaction_true,na.rm = TRUE),
            mean_mult_interaction_estimated= mean(mult_interaction_estimated,na.rm = TRUE),
            sd_mult_interaction_estimated = sd(mult_interaction_estimated,na.rm = TRUE),
            mean_log_mult_interaction_true = mean(log(mult_interaction_true),na.rm = TRUE),
            mean_log_mult_interaction_estimated= mean(log(mult_interaction_estimated),na.rm = TRUE),
            sd_log_mult_interaction_estimated = sd(log(mult_interaction_estimated),na.rm = TRUE))

hist(result_add_final_summary$mean_mult_interaction_estimated,xlab = "Estimated multiplicative interaction",main = '')
hist(result_add_final_summary$mean_log_mult_interaction_estimated,xlab = "Log transformed estimated multiplicative interaction",main = '')
hist(result_add_final_summary$mean_add_interaction_estimated,xlab = "Estimated additive interaction",main = '')
# create bias varaible
result_add_final_summary$bias_multi<- (result_add_final_summary$mean_mult_interaction_estimated - result_add_final_summary$mean_mult_interaction_true)/result_add_final_summary$mean_mult_interaction_true
result_add_final_summary$bias_add<- (result_add_final_summary$mean_add_interaction_estimated - result_add_final_summary$mean_add_interaction_true)/result_add_final_summary$mean_add_interaction_true
result_add_final_summary$bias_multi_sigma<- result_add_final_summary$sd_mult_interaction_estimated/result_add_final_summary$mean_mult_interaction_true
result_add_final_summary$bias_add_sigma<- result_add_final_summary$sd_add_interaction_estimated/result_add_final_summary$mean_add_interaction_true
result_add_final_summary$bias_log_multi<- (result_add_final_summary$mean_log_mult_interaction_estimated - result_add_final_summary$mean_log_mult_interaction_true)/result_add_final_summary$mean_log_mult_interaction_true
result_add_final_summary$bias_log_multi_sigma<- result_add_final_summary$sd_log_mult_interaction_estimated/result_add_final_summary$mean_log_mult_interaction_true

hist(result_add_final_summary$bias_add_sigma,xlab = "bias additive interaction",main = '')
hist(result_add_final_summary$bias_multi_sigma,xlab = "bias multiplicative interaction",main = '')
hist(result_add_final_summary$bias_log_multi,xlab = "bias log transformed multiplicative interaction",main = '')


#Create the label for each facet 

result_add_final_summary$label <- paste("RD_1_0=RD_0_1=",result_add_final_summary$tX,sep="")

result_add_final_summary$label_int <- paste("True add int=",result_add_final_summary$tXM,sep="")
save(result_add_final_summary,file = "addtive_0220.RData")
load("addtive_0220.RData")

#################plot log transformed interaction on multiplicative scale#######
result_add_final_summary %>% mutate(across(label_int, factor, 
                                           levels=c("True add int=-0.05","True add int=-0.02",
                                                    "True add int=0.05","True add int=0.1",
                                                    "True add int=0.15","True add int=0.2")))%>% 
  filter((tX ==0.05|near(tX,0.1)|tX ==0.15)&
           (near(tXM,-0.05)|near(tXM,-0.02)|near(tXM,0.05)|near(tXM,0.1)|near(tXM,0.15)|near(tXM,0.2)))%>% 
  ggplot(aes(x=as.numeric(pi)))+
  facet_grid(as.factor(label)~label_int) + 
  geom_line(aes(y=mean_log_mult_interaction_estimated, color = "Log transformed multiplicative interaction"),size=1)+
  geom_line(aes(y=mean_add_interaction_estimated, color = "Additive interaction"),size=1)+
  geom_line(aes(y = mean_log_mult_interaction_true),size=0.5, color = "Red",linetype = 2)+
  geom_line(aes(y = mean_add_interaction_true),size=0.5, color = "Blue",linetype = 2)+
  geom_ribbon(aes(ymin = mean_log_mult_interaction_estimated-1.96*sd_log_mult_interaction_estimated, 
                  ymax = mean_log_mult_interaction_estimated+1.96*sd_log_mult_interaction_estimated), 
              alpha = 0.2,fill = "Red")+
  geom_ribbon(aes(ymin = (mean_add_interaction_estimated-1.96*sd_add_interaction_estimated), 
                  ymax = (mean_add_interaction_estimated+1.96*sd_add_interaction_estimated)), 
              alpha = 0.2,fill = "Blue")+
  xlab("Probability of the expsore (Pi)")+
  # Custom the Y scales:
  scale_y_continuous(name = "Interaction")+
  theme_bw()+
  theme(panel.spacing = unit(0.1, "lines"),
        legend.position	= "top")+
  scale_color_manual(values = c(
    'Log transformed multiplicative interaction' = 'Red',
    'Additive interaction' = 'Blue')) +
  labs(color = 'Scale')

result_add_final_summary %>% mutate(across(label_int, factor, 
                                           levels=c("True add int=-0.05","True add int=-0.02",
                                                    "True add int=0.05","True add int=0.1",
                                                    "True add int=0.15","True add int=0.2")))%>% 
  filter((tX ==0.05|near(tX,0.1)|tX ==0.15)&
           (near(tXM,-0.05)|near(tXM,-0.02)|near(tXM,0.05)|near(tXM,0.1)|near(tXM,0.15)|near(tXM,0.2)))%>% 
  ggplot(aes(x=as.numeric(pi)))+
  facet_grid(as.factor(label)~label_int) + 
  geom_line(aes(y=mean_log_mult_interaction_estimated, color = "Log transformed multiplicative interaction"),size=1)+
  geom_line(aes(y=mean_add_interaction_estimated, color = "Additive interaction"),size=1)+
  geom_line(aes(y = mean_log_mult_interaction_true),size=0.5, color = "Red",linetype = 2)+
  geom_line(aes(y = mean_add_interaction_true),size=0.5, color = "Blue",linetype = 2)+
  xlab("Probability of the expsore (Pi)")+
  # Custom the Y scales:
  scale_y_continuous(name = "Interaction")+
  theme_bw()+
  theme(panel.spacing = unit(0.1, "lines"),
        legend.position	= "top")+
  scale_color_manual(values = c(
    'Log transformed multiplicative interaction' = 'Red',
    'Additive interaction' = 'Blue')) +
  labs(color = 'Scale')

##################plot corresponding bias line##########################
result_add_final_summary %>% mutate(across(label_int, factor, 
                                           levels=c("True add int=-0.05","True add int=-0.02",
                                                    "True add int=0.05","True add int=0.1",
                                                    "True add int=0.15","True add int=0.2")))%>% 
  filter((tX ==0.05|near(tX,0.1)|tX ==0.15)&
           (near(tXM,-0.05)|near(tXM,-0.02)|near(tXM,0.05)|near(tXM,0.1)|near(tXM,0.15)|near(tXM,0.2)))%>% 
  ggplot(aes(x=as.numeric(pi)))+
  facet_grid(as.factor(label)~label_int) + 
  geom_line(aes(y=bias_log_multi, color = "Relative bias of log transformed multiplicative interaction"),size=1)+
  geom_line(aes(y=bias_add, color = "Relative bias of additive interaction"),size=1)+
  geom_ribbon(aes(ymin = bias_log_multi-1.96*bias_log_multi_sigma, 
                  ymax = bias_log_multi+1.96*bias_log_multi_sigma), 
              alpha = 0.2,fill = "Red")+
  geom_ribbon(aes(ymin = bias_add-1.96*bias_add_sigma, 
                  ymax = bias_add+1.96*bias_add_sigma), 
              alpha = 0.2,fill = "Blue")+
  xlab("Probability of the expsore (Pi)")+
  # Custom the Y scales:
  scale_y_continuous(name = "Relative Bias of Interaction")+
  theme_bw()+
  theme(panel.spacing = unit(0.1, "lines"),
        legend.position	= "top")+
  scale_color_manual(values = c(
    'Relative bias of log transformed multiplicative interaction' = 'Red',
    'Relative bias of additive interaction' = 'Blue')) +
  labs(color = 'Scale')

result_add_final_summary %>% mutate(across(label_int, factor, 
                                           levels=c("True add int=-0.05","True add int=-0.02",
                                                    "True add int=0.05","True add int=0.1",
                                                    "True add int=0.15","True add int=0.2")))%>% 
  filter((tX ==0.05|near(tX,0.1)|tX ==0.15)&
           (near(tXM,-0.05)|near(tXM,-0.02)|near(tXM,0.05)|near(tXM,0.1)|near(tXM,0.15)|near(tXM,0.2)))%>% 
  ggplot(aes(x=as.numeric(pi)))+
  facet_grid(as.factor(label)~label_int) + 
  geom_line(aes(y=bias_log_multi, color = "Relative bias of log transformed multiplicative interaction"),size=1)+
  geom_line(aes(y=bias_add, color = "Relative bias of additive interaction"),size=1)+
  xlab("Probability of the expsore (Pi)")+
  # Custom the Y scales:
  scale_y_continuous(name = "Relative Bias of Interaction")+
  theme_bw()+
  theme(panel.spacing = unit(0.1, "lines"),
        legend.position	= "top")+
  scale_color_manual(values = c(
    'Relative bias of log transformed multiplicative interaction' = 'Red',
    'Relative bias of additive interaction' = 'Blue')) +
  labs(color = 'Scale')

