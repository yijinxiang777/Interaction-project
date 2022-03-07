
# install.packages("locfit")
library(locfit)
library(dplyr)
library(tidyr)
library(ggplot2)
library(doBy)
# code to load data from the cluster computation
setwd('/Users/xiangyijin/Desktop/Machine-learning/interaction_project/code_for_cluster/multi_int/result_500')

results_temp<-matrix(NA, ncol = 15)
for(i in c(seq(from = -0.5, to =1,by=0.1))){
  load(paste("/Users/xiangyijin/Desktop/Machine-learning/interaction_project/code_for_cluster/multi_int/result_500/mult_int_",i,"_0120",".rda",sep=""))
  results_temp = rbind(results_temp,as.matrix(result_multi_final))
}
result_multi_final<- as.data.frame(results_temp[-1,])

names(result_multi_final) <-c("seed","pi","marginal_y_value",
                              "marginal_x_value","marginal_m_value",
                              "OR_x1_m1","OR_x1_m0","OR_x0_m1",
                              "tX","tXM","mult_interaction_estimated","log_mult_interaction_estimated",
                              "addtive_interacttion_true","add_interaction_estimated","log_add_interaction_estimated")

# Gain the mean of estimates 
result_multi_final_summary <-result_multi_final%>%group_by(pi,tX, tXM)%>%
  summarise(mean_OR_x1_m1=mean(OR_x1_m1),
            mean_OR_x1_m0=mean(OR_x1_m0),
            mean_OR_x0_m1 =mean(OR_x0_m1),
            mean_add_interaction_true = mean(addtive_interacttion_true,na.rm =TRUE),
            mean_add_interaction_estimated = mean(add_interaction_estimated,na.rm =TRUE),
            sd_add_interaction_estimated = sd(add_interaction_estimated,na.rm = TRUE),
            mean_mult_interaction_true = mean(exp(tXM),na.rm =TRUE),
            mean_mult_interaction_estimated = mean(mult_interaction_estimated,na.rm =TRUE),
            sd_mult_interaction_estimated = sd(mult_interaction_estimated,na.rm = TRUE),
            mean_log_mult_interaction_true = mean(tXM,na.rm = TRUE),
            mean_log_mult_interaction_estimated= mean(log_mult_interaction_estimated,na.rm = TRUE),
            sd_log_mult_interaction_estimated = sd(log_mult_interaction_estimated,na.rm = TRUE))

hist(result_multi_final_summary$mean_mult_interaction_estimated,xlab = "Estimated multiplicative interaction",main = '')
hist(result_multi_final_summary$mean_log_mult_interaction_estimated,xlab = "Log transformed estimated multiplicative interaction",main = '')
hist(result_multi_final_summary$mean_add_interaction_estimated,xlab = "Estimated additive interaction",main = '')
hist(result_multi_final_summary$mean_log_add_interaction_estimated,xlab = "Log transformed estimated additive interaction",main = '')

result_multi_final_summary$bias_log_multi<- (result_multi_final_summary$mean_log_mult_interaction_estimated - result_multi_final_summary$mean_log_mult_interaction_true)/result_multi_final_summary$mean_log_mult_interaction_true
result_multi_final_summary$bias_add<- (result_multi_final_summary$mean_add_interaction_estimated - result_multi_final_summary$mean_add_interaction_true)/result_multi_final_summary$mean_add_interaction_true
result_multi_final_summary$bias_log_multi_sigma<- result_multi_final_summary$sd_log_mult_interaction_estimated/result_multi_final_summary$mean_log_mult_interaction_true
result_multi_final_summary$bias_add_sigma<- result_multi_final_summary$sd_add_interaction_estimated/result_multi_final_summary$mean_add_interaction_true

#Create the label for each facet 
result_multi_final_summary$label <- paste("OR_1_0=OR_0_1=",exp(result_multi_final_summary$tX),sep="")
result_multi_final_summary$label_int <- paste("True multi int=",exp(result_multi_final_summary$tXM),sep="")
names(result_multi_final_summary)
save(result_multi_final_summary,file = "multi_0220.RData")
load("multi_0220.RData")
#################plot log transformed interaction on multiplicative scale#######
result_multi_final_summary %>% mutate(across(label_int, factor, 
                                                 levels=c("True multi int=0.8", "True multi int=0.9",
                                                          "True multi int=1","True multi int=1.2",
                                                          "True multi int=1.4","True multi int=1.6",
                                                          "True multi int=1.8","True multi int=2")))%>% 
  filter((exp(tX) ==0.8|near(exp(tX),1.2)|exp(tX) ==1.8)&
           (near(exp(tXM),0.8)|near(exp(tXM),0.9)|
              near(exp(tXM),1.2)|
              near(exp(tXM),1.4)|near(exp(tXM),1.6)|
              near(exp(tXM),1.8)|near(exp(tXM),2)))%>% 
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

result_multi_final_summary %>% mutate(across(label_int, factor, 
                                             levels=c("True multi int=0.8", "True multi int=0.9",
                                                      "True multi int=1","True multi int=1.2",
                                                      "True multi int=1.4","True multi int=1.6",
                                                      "True multi int=1.8","True multi int=2")))%>% 
  filter((exp(tX) ==0.8|near(exp(tX),1.2)|exp(tX) ==1.8)&
           (near(exp(tXM),0.8)|near(exp(tXM),0.9)|
              near(exp(tXM),1.2)|
              near(exp(tXM),1.4)|near(exp(tXM),1.6)|
              near(exp(tXM),1.8)|near(exp(tXM),2)))%>% 
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
result_multi_final_summary %>% mutate(across(label_int, factor, 
                                           levels=c("True multi int=0.8", "True multi int=0.9",
                                                    "True multi int=1","True multi int=1.2",
                                                    "True multi int=1.4","True multi int=1.6",
                                                    "True multi int=1.8","True multi int=2")))%>% 
  filter((exp(tX) ==0.8|near(exp(tX),1.2)|exp(tX) ==1.8)&
           (near(exp(tXM),0.8)|near(exp(tXM),0.9)|
             near(exp(tXM),1.2)|
              near(exp(tXM),1.4)|near(exp(tXM),1.6)|
              near(exp(tXM),1.8)|near(exp(tXM),2)))%>% 
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
result_multi_final_summary %>% mutate(across(label_int, factor, 
                                             levels=c("True multi int=0.8", "True multi int=0.9",
                                                      "True multi int=1","True multi int=1.2",
                                                      "True multi int=1.4","True multi int=1.6",
                                                      "True multi int=1.8","True multi int=2")))%>% 
  filter((exp(tX) ==0.8|near(exp(tX),1.2)|exp(tX) ==1.8)&
           (near(exp(tXM),0.8)|near(exp(tXM),0.9)|
              near(exp(tXM),1.2)|
              near(exp(tXM),1.4)|near(exp(tXM),1.6)|
              near(exp(tXM),1.8)|near(exp(tXM),2)))%>% 
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
