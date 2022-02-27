#!/usr/bin/env Rscript
# args = commandArgs(trailingOnly=TRUE)
# grab the array id value from the environment variable passed from sbatch
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
# code for cluster
## import arguments from command line
pi_orign<-as.numeric(slurm_arrayid)
pi_array <-seq(from = -0.5, to =1,by=0.1)
pi_want = pi_array[pi_orign]
# load function
# install.packages("SimDesign")
library(SimDesign)
# install.packages("locfit")
library(locfit)
library(dplyr)
library(tidyr)
# install.packages("doBy")
library(doBy)
# create a function for multiplicative interaction 
pi = expit(pi_want)
########################################
mulinterdatagene <- function(n,pi,tX, tM, tXM,seed) {
  # set the value for two exposure
  set.seed(seed)
  x <- rbinom(n,1,pi)
  m <- rbinom(n,1,pi)
  mu <-matrix(-1,n,1)
  Py <- expit(x*tX + m*tM + tXM*x*m +mu) 
  y <- rbinom(n,1,Py)  #rbinom(n,1,pi) pi to py
  D <- data.frame(m,x,y)
  # marginal probability of m,x
  marginal_x_value <- as.matrix(D %>% summarise(marginal_x = sum(x)) )/n
  marginal_m_value <- as.matrix(D %>% summarise(marginal_m = sum(m)) )/n
  marginal_y_value <- as.matrix(D %>% summarise(marginal_y = sum(y)) )/n
  addtive_interacttion_true = round((exp(tX+tM+tXM) - exp(tX) - exp(tM) +1),2)
  # gain odds ratios
  n_x1_m1_d1=count(D%>%filter(x==1,m==1,y==1))
  n_x1_m1_d0=count(D%>%filter(x==1,m==1,y==0))
  n_x0_m0_d1=count(D%>%filter(x==0,m==0,y==1))
  n_x0_m0_d0=count(D%>%filter(x==0,m==0,y==0))
  n_x1_m0_d1=count(D%>%filter(x==1,m==0,y==1))
  n_x1_m0_d0=count(D%>%filter(x==1,m==0,y==0))
  n_x0_m1_d1=count(D%>%filter(x==0,m==1,y==1))
  n_x0_m1_d0=count(D%>%filter(x==0,m==1,y==0))
  #observed OR
  OR_x1_m1 = (n_x1_m1_d1*n_x0_m0_d0)/(n_x1_m1_d0*n_x0_m0_d1)
  names(OR_x1_m1) <- "OR_x1_m1"
  OR_x1_m0 = (n_x1_m0_d1*n_x0_m0_d0)/(n_x1_m0_d0*n_x0_m0_d1)
  names(OR_x1_m0) <- "OR_x1_m0"
  OR_x0_m1 = (n_x0_m1_d1*n_x0_m0_d0)/(n_x0_m1_d0*n_x0_m0_d1)
  names(OR_x0_m1) <- "OR_x0_m1"
  #gain estimates for interactiopn
  # model = glm(y~x+m+x*m, data=D,
  #             family = binomial(link="logit"))
  ####skip the model could not fit the sample####
  model=try(glm(y~x+m+x*m, data=D,
                family = binomial(link="logit")),TRUE)
  result=rep(NA,15) #the number depends on how many variables to store
  if (class(model)[1]=="glm"){
    #Estimate interaction
    #multiplicative  interaction
    mult_interaction_estimated = exp(coef(model)[4])
    log_mult_interaction_estimated = log(mult_interaction_estimated)
    # additive interaction
    add_interaction_estimated = exp(coef(model)[2]+coef(model)[3]+coef(model)[4])- exp(coef(model)[2]) - exp(coef(model)[3]) +1
    log_add_interaction_estimated = log(add_interaction_estimated)
    
    # Combine columns and one raw for each sample
    result[11:15] <- as.matrix(cbind(mult_interaction_estimated,log_mult_interaction_estimated,
                                     addtive_interacttion_true,add_interaction_estimated,log_add_interaction_estimated))
  }
  result[1:10] <-as.matrix(cbind(seed,pi,marginal_y_value,
                                 marginal_x_value,marginal_m_value,
                                 OR_x1_m1,OR_x1_m0,OR_x0_m1,
                                 tX,tXM))
  # return result:
  return(result)
  
}
#Apply the function
# create an empty matrix
result2 <-matrix(NA, ncol = 15)

  for (tX in c(log(seq(from = 0.8, to =1.8,by=0.05)))) {
    n=500
    tM = tX 
    for (tXM in c(log(seq(from = 0.8, to =2,by=0.05)))) {
      # tXM = log(2)
      for (i in 1:100) {
        # i - montro carlo sample size
        seed = i
        result2 <- rbind(result2,matrix(mulinterdatagene(n,pi,tX, tM, tXM,seed),nrow=1, byrow = TRUE))
        }
    }
  }

result_multi_final<- as.data.frame(result2[-1,])
names(result_multi_final) <-c("seed","pi","marginal_y_value",
                              "marginal_x_value","marginal_m_value",
                              "OR_x1_m1","OR_x1_m0","OR_x0_m1",
                              "tX","tXM","mult_interaction_estimated","log_mult_interaction_estimated",
                              "addtive_interacttion_true","add_interaction_estimated","log_add_interaction_estimated")
#Save the output
output = paste("./mult_int",pi_want,"_0221",".rda",sep="")
save(result_multi_final,file=output)

