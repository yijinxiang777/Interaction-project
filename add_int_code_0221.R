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
# create a function for additive interaction 
pi = expit(pi_want)
# create a function for linear scale (additive interaction) 

generateaddint <- function(pi,mu_n,tX, tM, tXM,seed) {
  # set the value for two exposure
  set.seed(seed)
  # manipulate the boundaries to make the Py within 0 to 1
  # make a list of n exposure-x, modifier - m, confounder - mu
  x <- rbinom(n,1,pi)
  m <- rbinom(n,1,pi)
  mu <-matrix(mu_n,n,1)
  # combine the three list and generate the probability of y-Py
  D <- data.frame(m,x,mu)
  Py <-(x*tX + m*tM + tXM*x*m +mu)
  # generate the outcome
  y <- rbinom(n,1,Py)  #rbinom(n,1,pi) pi to py
  D <- data.frame(m,x,y)
  # marginal probability of m,x
  marginal_x_value <- as.matrix(D %>% summarise(marginal_x = sum(x)) )/n
  marginal_m_value <- as.matrix(D %>% summarise(marginal_m = sum(m)) )/n
  marginal_y_value <- as.matrix(D %>% summarise(marginal_y = sum(y)) )/n
  # ture multiplicative interaction
  mult_interaction_true = round(((tX + tM + tXM + mu_n)*mu_n/((tX+mu_n)*(tM+mu_n))),2)
  # gain risk ratios
  n_x1_m1_d1=count(D%>%filter(x==1,m==1,y==1))
  n_x1_m1=count(D%>%filter(x==1,m==1))
  R_11 = n_x1_m1_d1/n_x1_m1
  n_x0_m0_d1=count(D%>%filter(x==0,m==0,y==1))
  n_x0_m0=count(D%>%filter(x==0,m==0))
  R_00 = n_x0_m0_d1/n_x0_m0
  n_x1_m0_d1=count(D%>%filter(x==1,m==0,y==1))
  n_x1_m0=count(D%>%filter(x==1,m==0))
  R_10 = n_x1_m0_d1/n_x1_m0
  n_x0_m1_d1=count(D%>%filter(x==0,m==1,y==1))
  n_x0_m1=count(D%>%filter(x==0,m==1))
  R_01 = n_x0_m1_d1/n_x0_m1
  #observed RR
  RR_x1_m1 = R_11/R_00
  names(RR_x1_m1) <- "RR_x1_m1"
  RR_x1_m0 = R_10/R_00
  names(RR_x1_m0) <- "RR_x1_m0"
  RR_x0_m1 = R_01/R_00
  names(RR_x0_m1) <- "RR_x0_m1"
  #gain estimates for interaction
  #linear probability model
  # model = glm(y~x+m+x*m, data=D,family=binomial(link="identity"))
  ####skip the model could not fit the sample#####
  model=try(glm(y~x+m+x*m, data=D,
                family=binomial(link="identity")),TRUE)
  result=rep(NA,13) #the number depends on how many variables to store
  if (class(model)[1]=="glm"){
    #Estimate interaction
    # additive interaction
    add_interaction_estimated =round(coef(model)[4],2)
    # multiplicative  interaction
    P11 = (coef(model)[1] + coef(model)[2] + coef(model)[3] + coef(model)[4])
    P00 = coef(model)[1]
    P10 = coef(model)[1] + coef(model)[2]
    P01 = coef(model)[1] + coef(model)[3]
    mult_interaction_estimated = round((P11*P00/(P10*P01)),2)
    
    # Combine columns and one raw for each sample
    result[11:13] <- as.matrix(cbind(add_interaction_estimated,
                                     mult_interaction_true,mult_interaction_estimated))
  }
  result[1:10] <-as.matrix(cbind(seed,pi,marginal_y_value,
                                 marginal_x_value,marginal_m_value,
                                 RR_x1_m1,RR_x1_m0,RR_x0_m1,
                                 tX,tXM))
  # return result:
  return(result)
  
}
# Create an empty matrix
result_add <-matrix(NA, ncol = 13)
# paramter changed: pi, tX= tM, tXM

for (tX in c(seq(0.05, to =0.15,by=0.01))) {
    # the value of confonder
    mu_n = 0.1
    #effect of m on outcome = effect of x on outcome
    tM = tX 
    #sample size
    n = 500
    for (tXM in c(seq(-0.05, to =0.2,by=0.01))) {
      for (i in 1:100) {
        # i - Monte Carlo simulation size
        seed = i
        result_add <- rbind(result_add,matrix(generateaddint(pi,mu_n,tX, tM, tXM,seed),nrow=1, byrow = TRUE))
      }  
    }
}
result_add_final<- as.data.frame(result_add[-1,])
names(result_add_final) <- c("seed","pi","marginal_y_value",
                             "marginal_x_value","marginal_m_value",
                             "RR_x1_m1","RR_x1_m0","RR_x0_m1",
                             "tX","tXM","add_interaction_estimated",
                             "mult_interaction_true","mult_interaction_estimated")
#Save the output
output = paste("./add_int",pi_want,"_0221",".rda",sep="")
save(result_add_final,file=output)


