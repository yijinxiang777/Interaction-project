# Interaction-project
Evaluating the "scale problem" under additive and multiplicative interaction when ML methods are used 

## Generate data with multiplicative interaction (binary exposure, binary modifer, binary outcome, and constant mu (-1))

### 1. Generate data

 + **Py** = expit(**tX**x + **tM**m + **tXM**xm + mu)   
   - tX - effect of x (exposure = 1)    
   - tM − effect of m (modifier = 1)   
   - tXM - effect of x and m combined (exposure = 1 & modifier = 1)  
   - Py - Probability of Y (outcome = 1) 
  
 + Generate dichotomous outcome(**Y**) based on **Py** using binomial distribution  

### 2.  Calculate True multiplicative interaction and true addtion interaction based on parameters for data generation 

   ![equation](https://latex.codecogs.com/svg.image?multint_{True}&space;=&space;e^{tXM})  
   ![equation](https://latex.codecogs.com/svg.image?addint_%7BTrue%7D=e%5E%7BtXM&plus;tX&plus;tM%7D-e%5E%7BtM%7D-e%5E%7BtX%7D&plus;1)
### 3.  Estimated multiplicative interaction and true addtion interaction   
 + Fit data with generalized linear model with logit function as link function   
![image](https://user-images.githubusercontent.com/50306880/155907800-cb75b2c6-b9b0-40fb-9536-39f1c251bfed.png). 

 + Generate estimates using estimated model parameters  
![equation](https://latex.codecogs.com/svg.image?addint_{Estimated}=e^{\gamma_{1}&plus;\gamma_{2}&plus;\gamma_{3}}-e^{\gamma_{1}}-e^{\gamma_{2}}&plus;1)   
 ![equation](https://latex.codecogs.com/svg.image?multint_{Estimated}=e^{\gamma_{3}})   
 
## Generate data with additive interaction
### 1. Generate data

 + **Py** = **tX**x + **tM**m + **tXM**xm + mu   
   - tX - effect of x (exposure = 1)    
   - tM − effect of m (modifier = 1)   
   - tXM - effect of x and m combined (exposure = 1 & modifier = 1)  
   - Py - Probability of Y (outcome = 1)   
**Note: Conditions to meet when tuning the parameters**  
![equation](https://latex.codecogs.com/svg.image?%5Cleft%5C%7B%5Cbegin%7Bmatrix%7D%200%3CtX%20&plus;%20tM%20&plus;%20tXM%20&plus;%20mu%20%3C%201%5C%5C%200%3CtX%20&plus;%20mu%20%3C%201%5C%5C%200%3CtM%20&plus;%20mu%20%3C%201%5C%5C%200%3C%20mu%20%3C%201%5C%5C%5Cend%7Bmatrix%7D%5Cright.)

 + Generate dichotomous outcome(**Y**) based on **Py** using binomial distribution  

### 2.  Calculate True multiplicative interaction and true addtion interaction based on parameters for data generation 

   ![equation](https://latex.codecogs.com/svg.image?addint_{True}&space;=&space;tXM)  
   ![equation](https://latex.codecogs.com/svg.image?multint_%7BTrue%7D=%5Cfrac%7B(mu&plus;tX&plus;tM&plus;tXM)mu%7D%7B(mu&plus;tX)(mu&plus;tM)%7D)
### 3.  Estimated multiplicative interaction and true addtion interaction   
 + Fit data with generalized linear model with logit function as link function   
![image](https://user-images.githubusercontent.com/50306880/155905213-6ed443fc-25ed-4efe-818e-53e8ebad9640.png)   
 + Generate estimates using estimated model parameters  
   ![equation](https://latex.codecogs.com/svg.image?addint_{Estimated}&space;=&space;\gamma_{3})  
   ![equation](https://latex.codecogs.com/svg.image?multint_{True}=\frac{(\gamma_{0}&space;&plus;\gamma_{1}&plus;\gamma_{2}&plus;\gamma_{3})\gamma_{0}}{(\gamma_{0}&plus;\gamma_{1})(\gamma_{0}&plus;\gamma_{2})})
 
