# Homework 6
John Weng  
November 18, 2015  

Explore the dependence of the probability that a person has positive self-esteem on log annual income (Income2005), intelligence (AFQT), years of education (educ), and gender (data= ex1223.csv).

**Organizing Data**
In first step, I need to do some data manipulations. I created E1 as to group those with Esteem1 = 1 and Esteem1 that are not equal 1. Next. I added l.Income2005 as the logged Income2005. 


```r
library("Sleuth3")
set.seed(1234)

E1 <- 0
E1[ex1223$Esteem1 != 1] = 0 
E1[ex1223$Esteem1 == 1] = 1
ex1223$E1 <- E1

l.Income2005 <- log(ex1223$Income2005)
ex1223$l.Income2005 <- l.Income2005 
```

Here I created the the Training and the Test data set. They are ex1223.train and  ex1223.test.

```r
dev <- sample(2, nrow(ex1223), replace=TRUE, prob=c(0.67, 0.33))
ex1223.train <- ex1223[dev==1,]
ex1223.test <- ex1223[dev==2,]
```
**Determine the Model**
The first model.1, I have Income2005, AFQT, Educ, Gender as my predictors. Here Income2005 is not logged. By backward elimination, I have reduce to to just AFQT and Educ. The third model, model.3, does seem to best model by AIC. 

```r
model.1 <- glm( E1 ~ Income2005+AFQT+Educ+Gender, data = ex1223.train, family="binomial")
summary(model.1) # AIC: 2344.9
```

```
## 
## Call:
## glm(formula = E1 ~ Income2005 + AFQT + Educ + Gender, family = "binomial", 
##     data = ex1223.train)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.782  -1.226   0.839   1.045   1.431  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.114e+00  3.055e-01  -3.647 0.000265 ***
## Income2005   1.041e-06  1.290e-06   0.807 0.419416    
## AFQT         9.138e-03  2.207e-03   4.140 3.47e-05 ***
## Educ         6.599e-02  2.559e-02   2.578 0.009925 ** 
## Gendermale  -1.560e-02  1.038e-01  -0.150 0.880564    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2399.9  on 1764  degrees of freedom
## Residual deviance: 2334.9  on 1760  degrees of freedom
## AIC: 2344.9
## 
## Number of Fisher Scoring iterations: 4
```

```r
model.2 <- update(model.1, ~ . - Gender) # remove model
summary(model.2) # AIC: 2343
```

```
## 
## Call:
## glm(formula = E1 ~ Income2005 + AFQT + Educ, family = "binomial", 
##     data = ex1223.train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7731  -1.2268   0.8402   1.0456   1.4343  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.126e+00  2.949e-01  -3.819 0.000134 ***
## Income2005   9.810e-07  1.224e-06   0.802 0.422658    
## AFQT         9.129e-03  2.206e-03   4.138 3.51e-05 ***
## Educ         6.653e-02  2.534e-02   2.626 0.008648 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2399.9  on 1764  degrees of freedom
## Residual deviance: 2335.0  on 1761  degrees of freedom
## AIC: 2343
## 
## Number of Fisher Scoring iterations: 4
```

```r
model.3 <- update(model.2, ~ . - Income2005 ) # remove income
summary(model.3) # AIC: 2341.6
```

```
## 
## Call:
## glm(formula = E1 ~ AFQT + Educ, family = "binomial", data = ex1223.train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7117  -1.2244   0.8416   1.0450   1.4343  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.152339   0.293199  -3.930 8.49e-05 ***
## AFQT         0.009390   0.002182   4.303 1.69e-05 ***
## Educ         0.070781   0.024799   2.854  0.00431 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2399.9  on 1764  degrees of freedom
## Residual deviance: 2335.6  on 1762  degrees of freedom
## AIC: 2341.6
## 
## Number of Fisher Scoring iterations: 4
```
model.4, I have l.Income2005, AFQT, Educ, Gender as my predictors. Here l.Income2005 is logged. By backward elimination, I have reduce to to just AFQT and Educ. model.5, does seem to best model by AIC, even though Gender appears to be statisticsally insignficent (p-value: 0.010045)

```r
model.4 <- glm( E1 ~ l.Income2005+AFQT+Educ+Gender, data = ex1223.train, family="binomial")
summary(model.4) # AIC: 2341.5
```

```
## 
## Call:
## glm(formula = E1 ~ l.Income2005 + AFQT + Educ + Gender, family = "binomial", 
##     data = ex1223.train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7205  -1.2211   0.8334   1.0428   1.5144  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -2.164920   0.580608  -3.729 0.000192 ***
## l.Income2005  0.114415   0.056298   2.032 0.042124 *  
## AFQT          0.008688   0.002215   3.922 8.78e-05 ***
## Educ          0.062784   0.025201   2.491 0.012725 *  
## Gendermale   -0.057849   0.104367  -0.554 0.579388    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2399.9  on 1764  degrees of freedom
## Residual deviance: 2331.5  on 1760  degrees of freedom
## AIC: 2341.5
## 
## Number of Fisher Scoring iterations: 4
```

```r
model.5 <- update(model.4, ~ . - Gender) # remove Gender
summary(model.5) # AIC: 2339.8
```

```
## 
## Call:
## glm(formula = E1 ~ l.Income2005 + AFQT + Educ, family = "binomial", 
##     data = ex1223.train)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.730  -1.221   0.833   1.041   1.492  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -2.110637   0.572319  -3.688 0.000226 ***
## l.Income2005  0.104306   0.053272   1.958 0.050232 .  
## AFQT          0.008666   0.002215   3.913 9.12e-05 ***
## Educ          0.064438   0.025032   2.574 0.010045 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2399.9  on 1764  degrees of freedom
## Residual deviance: 2331.8  on 1761  degrees of freedom
## AIC: 2339.8
## 
## Number of Fisher Scoring iterations: 4
```

```r
model.6 <- update(model.5, ~ . - l.Income2005 ) # remove income
summary(model.6) # AIC: 2341.6
```

```
## 
## Call:
## glm(formula = E1 ~ AFQT + Educ, family = "binomial", data = ex1223.train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.7117  -1.2244   0.8416   1.0450   1.4343  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.152339   0.293199  -3.930 8.49e-05 ***
## AFQT         0.009390   0.002182   4.303 1.69e-05 ***
## Educ         0.070781   0.024799   2.854  0.00431 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2399.9  on 1764  degrees of freedom
## Residual deviance: 2335.6  on 1762  degrees of freedom
## AIC: 2341.6
## 
## Number of Fisher Scoring iterations: 4
```
*Are both variables needed to obtain the best classification?*
No not all variables are need to obtain the best classification.
*Which classifier is better?*
l.Income2005, AFQT, and Educ

**confusion table**
Fitting the model.5 in the test data set. To Obtain  "confusion table" for my classifier. 

```r
pred <- predict(model.5, newdata=ex1223.test,type = "response")
pred <- round(pred)
t = table(Esteem = ex1223.test$E1,Esteem.Pred = pred)
catnames = levels(ex1223.train$E1)
rownames(t) <- c("Positive","Not Positive")
colnames(t) <- c("Positive","Not Positive")
t
```

```
##               Esteem.Pred
## Esteem         Positive Not Positive
##   Positive           92          243
##   Not Positive       84          400
```
