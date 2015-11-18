install.packages("Sleuth3")
library("Sleuth3")

attach(ex1223)
set.seed(1234)

E1 <- 0
E1[ex1223$Esteem1 != 1] = 0 
E1[ex1223$Esteem1 == 1] = 1
ex1223$E1 <- E1

l.Income2005 <- log(ex1223$Income2005)
ex1223$l.Income2005 <- l.Income2005 

dev <- sample(2, nrow(ex1223), replace=TRUE, prob=c(0.67, 0.33))
ex1223.train <- ex1223[dev==1,]
ex1223.test <- ex1223[dev==2,]

attach(ex1223)
sa.model <- glm( Esteem1~	+Imagazine+Inewspaper+Ilibrary+MotherEd+FatherEd+FamilyIncome78+Race+Gender+Educ+Science+Arith+Word+Parag+Numer+Coding+Auto+Math+Mechanic+Elec+AFQT+Income2005 +Esteem2+Esteem3+Esteem4+Esteem5+Esteem6+Esteem7+Esteem8+Esteem9+Esteem10,
  data= ex1223.train)
summary(sa.model)

model.1 <- glm( E1 ~ Income2005+AFQT+Educ+Gender, data = ex1223.train, family="binomial")
summary(model.1) # AIC: 2344.9

model.2 <- update(model.1, ~ . - Gender) # remove model
summary(model.2) # AIC: 2343

model.3 <- update(model.2, ~ . - Income2005 ) # remove income
summary(model.3) # AIC: 2341.6

anova(model.1,model.3)

model.4 <- glm( E1 ~ l.Income2005+AFQT+Educ+Gender, data = ex1223.train, family="binomial")
summary(model.4) # AIC: 2341.5

model.5 <- update(model.4, ~ . - Gender) # remove Gender
summary(model.5) # AIC: 2339.8

model.6 <- update(model.5, ~ . - l.Income2005 ) # remove income
summary(model.6) # AIC: 2341.6

pred <- predict(model.5, newdata=ex1223.test,type = "response")
pred <- round(pred)
t = table(Esteem = ex1223.test$E1,Esteem.Pred = pred)
catnames = levels(ex1223.train$E1)
rownames(t) <- c("Positive","Not Positive")
colnames(t) <- c("Positive","Not Positive")
t
anova(model.1,model.3)



