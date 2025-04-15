### FULL MODEL
fit <- glm(y ~ x1 + x2 + ... + xp, 
                 data = data, family = "binomial")

performance::check_collinearity(fit)
# Check for Multicollinearity


glmtoolbox::hltest(fit)

# The Hosmer-Lemeshow goodness-of-fit test
# p-value should not be significative
              
# MCFADDEN PSEUDO-R2
model <- glm(y ~ x1 + x2 + ... + xp, data = data, family="binomial")
nullmod <- glm(y ~ 1, data = data, family="binomial")
1-logLik(model)/logLik(nullmod)

# 'log Lik.' (df=)

### ACCURACY OF THE COMPLETE MODEL BY BOOTSTRAPPING

performance_accuracy(
+   model,
+   method = c(#"cv"#, #USE CROSSVALIDATION
+   "boot" #USE BOOTSTRAP
+   	), 
+   k = 5,
+   n = 1000,
+   verbose = TRUE
+ )

# Accuracy of Model Predictions
# this is the result

############################################################################################################

### MACHINE LEARNING

xtabs(~ x1 + x2, data = data)
# there is no zero for nominal variables 
# zeros are present for the quantitative variables

### DATA PARTITION
set.seed(21051986)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

mod.train <- glm(y ~ x1 + x2 + x3, 
              data = train, family = 'binomial')
summary(mod.train)

### PREDICTION
p1 <- predict(mod.train, train, type = 'response')
head(p1, n=30)

### MISCLASSIFICATION ERROR ON TRAIN DATA
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$y)
tab1

### MISCLASSIFICATION ERROR RATE 
1 - sum(diag(tab1))/sum(tab1)
# percentage value

### MISCLASSIFICATION ON TEST DATA
p2 <- predict(mod.train, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$compl.s)
tab2

### GOODNESS-OF-FIT 
pvalue = 1-pchisq(**NULL DEVIANCE VALUE** - **RESIDUAL DEVIANCE VALUE**, df=(**NULL DEVIANCE dof** - **RESIDUAL DEVIANCE dof**) #, lower.tail = F #specify lower tail to get extreme values or substract -1
)
# p-value < 0.001 MODEL IS VALID


#AIC(model)

p1.r <- predict(rest.mod, train, type = 'response')
pred1.rest <- ifelse(p1.r>0.5, 1, 0)
rest.tab1 <- table(Predicted = pred1.rest, Actual = train$y)
rest.tab1
1 - sum(diag(rest.tab1))/sum(rest.tab1)

p2.r <- predict(rest.mod, test, type = 'response')
pred2.rest <- ifelse(p2.r>0.5, 1, 0)
rest.tab2 <- table(Predicted = pred2.rest, Actual = test$y)
rest.tab2
1 - sum(diag(rest.tab2))/sum(rest.tab2)
