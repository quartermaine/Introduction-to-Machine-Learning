age     <- c(4, 8, 7, 12, 6, 9, 10, 14, 7) 
gender  <- as.factor(c(1, 0, 1, 1, 1, 0, 1, 0, 0))
bmi_p   <- c(0.86, 0.45, 0.99, 0.84, 0.85, 0.67, 0.91, 0.29, 0.88) 
m_edu   <- as.factor(c(0, 1, 1, 2, 2, 3, 2, 0, 1))
p_edu   <- as.factor(c(0, 2, 2, 2, 2, 3, 2, 0, 0))
f_color <- as.factor(c("blue", "blue", "yellow", "red", "red", "yellow", 
                       "yellow", "red", "yellow"))
asthma <- c(1, 1, 0, 1, 0, 0, 0, 1, 1)


xfactors <- model.matrix(asthma ~ gender + m_edu + p_edu + f_color)[, -1]  #we use model matrix to encode the categorical features
x<- as.matrix(data.frame(age, bmi_p, xfactors))


# Note alpha=1 for lasso only and can blend with ridge penalty down to
# alpha=0 ridge only.
glmmod <- glmnet(x, y=as.factor(asthma), alpha=1, family="binomial")

# Plot variable coefficients vs. shrinkage parameter lambda.
plot(glmmod, xvar="lambda")


