
### Multicollinearity

  # What if we omit a variable uncorrelated with our IV?

mods <- list(NULL)
coefs <- NULL
ses <- NULL
for(i in 1:1000){
  X <- rnorm(100,75,5) 
  Z <- rnorm(100, 50, 10)
  u <- rnorm(100,0,2)
  Y <- 5 + 2*X -3*Z + u
  mods[[i]] <- lm(Y~X)
  coefs <- rbind(coefs, coef(mods[[i]]))
  ses <- rbind(ses, sqrt(diag(vcov(mods[[i]]))))
}

#png("~/Dropbox/site/images/no_col1.png")
hist(coefs[,2])
#dev.off()
apply(coefs,2,mean)
apply(ses,2,mean)


### What if we omit a variable that is correlated with our IV?

mods <- list(NULL)
coefs <- NULL
ses <- NULL
for(i in 1:1000){
  X <- rnorm(100,0,5) 
  Z <- 5*X #rnorm(100, 0, 10) + 5*X
  u <- rnorm(100,0,2)
  Y <- 5 + 2*X -3*Z + u
  mods[[i]] <- lm(Y~X+Z)
  coefs <- rbind(coefs, coef(mods[[i]]))
  ses <- rbind(ses, sqrt(diag(vcov(mods[[i]]))))
}

#png("~/Dropbox/site/images/col_drop.png")
hist(coefs[,2])
#dev.off()
apply(coefs,2,mean)
apply(ses,2,mean)

X <- rnorm(100,0,5) 
Z <- 5*X #rnorm(100, 0, 10) + 5*X
Y <- 5 + 2*X -3*Z + u

summary(lm(Y~X+Z))

### What happens with collinearity?
# Compare variances with and without collinearity

mods_no <- list(NULL)
mods_col <- list(NULL)
mods_VIF <- list(NULL)
coefs_no <- NULL
var_no <- NULL
coefs_col <- NULL
var_col <- NULL
r2 <- NULL
VIF <- NULL

for(i in 1:1000){
  X <- rnorm(100,0,5)
  Z <- rnorm(100, 0, 10)
  Z1 <- Z + 5*X
  u <- rnorm(100,0,2)
  Y <- 5 + 2*X -3*Z + u
  Y1 <- 5 + 2*X -3*Z1 + u
  mods_no[[i]] <- lm(Y~X+Z)
  coefs_no <- rbind(coefs_no, coef(mods_no[[i]]))
  var_no <- rbind(var_no, diag(vcov(mods_no[[i]])))
  mods_col[[i]] <- lm(Y1~X+Z1)
  coefs_col <- rbind(coefs_col, coef(mods_col[[i]]))
  var_col <- rbind(var_col, diag(vcov(mods_col[[i]])))
  mods_VIF[[i]] <- lm(X~Z1)
  r2 <- rbind(r2, summary(mods_VIF[[i]])$r.squared)
  VIF <- 1/(1-r2)
}

#png("~/Dropbox/site/images/no_col.png")
hist(coefs_no[,2])
#dev.off()
#png("~/Dropbox/site/images/col.png")
hist(coefs_col[,2])
#dev.off

# Multiply var_no by VIF

cbind(var_no[1:10,2],var_col[1:10,2],var_no[1:10,2]*VIF[1:10])



#### Heteroskedasticity

library(lmtest)

X <- rnorm(100,10,5)
u <- rnorm(100,0,1:100^2)
Y <- 2*X + u
mod <- lm(Y~X)
summary(mod)
bptest(mod, studentize = F)

summary(lm(u~X))
#png("~/Dropbox/site/images/het.png")
plot(mod$residuals^2~ mod$fitted.values)
#dev.off()

mod_correct <- coeftest(mod, vcov=vcovHC)
mod_correct

X_V <- cbind(rep(1,100), X)
e <- mod$residuals
XpXinv <- solve(t(X_V)%*%X_V)

sum(e^2)
sqrt((diag(XpXinv %*%  (t(X_V)%*%X_V)) %*% XpXinv))
%*%solve(t(X_V)%*%X_V)

mods <- list(NULL)
coefs <- NULL
ses <- NULL
ses_correct <- NULL
for(i in 1:1000){
  X <- rnorm(100,10,2) #+ 5*W # N, mean, sd
  u <- rnorm(100,0,1:100^2)
  Y <- 2*X + u
  mods[[i]] <- lm(Y~X)
  coefs <- rbind(coefs, coef(mods[[i]]))
  ses <- rbind(ses, sqrt(diag(vcov(mods[[i]]))))
  ses_correct <- rbind(ses_correct, sqrt(diag(vcovHC(mods[[i]]))))
}

apply(ses,2,mean)
apply(ses_correct,2,mean)

