
##################################################################
#Limited Dependent Variable Models:  Probit Model
##################################################################

#install and load "Zelig" package
install.packages("Zelig")
library("Zelig")
data(mid)
head(mid)
#estimate logit model
logit_mod <- glm(conflict ~ power+contig+maxdem+mindem, data = mid, family=binomial(link="logit"))
summary(logit_mod)

names(logit_mod)


# Goodness of fit statistics

names(logit_mod)

# Get log likelihoods from deviances
logit_mod$null.deviance - logit_mod$deviance


logit_mod_null <- glm(conflict ~ power+contig+maxdem, data = mid, family=binomial(link="logit"))
summary(logit_mod_null)

anova(logit_mod_null, logit_mod, test="Chisq")
anova(logit_mod_null, logit_mod, test="LR")

# Compare with log likelihood from above

#install.packages("car")
library("car")
Anova(logit_mod)

#Calculate McFadden's pseudo-R2 
1-(logit_mod$deviance)/(logit_mod$null.deviance)



#estimate probit model
probit_mod_null <- glm(conflict ~ 1, data = mid, family=binomial(link="probit"))

probit_mod <- glm(conflict ~ power+contig+maxdem+mindem, data = mid, family=binomial(link="probit"))
summary(probit_mod)

library(stargazer)
stargazer(probit_mod, logit_mod)

anova(probit_mod_null, probit_mod, test="Chisq")
anova(probit_mod_null, probit_mod, test="LR")

#compare logit and probit coefficients:  calculate ratio of logit betas to probit ones
mean(coef(logit_mod)/coef(probit_mod))

max(mid$maxdem)

#Calculate and plot predicted probabilities vs. maxdem for logit model
pred_mean_logit <- predict(logit_mod,newdata=data.frame(maxdem=seq(-50,50,by=1),power=rep(mean(mid$power),101),mindem=rep(mean(mid$mindem),101), contig=rep(median(mid$contig),101)),type="response")
pred_min_logit <- predict(logit_mod,newdata=data.frame(maxdem=seq(-50,50,by=1),power=rep(min(mid$power),101),mindem=rep(min(mid$mindem),101), contig=rep(min(mid$contig),101)),type="response")
pred_max_logit <- predict(logit_mod,newdata=data.frame(maxdem=seq(-50,50,by=1),power=rep(max(mid$power),101),mindem=rep(max(mid$mindem),101), contig=rep(min(mid$contig),101)),type="response")

#png("~/Dropbox/site/images/probit_logit.png")
plot(seq(-50,50,by=1),pred_mean_logit,xlab="Age",ylab="Estimated Probability of Voting",type="l",ylim=c(0,1),lty="solid")
lines(seq(-50,50,by=1),pred_min_logit,lty="dashed")
lines(seq(-50,50,by=1),pred_max_logit,lty="dotted")
legend("top", legend = c("Minimum", "Mean", "Maximum") ,lty = c("dashed", "solid", "dotted"))

#Now do the same for the probit model, plot in dark green
pred_mean_probit <- predict(probit_mod,newdata=data.frame(maxdem=seq(-50,50,by=1),power=rep(mean(mid$power),101),mindem=rep(mean(mid$mindem),101), contig=rep(median(mid$contig),101)),type="response")
pred_min_probit <- predict(probit_mod,newdata=data.frame(maxdem=seq(-50,50,by=1),power=rep(min(mid$power),101),mindem=rep(min(mid$mindem),101), contig=rep(min(mid$contig),101)),type="response")
pred_max_probit <- predict(probit_mod,newdata=data.frame(maxdem=seq(-50,50,by=1),power=rep(max(mid$power),101),mindem=rep(max(mid$mindem),101), contig=rep(min(mid$contig),101)),type="response")

# Add to previous plot

lines(seq(-50,50,by=1),pred_mean_probit,lty="solid", col="darkgreen")
lines(seq(-50,50,by=1),pred_min_probit,lty="dashed", col="darkgreen")
lines(seq(-50,50,by=1),pred_max_probit,lty="dotted", col="darkgreen")

#dev.off()



logit_mod$fitted.values

mid$predicted
mid$conflict

mid$predicted <- logit_mod$fitted.values

mid$predicted <- ifelse(mid$predicted>=.5,1,0)

mid$correct1 <- ifelse(mid$conflict==1&mid$predicted==1,1,0)

sum(mid$correct1)/length(mid$correct1)

sum(mid$conflict)/length(mid$conflict)

xtabs(conflict+ predicted, data=mid)

xtabs()

mid$correct <- NULL
for(i in 1:length(mid$conflict)){
  if(mid$conflict[i]==0&&mid$predicted[i]==0|
     mid$conflict[i]==1&&mid$predicted[i]==1){
    mid$correct[i] <- 1
  }
  else{
    mid$correct[i] <- 0
  }
}

sum(mid$correct)/length(mid$correct)

coeffs <- NULL
for(i in 1:1000){
x <- rnorm(100,10,2)
yl <- rnorm(100,50,10)
e <- rnorm(100,0,1)
y <- .5*yl+ 2*x + e
y_dif <- y - yl
mod <- lm(y_dif ~ x)
mod2 <- lm(I(y - yl) ~ x)
coeffs[i] <- coef(mod)[2]
}

hist(coeffs)
mean(coeffs)
summary(mod)
summary(mod2)
