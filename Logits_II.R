

install.packages("Ecdat")
library(Ecdat)
data("Participation")
head(Participation)

Participation$work <- ifelse(Participation$lfp=="yes",1,0)


logit_mod <-glm(lfp ~ age+educ+nyc, data = Participation, family=binomial)
summary(logit_mod)

confint(logit_mod)
confint.default(logit_mod)

# Odds ratios and CIs

exp(cbind("Odds Ratio" = coef(logit_mod), confint(logit_mod)))


# Set up hypothetical individuals

install.packages("MASS")
library(MASS)


betadraw <- mvrnorm(1000, coef(logit_mod), vcov(logit_mod))
hypind <- c(1,mean(Participation$age), mean(Participation$educ),median(Participation$nyc))
cdfarg <- betadraw%*%hypind
hypprob <- exp(cdfarg)/(1 + exp(cdfarg))

meanprob <- mean(hypprob)
sdprob <- sd(hypprob)

meanprob
sdprob

# Try to change and compare hypothetical individuals

## With Zelig

install.packages("Zelig")
library(Zelig)


logit_mod2 <-zelig(work ~ age+educ+nyc, data = Participation, model="logit")
summary(logit_mod2)

hypind2 <- setx(logit_mod2, age=mean(Participation$age), educ=mean(Participation$educ), 
                nyc=median(Participation$nyc))

hypprob2 <- sim(logit_mod2, x = hypind2)
summary(hypprob2)
#plot(hypprob2)


# Zelig is also good for comparisons

hypindA <- setx(logit_mod2, age=3.995528, educ=9.307339, 
                nyc=1)

hypindB <- setx(logit_mod2, age=mean(Participation$age), educ=mean(Participation$educ), 
                nyc=2)

hypprob3 <- sim(logit_mod2, x = hypindA, x1 = hypindB)

summary(hypprob3)

# When is the first difference significant?
# How else could you set up comparisons? 




# Plot

betadraw <- mvrnorm(1000, coef(logit_mod), vcov(logit_mod))
hypind_seq <- cbind(1,seq(min(Participation$age),max(Participation$age), length.out=100), mean(Participation$educ),median(Participation$nyc))
xb2 <- hypind_seq%*%coef(logit_mod)
exp(xb2)/(1+exp(xb2))
logitprob <- plogis(xb2)

#png("~/Dropbox/site/images/logit_plot1.png")
plot(c(seq(min(Participation$age),max(Participation$age), length.out=100)), 
     logitprob, type="l", ylim=c(0,1), xlab="Age", ylab="Prob. LFP")
#dev.off()

## adding confidence intervals


medianp <- NULL
lci <- NULL
uci <- NULL

for (i in 1:100){
  xb3 <- betadraw%*%hypind_seq[i,]
  logitprob3 <- plogis(xb3)
  medianp[i] <- quantile(logitprob3, probs= 0.5) 
  lci[i] <- quantile(logitprob3, probs = 0.025)
  uci[i] <- quantile(logitprob3, probs = 0.975)
}

#png("~/Dropbox/site/images/logit_plot2.png")
plot(c(seq(min(Participation$age),max(Participation$age), length.out=100)), medianp, type="l", col=1, ylim=c(0,1), xlab="Age", ylab="Prob. LFP")
lines(c(seq(min(Participation$age),max(Participation$age), length.out=100)), lci, lty="dashed")
lines(c(seq(min(Participation$age),max(Participation$age), length.out=100)), uci, lty="dashed")
#dev.off()

# Increase range of age to see the shape of the curve
  # Recall age is divided by 10 in data

betadraw <- mvrnorm(1000, coef(logit_mod), vcov(logit_mod))

hypind_seq2 <- cbind(1,-5:15, mean(Participation$educ),median(Participation$nyc))

medianp <- NULL
lci <- NULL
uci <- NULL

for (i in 1:dim(hypind_seq2)[1]){
  xb4 <- betadraw%*%hypind_seq2[i,]
  logitprob4 <- plogis(xb4)
  medianp[i] <- quantile(logitprob4, probs= 0.5) 
  lci[i] <- quantile(logitprob4, probs = 0.025)
  uci[i] <- quantile(logitprob4, probs = 0.975)
}

#png("~/Dropbox/site/images/logit_plot3.png")
plot(-5:15, medianp, type="l", col=1, ylim=c(0,1), xlab="Age", ylab="Prob. LFP")
lines(-5:15, lci, lty="dashed")
lines(-5:15, uci, lty="dashed")
#dev.off()




# probs from linear prob model?

logit_mod <-glm(lfp ~ age+educ+nyc, data = Participation, family=binomial)
summary(logit_mod)

Participation$work <- ifelse(Participation$lfp=="yes",1,0)

linear_prob <-lm(work ~ age+educ+nyc, data = Participation)
summary(linear_prob)
linear_prob_plot <- predict(linear_prob)

plot(c(seq(min(Participation$age),max(Participation$age), length.out=872)), 
     linear_prob_plot, type="l", ylim=c(0,1), xlab="Age", ylab="Prob. LFP")

plot(linear_prob_plot)

X <- c(2,3,4)

1/(1+ exp(-X))

exp(X)/(1+exp(X))
