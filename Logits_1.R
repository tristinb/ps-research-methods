
install.packages("Ecdat")
library("Ecdat")

data("Participation")

head(Participation)
Participation$work <- ifelse(Participation$lfp=="yes",1,0)
# nyc: number of young children; noc: number of old children;
# lnnlinc: logged nonlabor income

logit_mod <-glm(work ~ age+educ +nyc, data = Participation, family=binomial)
summary(logit_mod)

# Why might the coefficient on age be negative?
# What happens to the log of a fraction? (try it) 
summary(Participation$educ)
# Calculate ME on odds; could do this separate
coef(logit_mod)
exp(coef(logit_mod))

min(Participation$educ)
#Calculate anmin(Participation$educ)d plot predicted probabilities vs. educ

educ_var <- seq(min(Participation$educ), max(Participation$educ), length.out = 21)
educ_var <- seq(-50, 100, length.out = 150)
prob_educ_mean <- predict(logit_mod, newdata= data.frame(educ=educ_var, age=mean(Participation$age), nyc=mean(Participation$nyc)),type="response")
prob_educ_max <- predict(logit_mod, newdata= data.frame(educ=educ_var, age=max(Participation$age), nyc=max(Participation$nyc)), type="response")
prob_educ_min <- predict(logit_mod, newdata= data.frame(educ=educ_var, age=min(Participation$age), nyc=min(Participation$nyc)), type="response")


#png("~/Dropbox/site/images/ME_ed.png")
plot(educ_var,prob_educ_mean,xlab="Education",ylab="Estimated Probability of Participation",type="l",xlim=c(1,21),ylim=c(0,1),lty="solid")
lines(educ_var,prob_educ_min,lty="dotted")
lines(educ_var,prob_educ_max,lty="dashed")
legend("topright", legend = c("Minimum", "Mean", "Maximum"), lty = c("dotted", "solid", "dashed"))
#dev.off()

# Look at ME for one unit change

prob_educ_mean

#all other vars at means, and educ at one, odds ratio of part is

prob_educ_mean[1]/ (1-prob_educ_mean[1])

#all other vars at means, and educ at 15, odds ratio of part is
prob_educ_mean[15]/ (1-prob_educ_mean[15])

mean(Participation$educ)

head(Participation)
Participation$part <- ifelse(Participation$lfp=="yes",1,0)
linear_prob <- lm(part~age+educ, data= Participation)
logit_mod <-glm(lfp ~ age+educ, data = Participation, family=binomial)
summary(logit_mod)
summary(linear_prob)


