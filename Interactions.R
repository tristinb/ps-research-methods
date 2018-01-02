
setwd("~/Dropbox/psci/teaching/206")
getwd()

# Read data from a website
salaries_dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/car/Salaries.csv", header=T)

# Data come from car package
# 397 observations on the following 6 variables
# rank a factor with levels AssocProf AsstProf Prof
# discipline a factor with levels A (theoretical departments) or B (applied departments).
# yrs.since.phd years since PhD.
# yrs.service years of service.
# sex a factor with levels Female Male
# salary nine-month salary, in dollars.

head(salaries_dat)
# Remove first column
salaries_dat <- salaries_dat[,-1]


mod <- lm(log(salary)~ yrs.service+yrs.since.phd+sex+rank, data=salaries_dat)
summary(mod)

# Notice that Female is the baseline for sex; can switch this
summary(update(mod, .~. - sex + relevel(sex, ref="Male")))
  # .~. repeats regression, relevel is a function that takes the "sex" group
    # and then sets the reference "ref" to male

# Or recode by hand, setting male as baseline
salaries_dat$Female <- ifelse(salaries_dat$sex=="Female",1,0)

# Now put "Female" into equation instead of sex
mod_2 <- lm(log(salary)~ yrs.service+yrs.since.phd+Female+rank, data=salaries_dat)
summary(mod_2)


# Now an interaction

mod_3 <- lm(log(salary) ~ Female*yrs.since.phd, data=salaries_dat)
summary(mod_3) #Notice R automatically includes lower order terms


###################################
##### Plot marginal effects #######
###################################

# 1: let yrs.since.phd (X) vary, hold female (Z) constant
  # Or take partial derivative with respect to X (dy/dx)

Z0 <- seq(min(salaries_dat$Female), max(salaries_dat$Female), length.out = 2)

# pull out coefficient estimates
ests <- coef(mod_3)

# calculate the effect of X over levels of Z
dy_dx <- ests["yrs.since.phd"] + ests["Female:yrs.since.phd"]*Z0
  # Notice that this represents X at Z=0 and X+X*Z at Z=1

# Pull out variance covariance matrix
cov <- vcov(mod_3)

# calculate the standard error of each estimated effect
# Page 70 of BCG gives SE = sqrt(Var(B1) + Z^2(Var(B3)) + 2*Z*Cov(B1*B3))
  # this is for dy/dx

se_dy_dx <- sqrt(cov["yrs.since.phd", "yrs.since.phd"] + Z0^2*cov["Female:yrs.since.phd", "Female:yrs.since.phd"] + 2*Z0*cov["yrs.since.phd", "Female:yrs.since.phd"])

# calculate upper and lower bounds of a 95% CI
upper <- dy_dx + 1.96*se_dy_dx
lower <- dy_dx - 1.96*se_dy_dx

# Plot ME

png("ME_1.png")
plot(c(-.5, 1.5), c(min(lower), max(upper)), xlab="", 
     ylab="ME of Years Since PhD", xaxt="n", type="n") # type="n" removes points
axis(1, at=0:1, labels=c("Male", "Female"))
arrows(Z0,lower,Z0,upper,code=3,angle=90)
points(Z0, dy_dx, pch=19) # pch fills points
dev.off()


# 2. Go the other way -- effect of Female over levels of years since PhD
  # or dy/dz

X0 <- seq(min(salaries_dat$yrs.since.phd), max(salaries_dat$yrs.since.phd), length.out = 500)

dy_dz <- ests["Female"] + ests["Female:yrs.since.phd"]*X0

se_dy_dz <- sqrt(cov["Female", "Female"] + X0^2*cov["Female:yrs.since.phd", "Female:yrs.since.phd"] + 2*X0*cov["Female", "Female:yrs.since.phd"])

upper_2 <- dy_dz + 1.96*se_dy_dz
lower_2 <- dy_dz - 1.96*se_dy_dz

# Plot ME and include histogram and rugplot ala Berry, Golder, Milton

# need more space on right of plot 
par("mar") # Default margins: 5.1 4.1 4.1 2.1 (B,L,T,R)

png("ME_2.png")
par(mar=c(4,4.1,4.1,3.2)) # more space on right, less on left

# histogram
hist(salaries_dat$yrs.since.phd, xlab=NULL, ylab=NULL, col = "light gray"
     ,lty=0, axes = F, main=NULL)
axis(4) # Put labels on right of plot
mtext("Observations", side=4, line=2)
rug(jitter(salaries_dat$yrs.since.phd, amount=1)) #rugplot
par(new=TRUE) # To overlap ME plot on histogram

# ME Plot
plot(c(min(X0), max(X0)), c(min(lower_2), max(upper_2)), xlab="Years Since PhD", 
     ylab="", main="Marginal Effect of Female on Salary", type="n") # type="n" removes points
lines(X0, dy_dz, lwd=2)
lines(X0, lower_2, lty = 3, lwd=2) # lwd makes lines thicker
lines(X0, upper_2, lty = 3, lwd=2)
mtext("ME of Female", side=2, line=2)
abline(h=0) 

# What was the ME of being female for someone 42 years since her PhD?
exp_sal <- ests["Female"] + ests["Female:yrs.since.phd"]*42
points(42,exp_sal) # plot this point (1 ==> Female)
abline(v=42, h=exp_sal)

dev.off()

par(mar=c(5.1,4.1,4.1,2.1)) # Go back to default settings with plot
# If you don't want to keep previous plots you could enter dev.off()




######################################
### Conditional Effects ########
######################################


summary(mod_3)

# Get predicted values (if more than one IV, hold it at mean)
preds1 <- predict(mod_3, newdata = data.frame(yrs.since.phd=X0, Female=0), 
                 interval = 'confidence')

head(preds1) # preds[,1] is fitted val; [,2] is lwr int; [,3] is upper int

#par(mfrow=c(1,2)) # plot side by side, 1 row, 2 cols

# X-axis whole range of x; Y-axis from min lr to max upper CI
png("male.png")
plot(c(min(X0), max(X0)), c(min(preds1[ ,2]), max(preds1[ ,3])), xlab="Years Since PhD", 
     ylab="Salary", type="n") # type="n" removes points
lines(X0, preds1[,1]) # X-axis is all vals of X; Y-axis is fitted vals
lines(X0, preds1[ ,2], lty = 3)
lines(X0, preds1[ ,3], lty = 3)

  # Average val for a male 20 years since PhD
exp_sal_1 <- ests["(Intercept)"] + ests["Female"]*0 + ests["yrs.since.phd"]*20 + ests["Female:yrs.since.phd"]*0
points(20,exp_sal_1) # plot this point (1 ==> Female)

dev.off()

# Female = 1

preds2 <- predict(mod_3, newdata = data.frame(yrs.since.phd=X0, Female=1), 
                  interval = "confidence")
png("female.png")
plot(c(min(X0), max(X0)), c(min(preds2[ ,2]), max(preds2[ ,3])), xlab="Years Since PhD", 
     ylab="Salary", type="n") # type="n" removes points
lines(X0, preds2[,1], col="blue")
lines(X0, preds2[ ,2], lty = 3, col="blue")
lines(X0, preds2[ ,3], lty = 3, col="blue")

# Predict female 20 years since PhD
exp_sal_2 <- ests["(Intercept)"] + ests["Female"] + ests["yrs.since.phd"]*20 + ests["Female:yrs.since.phd"]*20
points(20,exp_sal_2) # plot this point (1 ==> Female)

dev.off()


# Plot both lines, no CI
png("female_male.png")
plot(c(min(X0), max(X0)), c(min(preds2[ ,2]), max(preds1[ ,3])), xlab="Years Since PhD", 
     ylab="Salary", type="n") # type="n" removes points
lines(X0, preds1[,1], lty=1.5) # X-axis is all vals of X; Y-axis is fitted vals
lines(X0, preds2[,1], col="blue", lty=1.5)
legend("topleft",c("male", "female"), col=c("black","blue"), lty=c(1.5,1.5)) 
dev.off()
