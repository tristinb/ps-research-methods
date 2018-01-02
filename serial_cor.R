
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/LaborSupply.csv", header=T)

head(dat)

mod <- lm(lnhr~lnwg + kids, data=dat)

summary(mod)

# Check for serial correlation in residuals

dat$resids <- mod$residuals

library(DataCombine)
dat <- slide(dat, Var="resids", TimeVar="year", 
             GroupVar="id",NewVar = "lag_resids")


plot(resids~lag_resids,data=dat)


summary(lm(resids~-1+lag_resids, data=dat))


library(lmtest)
bgtest(mod,type="Chisq",fill=NA)

# Do we have heteroskedasticity?

bptest(mod, studentize = F)


#Correct for both with Newey-West HAC robust SEs
library("sandwich")
mod_2 <- NeweyWest(mod,lag=1,prewhite=FALSE)
coeftest(mod,mod_2)

# Or

coeftest(mod, vcov=NeweyWest(mod, lag=1, prewhite=FALSE))
#compare to OLS
summary(mod)


#Prais-Winsten transformation

install.packages("prais")
library("prais")
#One-shot estimation where we supply value of rho

# Use DW test to get value of rho (rho-hat = 1 - d/2)
dwtest(mod,alternative="two.sided")
rho <- 1- 1.1279/2

mod_pw <- prais.winsten(mod,data=dat,rho=rho)
mod_pw

# Now iterate to calculate rho

mod_pw_it <- prais.winsten(mod, data=dat, iter=50)
mod_pw_it

vcov(mod)
