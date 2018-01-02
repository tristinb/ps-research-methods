
# Predicted probabilities with poisson
  # Average death in a riot is 50 (made up) PR less than 50?
  
ppois(50, lambda=50)
ppois(50, lambda=50, lower.tail = F)

# Look at poisson distribution by hand

x <- 1:25
lambda <- 12
pois_dist <- lambda^(x)*exp(-lambda)/factorial(x)
#png("~/Dropbox/site/images/count_sims.png")
plot(x,pois_dist)
#dev.off()

# Or use the built in function
#png("~/Dropbox/site/images/count_sims.png")
plot(dpois(1:25,5), xlab="X", ylab="PR(Y|X)")
#dev.off()

library(Zelig)

data("Seatbelts")
View(Seatbelts)

Seat <- as.data.frame(Seatbelts)
head(Seat)

count_mod <- glm(DriversKilled~kms+rear+law, family="poisson", data=Seat)
summary(count_mod)

max(Seat$front)
hist(Seat$front)

# How do we find predicted counts?
mean(Seat$DriversKilled)
range(Seat$kms)

pred_mean_count <- predict(count_mod,newdata=data.frame(kms=seq(7500,22000,by=100),rear=rep(mean(Seat$rear),146),law=rep(1,146)),type="response")
pred_min_count <- predict(count_mod,newdata=data.frame(kms=seq(7500,22000,by=100),rear=rep(min(Seat$rear),146),law=rep(1,146)),type="response")
pred_max_count <- predict(count_mod,newdata=data.frame(kms=seq(7500,22000,by=100),rear=rep(max(Seat$rear),146),law=rep(0,146)),type="response")

#png("~/Dropbox/site/images/counts.png")
plot(seq(7500,22000,by=100),pred_mean_count,xlab="Kilometers Driven",ylab="Predicted Number of Deaths",ylim=c(min(pred_min_count),max(pred_max_count)),type="l",lty="solid")
lines(seq(7500,22000,by=100),pred_min_count,lty="dashed")
lines(seq(7500,22000,by=100),pred_max_count,lty="dotted")
legend("topright", legend = c("Minimum", "Mean", "Maximum") ,lty = c("dashed", "solid", "dotted"))
#dev.off()


# In Poisson var(mu) = mean(mu); test for overdispersion

library(AER)
dispersiontest(count_mod)


# Negative binomial model

library(MASS)

negbin_mod <- glm.nb(DriversKilled~kms+rear+law, data=Seat)
summary(negbin_mod)


