
################################################
## Appendix B: Respirable Crystalline Silica  ##
################################################


###########################
#  Silica Data
###########################

# last point is Parks' sample from 3/8/18
x <- c(300, 286, 212, 275, 155, 209, 273, 143, 35) # 6-hour PM10 STD concentration 
y <- c(20, 10, 8, 10, 10, 10, 17, 10, 12)           # 6-hour Silica concentration
cens <- c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE) # whether sample is censored


##############################
# Univariate Approach
##############################

# assume APCD samples are a random of days predicted to be wind events
# therefore exclude Park's sample

library(EnvStats)

# assume Censored lognomal distribution:
(fit1 <- elnormCensored(y[-9], cens[-9]))
predIntLnorm(fit1, conf.level = 0.95)
predIntLnorm(fit1, conf.level = 0.99)
exp(fit1$parameters)

###############################
# Bivariate Approach
###############################

# include Parks sample this time, since we don't need to assume a random sample of
# just event days

#################
## Figure B1: Silica vs PM10

svg("figb1.svg", width = 6, height = 4, 
    pointsize = 10)

#set up plot area
plot(x, y, type = "n",
     #xlim = c(0, 1000), ylim = c(0, 60),
     xlim = c(0, 400), ylim = c(0, 30),
     xlab = "PM10, ug/m3", ylab = "Silica, ug/m3",
     bty = "n", las = 1)

## Points: plot censored results at half of reporting level
colors <- RColorBrewer::brewer.pal(8, "Set1")[-6] # consistant colors
points(x, ifelse(cens, y/2, y), pch = 16, col = ifelse(cens, colors[1], colors[2]))

## legend 
text(x[c(3, 1)], y[c(3, 1)]/c(2,1),
     pos = c(1, 4), col = colors[1:2],
     labels = c("Censored \n(Less Than Reporting Limit)", "Uncensored"))

dev.off()

######
## Are they even correlated? Set censored values to half of reporting limit
cor.test(ifelse(cens, y/2, y), x, method = "kendall")
cor.test(ifelse(cens, y/2, y), x, method = "spearman")
cor.test(ifelse(cens, 0.8*y, y), x, method = "spearman") # exact value for censored obs doesn't matter

######
## fit censored linear model

library(crch)
m1 <- crch(y ~ x, link.scale = "id", left = ifelse(cens, y, y-5)) 
summary(m1)

#######
## Figure B2: fit + pred intervals 

svg("figb2.svg", width = 7, height = 3.5, 
    pointsize = 10)

## base plot
plot(x, y, type = "n",
     xlim = c(0, 1750), ylim = c(0, 60),
     xlab = "PM10, ug/m3", ylab = "Silica, ug/m3",
     xaxt ="n",
     bty = "n", las = 1)
axis(1, at = seq(0, 1000, by = 250))



## Points
colors <- RColorBrewer::brewer.pal(8, "Set1")[-6] # consistant colors
points(x, ifelse(cens, y/2, y), pch = 16, col = ifelse(cens, colors[1], colors[2]))

## regression line, prediction intervals
newd <- data.frame(x = seq(0, 1000, 5))
pred1 <- predict(m1, newd, type = "response", left = 10)
lines(newd$x, pred1)
upr1 <- predict(m1, newd, type = "quantile", at = 0.975,left = 10)
lwr1 <- predict(m1, newd, type = "quantile", at = 0.025,left = 10)
lines(newd$x, upr1)
lines(newd$x, lwr1)

## OSHA standard
lines(newd$x, rep(50, length(newd$x)), col = colors[4])

## labels
text(1000, 50, "OSHA Standard", pos = 4, col = colors[4])
text(1000, pred1[201], "Linear fit", pos = 4)
text(1000, upr1[201], "95% Prediction Interval Upper Bound", pos = 4)
text(1000, lwr1[201], "95% Prediction Interval Lower Bound", pos = 4)

dev.off()

############
## crch quantiles are a little tight, but this doesn't change overall conclusion:

## create some fake data
a <- runif(25, 0, 100)
b <- rnorm(25, a, 20)
newa <- data.frame(a = 1:101)
plot(a, b)

## prediction intervals from lm()
m.lm <- lm(b ~ a) 
lines(newa$a, predict(m.lm, newa))
lines(newa$a, predict(m.lm, newa, interval = "prediction")[, 3]) #upper bound of 95% PI
lines(newa$a, predict(m.lm, newa, interval = "prediction")[, 2]) #lower bound of 95% PI

## prediction interval from crch()
m.crch<- crch(b ~ a, link.scale = "identity")
lines(newa$a, predict(m.crch, newa), col = "blue") # identical to LM
lines(newa$a, predict(m.crch, newa, type = "quantile", at = 0.975), col = "blue") #upper bound of 95% PI
lines(newa$a, predict(m.crch, newa, type = "quantile", at = 0.025), col = "blue") #lower bound of 95% PI

##predcition interval from ciTools
lines(newa$a, ciTools::add_pi(newa, m.lm)$UPB0.975, col = "red")
lines(newa$a, ciTools::add_pi(newa, m.lm)$LPB0.025, col = "red")

##quantile interval form ciTools -  same as prediction interval for Gaussian case
lines(newa$a, ciTools::add_quantile(newa, m.lm, p = 0.975)$quantile0.975, col = "orange")
lines(newa$a, ciTools::add_quantile(newa, m.lm, p = 0.025)$quantile0.025, col = "orange")

## CRCH interval has less coverage; difference between it and LM gets
## smaller as n increaces.

##############################
## Clean up                 ##
##############################

rm(fit1, m.crch, m.lm, m1, newa, newd, a, b, cens, colors, lwr1, pred1, upr1, x, y)
detach("package:EnvStats", unload=TRUE)
detach("package:crch", unload=TRUE)
