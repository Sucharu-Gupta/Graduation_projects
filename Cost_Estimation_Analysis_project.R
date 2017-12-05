##########################################
# Check AIC, BIC, Cp
## PRELIMINARIES

cost.data <- read.csv("/users/ryanquigley/desktop/261A_Final-Project/proj4.csv", header = TRUE)

par(mfrow = c(2,2))
plot(ced.r.train$units, ced.r.train$avecost)
plot(ced.r.train$units.log, ced.r.train$avecost)
plot(ced.r.train$units.inv.sqrt, ced.r.train$avecost)
plot(ced.r.train$units.inv, ced.r.train$avecost)

## New Variables
cost.data$avecost <- (cost.data$totalcost / cost.data$units)
cost.data$units.log <- log(cost.data$units)
cost.data$units.inv.sqrt <- (cost.data$units)^(-1/2)
cost.data$units.inv <- (cost.data$units)^(-1)

cost.data$material.cost <- (cost.data$weight + cost.data$lost)*cost.data$cost
cost.data$complexity <- (cost.data$stamp + cost.data$chisel)
cost.data$complexity.diff <- (cost.data$stamp - cost.data$chisel)
cost.data$complexity.ave <- (cost.data$stamp + cost.data$chisel)/2
cost.data$super.complexity <- (cost.data$stamp + cost.data$chisel)/(cost.data$goal.sd)

cost.data$rework.num <- (cost.data$units*cost.data$rework)



cost.data$goal.sd.cat <- as.factor(cost.data$goal.sd)

cost.data$goal.sd.cat2 <- rep(1,length(cost.data$goal.sd))
cost.data$goal.sd.cat2[cost.data$goal.sd == 1.0] <- 0
cost.data$goal.sd.cat2 <- as.factor(cost.data$goal.sd.cat2)

cost.data$total.labor <- (cost.data$labor + cost.data$mach.hrs)
cost.data$total.labor.rw <- (cost.data$labor + cost.data$mach.hrs)*(1 + cost.data$rework)
cost.data$labor.rw <- (cost.data$labor)*(1 + cost.data$rework)
cost.data$mach.rw <- (cost.data$mach.hrs)*(1 + cost.data$rework)

cost.data$dev <- rep(0, length(cost.data$manager))
cost.data$dev[cost.data$manager == "Devon"] <- 1
cost.data$dev <- as.factor(cost.data$dev)

cost.data$breakdown.2 <- rep(0, length(cost.data$breakdown))
cost.data$breakdown.2[cost.data$breakdown == 2] <- 1
cost.data$breakdown.2 <- as.factor(cost.data$breakdown.2)

ced <- cost.data[,c(1:9,21:22,24:27,30,31,37,38)]
cam <- cost.data


## Training & Holdout Data

set.seed(261)
ced.holdout.variable <- rbinom(n = dim(ced)[1], size = 1, prob = 0.35)
ced.train <- ced[ced.holdout.variable == 0,]
ced.holdout <- ced[ced.holdout.variable == 1,]

set.seed(261)
cam.holdout.variable <- rbinom(n = dim(cam)[1], size = 1, prob = 0.35)
cam.train <- cam[cam.holdout.variable == 0,]
cam.holdout <- cam[cam.holdout.variable == 1,]

## Removing observations: 19, 311

ced.red <- ced[-c(19,311),]
cam.red <- cam[-c(19,311),]

set.seed(261)
ced.r.holdout.variable <- rbinom(n = dim(ced.red)[1], size = 1, prob = 0.35)
ced.r.train <- ced.red[ced.r.holdout.variable == 0,]
ced.r.holdout <- ced.red[ced.r.holdout.variable == 1,]

set.seed(261)
cam.r.holdout.variable <- rbinom(n = dim(cam.red)[1], size = 1, prob = 0.35)
cam.r.train <- cam.red[cam.r.holdout.variable == 0,]
cam.r.holdout <- cam.red[cam.r.holdout.variable == 1,]

##########################################
## COST ESTIMATION MODEL
## Testing models
my.ced.cand.1 <- lm(avecost ~ units.log + weight + complexity, data = ced.r.train)
my.ced.cand.2 <- lm(avecost ~ units.inv + weight + complexity, data = ced.r.train)
my.ced.cand.3 <- lm(avecost ~ units + weight + complexity, data = ced.r.train)

sst.ced <- sum((ced.r.train$avecost - mean(ced.r.train$avecost))^2)

ced.candidates <- data.frame(candidates = 0)
for (i in 1:9) {
	if(i == 1) ced.candidates[i,1] <- paste0("my.ced.cand.",i)
	else ced.candidates = rbind(ced.candidates, paste0("my.ced.cand.",i))

	x = get(ced.candidates[i,1])
	y = predict(x, ced.r.holdout)
	z = sum((ced.r.holdout$avecost - y)^2)/(dim(ced.r.holdout)[1])
	ced.candidates$PRESS[i] <- sum((x$resid/(1 - hatvalues(x)))^2)
	ced.candidates$SSE[i] <- sum((x$resid)^2) 
	ced.candidates$r2.PRESS[i] <- (1 - (sum((x$resid/(1 - hatvalues(x)))^2))/sst.ced)
	ced.candidates$adj.r.sq[i] <- summary(x)$adj.r.squared
	ced.candidates$ms.res.preds[i] <- sqrt(z)
	ced.candidates$ms.res.model[i] <- summary(x)$sigma
	ced.candidates$aic[i] <- AIC(x)
	ced.candidates$bic[i] <- BIC(x)
	#assign(paste0("my.ced.vif.",i),vif(x))
}
ced.candidates



hist(my.ced.cand.3$resid)
qqnorm(my.ced.cand.3$residuals)
## [1]  59 110 159 176 186 215
qqnorm(rnorm(dim(ced.train)[1]))
plot(my.ced.cand.3$fitted, my.ced.cand.3$resid)
plot(my.ced.cand.1$fitted, my.ced.cand.1$resid)
plot(rnorm(498), rnorm(498))
## [1]  59 110 159 176 186 215 289
plot(my.ced.cand.3$resid)
plot(ced.r.train$units.log, my.ced.cand.3$resid)
plot(ced.r.train$weight, my.ced.cand.3$resid)
plot(ced.r.train$complexity, my.ced.cand.3$resid)

plot(ced.r.train$chisel, my.ced.cand.3$resid)
plot(ced.r.train$stamp, my.ced.cand.3$resid)
plot(ced.r.train$goal.sd.cat2, my.ced.cand.3$resid, notch = TRUE)
## There is something going on with goal.sd.cat. 1.0 biased toward positive residuals => predicted cost is too low
plot(as.factor(ced.r.train$goal.sd), my.ced.cand.3$resid)
plot(ced.r.train$detail, my.ced.cand.3$resid)
plot(ced.r.train$rush, my.ced.cand.3$resid)
plot(ced.r.train$units, my.ced.cand.3$resid)


my.ced.final.red1 <- lm(avecost ~ units.log + weight + complexity, data = ced.red)



my.ced.final.red2 <- lm(avecost ~ units.inv + weight + complexity, data = ced.red)
my.ced.final.red3 <- lm(avecost ~ units + weight + complexity, data = ced.red)


##########################################
## COST ANALYSIS MODEL

options(contrasts = c("contr.treatment", "contr.poly"))
options(contrasts = c("contr.SAS", "contr.poly"))
## Winners:

my.cam.cand.2 <- lm(avecost ~ units.log + material.cost + labor + dev + complexity, data = cam.r.train)
my.cam.cand.1 <- lm(avecost ~ units.inv + material.cost + labor + dev + complexity, data = cam.r.train)
my.cam.cand.3 <- lm(avecost ~ units + material.cost + labor + dev + complexity, data = cam.r.train)






## Tests


sst.cam <- sum((cam.r.train$avecost - mean(cam.r.train$avecost))^2)
cam.candidates <- data.frame(candidates = 0)
for (i in 1:3) {
	if(i == 1) cam.candidates[i,1] <- paste0("my.cam.cand.",i)
	else cam.candidates = rbind(cam.candidates, paste0("my.cam.cand.",i))

	x = get(cam.candidates[i,1])
	y = predict(x, cam.r.holdout)
	z = sum((cam.r.holdout$avecost - y)^2)/(dim(cam.r.holdout)[1])
	cam.candidates$PRESS[i] <- sum((x$resid/(1 - hatvalues(x)))^2)
	cam.candidates$SSE[i] <- sum((x$resid)^2) 
	cam.candidates$r2.PRESS[i] <- (1 - (sum((x$resid/(1 - hatvalues(x)))^2))/sst.cam)
	cam.candidates$adj.r.sq[i] <- summary(x)$adj.r.squared
	cam.candidates$ms.res.preds[i] <- sqrt(z)
	cam.candidates$ms.res.model[i] <- summary(x)$sigma
	cam.candidates$aic[i] <- AIC(x)
	cam.candidates$bic[i] <- BIC(x)
	assign(paste0("my.cam.vif.",i),vif(x))
}
cam.candidates

my.cam.final1 <- lm(avecost ~ units.log + material.cost + labor + mach.hrs + dev + complexity, data = cam.red)
my.cam.final2 <- lm(avecost ~ units.log + material.cost + labor + dev + complexity, data = cam.red)


hist(my.cam.cand.1$resid)
qqnorm(my.cam.cand.1$residuals)
qqnorm(rnorm(dim(cam.train)[1]))
plot(my.cam.cand.1$fitted, my.cam.cand.1$resid)

plot(my.cam.cand.1$resid)
plot(cam.train$units.log, my.cam.cand.1$resid)
plot(cam.train$material.cost, my.cam.cand.1$resid)
plot(cam.train$labor, my.cam.cand.1$resid)
plot(cam.train$dev, my.cam.cand.1$resid, notch = TRUE)
plot(cam.train$complexity, my.cam.cand.1$resid)

plot(cam.train$units, my.cam.cand.1$resid)
plot(cam.train$goal.sd.cat, my.cam.cand.1$resid, notch = TRUE)
plot(cam.train$weight, my.cam.cand.1$resid)
plot(cam.train$chisel, my.cam.cand.1$resid)
plot(cam.train$stamp, my.cam.cand.1$resid)
plot(cam.train$detail, my.cam.cand.1$resid, notch = TRUE) ## slightly underestimating YES
plot(cam.train$rush, my.cam.cand.1$resid, notch = TRUE)

plot(cam.train$cost, my.cam.cand.1$resid)
plot(cam.train$lost, my.cam.cand.1$resid)
plot(cam.train$manager, my.cam.cand.1$resid, notch = TRUE)
plot(cam.train$room.temp, my.cam.cand.1$resid)
plot(cam.train$music, my.cam.cand.1$resid, notch = TRUE)
plot(as.factor(cam.train$shift), my.cam.cand.1$resid, notch = TRUE)
plot(cam.train$mach.hrs, my.cam.cand.1$resid)
plot(cam.train$plant, my.cam.cand.1$resid, notch = TRUE)
plot(as.factor(cam.train$breakdown), my.cam.cand.1$resid, notch = TRUE)
## Underestimating breakdowns equal to 2
plot(cam.train$rework, my.cam.cand.1$resid)


##########################################
## Steps
## 1) Review linear relationships between response and regressors to determine necessary transformations
## - Consider feature engineering for more useful columns (variables)
## 2) Save training set and holdout set
## 3) Candidate models
## 4) DO EVERYTHING WE HAVE DONE SO FAR IN CLASS
## - Significance of coefficients (variables)
## - Significance of regression
## - Residual analysis
##		- Histogram of Residuals
##		- Normal Probability Plot
##		- Residuals Against Fitted Values
##		- Residuals Against Regressors (in model)
##		- Residuals Against Regressors (NOT in model)
##		- Residuals against Time Sequence
##		?- Regressor vs. Regressor
##		- PRESS statisic

## - Outliers?
## - Multicollinearity: VIF
## -
## 5) For each candidate model, predict response based on regressors using the holdout data
## 6) Compare predicted y values to the actual value of y in the holdout data
## 7) Calculate prediction sum of sqaures (not PRESS) for only the holdout set
## - sum((y - y-hat)^2)/(number holdout out maybe minus p) 
## 8) Choose the FINAL model
## - lowest prediction sum of squares
## 9) Refit the chosen model with all the data
## 10) Use the outputs from this model to give required results
## - Estimated coefficients
## - CI for coefficients
## - CI/PI for data
## - R^2
## 11) Look for outlier observations in the holdout data set
## - You do not wan't incorrect entries to have any effect on the final entire model