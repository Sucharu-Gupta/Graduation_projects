##################################################
## Cost Estimation

results <- influence.measures(my.ced.cand.1)
names(results)
results.table <- results$infmat
p <- length(my.ced.cand.1$coefficients)
n <- dim(ced.train)[1]
2*p/n
## hii
hii <- sort(results.table[,8][results.table[,8] > 2*p/n], decreasing = TRUE)
rstud <- rstudent(my.ced.cand.1)[names(hii)]
ord.rstud <- order(abs(rstud), decreasing = TRUE)
rstud[ord.rstud]

## cooks
cook <- sort(results.table[,7], decreasing = TRUE)[1:10]

## DFBETAS
inf.dfbs.int <- results.table[,1][abs(results.table[,1]) > 2/sqrt(n)]
inf.dfbs.x1 <- results.table[,2][abs(results.table[,2]) > 2/sqrt(n)]
inf.dfbs.x2 <- results.table[,3][abs(results.table[,3]) > 2/sqrt(n)]
inf.dfbs.x3 <- results.table[,4][abs(results.table[,4]) > 2/sqrt(n)]
#inf.dfbs.x4 <- results.table[,5][abs(results.table[,5]) > 2/sqrt(n)]
sort(table(c(names(inf.dfbs.int), names(inf.dfbs.x1), names(inf.dfbs.x2), names(inf.dfbs.x3))), decreasing = TRUE)


## DFFITS
dffits <- results.table[,5][abs(results.table[,5]) > 2*sqrt(p/n)]
ord.dffits <- order(abs(dffits), decreasing = TRUE)
dffits[ord.dffits]


## COVRATIO
upper <- 1 + 3*p/n
lower <- 1 - 3*p/n
results.table[,6][(abs(results.table[,6]) > upper) | (abs(results.table[,6]) < lower)]


ced.train[c("311","400","473","453","433","342","159","84","248","215","151","42","37","430","326","247","226","169","162","72"),c(10,2,3,11,5,12,6,7)]

ced.holdout[c(64,43,35,161,66,26,155,134,59,38,53,125,77,98,80,5,82,100,104,31,138,133),c(10,2,3,11,5,12,6,7)]

#############################################
## Finding outliers in the holdout data set
## hii?

ced.xx <- summary(my.ced.cand.1)$cov.unscaled
ced.holdout.outs <- ced.holdout[,c(11,5,12)]
ced.holdout.outs$int <- rep(1,dim(ced.holdout)[1])
ced.holdout.outs <- ced.holdout.outs[,c(4,1:3)]


holdout.hii <- data.frame(Obs = ced.holdout$X)
for (i in 1:dim(ced.holdout)[1]) {
	x <- t(as.numeric(ced.holdout.outs[i,]))%*%ced.xx%*%as.numeric(ced.holdout.outs[i,])
	holdout.hii$hii[i] <- x[1,1]
}

## Residuals
ced.holdout.preds <- predict(my.ced.cand.1, ced.holdout)
ced.holdout.resids <- (ced.holdout$avecost - ced.holdout.preds)
holdout.hii$residuals <- ced.holdout.resids
holdout.hii$stand.residuals <- ced.holdout.resids/(summary(my.ced.cand.1)$sigma)^2
holdout.hii$stand.residuals.2 <- ced.holdout.resids/(sum((ced.holdout.resids)^2)/(dim(ced.holdout)[1] - dim(ced.xx)[1]))


holdout.hii[order(-holdout.hii[,1]),]


##################################################
## Cost Analysis

results <- influence.measures(my.cam.cand.1)
names(results)
results.table <- results$infmat
p <- length(my.cam.cand.1$coefficients)
n <- dim(cam.train)[1]
2*p/n
## hii
hii <- sort(results.table[,10][results.table[,10] > 2*p/n], decreasing = TRUE)
rstud <- rstudent(my.cam.cand.1)[names(hii)]
ord.rstud <- order(abs(rstud), decreasing = TRUE)
rstud[ord.rstud]

## cooks
cook <- sort(results.table[,9], decreasing = TRUE)[1:10]

## DFBETAS
inf.dfbs.int <- results.table[,1][abs(results.table[,1]) > 2/sqrt(n)]
inf.dfbs.x1 <- results.table[,2][abs(results.table[,2]) > 2/sqrt(n)]
inf.dfbs.x2 <- results.table[,3][abs(results.table[,3]) > 2/sqrt(n)]
inf.dfbs.x3 <- results.table[,4][abs(results.table[,4]) > 2/sqrt(n)]
inf.dfbs.x4 <- results.table[,5][abs(results.table[,5]) > 2/sqrt(n)]
inf.dfbs.x5 <- results.table[,6][abs(results.table[,6]) > 2/sqrt(n)]
sort(table(c(names(inf.dfbs.int), names(inf.dfbs.x1), names(inf.dfbs.x2), names(inf.dfbs.x3), names(inf.dfbs.x4), names(inf.dfbs.x5))), decreasing = TRUE)


## DFFITS
dffits <- results.table[,7][abs(results.table[,7]) > 2*sqrt(p/n)]
ord.dffits <- order(abs(dffits), decreasing = TRUE)
dffits[ord.dffits]


## COVRATIO
upper <- 1 + 3*p/n
lower <- 1 - 3*p/n
results.table[,8][(abs(results.table[,8]) > upper) | (abs(results.table[,8]) < lower)]

#############################################
## Finding outliers in the holdout data set

cam.xx <- summary(my.cam.cand.1)$cov.unscaled
cam.holdout.outs <- cam.holdout[,c(22,23,10,35,24)]
cam.holdout.outs$int <- rep(1,dim(cam.holdout)[1])
cam.holdout.outs <- cam.holdout.outs[,c(6,1:5)]


holdout.hii <- data.frame(Obs = cam.holdout$X)
for (i in 1:dim(cam.holdout)[1]) {
	x <- t(as.numeric(cam.holdout.outs[i,]))%*%cam.xx%*%as.numeric(cam.holdout.outs[i,])
	holdout.hii$hii[i] <- x[1,1]
}

## Residuals
cam.holdout.preds <- predict(my.cam.cand.1, cam.holdout)
cam.holdout.resids <- (cam.holdout$avecost - cam.holdout.preds)
holdout.hii$residuals <- cam.holdout.resids
holdout.hii$stand.residuals <- cam.holdout.resids/(summary(my.cam.cand.1)$sigma)^2
holdout.hii$stand.residuals.2 <- cam.holdout.resids/(sum((cam.holdout.resids)^2)/(dim(cam.holdout)[1] - dim(cam.xx)[1]))


holdout.hii[order(-holdout.hii[,2]),]

cam.train[c("311", "215", "400", "247", "84"),c(21,2,3,22,23,10,35,24)]
cam.train[c("311", "215", "400", "247", "84", "169", "258", "21", "347", "159", "399", "350", "342", "44", "307", "261"),c(21,2,3,22,23,10,35,24)]

cam.holdout[c(66,98,161,59,38,64,138,43,35,26,5,82,155,134,104,21,85,97,122,91,44,88,55,149,148,57),c(21,2,3,22,23,10,35,24)]
