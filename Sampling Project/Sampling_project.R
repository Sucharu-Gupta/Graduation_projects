###  Sampling And Design Project 2016
### Names : Sucharu Gupta, Nagadhatri Chennavajula

# Install the required packages
library(sampling)

# Get the chickWeight dataset 
data("ChickWeight")
dim(ChickWeight)

# population size 
N <- length(ChickWeight$weight)

# mean of time
mean.time <- mean(ChickWeight$Time)

#calculate sample size taking the standard deviation know from the historic data 
#(from internet sources like  google)
z <- qnorm(0.975) # 95% alpha value
std <- 3  # sd error which is unknown and which is know from online material like google
e <- 0.5  # error

# sample size calculation
n=round(((z^2)*(std^2))/(e^2 ))
cat(paste("Population Size:", N), paste("Sample Size :", n), sep="\n")

## From the found sample size we are using different sampling techniques
## to find the avg weight of the chicken
######Different Techniques used
# 1. Simple Random Sampling
# 2. Stratified Random Sampling - Propotional Allocation
# 3. Stratified Random Sampling - Neymar Allocation
# 4. Ration Estimation
# 5. Regression Estimation
# 6. Domain Estimation
# 7. Clustering

################# 1. Avg Weight using Simple Random Sampling #################
set.seed(1234)
sampwo_cw = srswor(n,N)
samplewo=ChickWeight[sampwo_cw==1,]

# histogram of weight 
hist(samplewo$weight,xlim = c(0, 400),ylim=c(0,0.008),axes=FALSE,ann=FALSE,
     col = "cornflowerblue", border = "aquamarine3",
     freq = FALSE)
axis(1, col.axis = "gray50", col.ticks = "gray50")
axis(2, col.axis = "gray50", col.ticks = "gray50",
     at = seq.int(0, 0.08, by = 0.001))
title(xlab = "weight", ylab = "Density", main="Chicken Weight")
# Data is slighly right skewed

# calculate the average weight using SRS
avg.chick.wt.srs = mean(samplewo$weight)

# Sum of the sqaure for the sample mean
s.sqr.srs <- var(samplewo$weight)

# variance of the popultion chicken weight
se.chick.wt.srs= sqrt((1-(n/N))*(s.sqr.srs/n))

# calculate z for 95% CI
lower.ci.srs = avg.chick.wt.srs - z*(se.chick.wt.srs)
upper.ci.srs = avg.chick.wt.srs + z*(se.chick.wt.srs)

lower.msg <- "Lower CI : "
upper.msg <- "Upper CI :"
final.msg = ""
lower = ""
upper = ""
header <- "################ Avg Chicken Weight using Simple Random Sampling ###########"
avg.srs.msg1 <- paste(header ,"Avg Chicken Weight : ",sep="\n")
header1 <- "################ Confidence Interval of Average Chicken Weight (SRS) ###########\n"
avg.srs.msg1 <- paste(paste(avg.srs.msg1, round(avg.chick.wt.srs, digits=3)))
lower <- paste(lower.msg,round(lower.ci.srs, digits=3))
upper <- paste(upper.msg,round(upper.ci.srs, digits=3))

ci.msg <- paste(header1, lower, upper, sep = "\n")
final.msg <- paste(avg.srs.msg1, ci.msg, sep = "\n")
cat(final.msg)

############### 2. Stratified RS #####################
str(ChickWeight)
#$ weight: num  42 51 59 64 76 93 106 125 149 171 ...
#$ Time  : num  0 2 4 6 8 10 12 14 16 18 ...
#$ Chick : Ord.factor w/ 50 levels "18"<"16"<"15"<..: 15 15 15 15 15 15 15 15 15 15 ...
#$ Diet  : Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...

# Diet   is a categrical variable with 4 levels
# get the population size of each strata
#NOTE : Size of each statra are not same, this we need to consider during clustering
freq <- table(ChickWeight$Diet)
N1 <- as.integer(freq)[1]
N2 <- as.integer(freq)[2]
N3 <- as.integer(freq)[3]
N4 <- as.integer(freq)[4]


st.msg <- "######## Size of Stratas ########"
cat(st.msg,paste("Diet1: ",N1),paste("Diet2: ",N2),paste("Diet3: ",N3),paste("Diet4: ",N4), sep = "\n")

#  get population strata's information
diet.type <- ChickWeight$Diet
diet1<-ChickWeight[diet.type == 1 ,]
diet2<-ChickWeight[diet.type == 2 ,]
diet3<-ChickWeight[diet.type == 3 ,]
diet4<-ChickWeight[diet.type == 4 ,]

# Sample size of each starta can be found using 
# 1. Propotional Allocation (n1/N1 = n2/N2 =....=n/N)
n1 = round((n/N) * N1) 
n2 = round((n/N) * N2) 
n3 = round((n/N) * N3) 
n4 = round((n/N) * N4) 

st.msg <- "######## Sample Size of each Stratas Using Propotional Allocation ########"
cat(st.msg,paste("Sample size for Diet1: ",n1),
    paste("Sample size for Diet2: ",n2),
    paste("Sample size for Diet3: ",n3),
    paste("Sample size for Diet4: ",n4), sep = "\n")

###Sampling the Strats based on propotional allocation####
set.seed(1234)
diet.strat.list <- strata(ChickWeight,"Diet",method=c("srswor"),c(n1,n2,n3,n4))


# Get the all the samples in the strata as mentioned in the ID_unit
sample.strat=ChickWeight[diet.strat.list$ID_unit,]

plot(sample.strat$Diet, col=c("blue","yellow","pink","grey"), 
     xlab="Diet",ylab="Frequency",
     main="Stratified RS - Propotional Allocation")

# Get the sample information from each strata
sam_diet1=sample.strat[sample.strat$Diet == 1,]
sam_diet2=sample.strat[sample.strat$Diet == 2,]
sam_diet3=sample.strat[sample.strat$Diet == 3,]
sam_diet4=sample.strat[sample.strat$Diet == 4,]

# sample mean weigth
sam.weight.bar1 <- mean(sam_diet1$weight)
sam.weight.bar2 <- mean(sam_diet2$weight)
sam.weight.bar3 <- mean(sam_diet3$weight)
sam.weight.bar4 <- mean(sam_diet4$weight)

# Avg chicken weight calculated using starification
strat.weight.bar <- (1/N)*((N1*sam.weight.bar1) + (N2*sam.weight.bar2) +
                             (N3*sam.weight.bar3) +(N4*sam.weight.bar4))

# sd in each starta 
s.sqr1 <- var(sam_diet1$weight)
s.sqr2 <- var(sam_diet2$weight)
s.sqr3 <- var(sam_diet3$weight)
s.sqr4 <- var(sam_diet4$weight)

#variance in each starta 
diet1.var1 <- (1-(n1/N1))* (s.sqr1/n1)
diet2.var2 <- (1-(n2/N2))* (s.sqr2/n2)
diet3.var3 <- (1-(n3/N3))* (s.sqr3/n3)
diet4.var4 <- (1-(n4/N4))* (s.sqr4/n4)

diet1.var1 <-  (s.sqr1/n1)
diet2.var2 <-  (s.sqr2/n2)
diet3.var3 <-  (s.sqr3/n3)
diet4.var4 <-  (s.sqr4/n4)

# Variance of the mean
var.mean.statras <- ((N1/N)^2 * diet1.var1) + ((N2/N)^2 * diet2.var2) + 
  ((N3/N)^2 * diet3.var3) + ((N4/N)^2 * diet4.var4)
se.mean.strata <- sqrt(var.mean.statras)

# Calculating the 95% Confience interval for the chick weight using stratification
ci.strata.lower <- strat.weight.bar - (z * se.mean.strata )
ci.strata.upper <- strat.weight.bar + (z * se.mean.strata )

final.msg = ""
lower = ""
upper = ""
header2 <- "################ Avg Chicken Weight using Stratified Random Sampling(Propotional Allocation) ###########"
avg.msg1 <- paste(header2 ,"Avg Chicken Weight : ",sep="\n")
header3 <- "################ Confidence Interval of Average Chicken Weight (Stratified Propotional Allocation) ###########\n"
avg.msg1 <- paste(paste(avg.msg1, round(strat.weight.bar, digits=3)))
lower <- paste(lower.msg,round(ci.strata.lower, digits=3))
upper <- paste(upper.msg,round(ci.strata.upper, digits=3))

ci.msg <- paste(header3, lower, upper,sep = "\n")
final.msg <- paste(avg.msg1, ci.msg, sep = "\n")
cat(final.msg)


################# 3. Neymar Allocation ###################

# Standard deviation of the strata in the population
pop.sh1 <- sd(diet1$weight)
pop.sh2 <- sd(diet2$weight)
pop.sh3 <- sd(diet3$weight)
pop.sh4 <- sd(diet4$weight)

#In Neymar Allocation cost is same
total.ss <- (N1*pop.sh1) + (N2*pop.sh2) + (N3*pop.sh3) + (N4*pop.sh4)
ney.n1 <- round(((N1*pop.sh1) / (total.ss))*n)
ney.n2 <- round(((N2*pop.sh2) / (total.ss))*n)
ney.n3 <- round(((N3*pop.sh3) / (total.ss))*n)
ney.n4 <- round(((N4*pop.sh4) / (total.ss))*n)

st.msg <- "######## Sample Size of each Stratas Using Neymar Allocation ########"
cat(st.msg,paste("Sample Diet1: ",ney.n1),
    paste("Sample Diet2: ",ney.n2),
    paste("Sample Diet3: ",ney.n3),
    paste("Sample Diet4: ",ney.n4), sep = "\n")

###Sampling the Strats based on Neymar allocation####
set.seed(1234)
diet.strat.ney.list <- strata(ChickWeight,"Diet",method=c("srswor"),c(ney.n1,ney.n2,ney.n3,ney.n4))

# Get the all the samples in the strata as mentioned in the ID_unit
ney.sample.strat=ChickWeight[diet.strat.ney.list$ID_unit,]

plot(ney.sample.strat$Diet,col=c("blue","yellow","pink","grey"), 
     xlab="Diet",
     ylab="Frequency",
     main="Stratified RS - Neymar Allocation")

# Get the sample information from each strata
ney_sam_diet1=ney.sample.strat[ney.sample.strat$Diet == 1,]
ney_sam_diet2=ney.sample.strat[ney.sample.strat$Diet == 2,]
ney_sam_diet3=ney.sample.strat[ney.sample.strat$Diet == 3,]
ney_sam_diet4=ney.sample.strat[ney.sample.strat$Diet == 4,]

# sample mean weigth
ney.sam.weight.bar1 <- mean(ney_sam_diet1$weight) 
ney.sam.weight.bar2 <- mean(ney_sam_diet2$weight) 
ney.sam.weight.bar3 <- mean(ney_sam_diet3$weight)
ney.sam.weight.bar4 <- mean(ney_sam_diet4$weight) 

# Avg chicken weight calculated using starification (Neymar Allocation) 
ney.strat.weight.bar <- (1/N)*((N1*ney.sam.weight.bar1) + (N2*ney.sam.weight.bar2) +
                             (N3*ney.sam.weight.bar3) +(N4*ney.sam.weight.bar4))

# sd in each starta
ney.s.sqr1 <- var(ney_sam_diet1$weight)
ney.s.sqr2 <- var(ney_sam_diet2$weight)
ney.s.sqr3 <- var(ney_sam_diet3$weight)
ney.s.sqr4 <- var(ney_sam_diet4$weight)

#variance in each starta 
ney.var.diet1 <- (1-(ney.n1/N1))* (ney.s.sqr1/ney.n1)
ney.var.diet2 <- (1-(ney.n2/N2))* (ney.s.sqr2/ney.n2)
ney.var.diet3 <- (1-(ney.n3/N3))* (ney.s.sqr3/ney.n3)
ney.var.diet4 <- (1-(ney.n4/N4))* (ney.s.sqr4/ney.n4)

# Variance of the mean
ney.var.mean.statras <- ((N1/N)^2 * ney.var.diet1) + ((N2/N)^2 * ney.var.diet2) + 
  ((N3/N)^2 * ney.var.diet3) + ((N4/N)^2 * ney.var.diet4)
ney.se.mean.strata <- sqrt(ney.var.mean.statras)

# Calculating the 95% Confience interval for the chick weight using stratification
ci.ney.strata.lower <- ney.strat.weight.bar - (z *ney.se.mean.strata )
ci.ney.strata.upper <- ney.strat.weight.bar + (z *ney.se.mean.strata )

final.msg = ""
lower = ""
upper = ""
header4 <- "################ Avg Chicken Weight using Stratified Random Sampling(Neymar Allocation) ###########"
avg.msg1 <- paste(header4 ,"Avg Chicken Weight : ",sep="\n")
header5 <- "########### Confidence Interval of Average Chicken Weight (Stratified Neymar Allocation) ########\n"
avg.msg1 <- paste(paste(avg.msg1, round(ney.strat.weight.bar, digits=3)))

lower <- paste(lower.msg,round(ci.ney.strata.lower, digits=3))
upper <- paste(upper.msg,round(ci.ney.strata.upper, digits=3))
ci.msg <- paste(header5, lower, upper,sep = "\n")

final.msg <- paste(avg.msg1, ci.msg, sep = "\n")
cat(final.msg)


############4.  Ratio Estimation #######################
# check the correlaiton between weight and time. 
# time is the auxillary variable (x)  used  to find (y) weight 
# AS time increases the weight also increase so there should be positive corelation between time and weight
r <- cor(ChickWeight$weight, ChickWeight$Time)

# As correlation = 0.83,  pretty big value so its useful to do ration estimates.
#As we know the avg time from population, we calulate the mean of the population time
overall.mean.time <- mean(ChickWeight$Time)

time <- samplewo$Time
weight <- samplewo$weight

# From the sample, calulate sample mean weight and mean time
mean_x <- mean(time)
mean_y <- mean(weight)

# check to see if ration estimation is good to perform to find the avg weight
cvx=sqrt(var(time))/mean(time)
cvy=sqrt(var(weight))/mean(weight)
cvx/(2*cvy) ##0.5631104

cat(paste(paste("correlation :",round(r, digits = 3), sep=" "), 
          paste("cvx/2*cvy : ",round(cvx/(2*cvy),digits=3)), sep="\n"))
#Since,R>cvx/2*cvy, It is useful to use ratio estimates
# High correlation, so better to use Time as auxillary variable

# Calculate the ration B hat
bhat <- mean_y/mean_x

# calculate the avg weight using ration estimates
avg.wt.ration <- bhat*overall.mean.time

# Confidence interval of the avg weight
sb.sqr <- sum((samplewo$weight - (bhat *samplewo$Time))^2)/(n-1)
var.ybar <- (N - n)/(N * n) * sb.sqr
se.bar <- sqrt(var.ybar)

avg.wt.ratio.upper <- avg.wt.ration + (z * se.bar)
avg.wt.ratio.lower <- avg.wt.ration - (z * se.bar)

final.msg = ""
lower = ""
upper = ""
header4 <- "################ Avg Chicken Weight using Ratio Estimation###########"
ratio.msg1 <- paste(header4 ,"Avg Chicken Weight : ",sep="\n")
header5 <- "################ Confidence Interval of Average Chicken Weight (Ratio) ###########\n"
ratio.msg1 <- paste(paste(ratio.msg1, round(avg.wt.ration, digits=3)))

lower <- paste(lower.msg,round(avg.wt.ratio.lower, digits=3))
upper <- paste(upper.msg,round(avg.wt.ratio.upper, digits=3))

ci.msg <- paste(header4, lower, upper,sep = "\n")
final.msg <- paste(ratio.msg1, ci.msg, sep = "\n")
cat(final.msg)


########### 5. Regression  Estimation ################

# since Time and Weight are highly correlated  0.8371017, The relation between x and y can be seen as a regression line
# and we can estimate avg weight of chicken using regression


# Plotting regression line
plot(time, weight, main="Time vs Weight", col="blue",ylim=c(0,300))
abline(lm(weight ~ time))

# finding the slope
(b1.hat <- (cov(time, weight)/var(time)))

#finding the intercept
(b0.hat <- (mean_y - (b1.hat * mean_x)))

(weight.bar.reg <-( b0.hat + (b1.hat*mean_x)))
#Avg weight of chicken from regression estimate 119.0217

# calculating  error
weight.hat <- b0.hat + (b1.hat * time)
e <- (weight - weight.hat)

# plotting the errors, errors does not have constant variance, so estimation using regression will 
# not give precise estimates
plot(time, e, main="Time vs Error", ylab="Error", xlab="Time",col="blue",ylim=c(0,300))

se.sq <- (1/(n-1)) * sum(e^2)

# variance of Avg weight using regression
var.reg <- (1- (n/N)) *(se.sq/n)
se.reg <- sqrt(var.reg)

# Confidence interval
avg.wt.reg.lower <- weight.bar.reg - (z * se.reg)
avg.wt.reg.upper <- weight.bar.reg + (z * se.reg)

final.msg = ""
lower =""
upper = ""
header6 <- "################ Avg Chicken Weight using Regression Estimation ###########"
reg.msg1 <- paste(header6 ,"Avg Chicken Weight : ",sep="\n")
header7 <- "################ Confidence Interval of Average Chicken Weight (Regression) ###########\n"

reg.msg1 <- paste(paste(reg.msg1, round(weight.bar.reg, digits=3)))
lower <- paste(lower.msg,round(avg.wt.reg.lower, digits=3))
upper <- paste(upper.msg,round(avg.wt.reg.upper, digits=3))

ci.msg <- paste(header6, lower, upper, sep = "\n")
final.msg <- paste(reg.msg1, ci.msg, sep = "\n")
cat(final.msg)

########################### 6.Domain Sampling ###########################

freq <- table(samplewo$Diet)
nd1 <- as.integer(freq[1])
# Did a sampling of 4 domains 
set.seed(1234)
s_domain = srswor(1,4)

# Estimates for subpopulations Diet=1
sample_domain1 <- samplewo[samplewo$Diet==1,]
dim(sample_domain1)

### average weight of the chickens in the domain 1
avg_wt_domain1 <- (sum(sample_domain1$weight))/nd1

#sum of the squares of the domain 
s.sq.domain1 <- (sum((sample_domain1$weight - avg_wt_domain1)^2))/(nd1-1)

# variance of the mean of  the domain 
var_avg_wt_diet1 <- ( 1 -( n/N))*(s.sq.domain1/nd1)
Sd_domain1 <- sqrt(var_avg_wt_diet1)

#confidence interval of mean using domain estimation
cl.upper_domain <- avg_wt_domain1 + (z * Sd_domain1)
cl.lower_domain <- avg_wt_domain1 - (z * Sd_domain1)


final.msg = ""
lower =""
upper = ""
header8 <- "################ Avg Chicken Weight using Domain Estimation ###########"
domain.msg1 <- paste(header8 ,"Avg Chicken Weight : ",sep="\n")
header9 <- "################ Confidence Interval of Average Chicken Weight (Domain) ###########\n"

domain.msg1 <- paste(paste(domain.msg1, round(avg_wt_domain1, digits=3)))
lower <- paste(lower.msg,round(cl.lower_domain, digits=3))
upper <- paste(upper.msg,round(cl.upper_domain, digits=3))

ci.msg <- paste(header6, lower, upper, sep = "\n")
final.msg <- paste(domain.msg1, ci.msg, sep = "\n")
cat(final.msg)

#Calculating the estimates using SRS for Domain 1 as size of domain is sufficiently large###
avg.chick.wt.domain = mean(sample_domain1$weight)

# Sum of the sqaure for the sample mean
s.sqr.domain <- var(sample_domain1$weight)

# variance of the popultion chicken weight
se.chick.wt.domain= sqrt((1-(nd1/N1))*(s.sqr.domain/nd1))

# calculate z for 95% CI
lower.ci.domain = avg.chick.wt.domain - z*(se.chick.wt.domain)
upper.ci.domain = avg.chick.wt.domain + z*(se.chick.wt.domain)

################### 7. Clustering #####################################
table(samplewo$Diet)
#1  2  3  4 
#58 22 35 23 

# As the sample sizes are diifferent for each Diet we will be just plotting the graph to see if
# clustering techniques is good or not.
plot(samplewo$weight, samplewo$Time, col=samplewo$Diet,
     xlab = "Weight (gms)",
     ylab = "Time (Days)",
     main = "Chicken Weight Dataset",
     pch = c(15,16, 17, 18)[as.numeric(samplewo$Diet)])

#Adding the expressions for legend
expr1 <- expression(Diet1)
expr2 <- expression(Diet2)
expr3 <- expression(Diet3)
expr4 <- expression(Diet4)

legend(200, 10, legend = c(expr1, expr2, expr3, expr4), 
       pch=c(15:18),col=1:4)

# From the plot we can see that clustering is a good technique for this dataset.

# from the analysis , Ratio estimates are better as they have precise estimates compared to other estimates

################ End of Analysis ####################s







