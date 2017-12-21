predict(my.ced.final.red, ced.holdout, interval = "confidence")
predict(my.ced.final.red, ced.holdout, interval = "prediction")

my.ced.final.red$coef[1] - qt(.975, df = my.ced.final.red$df.residual)*sqrt(vcov(my.ced.final.red)[1,1])
my.ced.final.red$coef[1] + qt(.975, df = my.ced.final.red$df.residual)*sqrt(vcov(my.ced.final.red)[1,1])

my.ced.final.red$coef[2] - qt(.975, df = my.ced.final.red$df.residual)*sqrt(vcov(my.ced.final.red)[2,2])
my.ced.final.red$coef[2] + qt(.975, df = my.ced.final.red$df.residual)*sqrt(vcov(my.ced.final.red)[2,2])

my.ced.final.red$coef[3] - qt(.975, df = my.ced.final.red$df.residual)*sqrt(vcov(my.ced.final.red)[3,3])
my.ced.final.red$coef[3] + qt(.975, df = my.ced.final.red$df.residual)*sqrt(vcov(my.ced.final.red)[3,3])

my.ced.final.red$coef[4] - qt(.975, df = my.ced.final.red$df.residual)*sqrt(vcov(my.ced.final.red)[4,4])
my.ced.final.red$coef[4] + qt(.975, df = my.ced.final.red$df.residual)*sqrt(vcov(my.ced.final.red)[4,4])


m.units <- mean(ced.train$units)
m.weight <- mean(ced.train$weight)
m.complex <- mean(ced.train$complexity)
new.data <- data.frame(units.log = log(m.units), weight = m.weight, complexity = m.complex)


## "Typical" Job
predict(my.ced.final.red, new.data, interval = "confidence")
predict(my.ced.final.red, new.data, interval = "prediction")

## Predict combinations of high-low as well