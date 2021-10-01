# power.WMW
# Computes power function for Wilcoxon rank-sum test, one-sided alternative
# y1 = data (for both groups if stacked = TRUE; 1st group if stacked = FALSE)
# y2 = grouping variable if stacked = TRUE; 2nd data column if stacked = FALSE
# gmratio is the ratio of geometric means
# power is the set of powers for which you want sample sizes
# NOTE: you can enter power <- c(0.2, 0.5, 0.7) if you wish
# NOTE: sample sizes will be rounded up to the next largest integer using the
# ceiling function
power.WMW.one.sample <- function(y, target, gmratio, conf = 95,  
                      power = seq(0.5, 0.95, by = 0.05)) { 
par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
yname <- deparse(substitute(y))
cat("------------------ \n")
conf <- conf / 100.0
n <- length(y)
w1 <- log(y)
w2 <- log(target)
avediff <- log(gmratio)
stddiff <- sqrt(sd(w1) ^ 2 + sd(w2) ^ 2)
pplus <- pnorm(0, avediff, stddiff)
if(pplus < 0.5) {
   pplus <- 1.0 - pplus
 }
zalpha <- qnorm(conf)
zbeta <- sqrt(12 * n1 * (pplus - .5) ^ 2) - zalpha
pow <- pnorm(zbeta)
RESULTSOBS3 <- data.frame(SampleSize = n, GMratio = gmratio,
                            PPlus = round(pplus, 3),
                            ObsrvPower = round(100 * pow, 2))
cat("Results for Wilcoxon rank-sum test (one-tailed) \n     with specified gmratio. \n")
cat("SampleSize is the required number of obs. \n")
print(RESULTSOBS3)
cat("------------------ \n")
zbet <- qnorm(power)
sampsize <- (zalpha + zbet) ^ 2 / (12 * (pplus - 0.5) ^ 2)
power <- 100*power
maintitle <- expression("WILCOXON RANK SUM TEST")
RESULTS <- data.frame(Power = power, SampleSize=sampsize)

plot(power ~ sampsize, main = maintitle, ylab = "Power of test", xlab = "Total sample size")
sampsize2 <- ceiling(sampsize)
zbeta2 <- sqrt(12 * (pplus - 0.5) ^ 2) - zalpha
power2 <- pnorm(zbeta2)
power2 <- 100 * (round(power2, digits = 3))
RESULTS2 <- data.frame(SampleSize = sampsize2, Power = power2)
cat("SampleSize is integer for closest Power\n    not less than specified Power \n")
cat("Sample sizes are rounded up to smallest integer\n     not less than the computed sample size \n")
print(RESULTS2)
}


y <-  sample.data.5.1$Phos
target <- log(20)
gmratio <- 1.18 
conf <- 95 
power <- seq(0.5, 0.95, by = 0.05)
par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
yname <- deparse(substitute(y))
conf <- conf / 100.0
n <- length(y)
w1 <- log(y)
w2 <- log(target)
avediff <- log(gmratio)
   stddiff <- sqrt(sd(w1) ^ 2 + sd(w2) ^ 2)
   pplus <- pnorm(0, avediff, stddiff)
   if(pplus < 0.5) {
      pplus <- 1.0 - pplus
   }
   zalpha <- qnorm(conf)
   zbeta <- sqrt(12 * n1 * (pplus - .5) ^ 2) - zalpha
   pow <- pnorm(zbeta)
   RESULTSOBS3 <- data.frame(SampleSize = n, GMratio = gmratio,
                             PPlus = round(pplus, 3),
                             ObsrvPower = round(100 * pow, 2))
   cat("Results for Wilcoxon rank-sum test (one-tailed) \n     with specified gmratio. \n")
   cat("SampleSize is the required number of obs. \n")
   print(RESULTSOBS3)
   cat("------------------ \n")
   zbet <- qnorm(power)
   sampsize <- (zalpha + zbet) ^ 2 / (12 * (pplus - 0.5) ^ 2)
   power <- 100*power
   maintitle <- expression("WILCOXON RANK SUM TEST")
   RESULTS <- data.frame(Power = power, SampleSize=sampsize)
   
   plot(power ~ sampsize, main = maintitle, ylab = "Power of test", xlab = "Total sample size")
   sampsize2 <- ceiling(sampsize)
   zbeta2 <- sqrt(12 * (pplus - 0.5) ^ 2) - zalpha
   power2 <- pnorm(zbeta2)
   power2 <- 100 * (round(power2, digits = 3))
   RESULTS2 <- data.frame(SampleSize = sampsize2, Power = power2)
   cat("SampleSize is integer for closest Power\n    not less than specified Power \n")
   cat("Sample sizes are rounded up to smallest integer\n     not less than the computed sample size \n")
   print(RESULTS2)



