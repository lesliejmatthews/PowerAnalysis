# power.WMW
# Computes power function for Wilcoxon rank-sum test, one-sided alternative
# y1 = data (for both groups if stacked = TRUE; 1st group if stacked = FALSE)
# y2 = grouping variable if stacked = TRUE; 2nd data column if stacked = FALSE
# gmratio is the ratio of geometric means
# power is the set of powers for which you want sample sizes
# NOTE: you can enter power <- c(0.2, 0.5, 0.7) if you wish
# NOTE: sample sizes will be rounded up to the next largest integer using the
# ceiling function
power.WMW <- function(y1, y2, gmratio, conf = 95, stacked = TRUE,  
                      power = seq(0.5, 0.95, by = 0.05)) { 
par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
y1name <- deparse(substitute(y1))
y2name <- deparse(substitute(y2))
if(class(y2) == "numeric") {
   stacked = FALSE
   cat("\n DATA ANALYZED:", y1name, y2name, "\n") 
 }
cat("------------------ \n")
if(stacked) {
   cat("\n DATA ANALYZED:", y1name, "by", y2name,"\n")
   cat("------------------ \n")
   xx <- data.frame(A = y1,B= y2)
   zz <- unstack(xx, A ~ B )
   x1 <- zz[[1]]
   x2 <- zz[[2]]
 } else{
   x1 <- y1
   x2 <- y2
}
conf <- conf / 100.0
n1 <- length(x1)
n2 <- length(x2)
w1 <- log(x1)
w2 <- log(x2)
avediff <- log(gmratio)
stddiff <- sqrt(sd(w1) ^ 2 + sd(w2) ^ 2)
pplus <- pnorm(0, avediff, stddiff)
if(pplus < 0.5) {
   pplus <- 1.0 - pplus
 }
ns <- n1+n2
nxrat <- n1 / ns
zalpha <- qnorm(conf)
zbeta <- sqrt(12 * nxrat * (1 - nxrat) * ns * (pplus - .5) ^ 2) - zalpha
pow <- pnorm(zbeta)
RESULTSOBS3 <- data.frame(SampleSize = ns, Nxratio = nxrat, GMratio = gmratio,
                            PPlus = round(pplus, 3),
                            ObsrvPower = round(100 * pow, 2))
cat("Results for Wilcoxon rank-sum test (one-tailed) \n     with specified gmratio. \n")
cat("SampleSize is the required number of obs in both groups together. \n")
cat("Nxratio is the proportion of SampleSize for 1st variable entered. \n")
print(RESULTSOBS3)
cat("------------------ \n")
zbet <- qnorm(power)
sampsize <- (zalpha + zbet) ^ 2 / (12 * nxrat * (1 - nxrat) * (pplus - 0.5) ^ 2)
power <- 100*power
maintitle <- expression("WILCOXON RANK SUM TEST")
RESULTS <- data.frame(Power = power, SampleSize=sampsize)

plot(power ~ sampsize, main = maintitle, ylab = "Power of test", xlab = "Total sample size")
sampsize2 <- ceiling(sampsize)
zbeta2 <- sqrt(12 * nxrat * (1 - nxrat) * sampsize2 * (pplus - 0.5) ^ 2) - zalpha
power2 <- pnorm(zbeta2)
power2 <- 100 * (round(power2, digits = 3))
RESULTS2 <- data.frame(SampleSize = sampsize2, Power = power2)
cat("SampleSize is integer for closest Power\n    not less than specified Power \n")
cat("Sample sizes are rounded up to smallest integer\n     not less than the computed sample size \n")
print(RESULTS2)
}


