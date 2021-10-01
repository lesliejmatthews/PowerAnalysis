power_at_n <- c(0) # initialize vector that stores power for each number of tosses
n_heads <- c() # save "critical" number of heads for that toss-amount that would result 
n_toss <- 2 # initialize the toss-counter
while(power_at_n[n_toss-1] < .90){ # continue as long as power is not 90%
  n_heads[n_toss] <- qbinom(.001, n_toss, .5, lower.tail = F) # retrieve critical value
  power_at_n[n_toss] <- pbinom(n_heads[n_toss], n_toss, .55, lower.tail = F) # calculate power (1-beta) for each coin-toss
  n_toss <- n_toss+1 # increase toss-number 
}

n_toss
tail(n_heads)
tail(power_at_n)

plot(1:(n_toss-1), power_at_n, xlab = "Number of coin-tosses", ylab = "Power", axes = FALSE)
abline(h = .90, col = "red")
axis(side = 1, at = seq(0,(n_toss-1),by=100))
axis(side = 2, at = seq(0,1,by=0.1))

set.seed(1) # make sure our simulation will give the same results when we repeat it
rbinom(n = 1, size = 20, prob = .50) # let r do 1 experiment with 20 coin tosses of a fair coin

set.seed(2) # make sure our simulation will give other results than before
rbinom(n = 1, size = 20, prob = .5) # run the experiment again

set.seed(1)
n_heads <- rbinom(n = 1000, size = 20, prob = .55) # run 1,000 experiments, of 20 coin tosses each, at once
str(n_heads) # show structure of vector

p_heads <- pbinom(n_heads, 20, .50, lower.tail = F) # for each of the 1000 coin toss experiments, calculate the probability of observing this many heads if the coin were fair (which it is not cause we simulated with 55% heads)

exp_power <- mean(p_heads < .001) # check where this chance drops below our alpha level

set.seed(1)
exp_power_at_n <- c(0) # create a vector where we can store the power for each sample-size
n_toss_start <- 19 # start at 21 tosses
n_toss_loop <- 2 # additional number of tosses tried (above 20)
while(exp_power_at_n[n_toss_loop-1] < .90){ # continue increasing the sample-size until power = 90%
  n_toss <- n_toss_start+n_toss_loop # calculate the current number of tosses
  n_heads <- rbinom(1000, n_toss, .55) # run 1000 experiments for any given number of tosses and store number of heads
  p_heads <- pbinom(n_heads, n_toss, .50, lower.tail = F) # calculate the probability of getting at least that many heads if the coin would be fair 
  exp_power_at_n[n_toss_loop] <- mean(p_heads < .001) # calculate power by checking what proportion of the probabilities is smaller than or equal to our alpha-level
  n_toss_loop = n_toss_loop+1
}

n_toss
n_heads
p_heads
exp_power_at_n
exp_power_at_n <- exp_power_at_n[-1] # remove the first 0 that we used to populate the vector for the first iteration of the loop
exp_power_at_n[length(exp_power_at_n)] 
# or
tail(exp_power_at_n)

plot(21:n_toss, exp_power_at_n, xlab = "Number of coin-tosses", ylab = "Power", ylim = c(0,1), axes = FALSE)
abline(h = .90, col = "red")
axis(side = 1, at = seq(0,(n_toss),by=100))
axis(side = 2, at = seq(0,1,by=0.1))

################################
# Part II - t-tests   https://julianquandt.com/post/power-analysis-by-data-simulation-in-r-part-ii/
################################
# Cohen's d is different of means divided by pooled sd
solve(0.5,1) # cohens d of .5, standard deviation is 2
solve(0.25,1) # cohens d of .25, 4

# Example
# 30 participants in each group
set.seed(1234)
group1 <- rnorm(30, 1, 2) # mean 1, sd 2
group2 <- rnorm(30, 0, 2) # mean 0, sd 4
mean(group1)
mean(group2)

# Histogram of both groups
hist(group1, col = "#addd8e", breaks = 10, main = "Histogram of both groups", xlab = "")
hist(group2, add = TRUE, breaks = 10, col= "#31a354")

t.test(group1, group2, paired = FALSE, var.equal = TRUE, conf.level = 0.9)

set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
for(i in 1:n_sims){
  group1 <- rnorm(30,1,2) # simulate group 1
  group2 <- rnorm(30,0,2) # simulate group 2
  p_vals[i] <- t.test(group1, group2, paired = FALSE, var.equal = TRUE, conf.level = 0.90)$p.value # run t-test and extract the p-value
}
mean(p_vals < .10) # check power (i.e. proportion of p-values that are smaller than alpha-level of .10)
# so our power is only 59.2% 

# So how many people do we need in order to achieve power of 95%?
set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
power_at_n <- c(0) # this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()
cohens_ds_at_n <- c() 
n <- 30 # sample-size 
i <- 2
while(power_at_n[i-1] < .95){
  for(sim in 1:n_sims){
    group1 <- rnorm(n,1,2) # simulate group 1
    group2 <- rnorm(n,0,2) # simulate group 2
    p_vals[sim] <- t.test(group1, group2, paired = FALSE, var.equal = TRUE, conf.level = 0.9)$p.value # run t-test and extract the p-value
    cohens_ds[sim] <- abs((mean(group1)-mean(group2))/(sqrt((sd(group1)^2+sd(group2)^2)/2))) # we also save the cohens ds that we observed in each simulation
  }
  power_at_n[i] <- mean(p_vals < .10) # check power (i.e. proportion of p-values that are smaller than alpha-level of .10)
  cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
  n <- n+1 # increase sample-size by 1
  i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
}
power_at_n <- power_at_n[-1] # delete first 0 from the vector
cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector

tail(power_at_n)
tail(cohens_ds_at_n)
tail(n) # use n-1 for sample size

# plot of power versus sample size
plot(30:(n-1), power_at_n, xlab = "Number of participants per group", ylab = "Power", ylim = c(0,1), axes = TRUE)
abline(h = .95, col = "red")

# plot of effect size (Cohen's d) versus sample size
plot(30:(n-1), cohens_ds_at_n, xlab = "Number of participants per group", ylab = "Cohens D", ylim = c(0.45,0.55), axes = TRUE)
abline(h = .50, col = "red")

# Simulating a within subject t-test (i.e. paired t-test) using real raw numbers not standardized effect size
# Paired t-test is equivalent to the one-sample t-test on DIFFERENCE scores

# Using a one-sample t-test approach
# Cohen's d = mean(difference) minus mean(before treatment) divided by standard deviation of the difference
# Since our null hypothesis is that there is no difference, we can substitute the mean(before treatment) as zero
# and solve the equation as above to calculate the sd we need for each given Cohen's d
solve(0.5, 1)

set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
power_at_n <- c(0) # this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()
cohens_ds_at_n <- c() 
n <- 2 # sample-size 
i <- 2
while(power_at_n[i-1] < .95){
  for(sim in 1:n_sims){
    difference <- rnorm(n,1,2) # simulate the difference score distribution
    p_vals[sim] <- t.test(difference, mu = 0, conf.level = 0.90)$p.value # run t-test and extract the p-value
    cohens_ds[sim] <- mean(difference)/sd(difference) # we also save the cohens ds that we observed in each simulation 
  }
  power_at_n[i] <- mean(p_vals < .10) # check power (i.e. proportion of p-values that are smaller than alpha-level of .10)
  cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
  n <- n+1 # increase sample-size by 1
  i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
}

tail(power_at_n)
tail(cohens_ds_at_n)
tail(n) # use n-1 for sample size

power_at_n <- power_at_n[-1] # delete first 0 from the vector
cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector

plot(2:(n-1), power_at_n, xlab = "Number of participants per group", ylab = "Power", ylim = c(0,1), axes = TRUE)
abline(h = .95, col = "red")

plot(2:(n-1), cohens_ds_at_n, xlab = "Number of participants per group", ylab = "Cohens D", ylim = c(0.0,1.0), axes = TRUE)
abline(h = .50, col = "red")

# using a correlated-samples paired t-test approach
# Simulating correlated normal-distributions and demystifying the multivariate normal
# in this case, bivariate normal
library(MASS)
pre_post_means <- c(pre = 0,post = 1) # define means of pre and post in a vector
pre_sd <- 2 # define sd of pre-measure
post_sd <- 2 # define sd of post-measure
correlation <- 0.5 # define their correlation

sigma <- matrix(c(pre_sd^2, pre_sd*post_sd*correlation, pre_sd*post_sd*correlation, post_sd^2), ncol = 2) # define variance-covariance matrix

set.seed(1)
bivnorm <- data.frame(mvrnorm(10000, pre_post_means, sigma)) # simulate bivariate normal

par(mfrow=c(1,2))
hist(bivnorm$pre, main = "pre-measure")
hist(bivnorm$post, main = "post-measure")

plot(bivnorm$pre, bivnorm$post, xlab = "pre-measure", ylab = "post-measure")

bivnorm_kde <- kde2d(bivnorm[,1], bivnorm[,2], n = 50) # calculate kernel density (i.e. the "height of the cone on the z-axis"; not so important to understand here)
par(mar = c(0, 0, 0, 0)) # tel r not to leave so much space around the plot
persp(bivnorm_kde, phi = 45, theta = 30, xlab = "pre-measure", ylab = "post-measure", zlab = "frequency") # plot the bivariate normal

# three different correlation levels (0.1, 0.5, 0.9)
mu_pre_post <- c(pre = 0, post = 1)
sd_pre <- 2
sd_post <- 2
correlations <- c(0.1, 0.5, 0.9)

set.seed(1)
n_sims <- 1000 # we want 1000 simulations
p_vals <- c()
# this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
cohens_ds <- c()

powers_at_cor <- list()
cohens_ds_at_cor <- list()

for(icor in 1:length(correlations)){ # do a power-simulation for each specified simulation
  n <- 2 # sample-size 
  i <- 2 # index of the while loop for saving things into the right place in the lists
  power_at_n <- c(0) 
  cohens_ds_at_n <- c() 
  sigma <- matrix(c(sd_pre^2, sd_pre*sd_post*correlations[icor], sd_pre*sd_post*correlations[icor], sd_post^2), ncol = 2) #var-covar matrix
  while(power_at_n[i-1] < .95){
    for(sim in 1:n_sims){
      bivnorm <- data.frame(mvrnorm(n, mu_pre_post, sigma)) # simulate the bivariate normal
      p_vals[sim] <- t.test(bivnorm$pre, bivnorm$post, paired = TRUE, var.equal = TRUE, conf.level = 0.9)$p.value # run t-test and extract the p-value
      cohens_ds[sim] <- abs((mean(bivnorm$pre)-mean(bivnorm$post))/(sqrt(sd(bivnorm$pre)^2+sd(bivnorm$post)^2-2*cor(bivnorm$pre, bivnorm$post)*sd(bivnorm$pre)*sd(bivnorm$post)))) # we also save the cohens ds that we observed in each simulation
    }
    power_at_n[i] <- mean(p_vals < .10) # check power (i.e. proportion of p-values that are smaller than alpha-level of .10)
    names(power_at_n)[i] <- n
    cohens_ds_at_n[i] <- mean(cohens_ds) # calculate means of cohens ds for each sample-size
    names(cohens_ds_at_n)[i] <- n
    n <- n+1 # increase sample-size by 1
    i <- i+1 # increase index of the while-loop by 1 to save power and cohens d to vector
  }
  power_at_n <- power_at_n[-1] # delete first 0 from the vector
  cohens_ds_at_n <- cohens_ds_at_n[-1] # delete first NA from the vector
  powers_at_cor[[icor]] <- power_at_n # store the entire power curve for this correlation in a list
  cohens_ds_at_cor[[icor]] <- cohens_ds_at_n # do the same for cohens d
  names(powers_at_cor)[[icor]] <- correlations[icor] # name the power-curve in the list according to the tested correlation
  names(cohens_ds_at_cor)[[icor]] <- correlations[icor] # same for cohens d
}

par(mfrow=c(1,3))
plot(2:(length(powers_at_cor$`0.1`)+1), powers_at_cor$`0.1`, xlab = "Number of participants", ylab = "Power", ylim = c(0,1), axes = TRUE, main = "correlation = 0.1")
abline(h = .95, col = "red")
plot(2:(length(powers_at_cor$`0.5`)+1), powers_at_cor$`0.5`, xlab = "Number of participants", ylab = "Power", ylim = c(0,1), axes = TRUE, main = "correlation = 0.5")
abline(h = .95, col = "red")
plot(2:(length(powers_at_cor$`0.9`)+1), powers_at_cor$`0.9`, xlab = "Number of participants", ylab = "Power", ylim = c(0,1), axes = TRUE, main = "correlation = 0.9")
abline(h = .95, col = "red")

par(mfrow=c(1,3))
plot(2:(length(cohens_ds_at_cor$`0.1`)+1), cohens_ds_at_cor$`0.1`, xlab = "Number of participants", ylab = "Cohens D", ylim = c(0,1), axes = TRUE, main = "correlation = 0.1")
abline(h = .50, col = "red")
plot(2:(length(cohens_ds_at_cor$`0.5`)+1), cohens_ds_at_cor$`0.5`, xlab = "Number of participants", ylab = "Cohens D", ylim = c(0,1), axes = TRUE, main = "correlation = 0.5")
abline(h = .50, col = "red")
plot(2:(length(cohens_ds_at_cor$`0.9`)+1), cohens_ds_at_cor$`0.9`, xlab = "Number of participants", ylab = "Cohens D", ylim = c(0,10), axes = TRUE, main = "correlation = 0.9")
abline(h = .50, col = "red")

# simulated t test
set.seed(1234)
group1 <- rnorm(30, 1, 2)
group2 <- rnorm(30, 0, 2)

t.test(group1, group2, paired = FALSE, var.equal = TRUE, conf.level = 0.9)

# same as above, but linear model with dummy coding 0,1 for groups one and two
names(group1) <- rep("group1", length(group1)) # name the vectors to use the names as the group variable
names(group2) <- rep("group2", length(group2))

lm_groupdat <- data.frame(score = c(group1,group2), group = c(names(group1), names(group2))) # make data-set from scores and names

lm_groupdat$dummy_group <- ifelse(lm_groupdat$group == "group1", 0, 1) # create dummy variable 

#indicate the intercept with a 1 in this case, to tell the model that we want 1 times in there 
#and tell it that we want dummy_group times the difference between the groups,  in there. 
summary(lm(score ~ 1+ dummy_group, dat = lm_groupdat)) # use summary function to get p-values

# one sample t.test approach
set.seed(1234)
group1 <- rnorm(30, 1, 2)
mean(group1)
mean(group2)
# null hypothesis mean is .4 alternative is mean different from 1.5 (two tailed)
t.test(group1, mu=1.5)
# is mean less than 1?
t.test(group1, mu=1, alternative="less") # probably yes
# is mean greater than 1?
t.test(group1, mu=1, alternative="greater") # no!

# run above as a linear model
summary(lm(data=lm_groupdat[lm_groupdat$group=="group1",], score ~ 1))

# linear model with four groups and interaction
focus <- rep(c("internal", "external"), each = 2)
media <- rep(c("text", "visual"), times = 2)
mean_TI <- 50
mean_VI <- 20
mean_TE <- 30
mean_VE <- 60

pd <- data.frame(score = c(mean_TI, mean_VI, mean_TE, mean_VE), focus = focus, media = media)

interaction.plot(pd$focus, pd$media, pd$score, ylim = c(0,100))


