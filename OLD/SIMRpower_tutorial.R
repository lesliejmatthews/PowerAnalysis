library(simr)
library(glme)

data(simdata)
head(simdata)

plot(y~x, data=simdata)

#random intercept model; each group (study site) has its own intercept
