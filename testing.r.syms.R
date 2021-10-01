# mean is zero, sd is 1, n is 10  mu = 1 indicates the theoretical value of the mean
# in this example mean is zero and mu is one.  we are testing whether the median of the
# sample differs from 1

set.seed(123)
mean(replicate(10000, wilcox.test(rnorm(15, 0, 1), mu = 1)$p.value < .05))

#result of this is the power i.e. probability of correctly rejecting the null hypothesis

# two sample wilcox test
# this is the sample data frame for distributions
dd <- data.frame(
  p1 = c(.4, .3, .1, .2)
  p2 = c(.6, .3, .1, 0)
)
sample_sizes <- seq(10, 150, length.out = 20)
powers <- sapply(sample_sizes, function(x) {
  tt <- replicate(500, {
    d1 <- sample(1:4, 2, T, prob = dd$p1)
    d2 <- sample(1:4, 2, T, prob = dd$p2)
    suppressWarnings(wilcox.test(d1, d2))$p.value
  })
  mean(tt < .05)
})
for_plot <- data.frame(
  sample_size = sample_sizes,
  power = powers
)
ggplot(for_plot, aes(x = sample_size, y = power)) +
  geom_smooth(se = F) +
  geom_point()

# one sample wilcox test
# this is the sample data frame for distributions
dd <- data.frame(
  p1 = c(.4, .3, .1, .2)
)
sample_sizes <- seq(10, 150, length.out = 20)
powers <- sapply(sample_sizes, function(x) {
  tt <- replicate(500, {
    d1 <- sample(1:4, x, T, prob = dd$p1)
    suppressWarnings(t.test(x=d1, mu=1.01, type="one.tailed", alternative="greater"))$p.value
  })
  mean(tt < .05)
})
for_plot <- data.frame(
  sample_size = sample_sizes,
  power = powers
)
ggplot(for_plot, aes(x = sample_size, y = power)) +
  geom_smooth(se = F) +
  geom_point()

