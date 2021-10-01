df <- as.data.frame(rnorm(50, mean=10, sd=2))

colnames(df) <- "x"
 
df$y <- df$x+2
