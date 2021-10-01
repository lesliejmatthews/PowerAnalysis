#'---
#'title: "Testing GitHub Markdown"
#'author: "Leslie"
#'date: "10/1/2021"
#'output: 
#'  html_document:
#'    keep_md: true
#'---

#+r
## brilliant working code here
library(ggplot2)
2 + 2
p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)
p1

