pvalue <-function(x, y) {
  corr=(cor.test(x, y, alternative="two.sided", exact=FALSE, method="kendall"))
  pval <-if(is.na(corr$p.value)) {1} else {corr$p.value}
  return(pval)
}