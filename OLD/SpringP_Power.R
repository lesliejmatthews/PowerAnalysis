library(RODBC)
library(tidyverse)
library(pwr)
library(wmwpow)
library(samplesize)
library(stats)

df <- read_csv("Sta1andSta2.csv") %>% filter(complete.cases(.)) 

# values needed for power analysis
alpha <- 0.05 # alpha for p-value
mu <- 1 # target difference for effect size
power <- 0.8

df.n <- df %>% mutate(diff = abs(Sta1_SprTP - Sta2_SprTP)) %>% group_by(LakeID) %>% 
  summarize(n = first(N), meanSta1 = mean(Sta1_SprTP), meanSta2 = mean(Sta2_SprTP),
  meanDiff = mean(diff), pooled.sd = sd(append(Sta1_SprTP, Sta2_SprTP)), 
  tt.pvalue = round(t.test(Sta1_SprTP, Sta2_SprTP,paired=T, alternative="two.sided")$p.value,4)) %>% 
  mutate(d = mu/pooled.sd) %>% rowwise(d) %>%
  mutate(tt.samples = pwr.t.test(d=d, power=power, sig.level = alpha,  type = "paired")$n) %>%
  mutate(w.sample2 = ifelse(tt.pvalue > 0.05 & tt.samples > n, "yes", ifelse(tt.pvalue < 0.05, "yes", "no")))


df.bh <- df %>% filter(LakeID=="BALD HILL")

n <- df.bh$N[1]
pooled.sd <- sd(append(df.bh$Sta1_SprTP, df.bh$Sta2_SprTP))
d <- mu / pooled.sd

# sample size for wilcoxon signed rank test
# wilcoxon test
w <- wilcox.test(as.numeric(df.bh$Sta1_SprTP),as.numeric(df.bh$Sta2_SprTP), paired=T, exact=F)$p.value
w

zstat <- qnorm(w$p.value/2)

# sample size
zstat^2/d^2

# non parametric power of wilcox signed rank test
df.nw <- df %>% mutate(diff = abs(Sta1_SprTP - Sta2_SprTP)) %>% group_by(LakeID) %>% 
  summarize(n = first(N), meanSta1 = mean(Sta1_SprTP), meanSta2 = mean(Sta2_SprTP),
  meanDiff = mean(diff), pooled.sd = sd(append(Sta1_SprTP, Sta2_SprTP)), 
  w.pvalue = round(wilcox.test(as.numeric(Sta1_SprTP),as.numeric(Sta2_SprTP), paired=T, exact=F)$p.value,4)) %>% 
  mutate(d = mu/pooled.sd) %>% rowwise(d) %>%
  mutate(zstat = qnorm(w.pvalue/2)) %>% mutate(w.samples=ceiling(zstat^2/d^2)) %>%
  mutate(w.sample2 = ifelse(w.pvalue > 0.05 & w.samples > n, "yes", ifelse(w.pvalue < 0.05, "yes", "no")))
write.csv(df.nw, "SpringP_Sta1_Sta2_wilcox_power.csv")
