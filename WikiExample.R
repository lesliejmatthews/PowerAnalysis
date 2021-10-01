

a1 <- c(6,8,4,5,3,4)
a2 <- c(8,12,9,11,6,8)
a3 <- c(13,9,11,8,7,12)
Trt <- c(rep("a1", 6),rep("a2", 6), rep("a3", 6)) 

c1 <- cbind(Trt[1:6], a1)
c2 <- cbind(Trt[7:12], a2)
c3 <- cbind(Trt[13:18], a3)

df <- as.data.frame(rbind(c1,c2,c3))
colnames(df) <- c("Trt", "Yield")

anova <- aov(Yield ~ Trt, df)
summary(anova)
