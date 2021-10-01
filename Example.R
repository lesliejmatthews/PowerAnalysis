
# EXAMPLE
d<-seq(.1,2,by=.1) # effect sizes
n<-1:100 # sample sizes
t.test.power.effect<-as.data.frame(do.call("cbind",lapply(1:length(d),function(i)
{
  sapply(1:length(n),function(j)
  {
    power.t.test(n=n[j],d=d[i],sig.level=0.05,power=NULL,type= "one.sample", alternative="greater")$power
  })
})))
t.test.power.effect[is.na(t.test.power.effect)]<-0 # some powesr couldn't be calculated, set these to zero
colnames(t.test.power.effect)<-paste (d,"effect size")

pwr.t.test(d=0.2,n=60,sig.level=0.10,type="one.sample",alternative="two.sided")

# effect size+ 2 ug/L with s.d. of 1 is a cohen's d of 2

d<-seq(.5,10,by=.5) # standard deviations
n<-1:150 # sample sizes
t.test.power.effect<-as.data.frame(do.call("cbind",lapply(1:length(d),function(i)
{
  sapply(1:length(n),function(j)
  {
    power.t.test(n=n[j],d=2/d[i],sig.level=0.05,power=NULL,type= "one.sample", alternative="greater")$power
  })
})))
t.test.power.effect[is.na(t.test.power.effect)]<-0 # some powesr couldn't be calculated, set these to zero
colnames(t.test.power.effect)<-paste (d,"effect size")



library(ggplot2);library(reshape)
obj<-cbind(size=1:150,t.test.power.effect) #flip object for melting
melted<-cbind(melt(obj, id="size"),effect=rep(d,each=150)) # melt and bind with effect for mapping
ggplot(data=melted, aes(x=size, y=value, color=as.factor(effect))) + geom_line(size=2,alpha=.5) +
  ylab("power") + xlab("sample size") + ggtitle("t-Test")+theme_bw() + scale_color_discrete(name="Effect Size")
# wow ggplot2 is amazing in its brevity
# need to tweak legend and lty, but otherwise very similar

