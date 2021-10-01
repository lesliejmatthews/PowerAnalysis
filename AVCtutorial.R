library(VCA)

data(VCAdata1)
datS5 <- subset(VCAdata1, sample==5)

varPlot(form=y~(device+lot)/day/run, Data=datS5)
