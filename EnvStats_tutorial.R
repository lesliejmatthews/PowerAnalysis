library(EnvStats)

data(EPA.94b.tccb.df)

df <- EPA.94b.tccb.df

head(df)

summaryStats(TcCB~Area, data=df)

ecdfPlot(df$TcCB)
