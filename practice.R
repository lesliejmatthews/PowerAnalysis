
library(gapminder)
gapminder

nz <- filter(gapminder, country == "New Zealand")
nz_model <- lm(lifeExp ~ year, data = nz)
summary(nz_model)
tidy(nz_model)
by_country <- group_by(gapminder, country)
do(by_country, glance(lm(lifeExp~year, data=.)))
do(by_country, tidy(lm(lifeExp ~ year, data = .)))


# ecdf
attach(rock)
head(rock)

Fn <- ecdf(rock$area)
plot(Fn)
dev.off()
