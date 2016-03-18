gapminder <- read.table(file="data/gapminder-FiveYearData.csv",sep=',', header=T)

plot_life_expectancy <- function(country)
{
  country_data <- gapminder[gapminder$country == country,]
  plot(country_data$year, country_data$lifeExp, xlab="Year", 
       ylab="Life Expectancy", ylim=c(0,80), type="l", main=country)
}

plot_life_expectancy (country = "Mexico")
plot_life_expectancy (country = "China")


countries <- levels(gapminder$country)
for(country in countries){
  plot_life_expectancy(country)
}


# Takes a dataset and multiplies the population column
# with the GDP per capita column.
calcGDP <- function(dat, year=NULL, country=NULL) {
  if(!is.null(year)) {
    dat <- dat[dat$year %in% year, ]
  }
  if (!is.null(country)) {
    dat <- dat[dat$country %in% country,]
  }
  gdp <- dat$pop * dat$gdpPercap
  
  new <- cbind(dat, gdp=gdp)
  return(new)
}


ddply(
  .data = gapminder,
       .variables = "continent",
      .fun = function(x) max(mean(x$lifeExp))
 )


daply(
   .data = gapminder,
     .variables = "continent",
     .fun = function(x) mean(x$lifeExp)
 ) -> ave_life_expectancy
ave_life_expectancy
ave_life_expectancy[which.max(ave_life_expectancy)]
ave_life_expectancy[which.min(ave_life_expectancy)]

then <- 
  daply(
  .data = gapminder[gapminder$year==1952,],
  .variables = "continent",
  .fun = function(x) mean(x$lifeExp)
)
now <- 
  daply(
    .data = gapminder[gapminder$year==2007,],
    .variables = "continent",
    .fun = function(x) mean(x$lifeExp)
  )

tpop <-
  ddply(
  .data = gapminder,
  .variable = c("continent", "year"),
  .fun = function(x) sum(x$pop)
)

tgdp <-
  ddply(
    .data = gapminder,
    .variable = c("continent", "year"),
    .fun = function(x) sum(x$pop * x$gdpPercap)
  )

cgdp <- cbind(tpop,tgdp[,3])
cgdp[,4] <- cgdp[,4]/cgdp$V1

ggplot(data=cgdp, aes(x=V1, y=tgdp[, 3], color=continent))+geom_point()
