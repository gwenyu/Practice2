gapminder <- read.table(file="data/gapminder-FiveYearData.csv",sep=',', header=T)


plot_life_expectancy <- function(country,color){
  country_data <- gapminder [gapminder$country == country,]
  plot(country_data$year, country_data$lifeExp, xlab="Year", 
       ylab="Life Expectancy", ylim=c(0,80), type="l", main=country, col=color)
}

plot_life_expectancy(country=="Mexico", color="green")
