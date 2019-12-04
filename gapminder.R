setwd("~/OneDrive/Documentos/Private/Salerno/Pessoal/Cursos/datacamp/tidyverse/tidyverse")

library(gapminder)
library(dplyr)
library(ggplot2)

data <- gapminder::gapminder

write.csv2(data, file = "gapminder.csv")

head(data)

summary(data)

str(data)

colnames(data)

# Filter the gapminder dataset for the year 1957
data %>%
  filter(year == 1957)

# Filter for China in 2002
data %>%
  filter(country == "China",
         year == 2002)

# Sort in ascending order of lifeExp
data %>%
  arrange(lifeExp)


# Sort in descending order of lifeExp
data %>%
  arrange(desc(lifeExp))

# Filter for the year 1957, then arrange in descending order of population
data %>%
  filter(year == 1957) %>%
  arrange(desc(pop))

# Use mutate to change lifeExp to be in months
data %>%
  mutate(lifeExp = 12 * lifeExp)

# Use mutate to create a new column called lifeExpMonths
data %>%
  mutate(lifeExpMonths = 12 * lifeExp)

# Filter, mutate, and arrange the gapminder dataset
data %>%
  filter(year == 2007) %>%
  mutate(lifeExpMonths = 12 * lifeExp) %>%
  arrange(desc(lifeExpMonths))

# Create gapminder_1952
data_1952 <- data %>%
  filter(year == 1952)

# Change to put pop on the x-axis and gdpPercap on the y-axis
ggplot(data_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()

# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot(data_1952, aes(x = pop, y = lifeExp)) +
  geom_point()

# Change this plot to put the x-axis on a log scale
ggplot(data_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10()

# Scatter plot comparing pop and gdpPercap, with both axes on a log scale
ggplot(data_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()+
  scale_x_log10()+
  scale_y_log10()

# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(data_1952, aes(x = pop, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()

# Add the size aesthetic to represent a country's gdpPercap
ggplot(data_1952, aes(x = pop, y = lifeExp, color = continent, size = gdpPercap)) +
  geom_point() +
  scale_x_log10()

# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(data_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ continent)

# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(data, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ year)

# Summarize to find the median life expectancy
data %>%
  summarize(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
data %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
data %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each year
data %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))


# Find median life expectancy and maximum GDP per capita in each continent in 1957
data %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each year/continent combination
data %>%
  group_by(continent, year) %>%
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

by_year <- data %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time
ggplot(by_year, aes(x = year, y = medianLifeExp)) +
  geom_point() +
  expand_limits(y = 0)

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- data %>%
  group_by(continent, year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) +
  geom_point() +
  expand_limits(y = 0)


# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- data %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, color = continent)) +
  geom_point()

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- data %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x = year, y = medianGdpPercap)) +
  geom_line() +
  expand_limits(y = 0)

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- data %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x= year, y = medianGdpPercap, color = continent)) +
  geom_line() +
  expand_limits(y = 0)

### Bar Plots
# Summarize the median gdpPercap by year and continent in 1952
by_continent <- data %>%
  filter(year == 1952) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x = continent, y = medianGdpPercap)) +
  geom_col()


# Filter for observations in the Oceania continent in 1952
oceania_1952 <- data %>%
  filter(continent == "Oceania", year == "1952")


# Create a bar plot of gdpPercap by country
ggplot(oceania_1952, aes(x = country, y = gdpPercap)) +
  geom_col()

### Histogram

data_1952 <- data %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Create a histogram of population (pop_by_mil)
ggplot(data_1952, aes(x = pop_by_mil)) +
  geom_histogram(bins = 50)


data_1952 <- data %>%
  filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
ggplot(data_1952, aes(x = pop)) +
  geom_histogram()+
  scale_x_log10()

### BoxPlot

data_1952 <-  data %>%
  filter(year == 1952)

# Create a boxplot comparing gdpPercap among continents
ggplot(data_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10()

data_1952 <- data %>%
  filter(year == 1952)

# Add a title to this graph: "Comparing GDP per capita across continents"
ggplot(data_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Comparing GDP per capita across continents")

