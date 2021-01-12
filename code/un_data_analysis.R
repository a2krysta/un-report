library(tidyverse)

gapminder_data <- read_csv("data/gapminder_data.csv")

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

#same thing, using piping
gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

# can then pipe result by adding %>% to end and use %>% (Command-shift-M) to keep adding results to the next function

# note: summarize == summarise

gapminder_data %>% summarise(avgPop = mean(pop),
                             recent_year = max(year))

#filter recent year only (2007) and find life exp
gapminder_data %>% filter(year == 2007) %>% summarise(averageLifeExp = mean(lifeExp))

#find average GDP per capita for the first year in the database
gapminder_data %>% summarise(first_year = min(year)) #1952
gapminder_data %>% filter(year==1952) %>% 
  summarise(GDPperCap = mean(gdpPercap), first_year=min(year))

# filter can use: >, <, >=, <=, True, False, etc

# is life expectancy getting higher over time? How can we check? Let's use "Group by"
gapminder_data %>% 
  group_by(year) %>%  #can change to continent as well
  summarise(averageLifeExp = mean(lifeExp))

#mutate to add a column
gapminder_data %>% 
  mutate(gdp = gpdPercap * pop)

# make a column that is population in millions
gapminder_data %>% 
  mutate (popMillions = pop/1000000)

# select -- to specify columns we want to keep

gapminder_data %>% 
  select(year, pop)

gapminder_data %>% 
  select(-continent)

gapminder_data %>% 
  select(country, continent, year, lifeExp)

# arrange(year) - arrange rows

#long versus wide / rows versus columns, and how to transform and pivot
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#rename() - rename columns

# create a new dataset with only data from the americas and 2007
# drop the continent and year columns

gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(continent == "Americas", year == 2007) %>% 
  select(-continent, -year)






  
  