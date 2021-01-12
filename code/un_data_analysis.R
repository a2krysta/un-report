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

#Goal for data cleaning: 
# - data from a year close to 2007,
# - A column for country
# - columns for diff types of CO2 emissions (total, per cap, etc)

#Read in a CSV that has weird column headers and an extra row, then select country, year, series, and value
co2_emissions <- read_csv("data/co2-un-data.csv", skip=2,
         col_names = c('region','country','year','series','value','footnotes', 'source')) %>% 
  select (country, year, series, value) %>% 
  mutate(series = recode(series, 
                         'Emissions (thousand metric tons of carbon dioxide)'='total_emissions',
                         'Emissions per capita (metric tons of carbon dioxide)' = 'per_capita_emissions')) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year) %>% 
  mutate(country = recode(country,
                          'Bolivia (Plurin. State of)'='Bolivia',
                          'United States of America' = 'United States',
                          'Venezuela (Boliv. Rep. of)' = 'Venezuela'))

#joining the datasets
inner_join(gapminder_data,co2_emissions) 

anti_join(gapminder_data,co2_emissions) #table with 4 entries that need to be matched up

# Change PR to be a part of the US
gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(continent == "Americas", year == 2007) %>% 
  select(-continent, -year) %>% 
  mutate(country = recode(country, 
                         'Puerto Rico' = 'United States')) %>% 
  group_by(country) %>% 
  summarise(lifeExp = sum(lifeExp * pop)/sum(pop),
            pop = sum(pop),
            gdpPercap = sum(gdpPercap*pop/sum(pop)))

#joining the datasets again
anti_join(gapminder_data,co2_emissions) # empty table
gapminder_co2<- inner_join(gapminder_data,co2_emissions, by="country") #by country is not really needed in this case

# mutate and the if_else
gap_co2_region <- 
  gapminder_co2 %>% 
  mutate(region = if_else(country == 'Canada' | country == 'United States' | country == 'Mexico',
                          "north", "south"))

# or
gap_co2_region <- 
  gapminder_co2 %>% 
  mutate(region = if_else(country %in% c( 'Canada', 'United States', 'Mexico'),
                          "north", "south"))

# && (and) ... ! (not)

# is there a relationship between gdp and co2?
# create a scatter plot of gdp vs co2 emissions, color by region

gap_co2_region %>% ggplot() +
  aes(x=gdpPercap, y=per_capita_emissions, color=region) +
  labs(x='GDP per capita', y='Per Capita Emissions') +
  geom_point()

write_csv(gap_co2_region, "data/gapminder_co2.csv")




         