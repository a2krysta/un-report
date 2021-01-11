# Load tidyverse packages
library(tidyverse)

# Read in data from a csv
gapminder_1997 <- read_csv("data/gapminder_1997.csv")

# make a plot
ggplot(data = gapminder_1997) +
  aes(x=gdpPercap, y=lifeExp) +
  labs(x = 'GDP per Capita', y= 'Life Expectancy', title = "Do People in Wealthy Countries Live Longer?") +
  geom_point() +
  aes(color = continent) +
  scale_color_brewer(palette = 'Set2') +
  aes(size = pop/1000000) +
  labs (size = "Population (in millions)") + 
  aes(shape = continent)

# Redo, condensed
ggplot(data = gapminder_1997, aes(x=gdpPercap, y=lifeExp, color = continent, size = pop/1000000, shape = continent )) +
  labs(x = 'GDP per Capita', y= 'Life Expectancy', title = "Do People in Wealthy Countries Live Longer?", 
       size = "Population (in millions)") +
  geom_point() +
  scale_color_brewer(palette = 'Set2')


#Different color palettes
RColorBrewer::display.brewer.all()

# use backtick when there are spaces in columns! 


gapminder_data <- read.csv("data/gapminder_data.csv")

# practice with group (continuous variables)
ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group= country) +
  geom_line()

#plotting categorical variables
ggplot(data=gapminder_1997,aes(x = continent, y = lifeExp) ) +
  geom_jitter() + geom_violin(alpha=0.5,aes(fill = continent))

#Univariate Plots + rotating text
ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 15) +
  theme_light() +
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust = 0.5))
  
  
# Saving plots

ggsave("figures/awesome_plot.jpg", width = 6, height = 4)

violin_plot <- ggplot(data=gapminder_1997,aes(x = continent, y = lifeExp) ) +
  geom_jitter() + geom_violin(alpha=0.5,aes(fill = continent))

violin_plot + theme_bw()
violin_plot <- violin_plot + theme_bw()
violin_plot 
ggsave("figures/awesome_violin_plot.jpg", plot = violin_plot, width = 6, height = 4)

#practice saving a plot

my_awesome_plot <- ggplot(gapminder_1997) + 
  aes(x=gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

ggsave("figures/my_awesome_plot.jpg", plot= my_awesome_plot, width = 6, height = 4)

#Faceting plots
# What if we wanted to plot everything on separate plots?
ggplot(gapminder_1997) + 
  aes(x=gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))








