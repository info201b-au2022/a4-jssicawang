library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)
install.packages("patchwork")
library(patchwork)
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# My question: What counties had the greatest proportion of black prisoners out of the total
# jail population in three representative states: Washington(West Coast), Alabama(South), and
# New York (East Coast)

# Creating a new column with proportions of black prisoners to total population for each entry 
# in the dataset
incarceration$black_jail_prop <- incarceration$black_pop_15to64 / incarceration$total_jail_pop

# Finding the county with the greatest proportion of black prisoners to total jail population
# in Washington
wa_black_jail_prop <- incarceration %>% 
  select(state, county, black_jail_prop) %>% 
  drop_na() %>% 
  filter(state == "WA") %>% 
  filter(black_jail_prop == max(black_jail_prop))
  pull(county)

# Finding the county with the greatest proportion of black prisoners to total jail population
# in Alabama
al_black_jail_prop <- incarceration %>% 
  select(state, county, black_jail_prop) %>% 
  drop_na() %>% 
  filter(state == "AL") %>% 
  filter(black_jail_prop == max(black_jail_prop))
  pull(county)

# Finding the county with the greatest proportion of black prisoners to total jail population
# in New York
ny_black_jail_prop <- incarceration %>% 
  select(state, county, black_jail_prop) %>% 
  drop_na() %>% 
  filter(state == "NY") %>% 
  filter(black_jail_prop == max(black_jail_prop))
  pull(county)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function 
  total_population <- df %>%
    group_by(year) %>%
    filter(total_jail_pop != "NA") %>%
  summarise(total_jail_pop = sum(total_jail_pop))
  return(total_population)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  ggplot(get_year_jail_pop(), aes(x = year), y = total_jail_pop()) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "American Jail Population (1970-2018)",
         x = "Year",
         y = "Jail Population") 
} 

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  state_population <- df %>%
    group_by(state) %>%
    filter(total_jail_pop != "NA") %>%
    summarise(total_jail_pop = sum(total_jail_pop))
  return(state_population)   
}

plot_jail_pop_by_states <- function()  {
  # TODO: Implement this function 
  ggplot(get_jail_pop_by_states(), aes(x = year), y = total_jail_pop()) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = state + "Jail Population (1970-2018)",
         x = "Year",
         y = "Jail Population") 
} 

plot_jail_pop_by_states(Alabama)

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
incarceration$black_jail_prop <- incarceration$black_pop_15to64 / incarceration$total_jail_pop
black_jail_prop <- incarceration %>% 
  select(state, black_jail_prop) %>% 
  drop_na() %>% 
  filter(black_jail_prop == max(black_jail_prop))
View(black_jail_prop) # I don't really understand why this creates a chart full of infinite values?

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
blank_theme <- theme_bw() + 
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    acis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

inequality_map <- ggplot(adm_rate_prop) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = prop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(min(adm_rate_prop), max(adm_rate_prop)), na.value = "white", low = "yellow", high = "red") +
  blank_theme
## Load data frame ---- 


