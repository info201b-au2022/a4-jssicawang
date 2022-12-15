library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
library(patchwork)
library(usmap)
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# The functions might be useful for A4
# source("../source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# My question: What counties had the greatest proportion of black prisoners out of the total
# jail population in three representative states: Washington(West Coast), Alabama(South), and
# New York (East Coast)? 

# Creating a new column with proportions of black prisoners to total population for each entry 
# in the dataset
incarceration$black_jail_prop <- incarceration$black_pop_15to64 / incarceration$total_jail_pop


incarceration <- incarceration %>% 
  rename(county = county_name)
# Finding the county with the greatest proportion of black prisoners to total jail population
# in Washington
wa_black_jail_prop <- incarceration %>% 
  select(state, county, black_jail_prop) %>% 
  drop_na() %>% 
  filter(state == "WA") %>% 
  filter(black_jail_prop == max(black_jail_prop)) %>% 
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
  total_population <- incarceration %>%
    select(year, total_jail_pop) %>%
    group_by(total_jail_pop) %>%
    drop_na() 
  return(data.frame(total_population))   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  jail_pop <- get_year_jail_pop()
  jail_pop_plot <- ggplot(data = jail_pop) +
    ggtitle ("US Prison Population Growth (1970-2018)") +
    geom_col(mapping = aes(x = year, y = total_jail_pop))
  return(jail_pop_plot)
} 

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
TEST <- c("CA", "WA", "NY")
get_jail_pop_by_states <- function(states) {
  state_jail_pop <- incarceration %>%
    select(year, state, total_jail_pop) %>%
    drop_na() %>%
    group_by(year) %>% 
    filter(state %in% states)
  
  
  DATA <- state_jail_pop %>% 
    gather(key = state_pop, value = state, state) %>% 
    group_by(state, year)
  return(DATA)   
}

STATES <- c("CA", "WA", "MIN")
plot_jail_pop_by_states <- function(states)  {
  jail_pop <- get_jail_pop_by_states(STATES)
  states_jail_pop_plot <- ggplot(data = jail_pop) +
    geom_line(mapping = aes(x = year, y = total_jail_pop), color = "state") 
  return(states_jail_pop_plot)
  # TODO: Implement this function 
  # ggplot(get_jail_pop_by_states(), aes(x = year), y = total_jail_pop()) +
  #   geom_bar(stat = "identity", position = "dodge") +
  #   labs(title = state + "Jail Population (1970-2018)",
  #        x = "Year",
  #        y = "Jail Population") 
} 

plot_jail_pop_by_states(STATES)

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
incarceration$black_jail_prop <- incarceration$black_jail_pop / incarceration$total_jail_pop
black_jail_prop_new <- incarceration %>% 
  select(state, black_jail_prop) %>% 
  drop_na() %>%
  group_by(state) %>%
  summarise(black_jail_prop_new = max(black_jail_prop))

black_jail_prop_plot <- ggplot(
  black_jail_prop_new, aes(x = state, y = black_jail_prop_new)) + 
  geom_bar(stat = "identity") +
  coord_flip()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# The two minorities that experience the most discrimination by the incarceration system are black people and LatinX people.
# This analysis compares the changes in proportion of the total jail population of black, LatinX, and white people over time.
black_jail_prop_across_us <- incarceration %>% 
  group_by(state) %>% 
  drop_na() %>% 
  filter(year == max(year)) %>% 
  select(black_jail_pop, total_pop) %>% 
  mutate(black_jail_pop / total_pop) %>% 
  summarize(
    prop = sum(black_jail_pop), total = max(total_pop),
    mutate = sum(black_jail_pop / total_pop)
  )

inequality_map <- plot_usmap(
  data = black_jail_prop_across_us, values = "prop", color = "black",
  name = "Proportion of Black Prisoners"
) +
  scale_fill_gradientn(
    colours = c("white", "blue"),
    breaks = c(0, 10, 100, 1000, 10000, 100000),
    trans = "log10", name = "Proportion of Black Prisoners"
  ) +
  labs(title = "Proportions of Black Prisoners to Total Jail Populations Across America in 2018") +
  theme(legend.position = "right")




  