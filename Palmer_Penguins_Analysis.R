#ANALYZING PALMER PENGUIN DATA SET THROUGH R (for Shortcut: Tools-Keyboard Shortcut)


install.packages("tidyverse")             #installing Packages
library(tidyverse)                        #loading Packages

install.packages("here")                #referencing file easier
install.packages("skimr")               #data cleaning task (summarize and skim)
install.packages("janitor")             #data cleaning (filter, sort)
install.packages("dplyr")
library(here) 
library(skimr) 
library(janitor) 

 
install.packages("palmerpenguins")        #installing Palmer Penguins Data
library("palmerpenguins")
View(penguins)                            #opens the data set in a new file

skim_without_charts(penguins)            #brief summary
glimpse(penguins)                        #summary of dataset along with some starting values
head(penguins)                           #shows first 10 rows of dataset

#DATA MANIPULATION USING SELECT

# 1. Use SELECT statement to select a particular column or exclude a column (CREATING SUBSET)


penguins %>%
  select(species, island)     #select column species & island

penguins %>%
  select(-species,-island)    #select all column except species & island

#2. Renaming column for better understanding
new <- penguins %>%
  rename(island_new = island)  #rename he column

new_1 <- rename_with(new, toupper)  #changes all the column names to upper
View(new_1)

clean_names(penguins)          #makes col-names unique & consistent: char, no & _

# ORGANIZING DATA
penguins %>%
  arrange(bill_length_mm)     #arranges data in asc order of beak length (for desc order use - sign before column)

penguins %>%
  arrange(-bill_depth_mm)

penguins %>%
  group_by(island) %>% drop_na() %>%
  summarize(mean_bill_length_mm = mean(bill_length_mm), 
            mean_bill_depth_mm = mean(bill_depth_mm))

penguins %>%
  group_by(island) %>% drop_na() %>%
  summarize(max_bill_length_mm = max(bill_length_mm), 
            min_bill_length_mm = min(bill_length_mm), 
            max_bill_depth_mm = max(bill_depth_mm),
            min_bill_depth_mm = min(bill_depth_mm))

penguins %>%
  group_by(island, species) %>% drop_na() %>%
  summarize(max_bl = max(bill_length_mm), 
            men_bl = mean(bill_length_mm))
            

penguins %>% filter(species == "Adelie")     #filter data


#TRANSFORMING DATA (combine, split, etc) 
#separate(dataset,new_col, into = c('col_name1', 'col_name2'), sep = " " )

unite(penguins, 'specie_gender', species, sex, sep = "-" )
new <- penguins %>% mutate(body_mass_kg = body_mass_g/1000, 
                    bill_length_m = bill_depth_mm/1000) %>% drop_na()

# USING VISUALIZATION

library(ggplot2)
ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species,
                           shape = sex))


# This graph shows a positive relation between flipper length and body mass.
# It also shows that Gentoo species have the highest flipper length to mass ratio
# It also shows that male penguins have high ratio compare to female ones in each penguin species


ggplot(data = penguins) + 
  geom_smooth(mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, shape = sex))   #line smooth graph

ggplot(data = penguins) + 
  geom_smooth(mapping = aes(x = flipper_length_mm, y = body_mass_g, 
                            linetype = species), color = "black")
# This graph shows a clear relationship between the 3 penguin species 
# Gentoo having the highest followed by Adelie and then Chinstrap

ggplot(data = penguins) + 
  geom_point(mapping = aes(x = bill_length_mm, y = body_mass_g, alpha = species, 
                           shape = sex), color = "purple")

ggplot(data = penguins) + 
  geom_bar(mapping = aes(x = species))     # using geom bar point (count how many time x is repeated)


ggplot(data = penguins) + 
  geom_bar(mapping = aes(x = island, fill = island))  #fill color the whole bar color only makes outline

ggplot(data = penguins) + 
  geom_bar(mapping = aes(x = island, fill = species)) 

# This graph shows us how different species of penguins are divided in different island

# USING FACET FUNCTION IN GGPLOT (WRAP & GRID)




ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  facet_wrap(~species)

# The facet wrap creates different plot for each species



ggplot(data = penguins) + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  facet_grid(island~species)

#helps to visualize complex data, divides the data into sub-categories



                           