#This script ads geographical information to each player using spatial joins

#author: Julien Migozzi, University of Oxford

#loading packages
library(tidyverse)
library(janitor)
library(sf)
library(mapview)

#Load data
players <-  st_read("data/xvdefrance.gpkg")
dpt <-  st_read("data/spatial/departements-20190101.shp") %>% 
  rename(dpt = nom) %>% 
  select(dpt, code_insee)

#Spatial join
players <-  st_join(players, dpt)

#Quick check
players %>% 
  st_set_geometry(NULL) %>% 
  group_by(dpt) %>% 
  count() %>% 
  arrange(desc(n))


# Extract the country name and create a new column
players$country <- str_extract(players$pob, "\\((.*?)\\)")

# Remove the parentheses from the country column
players$country <- str_replace(players$country, "\\(|\\)", "")

# Remove the trailing parentheses from the country column
players$country <- str_replace(players$country, "\\)$", "")

# Correct mistakes
players <- players %>% 
  mutate(country = ifelse(!is.na(code_insee), "France", country)) %>% 
  mutate(country = ifelse(nom == "Philibert Capitani", "Italie", country)) %>% 
  mutate(country = ifelse(nom == "Jean-Claude Noble", "France", country)) %>% 
  mutate(country = ifelse(nom == "Pierre Pedeutour", "Tunisie", country)) %>% 
  mutate(country = ifelse(nom == "Michel Hondagné-Monge", "Algérie", country)) %>% 
  mutate(country = ifelse(nom == "William Crichton", "Angleterre", country)) %>% 
  mutate(country = ifelse(nom == "Pierre Guillemin", "Angleterre", country)) %>% 
  mutate(country = ifelse(nom == "François Raymond", "Allemagne", country)) %>% 
  mutate(country = ifelse(nom == "Francis Daguerre", "France", country)) %>% 
mutate(country = ifelse(nom == "Jean-Claude Lasserre", "Algérie", country)) %>% 
mutate(country = ifelse(nom == "Vincent Debaty", "Belgique", country))

#Quick check
players %>% 
  st_set_geometry(NULL) %>% 
  group_by(country) %>% 
  count() %>% 
  arrange(desc(n))

# Create a general "region" variable for visualization
players <- players %>% 
  mutate(region = ifelse(country == "France", dpt, country)) %>% 
  mutate(region = ifelse(city == "Nouméa", "Nouvelle-Calédonie", region)) %>% 
  mutate(region = ifelse(city == "Saint-Pierre-et-Miquelon", "Saint-Pierre-et-Miquelon", region)) %>% 
  mutate(region = case_when(grepl("Wallis", city) ~ "Wallis et Futuna",
                          TRUE ~ region)) 

#Quick check
players %>% 
  st_set_geometry(NULL) %>% 
  group_by(region) %>% 
  count() %>% 
  arrange(desc(n))

# Save data
st_write(players, "data/xvdefrance.gpkg", append = F)



