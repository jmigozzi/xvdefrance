#This script 
# 1. scrapes Wikipedia to: 
# - collect the list of all French players
# - extract their place of birth (when available)
#Then
# 2. geolocates 
# - all players with OSM
# - performing some manual corrections
# 3. Export the output as a geopackage

#author: Julien Migozzi, University of Oxford

#loading packages
library(rvest)
library(tidyverse)
library(janitor)
library(Rcrawler)
library(sf)
library(mapview)
library(tidygeocoder)

Sys.setlocale("LC_TIME", "fr_FR") # for dates in French


#1. Source list of players ----

# Get Wikipedia url
url <- "https://fr.wikipedia.org/wiki/Liste_des_s%C3%A9lectionn%C3%A9s_en_%C3%A9quipe_de_France_de_rugby_%C3%A0_XV"

# Read page
page <- read_html(url)

# Get all the players in one df
all_players <- page %>% 
  html_nodes("table.wikitable") # 12 tables

all_players <- all_players %>%
  rvest::html_table(fill = TRUE) 

all_players <- bind_rows(all_players) %>%
  clean_names()

# Add url for each player
player_urls <- page %>% 
  html_nodes("td:nth-child(1) a") %>%
  html_attr("href")

# Remove URLs containing "#cite_note"
cleaned_url_list <- player_urls[!grepl("#cite_note", player_urls)]
cleaned_url_list <- player_urls[!grepl("#", player_urls)]

# Add urls to the df
all_players$url <- cleaned_url_list
all_players$url <- paste0("https://fr.wikipedia.org/", all_players$url)
all_players$url

#Save file
write.csv(all_players, "data/extract_wikipedia_list_french_players.csv", row.names = F)

#Load file
all_players <- read.csv("data/extract_wikipedia_list_french_players.csv", stringsAsFactors = F)

#2. Source DOB and POB of players ----

# Initialize an empty dataframe to store results
result_df <- data.frame(url = character(0), birth_info = character(0))

# Iterate through each player's Wikipedia URL (about 10 mins)
for (url in all_players$url) {  # Replace with your actual vector
  if (url != "") {
    tryCatch({
      page <- read_html(url)
      # page <- read_html("https://fr.wikipedia.org//wiki/Pierre_Albaladejo")
      # Extract birth information
      info <- page %>%
        html_nodes("tr:contains('Naissance')") %>%
          html_text2(preserve_nbsp = F) #%>%
        # gsub("\n", " ", .) %>%
        # trimws()
      
      if (length(info) == 0) {
        info <- NA
      }
      
      # Append to the result dataframe
      result_df <- result_df %>% 
        add_row(url = url, birth_info = info)
    }, error = function(e) {
      result_df <- result_df %>% 
        add_row(url = url, birth_info = NA)
    })
  } else {
    result_df <- result_df %>% 
      add_row(url = url, birth_info = NA)
  }
}

# Print the result dataframe
print(result_df)


# 3. Extract DOB and POB fro the birth info ----

  #Check variable
  result_df$birth_info

  # Remove "(43 ans)" from the info column using stringr
  result_df$birth_info <- str_remove(result_df$birth_info, "\\(\\d+ ans\\)")
  result_df$birth_info <-  str_remove_all(result_df$birth_info, "\\[\\d+\\]")
  
  # Extract date of birth using regular expressions
  result_df$dob <- sub("Naissance\\s+(\\d+\\s+\\S+\\s+\\d+).*", "\\1", result_df$birth_info)
  
  # Convert date_of_birth to Date format with lubridate
  result_df$dob <-  dmy(result_df$dob, locale = "fr_FR.utf8")
  

  # Extract place of birth using regular expressions
  result_df$place_of_birth <- sub("\\s*\\(\\d+\\s*ans\\)\\s*", "", result_df$birth_info)
  result_df$pob <- sub("Naissance\\s+\\d+(?:er)?\\s+\\S+\\s+\\d+\\s+", "", result_df$birth_info)
  result_df$pob <- gsub("à ", "", result_df$pob) #remove a

  result_df <- result_df %>% relocate(url, dob, pob, birth_info)
  
  #Manual corrections
  
  result_df <- result_df %>% 
    mutate(pob = case_when(pob == "Nay" ~ "Nay (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Alexandre_Chazalet" ~ "Valence (France)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Patrick_Arlettaz" ~ "Perpignan (France)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Eric_Melville" ~ "Le Cap (Afrique du Sud)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Jean-Francois_Phliponeau" ~ "Bordj El Kiffan (Algérie)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Charles_Ollivon" ~ "Bayonne (France)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Loann_Goujon" ~ "Albertville (France)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Maurice_Boyau" ~ "Si Mustapha (Algérie)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Marcel_Burgun" ~ "Saint-Pétersbourg (Russie)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Charles_Vareilles" ~ "Côn Son (Vietnam)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Jacques_Duffourcq" ~ "Salies-de-Béarn (France)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Jean-Francois_Imbernon" ~ "Sahorre (France)", TRUE ~ pob)) %>% 
   mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Jean-Pierre_Hortoland" ~ "Béziers (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Jean-Pierre_Pesteil" ~ "Vendres (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Ren%C3%A9_S%C3%A9guier" ~ "Vendres (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Guy_Gasparotto" ~ "Aucamville (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Michel_Arino" ~ "Penne-d'Agenais (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Fernand_Barthe" ~ "Saint-Macaire (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Guillaume_Bouic" ~ "Bazas (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Albert_Ambertc" ~ "Béziers (France)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Jacques_Ballarin" ~ "Buenos Aires (Argentine)", TRUE ~ pob)) %>% 
    mutate(pob = case_when(url == "https://fr.wikipedia.org//wiki/Auguste_Jarasse" ~ "Ussel (France)", TRUE ~ pob)) %>% 
    
    mutate(pob = case_when(pob == "" ~ NA, TRUE ~ pob)) %>% 
    group_by(url) %>% 
    mutate(n = n()) %>% 
    ungroup()
   

  # Remove duplicates
  result_df <- result_df %>%
    filter(!((n == 2 & !grepl("France", pob)))) 
  
  # Select variables
  result_df <- result_df %>% 
    select(url, dob, pob, birth_info)
   
  # Clean name of locations and create a new variable called city
  result_df <- result_df %>% 
mutate(city = gsub("\\s*\\([^\\)]+\\)","", pob)) %>% 
mutate(city = trimws(city)) %>% 
mutate(city = case_when(grepl("Paris", city) ~ "Paris", TRUE ~ city)) %>% 
mutate(city = case_when(grepl("^Lyon", city) ~ "Lyon", TRUE ~ city)) %>% 
mutate(city = case_when(grepl("Sainte-Foy-lès-Lyon", city) ~ "Sainte-Foy-lès-Lyon",
                        TRUE ~ city)) %>% 
  mutate(city = case_when(grepl("Bourgoin-Jallieu, Isère", city) ~ "Bourgoin-Jallieu",
                          TRUE ~ city)) %>% 
    mutate(city = case_when(grepl("Brive", city) ~ "Brive-la-Gaillarde",
                          TRUE ~ city)) %>% 
  mutate(city = case_when(grepl("Montelimar", city) ~ "Montélimar",
                          TRUE ~ city)) %>% 
    mutate(city = case_when(grepl("Naissance", city) ~ NA,
                            TRUE ~ city)) %>% 
  mutate(city = case_when(city == "Bourg" ~ "Bourg-en-Bresse",
                          TRUE ~ city)) 
  
#Save intermediary file
# write.csv(result_df, "data/extract_wikipedia_french_players_geoinformation.csv", row.names = F)

#Load
# result_df <- read.csv("data/extract_wikipedia_french_players_geoinformation.csv")

# Create main dataframe: Join data of pob and dob to list of players
  
all_data <- left_join(all_players, result_df, by = "url")
  
all_data <- all_data %>% 
  mutate(first = str_extract(premiere_selection, "\\b(\\d{1,2}(?:er)? [a-zA-Zéèêëàâäôöûüç]+ \\d{4})\\b")) %>% 
  mutate(last = str_extract(derniere_selection, "\\b(\\d{1,2}(?:er)? [a-zA-Zéèêëàâäôöûüç]+ \\d{4})\\b")) %>% 
  mutate(first_selection = dmy(first, locale = "fr_FR.utf8")) %>% 
  mutate(last_selection = dmy(last, locale = "fr_FR.utf8"))  %>% 
  mutate(decade = floor_date(first_selection, years(10)))

all_data <- all_data %>% 
  relocate(nom, matchs, dob, pob, city, no, decade, first_selection, last_selection) %>% 
  mutate(pob = trimws(pob))


  # 4. Geocoding ----
  # WIth OSM
  
all_data_geo <-  all_data %>%
    geocode(address = pob, method = "osm", verbose = TRUE) 

# Correct coding mistakes
all_data_geo <-  all_data_geo %>% 
  mutate(long = case_when(grepl("Saint-Jean-de-Luz", pob) ~ -1.6631, 
                          TRUE~ long) ) %>% 
mutate(lat = case_when(grepl("Saint-Jean-de-Luz", pob) ~ 43.3881, # wrong geocoding
                        TRUE~ lat) ) %>% 
  
  mutate(long = case_when(grepl("Benjamin Boyet", nom) ~ 4.874339,# wrong geocoding for Vienne
                         TRUE~ long)) %>% 
  mutate(lat = case_when(grepl("Benjamin Boyet", nom) ~ 45.525587, 
                         TRUE~ lat)) 
  # mutate(long = case_when(grepl("Auguste Jarasse", nom) ~ 2.316667,# wrong geocoding for Ussel
  #                         TRUE~ long)) %>% 
  # mutate(lat = case_when(grepl("Auguste Jarasse", nom) ~ 45.55, 
  #                        TRUE~ lat)) 

# Save results
write.csv(all_data_geo, "data/geoloc_french_players_osm.csv", row.names = F)

#Load results
# all_data_geo <-  read.csv("data/geoloc_french_players_osm.csv")

#5. Quick EDA with mapview ----

 
 players_sf <- all_data_geo %>% 
   filter(!is.na(long)) %>% # remove players without pob
    st_as_sf(coords = c("long", "lat"), crs = 4326)  # %>% 
   # st_jitter(factor = 0.00002) # avoid overlapping of points
mapview(players_sf)
 
#6. Save spatial data ----

 # saveRDS(players_sf, "all_players_geo.rds")
 st_write(players_sf, "data/xvdefrance.gpkg", append = F)
 