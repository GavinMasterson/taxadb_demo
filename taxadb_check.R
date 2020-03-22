library(tidyverse)
library(dplyr)
library(taxadb)
library(dplyr)

GISD_query <- read_delim("amrep_gisd.csv", trim_ws = TRUE, delim = ";") %>%
  .[,-8] %>% 
  separate(Species, c("Genus", "Specific_Epithet", "Subspecific_Epithet"), sep = " ", remove = FALSE) 

td_create("itis")
database <- filter_rank(c("Amphibia", "Reptilia"), "class")

db_check <- GISD_query %>% 
  left_join(database, by = c("Species" = "scientificName")) %>%
  select(species_GISD = Species,
         vernacularName_ITIS = vernacularName,
         order_GISD = Order, order_ITIS = order,
         family_GISD = Family, family_ITIS = family,
         taxonomicStatus_ITIS = taxonomicStatus)


synonym <- db_check[which(db_check$taxonomicStatus_ITIS == "synonym"),]
no_match <- db_check[which(is.na(db_check$order_ITIS) == TRUE),]

