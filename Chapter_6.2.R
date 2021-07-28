#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 6
#Section 6.2 Indirect reflexive constructions

#load libraries
library(tidyverse)

#load tables
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";") #COSER coordinates
enclaves_coser_duracion <- read_delim("COSER_duracion.csv", delim="\t") #COSER duration (no COSERID with relevant data is lacking)
indirectos <- read_delim("Subtabla_reflexivos2019_soloindirectos.csv", delim = ";") #COSER linguistic data

#clean tables
enclaves_coser_todo  <-  enclaves_coser_todo %>%
  rename(COSERID = ID)
indirectos <- indirectos %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>%
  mutate(Area_dialecta = ifelse(Area_dialectal == "Noroeste", "Northwest", "Rest of the territory"))

#Section 6.2.1 Recipient datives
##RM probability
indirectos %>%
  filter(Tipo_sintactico %in% c("Indirecto", "Mediado_indirecto"), 
         Tipo_semantico %in% c("Destinatario", "Reflexivo"), 
         is.na(Poseido)) %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 0), 
         total = sum(n))

#Section 6.2.2 Autobenefactives
##clean table
autobenefactives <- indirectos %>%
  filter(Tipo_sintactico %in% c("Indirecto", "Mediado_indirecto"), 
         Tipo_semantico == "Autobenefactivo", 
         is.na(Poseido), 
         Pron_reflexivo == "RM")

##number of examples
autobenefactives %>% 
  nrow()

##number of locations
autobenefactives %>% 
  distinct(COSERID) %>% 
  nrow()

##Create map with RM frequency
###create table (Standardise frequencies per duration of interview and join with coordinates)
autobenefactives_map <- autobenefactives %>%
  count(COSERID) %>%
  left_join(enclaves_coser_duracion) %>%
  mutate(prop_est_autob = n/Duracion_estandar) %>%
  rename(n_autob = n) %>%
  left_join(enclaves_coser_todo)

###write table to create map with QGIS
write_delim(autobenefactives_map, "autobenefactives_map.csv", delim = "\t")


#Section 6.2.3 Possessive dative
##clean table
possessive <- indirectos %>%
  filter(Tipo_sintactico %in% c("Indirecto", "Mediado_indirecto"), !is.na(Poseido), 
         Tipo_semantico != "Involuntario")

##number of examples
possessive %>% 
  nrow()

##number of locations
possessive %>% 
  distinct(COSERID) %>% 
  nrow()

##Create map with RM probability 
###Create table and join coordinates
possessive_map <- possessive %>%
  group_by(COSERID, Pron_reflexivo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  rename(No_RM = `No RM`) %>%
  mutate(No_RM = replace(No_RM, is.na(No_RM), 0), RM = replace(RM, is.na(RM), 0), 
         Prob_RM = RM/(RM + No_RM)) %>%
  ungroup() %>%
  left_join(enclaves_coser_todo)

###write table to create map with QGIS
write_delim(possessive_map, "dat_possessive_map.csv", delim = "\t")
  
