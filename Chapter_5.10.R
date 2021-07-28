#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 5
#Section 5.10. Conclusions

#load library
library(tidyverse)

#load tables
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";") #COSER coordinates
full_table <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data

#clean tables
enclaves_coser_todo  <-  enclaves_coser_todo %>% 
  rename(COSERID = ID)
full_table <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory")) 

##create table with relevant verbs
##table with intransitive verbs
intransitive <- full_table %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo")) 

##table with verbs from subcorpus E
intransitive_E <- intransitive %>%
  filter(Muestreo == "Exhaustivo") %>%
  filter(Verbo %in% c("volver", "entrar"))

##table with entrar in the rest of the corpus
intransitive_entrar <- intransitive %>%
  subset(COSERID %in% c(716, 723, 728, 959, 1012, 1015, 1603, 2321, 3209, 3412, 3610, 4108, 4117, 4301, 4401, 4403, 4407, 4419, 4602, 4611, 4613, 4714)) %>%
  subset(Verbo  == "entrar")

##join tables
intransitive <- rbind(intransitive_E, intransitive_entrar)

#Map of volverse and entrarse
##create table with cases of volver with RM
volverse <- intransitive %>%
  filter(Verbo == "volver", Pron_reflexivo == "RM") %>%
  count(COSERID) %>%
  mutate(Verbo = "volverse")

##create table with cases of entrar with RM
entrarse <- intransitive %>%
  filter(Verbo == "entrar", Pron_reflexivo == "RM") %>%
  count(COSERID) %>%
  mutate(Verbo = "entrarse")

##join tables (also with coordinates)
ambos <- rbind(volverse, entrarse) %>%
  left_join(enclaves_coser_todo)

##load map of Spain
spain <- map_data('world', "spain")

##create plot
ggplot(spain, aes(x = long, y = lat)) + #como tabla va nuestra tabla del mapa y como coordenadas x e y, la longitud y la latitud
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") + #y luego usamos la función geom_map para crear el perfil del mapa
  geom_point(data = ambos, aes(Longitude, Latitude), #y geom_point para crear los puntos
             alpha = 0.5, size = 5) + #alpha da la transparencua de los puntos y la posición permite que se muevan un poco para no solaparse  
  facet_wrap(~ Verbo) +
  labs(title = "Geographical distribution of \"entrarse\" and \"volverse\"") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(colour = "black"))

##save plot
ggsave("entrarse_volverse.png", height = 6, width = 17) #saves the last plot
