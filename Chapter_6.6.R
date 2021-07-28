#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 28th 2021
#Chapter 6
#Section 6.6. Spending time

#load libraries
library(tidyverse)
library(maps)
library(scatterpie)

#load tables
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";") #COSER coordinates
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data
emphasis <- read_delim("In_Trans_temporales_Dic2019.csv", delim=";") #COSER linguistic data with extra annotations (emphasis and focalization)

#clean tables
enclaves_coser_todo  <-  enclaves_coser_todo %>% 
  rename(COSERID = ID)
sintactica <- sintactica %>% 
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Animado", "Humano"), "Animate", "Inanimate")) %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM"), 
         Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory"))

#Create table with verbs of spending time
##verbs in subcorpus E
time_E <- sintactica %>%
  filter(Muestreo == "Exhaustivo", Tipo_sintactico == "Transitivo_tiempo", Verbo %in% c("echar", "llevar")) 
##verbs in corpus NE
time_NE <- sintactica %>%
  filter(Tipo_sintactico == "Transitivo_tiempo", Verbo %in% c("pasar", "tirar")) 
##join tables
time <- rbind(time_E, time_NE)

#Plot RM probability by verb and area
##create table
time_verbs <- time %>%
  group_by(Area_dialecta, Verbo) %>%
  count(Verbo, Pron_reflexivo) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

##Relevel Verbo and Area_dialecta
time_verbs$Verbo <- factor(time_verbs$Verbo, levels = c("tirar", "pasar", "echar", "llevar"))
time_verbs$Area_dialecta <- factor(time_verbs$Area_dialecta, levels = c("Rest of the territory", "Northwest"))

##create plot
ggplot(time_verbs, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in transitive verbs of spending time \nby geographical area", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialecta, scales = "free_x")

##save plot
ggsave("time_verbs.png", height = 4, width = 6) #saves the last plot

#Plot probability of RM by verb and delimitation of the object
##create table and plot data
time %>%
  filter(!is.na(Aspectualidades), 
         !Aspectualidades %in% c("Interrumpido", "Cprep")) %>%
  mutate(Aspectualidades = ifelse(Aspectualidades %in% c("Pronombre", "SD", "sd", "sD", "nada", "que", "algo"), "Delimited", 
                                ifelse(Aspectualidades %in% c("Desnudo", "Partitivo", "Cuantificador", "mucho", "poco", "menos", "mas", "tanto"), "Non-delimited", 
                                       ifelse(Aspectualidades == "Nulo", "Null", Aspectualidades)))) %>% 
  filter(Area_dialecta == "Rest of the territory") %>%
  group_by(Aspectualidades, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n)) %>% 
  ggplot(aes(x=Aspectualidades,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in transitive verbs of spending time \nby delimitation of the object (in the rest of the territory)", x="Delimitation of the object", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Verbo, scales = "free_x")

##save plot
ggsave("time_DO.png", height = 7, width = 9) #saves the last plot

##Plot probability of RM by verb and animacy of the subject
##create table and plot data
time %>% 
  filter(Area_dialecta == "Rest of the territory") %>%
  group_by(Animacion_sujeto, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n)) %>% 
  ggplot(aes(x=Animacion_sujeto,y=prop, group=Pron_reflexivo)) + geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in transitive verbs of spending time \nby animacy of the subject (in the rest of the territory)", x="Animacy of the subject", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Verbo, scales = "free_x")

##save plot
ggsave("time_animacy.png", height = 7, width = 9) #saves the last plot

#Emphasis and focalization
##estar - durative temporal adjuncts
emphasis %>%
  filter(Verbo == "estar") %>%
  group_by(Pron_reflexivo) %>%
  count(Complemento_temporal) %>%
  mutate(Total = sum(n), prop = n/Total*100)

##tirar
emphasis %>% 
  filter(Verbo == "tirar") %>%
  replace_na(list(Exageracion_esfuerzo_extra = "no")) %>%
  filter(Exageracion_esfuerzo_extra != "Interrumpido") %>%
  mutate(Exageracion_esfuerzo_extra = ifelse(Exageracion_esfuerzo_extra == "no", "no", "si")) %>%
  count(Exageracion_esfuerzo_extra) %>%
  mutate(Total = sum(n), prop = round(n/Total*100, 0))

#pasar
##if other adjunt is present
emphasis %>% 
  filter(Verbo == "pasar", 
         Otro_complemento != "interrumpido") %>%
  group_by(Otro_complemento) %>%
  count(Pron_reflexivo) %>%
  mutate(Total = sum(n), prop = round(n/Total*100, 0))

##gerund adjuncts vs. other adjuncts
emphasis %>% 
  filter(Verbo == "pasar", 
         !is.na(Tipo_complemento)) %>%
  mutate(Tipo_complemento = ifelse(Tipo_complemento == "gerundio", "gerund", "other")) %>%
  group_by(Tipo_complemento) %>%
  count(Pron_reflexivo) %>%
  mutate(Total = sum(n), prop = round(n/Total*100, 0))

#Map of RM probability by verb
##create table with frequencies from all verbs and join coordinates
time_map <- time %>%
  group_by(COSERID, Verbo, Pron_reflexivo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  ungroup() %>%
  rename(No = `No RM`, Si = RM) %>%
  mutate(Si = ifelse(is.na(Si), 0, Si), No = ifelse(is.na(No), 0, No), Total = Si + No) %>%
  left_join(enclaves_coser_todo) 

###Relevel Verbo
time_map$Verbo <- factor(time_map$Verbo, levels = c("echar", "tirar", "pasar", "llevar"))

##create table with instances of "llevar" besides subcorpus E and join coordinates
llevarse_no_exh <-  sintactica %>%
  filter(Muestreo != "Exhaustivo", 
         Tipo_sintactico == "Transitivo_tiempo", 
         Verbo == "llevar", 
         Pron_reflexivo == "RM"
         ) %>%
  select(COSERID, Pron_reflexivo, Verbo) %>%
  left_join(enclaves_coser_todo)

###Relevel Verbo
llevarse_no_exh$Verbo <- factor(llevarse_no_exh$Verbo, levels = c("echar", "tirar", "pasar", "llevar"))

##load map of Spain
spain <- map_data('world', "spain")

#create plot
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = time_map, 
                  aes(Longitude, Latitude, r = sqrt(Total)/7),
                  cols = c("No", "Si"), 
                  alpha = 0.5) +
  geom_point(data = llevarse_no_exh, aes(Longitude, Latitude), shape = 17, size = 2) +
  scale_fill_manual(
    breaks = c("No", "Si"),
    labels = c("No RM", "RM"),
    values = c("No" = "white",
               "Si" = "black")
  ) +
  facet_wrap(~ Verbo, nrow=2) +
  labs(title = "Presence of the RM in transitive verbs of spending time") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(1, 0),
        legend.justification = c(0.96, 0.1),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(colour = "black"))

##save plot
ggsave("time_map.png", height = 5, width=7)

  