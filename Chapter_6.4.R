#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 6
#Section 6.4. Inherently autobenefactive.

#load libraries
library(tidyverse)
library(maps)
library(scatterpie)

#load tables
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";") #COSER coordinates
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data

#clean tables
enclaves_coser_todo  <-  enclaves_coser_todo %>%
  rename(COSERID = ID)
sintactica <- sintactica %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Animado", "Humano"), "Animate", "Inanimate"))

#Verb: Merecer
##create table
merecer <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo == "merecer")
##proportion of RM
merecer %>% 
  count(Pron_reflexivo) %>%
  mutate(prop = round(n/sum(n)*100, 1))

#Verb: quedar
##create table
quedar <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo == "quedar")

##proportion of RM
quedar %>% 
  count(Pron_reflexivo) %>%
  mutate(prop = round(n/sum(n)*100, 1))

##proportion of RM in intransitive quedar with con/sin
sintactica %>% 
  filter(Verbo == "quedar", 
         Aspectualidades %in% c("Cprep_sin", "Cprep_con")) %>% 
  count(Pron_reflexivo) %>% 
  mutate(total = sum(n), 
         prop = round(n/sum(n)*100, 0))

##location of unmarked intransitive quedar with con/sin
sintactica %>% 
  filter(Verbo == "quedar", 
         Aspectualidades %in% c("Cprep_sin", "Cprep_con"), 
         Pron_reflexivo == "No") %>% 
  group_by(Area_dialecta) %>% 
  distinct(COSERID)


#Verb: ganar
##create table
ganar <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo == "ganar", Muestreo == "Exhaustivo")

##proportion of RM
ganar %>% 
  count(Pron_reflexivo) %>%
  mutate(sum = sum(n), prop = round(n/sum(n)*100, 1))

##Ganar: only cases with RM
sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo == "ganar", 
         Pron_reflexivo == "Si") %>% 
  nrow()

#Verb: echar
##create table
echar <- sintactica %>%
  filter(Tipo_sintactico == "Transitivo", Verbo == "echar") %>%
  mutate(Sdo_tipic_refl = ifelse(Sdo_tipic_refl == "novio", "novio", "other"))

##proportion of RM
echar %>%
  count(Sdo_tipic_refl, Pron_reflexivo) %>%
  group_by(Sdo_tipic_refl) %>%
  mutate(sum = sum(n), prop = round(n/sum(n)*100, 0))

#Verb: hacerse novio (trans): number of occurrences
sintactica %>%
  filter(Tipo_sintactico == "Transitivo", 
         Verbo == "hacer", 
         Sdo_tipic_refl == "novio") %>% 
  count(Pron_reflexivo)

#Verb: sacar
##create table
sacar <- sintactica %>%
  filter(Tipo_sintactico == "Transitivo", 
         Verbo == "sacar") 

##proportion of RM
sacar %>%
  count(Sdo_tipic_refl, Pron_reflexivo) %>%
  group_by(Sdo_tipic_refl) %>%
  mutate(sum = sum(n), prop = round(n/sum(n)*100, 0))

#Verb: pasar (‘to have a {good/bad} time')
##create table
pasar_bien <- sintactica %>%
  filter(Tipo_sintactico == "Transitivo", 
         Verbo == "pasar", 
         is.na(Tipo_semantico), 
         !Sdo_tipic_refl %in% c("negativo", "no_negativo"))

##proportion of RM by context
pasar_bien %>%
  count(Sdo_tipic_refl, Pron_reflexivo) %>%
  group_by(Sdo_tipic_refl) %>%
  mutate(sum = sum(n), prop = round(n/sum(n)*100, 0))

#Verb: pasar (‘to undergo, suffer' in other contexts)
##create table
pasar_otros <- sintactica %>%
  filter(Tipo_sintactico == "Transitivo", 
         Verbo == "pasar", 
         is.na(Tipo_semantico), 
         Sdo_tipic_refl %in% c("negativo", "no_negativo")) 

##proportion of RM
pasar_otros %>%
  count(Pron_reflexivo) %>%
  mutate(sum = sum(n), prop = round(n/sum(n)*100, 1))

##proportion of negative contexts
pasar_otros %>%
  count(Sdo_tipic_refl) %>%
  mutate(sum = sum(n), prop = round(n/sum(n)*100, 1))

#map of ganar, pasar_bien, echar_novio, echar_other, sacar_examen
##table of echar_novio
echar_novio <- echar %>%
  filter(Sdo_tipic_refl == "novio") %>%
  mutate(Verbo = "echar (novio/a)")

##table of echar_other
echar_other <- echar %>%
  filter(Sdo_tipic_refl == "other") %>%
  mutate(Verbo = "echar (other)")

##table of sacar_examen
sacar_examen <- sacar %>%
  filter(Sdo_tipic_refl == "examen")

##join tables and calculate proportions
inh_autoben_map <- rbind(ganar, pasar_bien, echar_other, sacar_examen, echar_novio) %>%
  group_by(COSERID, Verbo, Pron_reflexivo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  ungroup() %>%
  mutate(Si = ifelse(is.na(Si), 0, Si), No = ifelse(is.na(No), 0, No), Total = Si + No, 
         Verbo = ifelse(Verbo == "pasar", "pasar(lo) {bien/mal/etc.}", Verbo)) %>%
  left_join(enclaves_coser_todo) 

##load map of spain
spain <- map_data('world', "spain")

##plot data
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = inh_autoben_map, 
                  aes(Longitude, Latitude, r = sqrt(Total)/7),
                  cols = c("No", "Si"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("No", "Si"),
    labels = c("No RM", "RM"),
    values = c("No" = "white",
               "Si" = "black")
  ) +
  facet_wrap(~ Verbo, nrow=2) +
  labs(title = "Presence of the RM in selected inherently autobenefactive verbs") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, -0.3),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(colour = "black"))

##save plot
ggsave("inherently_autobenef_verbs_map.png", height = 5, width=10)
