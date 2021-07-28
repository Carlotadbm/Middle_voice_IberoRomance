#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 28th 2021
#Chapter 6
#Section 6.9. Lack of agentivity

#load libraries
library(tidyverse)
library(scatterpie)
library(maps)

#load tables
enclaves_q_todo <- read_delim("QUEST_total copia.csv", delim="\t") #questionnaire coordinates
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";") #COSER coordinates
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data
encontrar_q <- read_delim("encontrar_questionnaire.csv", delim = ",") #encontrar in the questionnaire
olvidar_q <- read_delim("olvidar_questionnaire.csv", delim = "\t") #olvidar in the questionnaire

#clean tables
enclaves_q_todo  <-  enclaves_q_todo %>%
  rename(CuestionarioID = ID) %>%
  select(CuestionarioID, Latitude, Longitude)
enclaves_coser_todo  <-  enclaves_coser_todo %>%
  rename(COSERID = ID)

#create table with dejar and encontrar in the appropriate contexts
unintentional <- sintactica %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Animado", "Humano"), "Animate", "Inanimate")) %>% 
  filter(Verbo %in% c("dejar", "encontrar"), Sdo_tipic_refl != "permitir") %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory"), 
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM"))

#Plot RM probability by verb and area
##create table
unintentional_verbs <- unintentional %>%
  group_by(Area_dialecta, Verbo) %>%
  count(Verbo, Pron_reflexivo) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

##relevel Verbo & Area_dialecta
unintentional_verbs$Verbo <- factor(unintentional_verbs$Verbo, levels = c("encontrar", "dejar"))
unintentional_verbs$Area_dialecta <- factor(unintentional_verbs$Area_dialecta, levels = c("Rest of the territory", "Northwest"))

##create plot
ggplot(unintentional_verbs, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in transitive verbs of finding and forgetting \nby geographical area", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~ Area_dialecta, scales = "free_x")

##save plot
ggsave("unintentional_verbs.png", height = 4, width = 6) #saves the last plot

#RM probability by verb and subject animacy
unintentional %>%
  filter(Area_dialecta == "Rest of the territory") %>%
  group_by(Verbo) %>%
  count(Animacion_sujeto) %>%
  group_by(Verbo) %>%
  mutate(Total = sum(n), prop = n/Total*100)

#RM probability by verb and delimitation of the object
unintentional %>%
  filter(Area_dialecta == "Rest of the territory") %>%
  mutate(Aspectualidades = ifelse(Aspectualidades %in% c("Pronombre", "SD", "nada", "que", "algo"), "Delimited", 
                                  ifelse(Aspectualidades %in% c("Claus", "Nulo", "Desnudo", "Partitivo", "Cuantificador", "mucho", "tanto", "poco", "menos", "mas"), "Non-delimited", 
                                         Aspectualidades))) %>%
  filter(Aspectualidades != "Interrumpido") %>% 
  group_by(Verbo) %>%
  count(Aspectualidades) %>%
  group_by(Verbo) %>%
  mutate(Total = sum(n), prop = n/Total*100)

#RM probability by verb and different meanings
unintentional %>%
  filter(Area_dialecta == "Rest of the territory") %>%
  group_by(Verbo) %>%
  count(Sdo_tipic_refl, Pron_reflexivo) %>%
  group_by(Verbo, Sdo_tipic_refl) %>%
  mutate(Total = sum(n), percentage = round(n/Total*100, 0))

#encontrar in the questionnaire: map
##create table with RM probabilities by place
encontrar_q_mapa <- encontrar_q %>%
  select(CuestionarioID, EjemploID, Video, Reflexivo) %>%
  mutate(Video = ifelse(Video == "202", "Encontrar algo en el suelo (video 202)",
                        "Encontrar las llaves (video 203)")) %>%
  group_by(CuestionarioID, Video, Reflexivo) %>%
  count(Reflexivo) %>%
  spread(Reflexivo, n) %>%
  mutate(No = replace(No, is.na(No), 0), Si = replace(Si, is.na(Si), 0), Total = Si + No) %>%
  ungroup() %>%
  left_join(enclaves_q_todo) %>%
  group_by(Video)

##load map of spain
spain <- map_data('world', "spain")

##plot map
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = encontrar_q_mapa, 
                  aes(Longitude, Latitude, r = sqrt(Total)/7),
                  cols = c("No", "Si"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("No", "Si"),
    labels = c("No RM", "RM"),
    values = c("No" = "white",
               "Si" = "black")
  ) +
  facet_wrap(~ Video) +
  labs(title = "Frequency of the RM with \"encontrar(se)\" in the questionnaire") +
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
ggsave("encontrar_q_map.png", width=13, height = 4.5)

#dejar meanings in the COSER data: map
##create table
dejar_mapa <- unintentional %>%
  filter(Verbo == "dejar") %>% 
  mutate(Sdo_tipic_refl = ifelse(Sdo_tipic_refl == "S", "Unplanned events of leaving", "Planned events of leaving")) %>%
  group_by(COSERID, Sdo_tipic_refl, Pron_reflexivo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  rename(No = `No RM`, Si = RM) %>%
  mutate(No = replace(No, is.na(No), 0), Si = replace(Si, is.na(Si), 0), Total = Si + No) %>%
  ungroup() %>%
  left_join(enclaves_coser_todo) %>%
  group_by(Sdo_tipic_refl)

##Relevel Sdo_tipic_refl
dejar_mapa$Sdo_tipic_refl <- factor(dejar_mapa$Sdo_tipic_refl, levels = c("Unplanned events of leaving", 
                                                                          "Planned events of leaving"))

##load map of spain
spain <- map_data('world', "spain")

##plot map
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = dejar_mapa, 
                  aes(Longitude, Latitude, r = sqrt(Total)/8),
                  cols = c("No", "Si"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("No", "Si"),
    labels = c("No RM", "RM"),
    values = c("No" = "white",
               "Si" = "black")
  ) +
  facet_wrap(~ Sdo_tipic_refl) +
  labs(title = "Frequency of the RM with \"dejar(se)\" by intentionality of the subject") +
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

##save map
ggsave("dejar_map.png", width=13, height = 4.5)

#olvidar map
##create table
olvidar_q_mapa <- olvidar_q %>%
  filter(Tipo_sintactico == "Transitivo") %>%
  mutate(Reflexivo = ifelse(Reflexivo == "Si", "RM", "No RM")) %>%
  select(CuestionarioID, EjemploID, Verbo, Reflexivo) %>%
  group_by(CuestionarioID, Verbo, Reflexivo) %>%
  count(Reflexivo) %>%
  ungroup() %>%
  left_join(enclaves_q_todo) %>%
  group_by(Verbo)

##load map of spain
spain <- map_data('world', "spain")

##plot map
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_point(data = olvidar_q_mapa, aes(Longitude, Latitude, shape = Verbo, colour = Reflexivo), size = 10) +
  labs(title = "Presence of the RM in video 76 (Olvidar las llaves) in the questionnaire") +
  coord_fixed() +
  theme_bw() +
  scale_color_manual(values = c('lightgrey', 'gray38')) +
  theme(legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(colour = "black"))

##save map
ggsave("olvidar_q_map.png", width=13, height = 9)
