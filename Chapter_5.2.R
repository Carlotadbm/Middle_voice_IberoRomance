#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 5
#Section 5.2 Analogical effects of the semantic class

#load libraries
library(tidyverse)
library(scatterpie)

#load tables
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";") #COSER full coordinates
full_table <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data

#clean tables
enclaves_coser_todo  <-  enclaves_coser_todo %>%
  rename(COSERID = ID)
full_table <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM"))

#create table with intransitives
intransitive <- full_table %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo"))

##clean table: remove verbs with no cases of RM
###create a table with all verbs with no RM
intransitive_verbs <- intransitive %>%
  group_by(Pron_reflexivo)  %>% 
  count(Verbo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  filter(RM == 0) %>% 
  arrange(RM)

###clean intransitive table (removing those verbs and renaming variables)
intransitive <- intransitive %>%
  filter(!Verbo %in% intransitive_verbs$Verbo) %>% 
  mutate(Tipo_semantico = ifelse(is.na(Tipo_semantico), "Other", Tipo_semantico)) %>% 
  mutate(Tipo_semantico = ifelse(Tipo_semantico %in% c("Pseudocopulativo", "Other"), "Other",
                                 ifelse(Tipo_semantico %in% c("Espontaneo", "Espontaneo_metaforico"), "Spontaneous", 
                                        ifelse(Tipo_semantico %in% c("Movimiento", "Movimiento_metaforico"), "Transl. motion", 
                                               ifelse(Tipo_semantico %in% c("No_traslacional", "No_traslacional_metaforico"), "Non-transl. motion", 
                                                             ifelse(Tipo_semantico == "Emocion", "Emotion", 
                                                                           ifelse(Tipo_semantico == "Corporal", "Grooming",
                                                                                         ifelse(Tipo_semantico %in% c("Proceso_corporal", "Proceso_corporal_metaforico"), "Body process", "Nat. reciprocal"))))))))
  

#check number of verbs
intransitive %>% 
  distinct(Verbo) %>% 
  nrow() 

#check number of verbs by semantic type (considering polysemy)
intransitive %>% 
  group_by(Tipo_semantico) %>% 
  distinct(Verbo) %>% 
  nrow()

#check verbs which are ambiguous between intransitive and anticausative
intransitive %>% 
  filter(Tipo_sintactico == "Intransitivo_Anticausativo") %>% 
  distinct(Verbo) %>% 
  arrange(Verbo) %>% 
  print(n = Inf) 

#Plot of verbs by semantic type
##create table with number of verbs by semantic type and percentages
intransitive_semantic_types <- intransitive %>%
  group_by(Tipo_semantico) %>%
  count(Verbo) %>%
  select(-n) %>%
  count(Tipo_semantico) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(total = sum(n), percentage = round(n*100/total, 1))

##create barplot
ggplot(intransitive_semantic_types, aes(x=reorder(Tipo_semantico, percentage, desc), y=percentage)) + 
  geom_col(aes(), position = "stack", fill = "dark grey") + 
  labs(title="Proportion of intransitive verbs per semantic type", x="Semantic types", y="Percentage of verbs") +
  geom_text(aes(label = n), position = position_stack(vjust = 1)) + 
  theme_classic() + 
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("intransitive_semantic_types.png", width = 7, height = 4) #saves the last plot

#Table: Reflexive intransitive verbs by semantic category in the COSER data
##create table
intransitive_semantic_types_verbs <- intransitive %>%
  group_by(Tipo_semantico) %>%
  count(Verbo) #%>% View()

##write table as csv
write_csv(intransitive_semantic_types_verbs, "intransitive_semantic_types_verbs.csv")

#Plot reciprocal verbs
##create table with RM probabilities in reciprocal verbs
intransitive_reciprocal <- intransitive %>%
  filter(Tipo_semantico == "Nat. reciprocal") %>%
  mutate(Verbo = ifelse(Verbo == "rennir", "reñir", Verbo)) %>%
  count(Verbo, Pron_reflexivo) %>%
  group_by(Verbo) %>%
  mutate(total=sum(n), prop=(n/total*100))

##check global marking proportion
intransitive %>%
  filter(Tipo_semantico == "Nat. reciprocal") %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), prop=round((n/total*100),1))

##relevel Verbs 
intransitive_reciprocal$Verbo <- factor(intransitive_reciprocal$Verbo, 
                                        levels = c("conformar", "pelear", "luchar", "reñir", "discutir"))

##create plot
ggplot(intransitive_reciprocal, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Naturally reciprocal intransitive verbs", x="Verb", y="Presence of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("intransitive_reciprocal.png", height = 4, width = 6) #saves the last plot

#Map of reciprocal verbs
##create table with RM probabilities by verb and place, join coordinates
intransitive_reciprocal_mapa <- intransitive %>%
  filter(Tipo_semantico == "Nat. reciprocal", Verbo %in% c("discutir", "pelear", "rennir")) %>%
  group_by(COSERID, Verbo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  rename(No = `No RM`, Si = RM) %>%
  mutate(Si = ifelse(is.na(Si), 0, Si), No = ifelse(is.na(No), 0, No), Total = Si + No) %>%
  ungroup() %>%
  mutate(Verbo = ifelse(Verbo == "rennir", "reñir", Verbo)) %>%
  left_join(enclaves_coser_todo)

##load map of Spain
spain <- map_data('world', "spain")

##create map
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = intransitive_reciprocal_mapa, 
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
  labs(title = "Presence of the RM in selected intransitive reciprocal verbs") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(1, 0),
        legend.justification = c(1.5, -0.3),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(colour = "black"))

##save plot
ggsave("intransitive_reciprocal_verbs_map.png", height = 5, width=7)

