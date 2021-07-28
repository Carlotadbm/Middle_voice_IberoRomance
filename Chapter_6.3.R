#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 6
#Section 6.3

#load library
library(tidyverse)

#load tables
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";") #COSER coordinates
enclaves_coser_duracion <- read_delim("COSER_duracion.csv", delim="\t")  #COSER duration (no COSERID with relevant data is lacking)
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data

#clean tables
enclaves_coser_todo  <-  enclaves_coser_todo %>%
  rename(COSERID = ID)
sintactica <- sintactica %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Animado", "Humano"), "Animate", "Inanimate"))

#create table with marked transitive verbs (also in the appendix: how many)
transitive <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path")) %>%
  filter(Pron_reflexivo == "Si") 

#number of verbs
transitive %>%
  distinct(Verbo) %>%
  nrow() 

#Check semantic types
##clean table and calculate percentages
transitive_semantic_types <- transitive %>%
  replace_na(list(Tipo_semantico = "Other")) %>%
  mutate(Tipo_semantico = ifelse(Tipo_semantico == "Movimiento", "Translational motion",
                                 ifelse(Tipo_semantico == "Cognicion", "Cognition",
                                        ifelse(Tipo_semantico == "Percepcion", "Perception",
                                               ifelse(Tipo_semantico %in% c("Proceso_corporal", "Corporal"), "Grooming & Body processes", 
                                                      ifelse(Tipo_semantico == "Emocion", "Emotion", 
                                                             ifelse(Tipo_semantico == "Espontaneo", "Spontaneous", "Other"))))))) %>%
  group_by(Tipo_semantico) %>%
  count(Verbo) %>%
  select(-n) %>%
  count(Tipo_semantico) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(total = sum(n), percentage = round(n*100/total, 1))

##calculate percentages and number of verbs in Kemmer's classes
transitive_semantic_types %>% 
  mutate(Tipo_semantico = ifelse(Tipo_semantico == "Other", "Other", "Kemmer's")) %>% 
  group_by(Tipo_semantico) %>% 
  mutate(n_groups = sum(n),
         percentage_groups = sum(percentage))

##create plot
ggplot(transitive_semantic_types, aes(x=reorder(Tipo_semantico, percentage, desc), y=percentage)) + 
  geom_col(aes(), position = "stack", fill = "dark grey") + 
  labs(title="Proportion of transitive verbs by semantic type", x="Kemmer's semantic types", y="Percentage of verbs") +
  geom_text(aes(label = n), position = position_stack(vjust = 1)) + 
  theme_classic() + 
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0, 60)

##save plot
ggsave("transitive_semantic_types.png", width = 7, height = 6) #saves the last plot

#find out which verb appear in each semantic class
##create table
transitive_semantic_types_verbs <- transitive %>%
  group_by(Tipo_semantico) %>%
  count(Verbo)

##write as a csv
write_csv(transitive_semantic_types_verbs, "transitive_semantic_types_verbs.csv")

#Focus on the verbs which were exhaustively collected
##Create table
###verbs in subcorpus E
transitive_exh_E <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo %in% c("saber", "llevar", "comer", "ver", "dejar", "tomar", "estudiar","mirar", "ganar", 
                      "aprender", "pensar", "entender", "leer", "encontrar"), 
         Muestreo == "Exhaustivo")

###verbs in corpus NE
transitive_exh_NE <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo %in% c("traer", "creer", "pasar",  "beber", "subir",  "recordar", "bajar"))

###join tables
transitive_exh <- rbind(transitive_exh_E, transitive_exh_NE)

##Calculate number of verbs
transitive_exh %>% 
  distinct(Verbo) %>% 
  nrow()

##Calculate presence of the RM
transitive_exh %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), prop=(n/total*100))

##Plot frequency of the RM by verb
###create table with RM probabilities
transitive_exh_vbs <- transitive_exh %>%
  count(Verbo, Pron_reflexivo) %>%
  group_by(Verbo) %>%
  mutate(total=sum(n), prop=(n/total*100))

###Order verbs by frequency of the RM
transitive_exh_vbs_ordered <- transitive_exh_vbs %>%
  spread(Pron_reflexivo, prop) %>%
  filter(!is.na(No)) %>%
  arrange(No)

###relevel Verbo
transitive_exh_vbs$Verbo <- factor(transitive_exh_vbs$Verbo, levels = transitive_exh_vbs_ordered$Verbo)

###create plot
ggplot(transitive_exh_vbs %>% mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")), 
       aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in exhaustively collected transitive verbs", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###save plot
ggsave("transitive_exh_vbs.png", height = 6, width = 9) #saves the last plot

#Animacy of the subject
##How many verbs take inanimate subjects?
transitive_exh %>%
  group_by(Animacion_sujeto) %>%
  count(Verbo) %>%
  select(-n) %>%
  count(Animacion_sujeto)

##Which verbs take inanimate subjects?
transitive_exh %>%
  group_by(Animacion_sujeto) %>%
  count(Verbo) %>%
  arrange(desc(Animacion_sujeto))

#Map with relative frequency of RM
##create table with relative frequencies
transitive_map <- transitive %>%
  mutate(Muestreo = ifelse(Muestreo == "No_exhaustivo2017", "No_exhaustivo", Muestreo)) %>%
  group_by(Muestreo) %>%
  count(COSERID) %>%
  mutate(mean = mean(n)) %>% ##Calculate mean per subcorpus
  ungroup() %>%
  mutate(mean_difference = mean/min(mean)) %>% #divide mean frequency by the mean frequency in subcorpus NE
  left_join(enclaves_coser_duracion) %>%
  mutate(prop_est_trans = (n/Duracion_estandar)/mean_difference) %>% #calculate estimate
  left_join(enclaves_coser_todo)

##save table to create map in QGIS
write_delim(transitive_map, "transitive_map.csv", delim = "\t")

