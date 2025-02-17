#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 5
#Section 5.6. Emotion and corporal processes

#load library
library(tidyverse)

#load table
enclaves_q_todo <- read_delim("QUEST_total copia.csv", delim="\t") #questionnaire coordinates
full_table <- read_delim("VM_fusion_blanco_revisado_2.csv", delim="\t") #COSER linguistic data

#clean tables
enclaves_q_todo  <-  enclaves_q_todo %>%
  rename(CuestionarioID = ID) %>%
  select(CuestionarioID, Latitude, Longitude)

full_table <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Evento", "Impersonal", "Inanimado", "Vehiculo"), "Inanimate", "Animate")) %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory")) 

##create table with relevant verbs
##table with intransitive verbs
intransitive <- full_table %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo")) 

##table with verbs from subcorpus E
intransitive_E <- intransitive %>%
  filter(Muestreo == "Exhaustivo") %>%
  filter(Verbo == "dormir")

##table with verbs from subcorpus NE (whole corpus)
intransitive_NE <- intransitive %>%
  filter(Verbo %in% c("reir", "cagar", "mear"))

##join tables
intransitive <- rbind(intransitive_E, intransitive_NE)

##disregard metaphorical uses
corporal <- intransitive %>%
  filter(Tipo_semantico != "Proceso_corporal_metaforico")

#Plot RM probability by verb
##clean table
corporal_verbs <- corporal %>%
  count(Verbo, Pron_reflexivo) %>%
  group_by(Verbo) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

##relevel Verbo
corporal_verbs$Verbo <- factor(corporal_verbs$Verbo, levels = c("reir", "mear", "cagar", "dormir"))

##create plot
ggplot(corporal_verbs, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in intransitive verbs \ndepicting corporal processes", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("corporal_verbs.png", height = 4, width = 6) #saves the last plot

#Typical meanings - dormir (full_table must be used)
full_table %>%
  filter(Verbo == "dormir", Pron_reflexivo == "RM") %>%
  group_by(Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  ungroup() %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100)

#Typical meanings - cagar, mear
corporal %>% 
  filter(Verbo %in% c("cagar", "mear")) %>% 
  group_by(Verbo, Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100)

#datives
corporal %>% 
  filter(!is.na(Tipo_dativo)) %>% 
  mutate(Tipo_dativo = ifelse(Tipo_dativo %in% c("Afectado", "Destinatario_posesivo"), "Affected_possessor", Tipo_dativo)) %>% 
  group_by(Verbo, Tipo_dativo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100)

#reir map (questionnaire)
##create table 
reir_q <- read_delim("reir_mapa.csv", delim = "\t") %>%
  left_join(enclaves_q_todo)

##write table as csv to be used in QGIS
write_delim(reir_q, "reir_mapa_coords.csv", delim = "\t")
