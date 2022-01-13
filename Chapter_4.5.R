#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 4
#Section 4.5: Event structure of the predicate

#load library
library(tidyverse)

#load table
anticausative <- read_delim("Subtabla_anticausativos2019.csv", delim=";")


#Generate table with datives
datives <- anticausative %>%
  filter(!is.na(Tipo_dativo)) %>% #filter out cases without datives
  mutate(Tipo_dativo = ifelse(Tipo_dativo %in% c("Causante_afectado", "Causante_involuntario", "Afectacion_amplia"), "Causer", #redo classification
                              if_else(Tipo_dativo == "Posesivo_afectado", "Possessive", 
                                      ifelse(Tipo_dativo == "Destinatario", "Goal", "Experiencer"))))
#number of examples
datives %>% 
  nrow()

#number of examples by area
datives %>% 
  count(Area_dialectal)

#Dative types per area
##create table with RM probability by dative type and area
datives_types <- datives %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>%
  mutate(Area_dialectal = ifelse(Area_dialectal == "Resto", "Rest of the territory", "Northwest")) %>%
  count(Area_dialectal, Tipo_dativo, Pron_reflexivo) %>%
  group_by(Area_dialectal, Tipo_dativo) %>%
  mutate(total=sum(n), prop=(n/total*100))

##relevel factor
datives_types$Tipo_dativo <- factor(datives_types$Tipo_dativo, levels = c("Experiencer", "Causer", "Goal", "Possessive"))

#generate plot
ggplot(datives_types, aes(x = Tipo_dativo, y= prop, group = Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in anticausative verbs \nby dative type and geographical area", x="Dative type", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialectal)

##save plot
ggsave("antic_dative_types.png", height = 4, width = 6) #saves the last plot

#Dative vs. non-dative per area
##create table with RM probability by Presence of dative and area
dative_presence <- anticausative %>%
  filter(Presencia_del_dativo != "No_se_sabe") %>%
  mutate(Area_dialectal = ifelse(Area_dialectal == "Resto", "Rest of the territory", "Northwest")) %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>%
  mutate(Presencia_del_dativo = ifelse(Presencia_del_dativo == "No", "No dative", "Dative")) %>%
  count(Area_dialectal, Presencia_del_dativo, Pron_reflexivo) %>%
  group_by(Area_dialectal, Presencia_del_dativo) %>%
  mutate(total=sum(n), prop=(n/total*100))

##create plot
ggplot(dative_presence, aes(x=Presencia_del_dativo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with anticausative verbs \ndepending on the presence of a dative by geographical area", x="Presence of a dative", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialectal)

##save plot
ggsave("antic_dative_presence.png", height = 4, width = 6) #saves the last plot


#Correlation between presence of RM by verb and presence of dative by verb
#grouped by verb, selected those who have more than 9 occurrences
##create table with RM probabilities by verb and area
datives_RM_vbs <- anticausative %>%
  filter(Presencia_del_dativo != "No_se_sabe") %>%
  mutate(Area_dialectal = ifelse(Area_dialectal == "Resto", "Rest of the territory", "Northwest")) %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No_RM", "RM")) %>%
  mutate(Presencia_del_dativo = ifelse(Presencia_del_dativo == "No", "No_dat", "Dat")) %>%
  group_by(Area_dialectal, Verbo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = No_RM + RM, RM_prob = RM/Total) %>%
  filter(Total > 9) %>%
  ungroup()

##create table with dative probabilities by verb and area
datives_Dat_vbs <- anticausative %>%
  filter(Presencia_del_dativo != "No_se_sabe") %>%
  mutate(Area_dialectal = ifelse(Area_dialectal == "Resto", "Rest of the territory", "Northwest")) %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No_RM", "RM")) %>%
  mutate(Presencia_del_dativo = ifelse(Presencia_del_dativo == "No", "No_dat", "Dat")) %>%
  group_by(Area_dialectal, Verbo) %>%
  count(Presencia_del_dativo) %>%
  spread(Presencia_del_dativo, n)  %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = No_dat + Dat, Dat_prob = Dat/Total) %>%
  filter(Total > 9) %>%
  ungroup()

##join tables
datives_cor_vbs <- full_join(datives_Dat_vbs, datives_RM_vbs, by=c("Verbo", "Area_dialectal"))

##create plot
ggplot(datives_cor_vbs, aes(Dat_prob, RM_prob)) + 
  geom_point() + 
  labs(title="Correlation between probability of the RM and probability of \na dative by verb", x="Probability of a dative", y="Probability of the RM") + 
  facet_wrap(c("Area_dialectal"))

##save plot
ggsave("datives_cor_vbs_noline.png", width = 9, height = 7) #saves the last plot

