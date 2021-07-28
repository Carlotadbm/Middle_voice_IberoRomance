#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 3
#Section 3.3.

#load libraries
library(tidyverse)

#load table
semantica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t")

#clean table
semantica <- semantica %>%
  filter(Muestreo == "Exhaustivo") %>% #corpus E
  filter(!Tipo_sintactico %in% c("Transitivo_tiempo", "Indirecto", "Directo", "Sin_cambio_valencial", "Logoforico_indirecto", "Discontinuo",
                                 "Transitivo_path", "Mediado", "Mediado_indirecto", "Logoforico", "Discontinuo_indirecto", "Transitivo")) %>% #Syntactic types included
  filter(!is.na(Tipo_sintactico)) #remove NAs
  
#Section 3.3.1.1
#Grooming or body actions
#clean table
grooming <- semantica %>%
  filter(Tipo_semantico %in% c("Corporal", "Corporal_metaforico")) #by semantic type

#number of examples
nrow(grooming) 
#proportion of reflexive uses
grooming %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
grooming %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
grooming %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 


#Section 3.3.1.2
#Body processes
#clean table
body_process <- semantica %>%
  filter(Tipo_semantico %in% c("Proceso_corporal", "Proceso_corporal_metaforico")) #by semantic type

#Check number of examples
nrow(body_process) 
#Proportion of reflexive uses
body_process %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
body_process %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
body_process %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#Section 3.3.1.3
#Body posture
#clean table
posture <- semantica %>%
  filter(Tipo_semantico %in% c("Postural", "Postural_metaforico")) #by semantic type

#Check number of examples
nrow(posture) 
#Proportion of reflexive uses
posture %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
posture %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
posture %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 


#Section 3.3.1.4
#Non translational motion
#clean table
non_translational <- semantica %>%
  filter(Tipo_semantico == "No_traslacional") #by semantic type

#Check number of examples
nrow(non_translational) 
#Proportion of reflexive uses
non_translational %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
non_translational %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
non_translational %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 


#Section 3.3.1.5
#Translational motion
translational <- semantica %>%
  filter(Tipo_semantico %in% c("Movimiento", "Movimiento_metaforico")) #by semantic type

#Check number of examples
nrow(translational) 
#Proportion of reflexive uses
translational %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
translational %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
translational %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#Section 3.3.2.1
#Emotion
#clean table
emotion <- semantica %>%
  filter(Tipo_semantico == "Emocion") #by semantic type

#Check number of examples
nrow(emotion) 
#Proportion of reflexive uses
emotion %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
emotion %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
emotion %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 


#Section 3.3.2.2
#Cognition
#clean table
cognition <- semantica %>%
  filter(Tipo_semantico == "Cognicion") #by semantic type

#Check number of examples
nrow(cognition) 
#Proportion of reflexive uses
cognition %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
cognition %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
cognition %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#Section 3.3.2.3
#Perception
#clean table
perception <- semantica %>%
  filter(Tipo_semantico == "Percepcion") #by semantic type

#Check number of examples
nrow(perception) 
#Proportion of reflexive uses
perception %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
perception %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
perception %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#Section 3.3.3
#Spontaneous
spontaneous <- semantica %>%
  filter(Tipo_semantico %in% c("Espontaneo", "Espontaneo_metaforico")) #by semantic type

#Check number of examples
nrow(spontaneous) 
#Proportion of reflexive uses
spontaneous %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
spontaneous %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
spontaneous %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 


#Section 3.3.4
#Naturally reciprocal
#load reciprocal table (they are on a different table)
symmetric <- read_delim("Subtabla_recíprocos2019.csv", delim=";")
#clean table
symmetric <- symmetric %>%
  filter(Muestreo == "Exhaustivo") %>% #subcorpus E
  filter(!Tipo_sintactico %in% c("Discontinuo", "Indirecto", "Directo", "Discontinuo_indirecto")) %>% #by syntactic type
  filter(!is.na(Tipo_sintactico))

#Check syntactic class
symmetric %>% 
  count(Tipo_sintactico)

#Check number of examples
nrow(symmetric) 
#Proportion of reflexive uses
symmetric %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
symmetric %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
symmetric %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#Section 3.3.5
#No semantic ascription
#load table
semantica_todo <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t")

#clean table
semantica_todo <- semantica_todo %>%
  filter(!Tipo_sintactico %in% c("Transitivo_tiempo", "Indirecto", "Directo", "Sin_cambio_valencial", "Logoforico_indirecto", "Discontinuo",
                                "Transitivo_path", "Mediado", "Mediado_indirecto", "Logoforico", "Discontinuo_indirecto", "Transitivo", "Auxiliar")) %>% #by syntactic type
  filter(!is.na(Tipo_sintactico)) %>%
  filter(Pron_reflexivo == "Si") %>% #by presence of the reflexive
  mutate(Tipo_sintactico = ifelse(Tipo_sintactico %in% c("Antipasiva_CR", "Antipasiva_SO"), "Antipasiva",
                                  ifelse(Tipo_sintactico %in% c("Anticausativo_discontinuo", "Intransitivo_Anticausativo"), "Anticausativo", Tipo_sintactico)))%>%
  mutate(Semantic_ascription = ifelse(Tipo_semantico %in% c("Cognicion", "Corporal", "Corporal_metaforico", "Emocion", "Espontaneo", "Espontaneo_metaforico",
                                                            "Movimiento", "Movimiento_metaforico", "No_traslacional", "No_traslacional_metaforico", "Percepcion", "Postural", 
                                                            "Postural_metaforico", "Proceso_corporal", "Proceso_corporal_metaforico", "Reciproco", "Reciproco_natural", 
                                                            "Reciproco_Pseudocopulativo"), "Yes", "No"))
#table: number of verbs per category 
semantica_todo %>%
  group_by(Semantic_ascription) %>% 
  count(Tipo_sintactico, Verbo) %>%
  select(-n) %>%
  count(Tipo_sintactico) %>%
  spread(Semantic_ascription, n) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
  mutate(Total = No + Yes, percentage_N = round(No/Total*100, 1), percentage_Y = round(Yes/Total*100, 1))


#Section 3.3.6
#Pseudocpulative
#clean table
pseudocopulative <- semantica %>%
  filter(Tipo_semantico %in% c("Pseudocopulativo", "Reciproco_Pseudocopulativo")) %>%  #by semantic type
  mutate(Tipo_sintactico = ifelse(Tipo_sintactico == "Anticausativo_discontinuo", "Anticausativo", Tipo_sintactico)) #by syntactic type

#Check number of examples
nrow(pseudocopulative) 
#Proportion of reflexive uses
pseudocopulative %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of examples per verb
pseudocopulative %>% 
  count(Verbo, sort = T)
#Check number of verbs
pseudocopulative %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
pseudocopulative %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#RM per syntactic type
pseudocopulative %>%
  group_by(Tipo_sintactico) %>%
  count(Pron_reflexivo) %>%
  mutate(total = sum(n), perc = round(n/total*100, 1))


#Section 3.3.7
#Auxiliary verb
#load table
auxiliary <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t")
#clean table
auxiliary <- auxiliary %>%
  filter(Tipo_semantico %in% c("Discursivo", "Perifrastico")) %>% #by semantic type
  filter(Pron_reflexivo == "Si") #by presence of TM


#Check number of examples
nrow(auxiliary) 
#Check number of verbs
auxiliary %>% 
  distinct(Verbo) %>% 
  nrow() 

