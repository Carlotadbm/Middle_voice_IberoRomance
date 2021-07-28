#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 28th 2021
#Appendix 4

#load library
library(tidyverse)

#load tables
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #All COSER linguistic data
anticausative_full <- read_delim("Subtabla_anticausativos2019.csv", delim=";") #Anticausastive table

#clean tables
sintactica <- sintactica %>% 
  mutate(Verbo = str_replace_all(Verbo, "nn", "ñ")) 
anticausative_full <- anticausative_full %>% 
  mutate(Verbo = str_replace_all(Verbo, "nn", "ñ")) 

######Change of diathesis: Create tables with verbs and number of examples in each category
##Anticausative verbs
anticausative <- anticausative_full %>%
  count(Verbo) %>%
  rename(Anticausative = n)

##Absolute uses
absolute <- sintactica %>%
  filter(Tipo_sintactico == "Absoluto") %>%
  filter(Pron_reflexivo == "Si") %>%
  count(Verbo) %>%
  rename(Absolute = n)

##Deobjective uses
deobjective <- sintactica %>%
  filter(Tipo == "Verbo_se") %>%
  filter(Tipo_sintactico == "Deobjetivo") %>%
  count(Verbo) %>%
  rename(Deobjective = n)

##Conversive uses
conversive <- sintactica %>%
  filter(Tipo == "Verbo_se") %>%
  filter(Tipo_sintactico == "Inverso") %>%
  count(Verbo) %>%
  rename(Conversive = n)

##Antipassive
antipassive <- sintactica %>%
  filter(Tipo_sintactico %in% c("Antipasiva_SO", "Antipasiva_CR")) %>%
  count(Verbo) %>%
  rename(Antipassive = n)

##Intransitive verbs
intransitive <- sintactica %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo")) %>%
  filter(Pron_reflexivo == "Si") %>%
  count(Verbo) %>%
  rename(Intransitive = n)

##Non-reversible verbs
nonreversible <- sintactica %>%
  filter(Tipo_sintactico == "Irreversible") %>%
  count(Verbo) %>%
  rename(Nonreversible = n)

##Transitive verbs
transitive <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path")) %>%
  filter(Pron_reflexivo == "Si") %>%
  count(Verbo) %>%
  rename(Transitive = n)

##join them and prepare table for publishing
diathesis <- full_join(anticausative, absolute, by = "Verbo") %>%
  full_join(deobjective, by = "Verbo") %>%  
  full_join(conversive, by = "Verbo") %>%
  full_join(antipassive, by = "Verbo") %>%
  full_join(intransitive, by = "Verbo") %>%
  full_join(nonreversible, by = "Verbo") %>%
  full_join(transitive, by = "Verbo") %>%
  mutate_if(is.numeric, ~ifelse(!is.na(.), str_c("yes (N = ", ., ")"), .)) %>%
  mutate_if(is.character, ~ifelse(is.na(.), "no", .)) %>% 
  rename(Verb = Verbo) %>% 
  arrange(Verb)

##write table as csv
write_delim(diathesis, "diathesis_appendix4.csv", delim = "\t")

######Semantic type: Create tables with verbs and number of examples in each category 
##clean table
semantica <- sintactica %>%
  filter(Muestreo == "Exhaustivo") %>% #only subcorpus E
  filter(!Tipo_sintactico %in% c("Transitivo_tiempo", "Indirecto", "Directo", "Sin_cambio_valencial", "Logoforico_indirecto", "Discontinuo",
                                "Transitivo_path", "Mediado", "Mediado_indirecto", "Logoforico", "Discontinuo_indirecto", "Transitivo")) %>% #remove syntactic types that are not relevant
  filter(!is.na(Tipo_sintactico)) #remove syntactic types that are not relevant

##Grooming or body actions
grooming <- semantica %>%
  filter(Tipo_semantico %in% c("Corporal", "Corporal_metaforico")) %>%
  count(Verbo) %>%
  rename(Grooming = n)

##Body processes
body_process <- semantica %>%
  filter(Tipo_semantico %in% c("Proceso_corporal", "Proceso_corporal_metaforico")) %>%
  count(Verbo) %>%
  rename(Body_process = n)

##Body posture
posture <- semantica %>%
  filter(Tipo_semantico %in% c("Postural", "Postural_metaforico")) %>%
  count(Verbo) %>%
  rename(Posture = n)

##Non translational motion
non_translational <- semantica %>%
  filter(Tipo_semantico == "No_traslacional") %>%
  count(Verbo) %>%
  rename(Non_translational = n)

##Translational motion
translational <- semantica %>%
  filter(Tipo_semantico %in% c("Movimiento", "Movimiento_metaforico")) %>%
  count(Verbo) %>%
  rename(Translational = n)

##Emotion
emotion <- semantica %>%
  filter(Tipo_semantico == "Emocion") %>%
  count(Verbo) %>%
  rename(Emotion = n)

##Cognition
cognition <- semantica %>%
  filter(Tipo_semantico == "Cognicion") %>%
  count(Verbo) %>%
  rename(Cognition = n)

##Perception
perception <- semantica %>%
  filter(Tipo_semantico == "Percepcion") %>%
  count(Verbo) %>%
  rename(Perception = n)

##Spontaneous
spontaneous <- semantica %>%
  filter(Tipo_semantico %in% c("Espontaneo", "Espontaneo_metaforico")) %>%
  count(Verbo) %>%
  rename(Spontaneous = n)

##Naturally reciprocal
symmetric <- read_delim("Subtabla_recíprocos2019.csv", delim=";") %>% #load table
  filter(Muestreo == "Exhaustivo") %>% #only subcorpus E
  filter(!Tipo_sintactico %in% c("Discontinuo", "Indirecto", "Directo", "Discontinuo_indirecto")) %>% #select relevant syntactic types
  filter(!is.na(Tipo_sintactico)) %>% #select relevant syntactic types 
  count(Verbo) %>%
  rename(Symmetric = n)

##No semantic ascription
no_semantic_ascription <- sintactica %>%
  filter(!Tipo_sintactico %in% c("Transitivo_tiempo", "Indirecto", "Directo", "Sin_cambio_valencial", "Logoforico_indirecto", "Discontinuo",
                                "Transitivo_path", "Mediado", "Mediado_indirecto", "Logoforico", "Discontinuo_indirecto", "Transitivo", "Auxiliar")) %>% #remove syntactic types that are not relevant
  filter(!is.na(Tipo_sintactico)) %>% #remove syntactic types that are not relevant
  filter(Pron_reflexivo == "Si") %>% #keep only cases with RM
  mutate(Semantic_ascription = ifelse(Tipo_semantico %in% c("Cognicion", "Corporal", "Corporal_metaforico", "Emocion", "Espontaneo", "Espontaneo_metaforico",
                                                            "Movimiento", "Movimiento_metaforico", "No_traslacional", "No_traslacional_metaforico", "Percepcion", "Postural", 
                                                            "Postural_metaforico", "Proceso_corporal", "Proceso_corporal_metaforico", "Reciproco", "Reciproco_natural", 
                                                            "Reciproco_Pseudocopulativo"), "Yes", "No")) %>%
  filter(Semantic_ascription == "No") %>% 
  count(Verbo) %>%
  rename(No_semantic_ascription = n)

##Pseudocopulative
pseudocopulative <- semantica %>%
  filter(Tipo_semantico %in% c("Pseudocopulativo", "Reciproco_Pseudocopulativo")) %>%
  count(Verbo) %>%
  rename(Pseudocopulative = n)

##Auxiliary verb
auxiliary <- sintactica %>%
  filter(Tipo_semantico %in% c("Discursivo", "Perifrastico")) %>%
  filter(Pron_reflexivo == "Si") %>%
  count(Verbo) %>%
  rename(Auxiliary = n)

##join them and prepare table for publishing
semantics <- full_join(grooming, body_process, by = "Verbo") %>%
  full_join(posture, by = "Verbo") %>%
  full_join(non_translational, by = "Verbo") %>%
  full_join(translational, by = "Verbo") %>%
  full_join(emotion, by = "Verbo") %>%
  full_join(cognition, by = "Verbo") %>%
  full_join(perception, by = "Verbo") %>%
  full_join(spontaneous, by = "Verbo") %>%
  full_join(symmetric, by = "Verbo") %>%
  full_join(no_semantic_ascription, by = "Verbo") %>%
  full_join(pseudocopulative, by = "Verbo") %>%
  full_join(auxiliary, by = "Verbo") %>% 
  mutate_if(is.numeric, ~ifelse(!is.na(.), str_c("yes (N = ", ., ")"), .)) %>%
  mutate_if(is.character, ~ifelse(is.na(.), "no", .)) %>% 
  rename(Verb = Verbo) %>% 
  arrange(Verb)

#write table as csv
write_delim(semantics, "semantics_appendix4.csv", delim = "\t")
