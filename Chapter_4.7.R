#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 4
#Section 4.7. GLMR

#load libraries
library(tidyverse)
library(lme4)
library(broom.mixed)

#read tables
anticausative <- read_delim("Subtabla_anticausativos2019.csv", delim=";") #COSER anticausatives
anticausative_ETT <- read_delim("verbosETT_tesis.csv", delim="\t") #ETT anticausatives

#clean and transform table from EsTenTen so as to calculate probability of transitive uses
anticausative_ETT_transitivity <- anticausative_ETT %>%
  filter(!Valencia %in% "Nada") %>% #no valid valency change
  filter(!is.na(Valencia)) %>% #no valid valency change
  filter(!Verbo %in% c("descansar", "pesar", "enseñar")) %>% 
  mutate(Transitividad = ifelse(Valencia == "Pasiva se", "Transitivo",
                                ifelse(Valencia == "Causativo", "Transitivo",
                                       ifelse(Valencia == "Reflexivo", "Transitivo",
                                              ifelse(Valencia == "Absoluto", "Transitivo", 
                                                     ifelse(Valencia == "Se_impersonal", "Intransitivo", 
                                                            ifelse(Valencia == "Se – impersonal", "Intransitivo",
                                                                   ifelse(Valencia == "Régimen", "Intransitivo", 
                                                                          ifelse(Valencia == "Se", "Intransitivo",
                                                                                 ifelse(Valencia == "Ambigua", "Nada", 
                                                                                        ifelse(Valencia == "Auxiliar", "Nada", 
                                                                                               Valencia))))))))))) %>% 
  filter(Transitividad != "Nada") %>% 
  group_by(Verbo) %>%
  count(Transitividad) %>%
  spread(Transitividad, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  rename(Transitive = Transitivo, Intransitive = Intransitivo) %>%
  mutate(Prob_trans = Transitive/(Transitive + Intransitive)) %>%
  ungroup() %>% 
  select(Verbo, Prob_trans)

#clean COSER table and join ETT table
anticausative_glm <- anticausative %>% 
  mutate(Area_dialectal = ifelse(Area_dialectal == "Noroeste", "Northwest", "Rest of the territory")) %>% 
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Inanimado", "Evento", "Impersonal", "Vehiculo"), 
                                   "Inanimate", "Animate")) %>% 
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No_RM", "RM")) %>% 
  mutate(Presencia_del_dativo = ifelse(Presencia_del_dativo == "No", "No_dat", "Dat")) %>% 
  right_join(anticausative_ETT_transitivity) %>% #join table with data from ETT, we keep only verbs for which we calculated this probability
  filter(is.na(Tipo_perifrasis)) #filter out cases in periphrases


#Mixed model
##Transform into factor (first level is reference level)
anticausative_glm$Pron_reflexivo <- factor(anticausative_glm$Pron_reflexivo, levels = c("No_RM", "RM"))
anticausative_glm$Area_dialectal <- factor(anticausative_glm$Area_dialectal, levels = c("Rest of the territory", "Northwest"))
anticausative_glm$Animacion_sujeto <- factor(anticausative_glm$Animacion_sujeto, levels = c("Inanimate", "Animate"))
anticausative_glm$Presencia_del_dativo <- factor(anticausative_glm$Presencia_del_dativo, levels = c("No_dat", "Dat"))
anticausative_glm$COSERID <- factor(anticausative_glm$COSERID)

##calculate model
model_anticausative <- glmer(Pron_reflexivo ~ Area_dialectal + Animacion_sujeto + Presencia_del_dativo + Prob_trans + (1|COSERID), 
                     family = "binomial", data = anticausative_glm)

##summary statistics of model
summary(model_anticausative)
range(resid(model_anticausative)) 
hist(resid(model_anticausative))

##tidy model
model_anticausative_tidy <- tidy(model_anticausative, exponentiate = F, conf.int = T) %>% 
  mutate(across(4:9, round, 3))

##write table
write_delim(model_anticausative_tidy, "model_anticausative_tidy.csv", delim = "\t")
