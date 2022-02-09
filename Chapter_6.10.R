#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 28th 2021
#Chapter 6
#Section 6.10. Movement

#load libraries
library(tidyverse)
library(lme4)
library(broom.mixed)

#load and clean table
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Animado", "Humano"), "Animate", "Inanimate")) %>% 
  replace_na(list(Tiempo_verbal = "Other")) %>%
  mutate(Tiempo_verbal = ifelse(Tiempo_verbal == "imperativo", "Imperative", "Other"), 
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>% 
  replace_na(list(Presencia_del_dativo = "No")) %>%
  mutate(Presencia_del_dativo = ifelse(Presencia_del_dativo == "No", "No dative", "Dative"))


#create table with transitive verbs that were collected exhaustively
###in subcorpus E
transitive_exh_E <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo %in% c("saber", "llevar", "comer", "ver", "dejar", "tomar", "estudiar","mirar", "ganar", 
                      "aprender", "pensar", "entender", "leer", "encontrar"), 
         Muestreo == "Exhaustivo")

###in corpus NE
transitive_exh_NE <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo %in% c("traer", "creer", "pasar",  "beber", "subir",  "recordar","bajar"))

###join them
transitive_exh <- rbind(transitive_exh_E, transitive_exh_NE)

#Check out all marked movement transitive verbs
sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), Tipo_semantico == "Movimiento", 
         Pron_reflexivo == "Si") %>% 
  count(Verbo, sort = T) %>% 
  mutate(total = sum(n))


#Frequent verbs: llevar, traer, ir & venir
##create table with transitive verbs
movement <- transitive_exh %>%
  filter(Verbo %in% c("llevar", "traer"), 
         Tipo_semantico == "Movimiento")

##create table with intransitive verbs
movement_intr <- sintactica %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo"), 
         Muestreo == "Exhaustivo", 
         Verbo %in% c("ir", "venir"), 
         Tipo_semantico == "Movimiento")

##join them
movement_full <- rbind(movement, movement_intr)

#Plot frequency of RM by verb and region
##create table
movement_verbs <- movement_full %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest"), 
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>%
  group_by(Verbo, Area_dialecta) %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), percentage=(n/total*100)) 

##relevel Verbo and Area_dialecta
movement_verbs$Verbo <- factor(movement_verbs$Verbo, levels = c("ir", "llevar", "venir", "traer"))
movement_verbs$Area_dialecta <- factor(movement_verbs$Area_dialecta, levels = c("Rest of the territory", "Northwest"))

##create plot
ggplot(movement_verbs, aes(x=Verbo,y=percentage, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in \"llevar(se)\" and \"traer(se)\" \ncompared to \"ir(se)\" and \"venir(se)\" by geographical area", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialecta)

##save plot
ggsave("movement_trans_verbs.png", height = 4, width = 6) #saves the last plot

#Plot frequency of RM by animacy in the rest of the territory
##create table
movement_animacy <- movement %>% 
  filter(Area_dialecta == "Resto") %>%
  group_by(Animacion_sujeto, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))

##create plot
ggplot(movement_animacy, aes(x=Animacion_sujeto,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"llevar(se)\" and \"traer(se)\" \nby subject animacy in the rest of the territory", x="Verb", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Verbo)

##save plot
ggsave("movement_animacy_trans.png", height = 4, width = 6) #saves the last plot

#Plot frequency of RM by verb tense in the rest of the territory
##create table
movement_tense <- movement %>%
  filter(Animacion_sujeto == "Animate", Area_dialecta == "Resto") %>%
  group_by(Area_dialecta, Tiempo_verbal, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))

##create plot
ggplot(movement_tense, aes(x=Tiempo_verbal,y=prop, group=Pron_reflexivo)) + geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"llevar(se)\" and \"traer(se)\" \nby verb tense in the rest of the territory", x="Verb", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Verbo)

##save plot
ggsave("movement_imperative_trans.png", height = 4, width = 6) #saves the last plot

#Plot RM probability by locative adjuncts
##llevar
###create table
movement_loc <- movement %>% 
  mutate(Compl_direccion = ifelse(Compl_direccion == "Absoluto", "No locative phrase",
                                  ifelse(Compl_direccion == "Cprep_a", "Goal (a)",
                                         ifelse(Compl_direccion == "Cprep_de", "Source (de)",
                                                ifelse(Compl_direccion == "Cprep_por", "Path (por)",
                                                       "Other"))))) %>%
  filter(Area_dialecta == "Resto", !is.na(Compl_direccion)) %>%
  group_by(Compl_direccion, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))

###Relevel Compl_direccion
movement_loc$Compl_direccion <- factor(movement_loc$Compl_direccion, levels = c("Source (de)", "Path (por)", "No locative phrase", "Goal (a)", "Other"))

###create plot
ggplot(movement_loc %>% filter(Verbo == "llevar"), aes(x=Compl_direccion,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"llevar(se)\" \nby locative phrase in the rest of the territory", x="Locative phrase", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Verbo)

###save plot
ggsave("movement_locative_llevar.png", height = 4, width = 7) #saves the last plot

##traer and venir
###create table with venir
movement_loc2 <- movement_intr %>%
  filter(Verbo == "venir", Area_dialecta == "Resto") %>%
  mutate(Compl_direccion = ifelse(Aspectualidades == "Absoluto", "No locative phrase",
                                  ifelse(Aspectualidades == "Cprep_a", "Goal (a)",
                                         ifelse(Aspectualidades == "Cprep_de", "Source (de)",
                                                ifelse(Aspectualidades == "Cprep_por", "Path (por)",
                                                       "Other"))))) %>%
  filter(!is.na(Compl_direccion)) %>%
  group_by(Compl_direccion, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n)) %>%
  rbind(movement_loc) %>% #join table with llevar and traer
  filter(Verbo != "llevar") #remove data for llevar

###Relevel Compl_direccion
movement_loc2$Compl_direccion <- factor(movement_loc2$Compl_direccion, levels = c("Goal (a)", "No locative phrase", "Source (de)", "Path (por)", "Other"))

###create plot
ggplot(movement_loc2, aes(x=Compl_direccion,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"traer(se)\" and \"venir(se)\" \nby locative phrase in the rest of the territory", x="Locative phrase", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Verbo)

###save plot
ggsave("movement_locative_traer_venir.png", height = 4, width = 7) #saves the last plot

#Plot RM probabilty by presence of a dative and verb
##create table
movement_dative <- movement %>% 
  filter(Area_dialecta == "Resto") %>%
  group_by(Presencia_del_dativo, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), percentage = n/sum(n))

##create plot
ggplot(movement_dative, aes(x=Presencia_del_dativo,y=percentage, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"llevar(se)\" and \"traer(se)\" \nby presence of a dative in the rest of the territory", x="Presence of a dative", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Verbo)

##save plot
ggsave("movement_dative_trans.png", height = 4, width = 7) #saves the last plot

#Dative types
movement %>% 
  replace_na(list(Tipo_dativo = "No dative")) %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>%
  mutate(Tipo_dativo = ifelse(Tipo_dativo == "Ablativo_2", "Ablativo", Tipo_dativo)) %>% 
  filter(Area_dialecta == "Resto") %>%
  group_by(Tipo_dativo, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), percentage = n/sum(n))

#Dative and DO type
movement %>% 
  replace_na(list(Presencia_del_dativo = "No")) %>%
  filter(Area_dialecta == "Resto", !is.na(Aspectualidades)) %>%
  mutate(Presencia_del_dativo = ifelse(Presencia_del_dativo == "No", "No dative", "Dative"),
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM"), 
         Aspectualidades = ifelse(Aspectualidades == "Pronombre", "Pronoun", "Other")) %>%
    group_by(Presencia_del_dativo, Verbo) %>%
  count(Aspectualidades) %>%
  mutate(Total=sum(n), percentage = round(n/sum(n)*100, 0))

#Mixed model
##create table with data from the rest of the territory
movement1 <- movement %>% 
  filter(Area_dialecta == "Resto")

##Transform columns into factor (first level is reference level)
movement1$Pron_reflexivo <- factor(movement1$Pron_reflexivo, levels = c("No RM", "RM"))
movement1$Animacion_sujeto <- factor(movement1$Animacion_sujeto, levels = c("Inanimate", "Animate"))
movement1$Tiempo_verbal <- factor(movement1$Tiempo_verbal, levels = c("Other", "Imperative"))
movement1$Presencia_del_dativo <- factor(movement1$Presencia_del_dativo, levels = c("No dative", "Dative"))
movement1$Verbo <- factor(movement1$Verbo)
movement1$COSERID <- factor(movement1$COSERID)

##calculate model
movement1_model <- glmer(Pron_reflexivo ~ Animacion_sujeto + Tiempo_verbal + Verbo + (1|COSERID), 
                         family = "binomial", data = movement1)

##calculate model summary statistics
summary(movement1_model)

##tidy model
movement1_model_tidy <- tidy(movement1_model, exponentiate = F, conf.int = T) %>% 
  mutate(across(4:9, round, 3))

##write model
write_delim(movement1_model_tidy, "6.10_trans_movement_model_tidy.csv", delim = "\t")


#Plot RM probability with subir, bajar
##creata table with subir and bajar
###create table with transitive uses
movement_subir_bajar <- transitive_exh %>%
  filter(Verbo %in% c("subir", "bajar"), Tipo_sintactico == "Transitivo", Tipo_semantico == "Movimiento") %>%
  mutate(Verbo = ifelse(Verbo == "subir", "subir trans", "bajar trans"))

###create table with intransitive uses
movement_intr2 <- sintactica %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo"),
         Verbo %in% c("subir", "bajar"), Tipo_semantico == "Movimiento") %>%
  mutate(Verbo = ifelse(Verbo == "subir", "subir intrans", "bajar intrans"))

###join tables
movement_full_subir_bajar <- rbind(movement_subir_bajar, movement_intr2)

###create table with frequency of RM by verb and area
movement_verbs_subir_bajar <- movement_full_subir_bajar %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest")) %>%
  group_by(Verbo, Area_dialecta) %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

###relevel Verbo and Area_dialecta
movement_verbs_subir_bajar$Verbo <- factor(movement_verbs_subir_bajar$Verbo, levels = c("subir intrans", "subir trans", "bajar intrans", "bajar trans"))
movement_verbs_subir_bajar$Area_dialecta <- factor(movement_verbs_subir_bajar$Area_dialecta, levels = c("Rest of the territory", "Northwest"))

###create plot
ggplot(movement_verbs_subir_bajar, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in transitive and intransitive \n\"subir(se)\" and \"bajar(se)\" by geographical area", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialecta)

###save plot
ggsave("movement_trans_verbs_subir_bajar.png", height = 4, width = 6) #saves the last plot

#Random sample of llevarse to check for the presence of of possessives and "para" and "con" phrases
#The random sample used can be found in "llevarse_random.csv" and it was manually inspected
#It was created with the following code
#random <- read_delim("Creer_traer_llevar_all.csv", delim = "\t") #load table with all examples of relevant verbs (the text of the examples is in)

#llevar_random <- random %>% #select a random sample of 100 cases of marked llevarse (select EjemploID)
#  filter(Verbo == "llevar", 
#         Pron_reflexivo == "Sí") %>% 
#  sample_n(100) %>%
#  select(EjemploID) %>%
#  mutate(Random = "Si") 

#left_join(llevar_random, random) %>% #join it to the full table and write it as a csv file
#  write_csv("llevarse_random.csv")

#pasar
transitive_exh %>%
  filter(Verbo == "pasar", Tipo_sintactico == "Transitivo", Tipo_semantico == "Movimiento") %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest")) %>%
  group_by(Verbo, Area_dialecta) %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), prop=(n/total*100))


#####Section 6.10.1: DO that conveys a path
##Frequency of the RM by verb (recorrer, subir, etc.)
sintactica %>%
  filter(Tipo_sintactico == "Transitivo_path") %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest")) %>%
  group_by(Verbo, Area_dialecta) %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), percentage=round((n/total*100),0))

##Frequency of the RM with light verbs
sintactica %>%
  filter(Verbo %in% c("dar", "dar_una_vuelta", "pegar"), Tipo_sintactico == "Transitivo", 
         Tipo_semantico == "Movimiento") %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest"), 
         Verbo = ifelse(Verbo == "pegar", "pegar", "dar")) %>%
  group_by(Verbo, Area_dialecta) %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), percentage=round((n/total*100), 0))
