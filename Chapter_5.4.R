#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 5
#Section 5.4. Translational motion verbs

#load library
library(tidyverse)

#load table
full_table <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t")

#create table intransitive
intransitive <- full_table %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo")) %>% 
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Evento", "Impersonal", "Inanimado", "Vehiculo"), "Inanimate", "Animate")) %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory")) %>% 
  mutate(Tiempo_verbal = ifelse(is.na(Tiempo_verbal), "Other", Tiempo_verbal)) %>% 
  mutate(Tiempo_verbal = ifelse(str_detect(Tiempo_verbal, "[iI]mperativo"), "Imperative", "Other"))

#select verbs
intransitive_E <- intransitive %>%
  filter(Muestreo == "Exhaustivo") %>%
  filter(Verbo %in% c("estar", "salir", "volver", "ir", "venir", "morir", "vivir", "nacer", 
                      "dormir", "escapar", "valer", "andar", "llegar", "quedar", "entrar"))

intransitive_NE <- intransitive %>%
  filter(Verbo %in% c("pasar", "caer", "esperar", "marchar", "reir", 
                      "cagar", "montar", "arder", "crecer", "mear", "subir", "bajar", "pasar"))


#create table with exhaustive verbs
##"Movimiento_metaforico" not included: only 137 examples
movement <- rbind(intransitive_E, intransitive_NE) %>%
  filter(Tipo_semantico == "Movimiento", 
         Verbo %in% c("ir", "marchar", "salir", "escapar", "venir", "subir", "bajar", "montar", "pasar"))

#plot frequency of RM by verbs
##create table
movement_verbs <- movement %>%
  count(Verbo, Pron_reflexivo) %>%
  group_by(Verbo) %>%
  mutate(total=sum(n), prop=(n/total*100))

##relevel Verbo
movement_verbs$Verbo <- factor(movement_verbs$Verbo, levels = c("escapar", "montar", "marchar", "subir", "ir", "bajar", "pasar",
                               "salir", "venir"))

##create plot
ggplot(movement_verbs, aes(x = Verbo , y = prop, group = Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in translational motion intransitive verbs", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("movement_verbs.png", height = 4, width = 6) 

#Different meanings/contexts within the same verb
movement %>% 
  group_by(Verbo, Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100) %>% 
  print(n = Inf)

#Ir(se): locative adjuncts 
##RM probability
movement %>% 
  filter(Verbo == "ir") %>% 
  filter(Aspectualidades %in% c("Cprep_a", "Cprep_para")) %>% 
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100)

#Animacy
##Montar group (subir, bajar)
###create table
movement_animacy_montar <- movement %>% 
  filter(Verbo %in% c("subir", "bajar")) %>%
  group_by(Area_dialecta, Animacion_sujeto, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))

###create plot
ggplot(movement_animacy_montar, aes(x=Animacion_sujeto,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"subir(se)\" and \"bajar(se)\" \nby geographical area and subject animacy", x="Verb", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(c("Area_dialecta", "Verbo"))

###save plot
ggsave("movement_animacy_montar.png", height = 6, width = 6) 

##Marchar group (marchar, ir, venir)
###create table
movement_animacy_marchar <- movement %>% 
  filter(Verbo %in% c("marchar", "ir", "venir")) %>%
  group_by(Area_dialecta, Animacion_sujeto, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))

###create plot
ggplot(movement_animacy_marchar, aes(x=Animacion_sujeto,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"ir(se)\", \"venir(se)\" and \"marchar(se)\" \nby geographical area and subject animacy", x="Verb", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(c("Area_dialecta", "Verbo"))

###save plot
ggsave("movement_animacy_marchar.png", height = 6, width = 6) 

#Imperatives
##create table
movement_tense_marchar <- movement %>% 
  filter(Verbo %in% c("marchar", "ir", "venir")) %>%
  filter(!is.na(Tiempo_verbal)) %>%
  filter(Animacion_sujeto == "Animate") %>%
  group_by(Area_dialecta, Tiempo_verbal, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))
  
##create plot
ggplot(movement_tense_marchar, aes(x=Tiempo_verbal,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"ir(se)\", \"venir(se)\" and \"marchar(se)\" \nby geographical area and verb tense", x="Verb", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(c("Area_dialecta", "Verbo"))

##save plot
ggsave("movement_imperative_marchar.png", height = 6, width = 6)

#Datives: how many examples by verb?
movement %>%
  filter(Presencia_del_dativo == "Si") %>% 
  group_by(Verbo) %>% 
  count(Verbo)

#Ir, salir, pasar: locative adjuncts
##create table
movement_loc_salir <- movement %>% 
  filter(!is.na(Aspectualidades)) %>%
  mutate(Aspectualidades = ifelse(Aspectualidades == "Absoluto", "No locative phrase",
                                ifelse(Aspectualidades == "Cprep_a", "Goal (a)",
                                       ifelse(Aspectualidades == "Cprep_de", "Source (de)",
                                              ifelse(Aspectualidades == "Cprep_por", "Path (por)",
                                       "Other"))))) %>%
  filter(Verbo %in% c("ir", "salir", "pasar")) %>%
  group_by(Area_dialecta, Aspectualidades, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))

##check percentages out
movement_loc_salir %>% 
  mutate(prop = round(prop*100, 0)) %>% 
  print(n = Inf)

##relevel Aspectualidades
movement_loc_salir$Aspectualidades <- factor(movement_loc_salir$Aspectualidades, levels = c("Source (de)", "Goal (a)", "Path (por)", "No locative phrase", "Other"))

##create plot
ggplot(movement_loc_salir, aes(x=Aspectualidades,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"ir(se)\", \"salir(se)\" and \"pasar(se)\" \nby locative phrase and geographical area", x="Locative phrase", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(c("Area_dialecta", "Verbo"))

##save plot
ggsave("movement_locative_salir.png", height = 6, width = 7) #saves the last plot

#Subir, montar: locative adjuncts
##create table
movement_loc_subir <- movement %>% 
  filter(!is.na(Aspectualidades)) %>%
  mutate(Aspectualidades = ifelse(Aspectualidades == "Absoluto", "No locative phrase",
                                  ifelse(Aspectualidades == "Cprep_a", "Goal (a)",
                                         ifelse(Aspectualidades == "Cprep_en", "Goal (en)",
                                                ifelse(Aspectualidades == "Cprep_para", "Goal (para)",
                                                       ifelse(Aspectualidades %in% c("Adverbio", "Adv"), "Goal (adverb)",
                                                              "Other")))))) %>%
  filter(Verbo %in% c("subir", "montar")) %>%
  group_by(Area_dialecta, Aspectualidades, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))

##relevel Aspectualidades
movement_loc_subir$Aspectualidades <- factor(movement_loc_subir$Aspectualidades, levels = c("Goal (en)", "Goal (a)", "Goal (para)", "Goal (adverb)", "No locative phrase", "Other"))

##create plot
ggplot(movement_loc_subir, aes(x=Aspectualidades,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"montar(se)\" and \"subir(se)\" \nby geographical area and locative phrases", x="Locative phrase", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(c("Area_dialecta", "Verbo"), scales = "free_x")

##save plot
ggsave("movement_locative_subir.png", height = 6, width = 7) #saves the last plot


#Mixed model
##Remove data from Northwest
movement <- movement %>% 
  filter(Area_dialecta != "Northwest")
##Transform columns into factor (first level is reference level)
movement$Pron_reflexivo <- factor(movement$Pron_reflexivo, levels = c("No RM", "RM"))
movement$Animacion_sujeto <- factor(movement$Animacion_sujeto, levels = c("Inanimate", "Animate"))
movement$Tiempo_verbal <- factor(movement$Tiempo_verbal, levels = c("Other", "Imperative"))
movement$COSERID <- factor(movement$COSERID)
movement$Verbo <- factor(movement$Verbo)

##calculate model
movement_model <- glmer(Pron_reflexivo ~ Animacion_sujeto + Tiempo_verbal + (1|COSERID) + (1|Verbo), 
                     family = "binomial", data = movement)

##calculate model summary statistics
summary(movement_model)
range(resid(movement_model)) #normal distribution?
hist(resid(movement_model)) #normal distribution?

##tidy model
movement_model_tidy <- tidy(movement_model, exponentiate = F, conf.int = T) #statistic es el z-value

##write model
write_delim(movement_model_tidy, "5.4_movement_model_tidy.csv", delim = "\t")
