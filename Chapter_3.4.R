#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 3
#Section 3.4. Conclusions

#load libraries
library(tidyverse)
library(ggrepel)
library(lme4)
library(broom.mixed)

#load table
middle_voice <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t")

#clean table
middle_voice_1 <-  middle_voice %>%
  filter(Muestreo == "Exhaustivo") %>% #by subcorpus
  filter(!Tipo_sintactico %in% c("Irreversible", "Transitivo_tiempo", "Indirecto", "Directo", "Sin_cambio_valencial", "Logoforico_indirecto", "Discontinuo",
                                "Transitivo_path", "Mediado", "Mediado_indirecto", "Logoforico", "Discontinuo_indirecto", "Transitivo", "Auxiliar")) %>% #by syntactic type
  filter(!is.na(Tipo_sintactico)) %>% 
  mutate(Tipo_sintactico = ifelse(Tipo_sintactico == "Intransitivo", "No_valency_change", "Valency_change")) %>% 
  mutate(Tipo_semantico = ifelse(is.na(Tipo_semantico), "Other", Tipo_semantico)) %>% 
  mutate(Tipo_semantico = ifelse(Tipo_semantico %in% c("Discursivo", "Pseudopasiva", "Pseudocopulativo", "Other"), "Other", "Kemmers"))

#Mixed model
##Transform columns into factor (first level is reference level)
middle_voice_1$Pron_reflexivo <- factor(middle_voice_1$Pron_reflexivo, levels = c("No", "Si"))
middle_voice_1$Tipo_sintactico <- factor(middle_voice_1$Tipo_sintactico, levels = c("No_valency_change", "Valency_change"))
middle_voice_1$Tipo_semantico <- factor(middle_voice_1$Tipo_semantico, levels = c("Other", "Kemmers"))
middle_voice_1$COSERID <- factor(middle_voice_1$COSERID)

##calculate model
model_voice <- glmer(Pron_reflexivo ~ Tipo_sintactico + Tipo_semantico + (1|COSERID), 
                     family = "binomial", data = middle_voice_1)

##calculate model summary statistics
summary(model_voice)
range(resid(model_voice))
hist(resid(model_voice)) #normal distribution?

##tidy model
model_voice_tidy <- tidy(model_voice, exponentiate = F, conf.int = T) #statistic es el z-value

##write model
write_delim(model_voice_tidy, "model_voice_tidy.csv", delim = "\t")

#Correlation: probability RM ~ probability no diathesis change, by semantic category
##clean table
middle_voice2 <-  middle_voice %>%
  filter(Muestreo == "Exhaustivo") %>% #by sucorpus
  filter(!Tipo_sintactico %in% c("Irreversible", "Transitivo_tiempo", "Indirecto", "Directo", "Sin_cambio_valencial", "Logoforico_indirecto", "Discontinuo",
                                "Transitivo_path", "Mediado", "Mediado_indirecto", "Logoforico", "Discontinuo_indirecto", "Transitivo", "Auxiliar")) %>% #by syntactic type
  filter(!is.na(Tipo_sintactico)) %>% 
  mutate(Tipo_sintactico = ifelse(Tipo_sintactico == "Intransitivo", "No_valency_change", "Valency_change")) %>% 
  mutate(Tipo_semantico = ifelse(is.na(Tipo_semantico), "Other", Tipo_semantico)) %>% 
  mutate(Tipo_semantico = ifelse(Tipo_semantico %in% c("Discursivo", "Pseudopasiva", "Pseudocopulativo", "Other"), "Other",
                                 ifelse(Tipo_semantico %in% c("Espontaneo", "Espontaneo_metaforico"), "Spontaneous", 
                                        ifelse(Tipo_semantico %in% c("Movimiento", "Movimiento_metaforico"), "Transl. motion", 
                                               ifelse(Tipo_semantico %in% c("Postural", "Postural_metaforico"), "Body posture",
                                                      ifelse(Tipo_semantico == "No_traslacional", "Non-transl. motion", 
                                                             ifelse(Tipo_semantico == "Cognicion", "Cognition", 
                                                                    ifelse(Tipo_semantico == "Emocion", "Emotion", 
                                                                           ifelse(Tipo_semantico == "Percepcion", "Perception", 
                                                                                  ifelse(Tipo_semantico %in% c("Corporal", "Corporal_metaforico"), "Grooming",
                                                                                         ifelse(Tipo_semantico %in% c("Proceso_corporal", "Proceso_corporal_metaforico"), "Body process", "Nat. reciprocal"))))))))))) %>% 
  filter(Tipo_semantico != "Other")

##calculate probability RM
middle_voice2_RM <- middle_voice2 %>%
  select(Area_dialecta, COSERID, EjemploID, Pron_reflexivo, Verbo, Tipo_semantico, Tipo_sintactico) %>%
  group_by(Tipo_semantico, Area_dialecta) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  rename(RM = Si, No_RM = No) %>%
  mutate(Total = RM + No_RM, RM_prob_area = RM/Total) %>%
  ungroup() %>%
  group_by(Tipo_semantico) %>%
  mutate(Prob_RM = sum(RM_prob_area)/2) %>%
  filter(Area_dialecta == "Resto") %>% #otherwise they're duplicated
  select(Tipo_semantico, Prob_RM)

##Calculate probability valency change
middle_voice2_VC <- middle_voice2 %>%
  select(Area_dialecta, COSERID, EjemploID, Pron_reflexivo, Verbo, Tipo_semantico, Tipo_sintactico) %>%
  group_by(Tipo_semantico, Area_dialecta) %>%
  count(Tipo_sintactico) %>%
  spread(Tipo_sintactico, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = No_valency_change + Valency_change, VC_prob_area = No_valency_change/Total) %>%
  ungroup() %>% 
  group_by(Tipo_semantico) %>%
  mutate(Prob_NVC = sum(VC_prob_area)/2) %>%
  filter(Area_dialecta == "Resto") %>%
  select(Tipo_semantico, Prob_NVC)

##Join tables
middle_voice2_cor <- full_join(middle_voice2_RM, middle_voice2_VC)

##Calculate correlation indexes (R, R squared, linear regression coefficients)
round(cor(middle_voice2_cor$Prob_NVC, middle_voice2_cor$Prob_RM), 2)
round(cor(middle_voice2_cor$Prob_NVC, middle_voice2_cor$Prob_RM)^2, 2)
lm <- lm(middle_voice2_cor$Prob_RM ~ middle_voice2_cor$Prob_NVC)

##Plot with correlation line
ggplot(middle_voice2_cor, aes(Prob_NVC, Prob_RM)) + 
  geom_point() + 
  labs(title="Correlation between presence of the RM and no valency change \nby semantic category of the verb", x="Probability of no valency change", y="Probability of the RM") + 
  geom_abline(aes(intercept=  lm$coefficients[1], slope=lm$coefficients[2])) + 
  ylim(0,1) + 
  xlim(0,1) +
  geom_text_repel(aes(x = Prob_NVC, 
                      y = Prob_RM, 
                      label = Tipo_semantico), segment.alpha = 0) #showing labels with no overlap (and no lines)
##save plot
ggsave("middle_voice2_cor.png", width = 7, height = 7) #saves the last plot

#Verbs with change of diathesis by semantic type
##clean table
middle_voice3 <-  middle_voice %>%
  filter(Pron_reflexivo == "Si") %>% ##by presence of RM
  filter(!Tipo_sintactico %in% c("Irreversible", "Transitivo_tiempo", "Indirecto", "Directo", "Sin_cambio_valencial", "Logoforico_indirecto", "Discontinuo",
                                "Transitivo_path", "Mediado", "Mediado_indirecto", "Logoforico", "Discontinuo_indirecto", "Transitivo", "Auxiliar", "Intransitivo")) %>% #by syntactic type
  filter(!is.na(Tipo_sintactico)) %>% 
  mutate(Tipo_semantico = ifelse(is.na(Tipo_semantico), "Other", Tipo_semantico)) %>% 
  mutate(Tipo_semantico = ifelse(Tipo_semantico %in% c("Discursivo", "Pseudopasiva", "Pseudocopulativo", "Perifrastico", "Other"), "Other",
                                 ifelse(Tipo_semantico %in% c("Espontaneo", "Espontaneo_metaforico"), "Spontaneous", 
                                        ifelse(Tipo_semantico %in% c("Movimiento", "Movimiento_metaforico"), "Transl. motion", 
                                               ifelse(Tipo_semantico %in% c("Postural", "Postural_metaforico"), "Body posture",
                                                      ifelse(Tipo_semantico == "No_traslacional", "Non-transl. motion", 
                                                             ifelse(Tipo_semantico == "Cognicion", "Cognition", 
                                                                    ifelse(Tipo_semantico == "Emocion", "Emotion", 
                                                                           ifelse(Tipo_semantico == "Percepcion", "Perception", 
                                                                                  ifelse(Tipo_semantico %in% c("Corporal", "Corporal_metaforico"), "Grooming",
                                                                                         ifelse(Tipo_semantico %in% c("Proceso_corporal", "Proceso_corporal_metaforico"), "Body process", "Nat. reciprocal")))))))))))
##create table with number of verbs by semantic category
semantic_types <- middle_voice3 %>%
  group_by(Tipo_semantico) %>%
  count(Verbo) %>%
  select(-n) %>%
  count(Tipo_semantico) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(total = sum(n), percentage = round(n*100/total, 1))

##create plot 
ggplot(semantic_types, aes(x=reorder(Tipo_semantico, n, desc), y=percentage)) + 
  geom_col(aes(), position = "stack", fill = "dark grey") + 
  labs(title="Verbs with change of diathesis per semantic type", x="Semantic types", y="Percentage of verbs") +
  geom_text(aes(label = n), position = position_stack(vjust = 1)) + 
  theme_classic() + 
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0,40)

##save plot 
ggsave("semantic_types.png", width = 7, height = 4) #saves the last plot


#Plot anticausative and conversive verbs by semantic category
##clean table
antic_conv <-  middle_voice %>%
  filter(Tipo_sintactico %in% c("Anticausativo_discontinuo", "Anticausativo", "Inverso", "Intransitivo_Anticausativo")) %>% #by syntactic type
  mutate(Tipo_semantico = ifelse(is.na(Tipo_semantico), "Other", Tipo_semantico)) %>%  
  mutate(Tipo_semantico = ifelse(Tipo_semantico %in% c("Discursivo", "Pseudopasiva", "Pseudocopulativo", "Perifrastico", "Other"), "Other",
                                 ifelse(Tipo_semantico %in% c("Espontaneo", "Espontaneo_metaforico"), "Spontaneous", 
                                        ifelse(Tipo_semantico %in% c("Movimiento", "Movimiento_metaforico"), "Transl. motion", 
                                               ifelse(Tipo_semantico %in% c("Postural", "Postural_metaforico"), "Body posture",
                                                      ifelse(Tipo_semantico == "No_traslacional", "Non-transl. motion", 
                                                             ifelse(Tipo_semantico == "Cognicion", "Cognition", 
                                                                    ifelse(Tipo_semantico == "Emocion", "Emotion", 
                                                                           ifelse(Tipo_semantico == "Percepcion", "Perception", 
                                                                                  ifelse(Tipo_semantico %in% c("Corporal", "Corporal_metaforico"), "Grooming",
                                                                                         ifelse(Tipo_semantico %in% c("Proceso_corporal", "Proceso_corporal_metaforico"), "Body process", "Nat. reciprocal"))))))))))) %>%
  filter(Tipo_semantico != "Other") #by semantic type

##create table with probability of RM by semantic category
antic_conv_graph <-  antic_conv %>%
  group_by(Tipo_semantico) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  rename(RM = Si, No_RM = No) %>%
  mutate(Total = RM + No_RM, RM_prob = RM/Total) %>%
  arrange(desc(RM_prob)) %>%
  ungroup()

##plot data
ggplot(antic_conv_graph, aes(x=reorder(Tipo_semantico, RM_prob, desc), y=RM_prob)) + 
  geom_point(size=2) + 
  geom_line(group = 1) + #geom_line always needs a group, in order to know which dots to connect, if there's no group: group=1
  labs(title="Presence of the RM in subject demoting/deleting diatheses", x="Semantic type", y="Relative frequency") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0,1)

##save plot
ggsave("antic_conv_graph.png", width=7, height = 4) #saves the last plot

