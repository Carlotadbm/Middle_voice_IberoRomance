#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 5
#Section 5.5. Morirse and caerse

#load libraries
library(tidyverse)
library(scatterpie)
library(lme4)
library(broom.mixed)

#load tables
full_table <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data
enclaves_q_todo <- read_delim("QUEST_total copia.csv", delim="\t") #questionnaire coordinates
caer_q <- read_delim("caer_questionnaire.csv", delim = ",") #questionnaire caer

#clean tables
enclaves_q_todo  <-  enclaves_q_todo %>%
  rename(CuestionarioID = ID) %>%
  select(CuestionarioID, Latitude, Longitude)

full_table <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Evento", "Impersonal", "Inanimado", "Vehiculo"), "Inanimate", "Animate")) %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory")) %>% 
  mutate(Presencia_del_dativo = ifelse(Presencia_del_dativo == "Si", "Yes", "No"))

#create table with relevant verbs
##create table with all intranstive verbs
intransitive <- full_table %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo")) 

##select cases of morir in subcorpus E
intransitive_E <- intransitive %>%
  filter(Muestreo == "Exhaustivo") %>%
  filter(Verbo =="morir")

##select cases of caer in subcorpus NE (whole corpus)
intransitive_NE <- intransitive %>%
  filter(Verbo == "caer")

##join tables
intransitive <- rbind(intransitive_E, intransitive_NE)

##disregard metaphorical uses
accidental <- intransitive %>%
  filter(Tipo_semantico == "Espontaneo")

#RM probability by meanings (accidentality)
accidental %>% 
  group_by(Verbo, Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100)

#total RM probability by verb
accidental %>% 
  group_by(Verbo) %>% 
  count(Pron_reflexivo) %>%
  mutate(Total = sum(n), Perc = round(n/sum(n)*100, 1))

#Plot RM probability by subject animacy and area
accidental %>% 
  group_by(Area_dialecta, Animacion_sujeto, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n)) %>% 
  ggplot(aes(x=Animacion_sujeto,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"caer(se)\" and \"morir(se)\" \nby geographical area and animacy of the subject", x="Animacy of the subject", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(c("Area_dialecta", "Verbo"))

##save plot
ggsave("accidental_animacy.png", height = 6, width = 6) #saves the last plot

#Plot RM probability by presence of a dative and area
accidental %>% 
  group_by(Area_dialecta, Presencia_del_dativo, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n)) %>% 
  ggplot(aes(x=Presencia_del_dativo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"caer(se)\" and \"morir(se)\" \nby geographical area and presence of a dative", x="Presence of a dative", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(c("Area_dialecta", "Verbo"))

##save plot
ggsave("accidental_datives.png", height = 6, width = 6) #saves the last plot

#Caer: Dative types
##create table
caer_datives <- accidental %>% 
  filter(Presencia_del_dativo == "Yes", Verbo == "caer") %>%
  mutate(Tipo_dativo = ifelse(Tipo_dativo == "Ambiguo", "Ambiguous", 
                              ifelse (Tipo_dativo == "Causante_involuntario", "Cause",
                                      ifelse(Tipo_dativo == "Destinatario", "Goal", "Source/Possessive")))) %>%
  group_by(Tipo_dativo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))
##relevel Tipo_dativo
caer_datives$Tipo_dativo <- factor(caer_datives$Tipo_dativo, 
                                   levels = c("Cause", "Source/Possessive", "Goal", "Ambiguous"))
##create plot
ggplot(caer_datives, aes(x=Tipo_dativo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM type with \"caer(se)\" by dative", x="Dative type", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("caer_datives.png", height = 6, width = 8) #saves the last plot

#Map of caer in the questionnaire
##clean questionnaire table
caer_q_mapa <- caer_q %>%
  select(CuestionarioID, EjemploID, Video, Reflexivo) %>%
  filter(Video != 17) %>%
  mutate(Video = ifelse(Video == "13", "Caer la pelota de la mesa (video 13)", 
                        ifelse(Video == "14", "Caérsele un libro a alguien (video 14)", 
                               ifelse(Video == "41", "Caer agua del tejado (video 41)", 
                                      "Caer hojas de un árbol (vídeo 42)")))) %>%
  group_by(CuestionarioID, Video, Reflexivo) %>%
  count(Reflexivo) %>%
  spread(Reflexivo, n) %>%
  mutate(No = replace(No, is.na(No), 0), Si = replace(Si, is.na(Si), 0), Total = Si + No) %>%
  ungroup() %>%
  left_join(enclaves_q_todo) %>%
  group_by(Video)

##relevel Video
caer_q_mapa$Video <- factor(caer_q_mapa$Video, levels = c("Caérsele un libro a alguien (video 14)", 
                                                          "Caer la pelota de la mesa (video 13)", 
                                                          "Caer agua del tejado (video 41)", 
                                                          "Caer hojas de un árbol (vídeo 42)"))

##load map of Spain
spain <- map_data('world', "spain")

##create plot
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = caer_q_mapa, 
                  aes(Longitude, Latitude, r = sqrt(Total)/7),
                  cols = c("No", "Si"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("No", "Si"),
    labels = c("No RM", "RM"),
    values = c("No" = "white",
               "Si" = "black")
  ) +
  facet_wrap(~ Video) +
  labs(title = "Frequency of the RM with \"caer(se)\" in the questionnaire") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(colour = "black"))

##save plot
ggsave("caer_q_map.png", width=13, height = 9)

#Mixed model
accidental1 <- accidental %>% 
  filter(Area_dialecta != "Northwest")
##Transform columns into factor (first level is reference level)
accidental1$Pron_reflexivo <- factor(accidental1$Pron_reflexivo, levels = c("No RM", "RM"))
accidental1$Animacion_sujeto <- factor(accidental1$Animacion_sujeto, levels = c("Inanimate", "Animate"))
accidental1$Presencia_del_dativo <- factor(accidental1$Presencia_del_dativo, levels = c("No", "Yes"))
accidental1$COSERID <- factor(accidental1$COSERID)
accidental1$Verbo <- factor(accidental1$Verbo)

##calculate model
accidental_model <- glmer(Pron_reflexivo ~ Animacion_sujeto + Presencia_del_dativo + Verbo + (1|COSERID), 
                     family = "binomial", data = accidental1)

##calculate model summary statistics
summary(accidental_model)
range(resid(accidental_model))
hist(resid(accidental_model)) #normal distribution?

##tidy model
accidental_model_tidy <- tidy(accidental_model, exponentiate = F, conf.int = T) #statistic es el z-value

##write model
write_delim(accidental_model_tidy, "5.5_accidental_model_tidy.csv", delim = "\t")
