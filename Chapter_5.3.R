#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 5
#Section 5.3
##See also script Chapter_4.6.R (line 128)

#load library
library(tidyverse)

#load table
full_table <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t")

#clean table
full_table <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM"))

#create table with intransitives
intransitive <- full_table %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo"))

##clean table: remove verbs with no cases of RM
###create a table with all verbs with no RM
intransitive_verbs <- intransitive %>%
  group_by(Pron_reflexivo)  %>% 
  count(Verbo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  filter(RM == 0) %>% 
  arrange(RM)

###clean intransitive table (removing those verbs and renaming variables)
intransitive <- intransitive %>%
  filter(!Verbo %in% intransitive_verbs$Verbo) %>% 
  mutate(Tipo_semantico = ifelse(is.na(Tipo_semantico), "Other", Tipo_semantico)) %>% 
  mutate(Tipo_semantico = ifelse(Tipo_semantico %in% c("Pseudocopulativo", "Other"), "Other",
                                 ifelse(Tipo_semantico %in% c("Espontaneo", "Espontaneo_metaforico"), "Spontaneous", 
                                        ifelse(Tipo_semantico %in% c("Movimiento", "Movimiento_metaforico"), "Transl. motion", 
                                               ifelse(Tipo_semantico %in% c("No_traslacional", "No_traslacional_metaforico"), "Non-transl. motion", 
                                                      ifelse(Tipo_semantico == "Emocion", "Emotion", 
                                                             ifelse(Tipo_semantico == "Corporal", "Grooming",
                                                                    ifelse(Tipo_semantico %in% c("Proceso_corporal", "Proceso_corporal_metaforico"), "Body process", "Nat. reciprocal")))))))) %>% 
  
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Humano"), "Animate", "Inanimate")) %>% 
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory")) 

#select relevant verbs: same verbs as in section 3.2.5.1 plus subir, bajar and pasar
##table with verbs from subcorpus E
intransitive_E <- intransitive %>%
  filter(Muestreo == "Exhaustivo") %>%
  filter(Verbo %in% c("estar", "salir", "volver", "ir", "venir", "morir", "vivir", "nacer", 
                      "dormir", "escapar", "valer", "andar", "llegar", "quedar", "entrar"))

##table with verbs from subcorpus NE (whole corpus)
intransitive_NE <- intransitive %>%
  filter(Verbo %in% c("pasar", "caer", "esperar", "marchar", "reir", 
                      "cagar", "montar", "arder", "crecer", "mear", "subir", "bajar", "pasar"))

##table with entrar, llegar, quedar
intransitive_entrar_llegar_quedar <- intransitive %>%
  filter(COSERID %in% c(716, 723, 728, 959, 1012, 1015, 1603, 2321, 3209, 3412, 3610, 4108, 4117, 4301, 4401, 4403, 4407, 4419, 4602, 4611, 4613, 4714)) %>%
  filter(Verbo %in% c("llegar", "entrar", "quedar"))

##join tables
intransitive_exhaustive <- rbind(intransitive_E, intransitive_NE, intransitive_entrar_llegar_quedar)

#plot RM by area and animacy of subject
intransitive_exhaustive %>%
  count(Area_dialecta, Animacion_sujeto, Pron_reflexivo) %>%
  group_by(Area_dialecta, Animacion_sujeto) %>%
  mutate(total=sum(n), prop=(n/total*100)) %>% 
  ggplot(aes(x=Animacion_sujeto,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with intransitive verbs depending \non the animacy of the subject by geographical area", x="Animacy of the subject", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialecta)

##save plot
ggsave("intransitive_exhaustive.png", height = 4, width = 6) #saves the last plot

#proportion of animate subjects per area
intransitive_exhaustive %>% 
  group_by(Area_dialecta) %>% 
  count(Animacion_sujeto) %>% 
  mutate(Total = sum(n), 
         Percentage = round(n/sum(n)*100, 1))

#Dotplots with facets
intransitive_exhaustive %>%
  group_by(Area_dialecta, Animacion_sujeto, Verbo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = RM + `No RM`, RM_prob = RM/Total) %>%
  filter(Total > 9) %>%
  ungroup() %>% 
  ggplot(aes(x=RM_prob)) + 
  geom_dotplot(binwidth=0.0224) + 
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title="Relative frequency of the RM by intransitive verb depending on \nanimacy of the subject and geographical area", x="Relative frequency of the RM") + 
  facet_wrap(c("Animacion_sujeto", "Area_dialecta"))
##save plot
ggsave("intransitive_exhaustive_dotplot.png", width = 6, height = 7) #saves the last plot

##See script Chapter_4.6.R (line 128) for equivalent plot with anticausative verbs

