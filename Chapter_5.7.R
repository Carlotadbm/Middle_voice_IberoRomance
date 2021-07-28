#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 5
#Section Section 5.7. Agentivity: Quedarse, estarse, esperarse

#load libraries
library(tidyverse)
library(scatterpie)
library(lme4)
library(broom.mixed)

#load tables
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";") #COSER coordinates
full_table <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") #COSER linguistic data

#clean tables
enclaves_coser_todo  <-  enclaves_coser_todo %>%
  rename(COSERID = ID)

full_table <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Evento", "Impersonal", "Inanimado", "Vehiculo"), "Inanimate", "Animate")) %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory")) 

#create table with relevant verbs
##table with intransitive verbs
intransitive <- full_table %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo")) 

##table with verbs from subcorpus E
intransitive_E <- intransitive %>%
  filter(Muestreo == "Exhaustivo") %>%
  filter(Verbo %in% c("estar", "quedar"))

##table with verbs from subcorpus NE (whole corpus)
intransitive_NE <- intransitive %>%
  filter(Verbo == "esperar")

##table with quedar
intransitive_quedar <- intransitive %>%
  filter(COSERID %in% c(716, 723, 728, 959, 1012, 1015, 1603, 2321, 3209, 3412, 3610, 4108, 4117, 4301, 4401, 4403, 4407, 4419, 4602, 4611, 4613, 4714)) %>%
  filter(Verbo == "quedar")

##join tables
intransitive <- rbind(intransitive_E, intransitive_NE, intransitive_quedar)

##split quedar in two types and remove cases to be disregarded
agentive1 <- intransitive %>%
  mutate(Verbo = replace(Verbo, Verbo == "quedar" & is.na(Aspectualidades), "quedar_fuera")) %>%
  filter(Verbo %in% c("estar", "quedar", "esperar"), 
         !Aspectualidades %in% c("Cita", "Cprep_con", "Cprep_sin", "acordar", "Acordar")) %>%
  mutate(Verbo = replace(Verbo, Verbo == "quedar" & Aspectualidades == "Ptvo", "quedar (attributive)")) %>%
  mutate(Verbo = replace(Verbo, Verbo == "quedar", "quedar (locative)"))


#Map estar & esperar
##are there cases in the Northwest?
agentive1 %>% 
  group_by(Verbo, Pron_reflexivo) %>% 
  count(Area_dialecta)

##create table with RM/no RM cases for esperar
esperar_geo <- agentive1 %>%
  filter(Verbo == "esperar") %>%
  group_by(COSERID, Pron_reflexivo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
    rename(No_RM = `No RM`) %>%
    mutate(No_RM = replace(No_RM, is.na(No_RM), 0), RM = replace(RM, is.na(RM), 0), 
           Total = RM + No_RM, Verb = "esperar") %>%
    ungroup() 

##create table with RM/no RM cases for estar
estar_geo <- full_table %>%
  filter(Verbo == "estar", Pron_reflexivo == "RM") %>% #we only want marked cases here
  group_by(COSERID, Pron_reflexivo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate(No_RM =  0, RM = replace(RM, is.na(RM), 0), 
         Total = RM + No_RM, Verb = "estar") %>%
  ungroup()

##join tables
esperar_estar_geo <- rbind(esperar_geo, estar_geo) %>%
  left_join(enclaves_coser_todo)

##relevel Verb
esperar_estar_geo$Verb <- factor(esperar_estar_geo$Verb, levels = c("estar", "esperar"))

##load map of spain
spain <- map_data('world', "spain")

##create map
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = esperar_estar_geo, 
                  aes(Longitude, Latitude, r = sqrt(Total)/10),
                  cols = c("No_RM", "RM"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("No_RM", "RM"),
    labels = c("No RM", "RM"),
    values = c("No_RM" = "white",
               "RM" = "black")
  ) +
  facet_wrap(~ Verb) +
  labs(title = "Frequency of the RM with \"estar(se)\" and \"esperar(se)\"") +
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

##save map
ggsave("esperar_estar_map.png", width=13, height = 5)

#create new table without estar and esperar in the Northwest
##table with NW without these two verbs
agentive_NW <- agentive1 %>%
  filter(!Verbo %in% c("estar", "esperar") & Area_dialecta == "Northwest") 

##table with Rest of the territory
agentive_RoT <- agentive1 %>%
  filter(Area_dialecta == "Rest of the territory") 

##join tables
agentive <- rbind(agentive_NW, agentive_RoT)

#Map of RM probabilities for quedar
##create table
quedar_mapa <- agentive %>%
  filter(Verbo %in% c("quedar (locative)", "quedar (attributive)")) %>%
  group_by(COSERID, Pron_reflexivo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  rename(No_RM = `No RM`) %>%
  mutate(No_RM = replace(No_RM, is.na(No_RM), 0), RM = replace(RM, is.na(RM), 0), 
         Prob_RM = RM/(RM + No_RM)) %>%
  ungroup() %>%
  left_join(enclaves_coser_todo)

##write table for creating map with QGIS
write_delim(quedar_mapa, "quedar_mapa.csv", delim = "\t")


#Plot presence of the RM by verb and area
##create table
agentive_verbs <- agentive %>%
  group_by(Area_dialecta, Verbo) %>%
  count(Verbo, Pron_reflexivo) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

##relevel Verbo and Area_dialecta
agentive_verbs$Verbo <- factor(agentive_verbs$Verbo, 
                               levels = c("quedar (locative)", "quedar (attributive)", "esperar", "estar"))
agentive_verbs$Area_dialecta <- factor(agentive_verbs$Area_dialecta, 
                               levels = c("Rest of the territory", "Northwest"))

##create plot
ggplot(agentive_verbs, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in \"quedar(se)\", \"esperar(se)\" and \"estar(se)\"", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Area_dialecta, scales = "free_x")

##save plot
ggsave("agentive_verbs.png", height = 5, width = 8) #saves the last plot

#RM probability by meanings of quedar
agentive %>% #todos los que no son N/S no me interesan: sujetos no humanos
  filter(Verbo == "quedar (locative)", Animacion_sujeto == "Animate") %>%
  group_by(Area_dialecta, Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = round(n/sum(n)*100, 0))

#Estar imperative
full_table %>% 
  filter(Verbo == "estar", 
         Tiempo_verbal == "imperativo", 
         Area_dialecta == "Rest of the territory", 
         Muestreo == "Exhaustivo") %>% 
  count(Pron_reflexivo)


#Estar contexts
##create table of all cases of estar with RM
estar_contexts <- full_table %>% 
  filter(Verbo == "estar", 
         Pron_reflexivo == "RM", 
         Aspectualidades != "Interrumpido") %>%
  mutate(Aspectualidades = replace(Aspectualidades, 
                                   Aspectualidades %in% c("Adverbio", "Clausula", "Cprep_con", "Existencial"), "Other")) %>%
  count(Aspectualidades) %>%
  rename(estarse = n)

##create table of the 150 random cases of estar with no RM
###The random selection was created with the following code:
#estar <- read_csv("EstarID.csv") #read table with IDs of all examples of unmarked estar
#muestra <- sample_n(estar, 150) #select a random sample with 150 cases
#write_csv(muestra, "EstarID_aleatorias.csv") #write table as csv
estar_random <- full_table %>%
  filter(Verbo == "estar", 
         Pron_reflexivo == "No RM", 
         !is.na(Aspectualidades), 
         Aspectualidades != "Interrumpido") %>%
  mutate(Aspectualidades = replace(Aspectualidades, 
                                   Aspectualidades %in% c("Adverbio", "Clausula", "Cprep_con", "Existencial", "Ptvo_preposicional"), "Other")) %>%
  count(Aspectualidades) %>%
  rename(estar = n)

##join tables
estar_contexts <- estar_contexts %>%
  full_join(estar_random) %>%
  gather("estarse", "estar", key = Verb, value = n) %>%
  mutate(Aspectualidades = ifelse(Aspectualidades == "Locativo", "Locative",
                                  ifelse(Aspectualidades == "Adjetivo", "Adjective",
                                         Aspectualidades)))
##relevel Aspectualidades & Verb
estar_contexts$Aspectualidades <- factor(estar_contexts$Aspectualidades, 
                                         levels = c("Temporal", "Locative", "Adjective", "Other"))
estar_contexts$Verb <- factor(estar_contexts$Verb, 
                                         levels = c("estarse", "estar"))

##create plot
ggplot(estar_contexts, aes(x=Aspectualidades,y=n)) + geom_col(position = "stack", fill = "darkgray") + 
  labs(title="Syntactic-semantic contexts of \"estar(se)\"", x="Context", y="Cases of estar") +
  geom_text(aes(label = n), position = "identity") + theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~ Verb, scales = "free_y")

##save plot
ggsave("estar_contexts.png", height = 5, width = 8) #saves the last plot

#Estarse: adjectives
full_table %>% 
  filter(Verbo == "estar", Pron_reflexivo == "RM", Aspectualidades == "Adjetivo") %>%
  count(Sdo_tipic_refl) %>%
  arrange(desc(n))

#quedar adjectives
##create table
quedar_adjs <- agentive %>% 
  filter(Verbo == "quedar (attributive)", 
         !Sdo_tipic_refl %in% c("asi", "con")) %>% 
  group_by(Animacion_sujeto, Pron_reflexivo) %>%
  count(Sdo_tipic_refl) %>%
  mutate(Total = sum(n)) %>% 
  slice_max(n, n = 5) %>% 
  ungroup()

##relevel Sdo_tipic_refl & Pron_reflexivo
quedar_adjs$Sdo_tipic_refl <- factor(quedar_adjs$Sdo_tipic_refl, 
                                            levels = c("cuajado", "duro", "frio", "blanco", "bien", "limpio", "solo", "seco",
                                                       "dormido", "parado", "soltero", "viudo", "atado", "atontado", "colgado",
                                                       "contento", "de_madre", "mal"))
quedar_adjs$Pron_reflexivo <- factor(quedar_adjs$Pron_reflexivo, 
                                    levels = c("RM", "No RM"))

##create plot 
quedar_adjs %>% 
  ggplot(aes(x = Sdo_tipic_refl, y = n)) + 
  geom_col(position = "stack", fill = "darkgray") + 
  labs(title="Attributive predicates with \"quedar(se)\" by animacy of the subject", x="Context", y="Cases of estar") +
  geom_text(aes(label = n), position = "identity") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(c("Animacion_sujeto", "Pron_reflexivo"), scales = "free")

##save plot
ggsave("quedar_adjs.png", height = 7, width = 8) #saves the last plot

#esperar complementos
##create table
esperar_contexts <- agentive %>% 
  filter(Verbo == "esperar", !is.na(Aspectualidades)) %>%
  mutate(Aspectualidades = ifelse(Aspectualidades == "Absoluto", "No complements", 
                                  ifelse(Aspectualidades %in% c("Claus", "Cprep_a", "Cprep_para"), 
                                         "Clause (event)", "Temporal phrase"))) %>%
  group_by(Pron_reflexivo) %>%
  count(Aspectualidades) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

##relevel Aspectualidades
esperar_contexts$Aspectualidades <- factor(esperar_contexts$Aspectualidades,
                               levels = c("No complements", "Temporal phrase", "Clause (event)"))

##create plot
ggplot(esperar_contexts, aes(x=Aspectualidades,y=prop, group=Pron_reflexivo)) + geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in \"esperar(se)\" \nby syntactic context", x="Context", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("esperar_contexts.png", height = 5, width = 8) #saves the last plot

#esperar imperative
agentive %>% 
  filter(Verbo == "esperar") %>%
  mutate(Tiempo_verbal = ifelse(Tiempo_verbal %in% c("imperativo", "Imperativo"), "Imperative", "Other")) %>%
  group_by(Tiempo_verbal) %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), prop=round(n/total*100,0)) 

#Fisher test with the data form the rest of the territory
##generate table
esperar_fisher <- agentive %>% 
  filter(Verbo == "esperar") %>%
  mutate(Tiempo_verbal = ifelse(Tiempo_verbal %in% c("imperativo", "Imperativo"), "Si", "No"),
         Pron_reflexivo = ifelse(Pron_reflexivo == "RM", "Si", "No"))


##Check whether there are duplicate places (i.e. the sample is not independent)
###create logical vector (duplicated / not duplicate)
duplicate_locations <- duplicated(esperar_fisher$COSERID)
###sum TRUE values
sum(duplicate_locations, na.rm = TRUE)
###histogram by COSERID
hist(count(esperar_fisher,COSERID)$n) 
###summary by COSERID
summary(tibble(count(esperar_fisher,COSERID)$n)) #most places have between 1-2 occurrences

##create function to generate random samples that remove duplicates but keep all variables
#dplyr functions: https://medium.com/optima-blog/writing-your-own-dplyr-functions-a1568720db0d
random_duplicates <- function (df, group_col) {
  df %>% group_by(.dots = lazyeval::lazy(group_col)) %>% #The lazyeval part makes the function read a column name with no quotes, tidyverse style
    sample_n(1)
}

##Create 1000 permutations, from which we get the estimate (odd-ratio)
ft_perm_esperar_fisher <- sapply(1:1000, function (y) {
  
  # Randomly chose one data point for each duplicate location
  esperar_fisher_random1 <- random_duplicates(esperar_fisher, COSERID) 
  
  # Compute fisher test statistics
  ft <- fisher.test(table(esperar_fisher_random1$Pron_reflexivo, esperar_fisher_random1$Tiempo_verbal))
  # Extract p-values from results
  
  estimate <- ft$estimate
  return(estimate)
}) 

##summary of results
summary(ft_perm_esperar_fisher)

#Animacy of the subject: RM probability for all verbs
agentive %>% 
  group_by(Verbo) %>% 
  count(Animacion_sujeto)

#Animacy of the subject: Plot RM probability for quedar
agentive %>% 
  filter(!Verbo %in% c("estar", "esperar")) %>%
  group_by(Area_dialecta, Animacion_sujeto, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n)) %>% 
  ggplot(aes(x=Animacion_sujeto,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with \"quedar(se)\" \nby geographical area and animacy of the subject", x="Animacy of the subject", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(c("Area_dialecta", "Verbo"))

##save plot
ggsave("agentive_animacy.png", height = 6, width = 6) #saves the last plot

#Animacy of the subject: RM probability for estar
agentive %>% 
  filter(Verbo == "estar") %>%
  group_by(Animacion_sujeto) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = round(n/sum(n)*100, 1)) 

#Mixed model
##create table only with Rest of the territory
agentive2 <- agentive %>% 
  filter(Area_dialecta != "Northwest")

##Transform columns into factor (first level is reference level)
agentive2$Pron_reflexivo <- factor(agentive2$Pron_reflexivo, levels = c("No RM", "RM"))
agentive2$Animacion_sujeto <- factor(agentive2$Animacion_sujeto, levels = c("Inanimate", "Animate"))
agentive2$COSERID <- factor(agentive2$COSERID)
agentive2$Verbo <- factor(agentive2$Verbo)

##calculate model
agentive_model <- glmer(Pron_reflexivo ~ Animacion_sujeto + (1|Verbo) + (1|COSERID), 
                     family = "binomial", data = agentive2)

##calculate model summary statistics
summary(agentive_model)
range(resid(agentive_model))
hist(resid(agentive_model)) #normal distribution?

##tidy model
agentive_model_tidy <- tidy(agentive_model, exponentiate = F, conf.int = T) #statistic es el z-value

##write model
write_delim(agentive_model_tidy, "5.7_agentive_model_tidy.csv", delim = "\t")


