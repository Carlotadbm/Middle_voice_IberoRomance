#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 3
#Section 3.2. (And section 4.6, see line 111, 220, 282)

#load libraries
library(tidyverse)
library(maps)
library(scatterpie)
#library(ggforce)

#read table with RM data
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t")

#read tables with coordinates
#Whole corpus
enclaves_coser_todo <- read_delim("COSER_dots_total_2019.csv", delim=";")
enclaves_coser_todo  <-  enclaves_coser_todo %>%
  rename(COSERID = ID) #rename ID so that it has the same name as in sintactica

#Corpus E
enclaves_coser_E <- read_delim("COSER_dots_filter.csv", delim="\t")
enclaves_coser_E  <-  enclaves_coser_E %>%
  rename(COSERID = ID) #rename ID so that it has the same name as in sintactica


#SECTION 3.2.1. Anticausative
#Read table
anticausative <- read_delim("Subtabla_anticausativos2019.csv", delim=";")


#Check number of examples
nrow(anticausative) #number of examples
#proportion of reflexive uses
anticausative %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
anticausative %>% 
  distinct(Verbo) %>% 
  nrow() 

#table RM (verbs)
anticausative %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 
  

#anticausative map
##create table with proportions of RM for map
anticausative_map <- anticausative %>%
  group_by(COSERID, Pron_reflexivo) %>%
  summarize(n()) %>%
  spread(Pron_reflexivo, `n()`) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
  rename(RM = Si, No_RM = No) %>%
  mutate(Total = RM + No_RM, Prob_RM = RM/Total)

##join coordinates
anticausative_map <- full_join(anticausative_map, enclaves_coser_todo) 

##write table for creating map with QGIS
write_csv(anticausative_map, "anticausative_map.csv")


#SECTION 3.2.2. Absolute uses
#create table
absolute <- sintactica %>%
  filter(Tipo_sintactico == "Absoluto") %>%
  filter(Pron_reflexivo == "Si")

#Check number of examples
nrow(absolute) #number of examples

#Check number of verbs
absolute %>% 
  distinct(Verbo) %>% 
  nrow() 

#map of coger
##create table
coger_abs_map <- absolute %>%
  filter(Verbo == "coger") %>%
  group_by(COSERID, Pron_reflexivo) %>%
  summarize(n()) %>%
  spread(Pron_reflexivo, `n()`) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
  rename(RM = Si)

##number of places with cases
coger_abs_map %>% 
  distinct(COSERID) %>% 
  nrow()

##join coordinates
coger_abs_map <- full_join(coger_abs_map, enclaves_coser_todo)

##write table for creating map with QGIS
write_csv(coger_abs_map, "coger_abs_map.csv")

#SECTION 3.2.3. Deobjective verbs

##clean table
deobjective <- sintactica %>%
  filter(Tipo == "Verbo_se") %>%
  filter(Tipo_sintactico == "Deobjetivo") 


#animacy of the subject (section 4.6)
deobjective %>% 
  count(Animacion_sujeto) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1),
         Total = sum(n))
#end section 4.6

#Check number of examples
nrow(deobjective) #number of examples
#proportion of reflexive uses
deobjective %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
deobjective %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
deobjective %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#plot most frequent deobjective verbs
##create table with proportions per verb
deobjective_verbs <- deobjective %>%
  count(Verbo, Pron_reflexivo) %>%
  group_by(Verbo) %>%
  mutate(total=sum(n), prop=n/total*100) %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>% 
  mutate(Verbo = ifelse(Verbo == "apannar", "apañar", Verbo)) %>% 
  filter(total > 9) %>%
  arrange(desc(prop))

##Create ordered vector with the relevant distinct verbs
deobjective_verbs_unique <- unique(deobjective_verbs$Verbo)

##Order column Verbo by frequency for the plot
deobjective_verbs$Verbo <- factor(deobjective_verbs$Verbo, levels = deobjective_verbs_unique) 

##create barplot
ggplot(deobjective_verbs, aes(x=Verbo, y=n, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Deobjective verbs", x="Verb", y="Presence of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() + 
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
##save plot
ggsave("deobjective_verbs.png", width=7, height = 4) 

#Map with deobjective verbs
##create table with proportions per verb
deobjective_map_verbs <- deobjective %>%
  group_by(COSERID, Verbo, Pron_reflexivo) %>%
  summarize(n()) %>%
  spread(Pron_reflexivo, `n()`) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
  mutate(Total = Si + No) %>%
  filter(Verbo %in% c("aguantar", "asomar", "callar", "confesar", "explicar")) %>%
  ungroup() %>%
  group_by(Verbo) %>%
  mutate(Rango1 = range(Total)[1], Rango2 = range(Total)[2])

##join coordinates
deobjective_map_verbs <- inner_join(deobjective_map_verbs, enclaves_coser_todo)

##load map of Spain
spain <- map_data('world', "spain")

##create map
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = deobjective_map_verbs, 
                  aes(Longitude, Latitude, r = sqrt(Total)/7),
                  cols = c("No", "Si"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("No", "Si"),
    labels = c("No RM", "RM"),
    values = c("No" = "white",
               "Si" = "black")
  ) +
  facet_wrap(~ Verbo) +
  labs(title = "Presence of the RM in selected de-objective verbs") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(0.86, 0.12),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(colour = "black"))

##save map
ggsave("deobjective_verbs_map.png", width=13)


#Section 3.2.4. Conversive verbs
##create table
conversive <- sintactica %>%
  filter(Tipo_sintactico == "Inverso") 

#animacy of the subject (section 4.6)
conversive %>% 
  count(Animacion_sujeto) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1),
         Total = sum(n))
#end section 4.6

#Check number of examples
nrow(conversive) #number of examples
#proportion of reflexive uses
conversive %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
conversive %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
conversive %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

nrow(conversive)
#number of verbs
conversive %>% 
  distinct(Verbo) %>% 
  nrow()

#Proportion of reflexive uses by semantic type
conversive %>% 
  mutate(Tipo_semantico = ifelse(is.na(Tipo_semantico), "Other", Tipo_semantico)) %>% 
  mutate(Tipo_semantico = ifelse(Tipo_semantico == "Emocion", "Emocion", "Other")) %>% 
  group_by(Tipo_semantico) %>% 
  distinct(Verbo) %>% 
  count(Tipo_semantico) %>% 
  ungroup() %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#map with proportion of reflexive uses
##create table with proportion of reflexive uses
conversive_map <- conversive %>%
  group_by(COSERID, Pron_reflexivo) %>%
  summarize(n()) %>%
  spread(Pron_reflexivo, `n()`) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
  rename(RM = Si, No_RM = No) %>%
  mutate(Total = RM + No_RM, RM_prob = RM/Total)

##join coordinates
conversive_map <- full_join(conversive_map, enclaves_coser_todo)

##write table for creating map with QGIS
write_csv(conversive_map, "conversive_map.csv")

#Section 3.2.5. Antipassive verbs

#create table
antipassive <- sintactica %>%
  filter(Tipo_sintactico %in% c("Antipasiva_SO", "Antipasiva_CR")) 

#animacy of the subject (section 4.6)
antipassive %>% 
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Humano"), "Animate", "Inanimate")) %>% 
  count(Animacion_sujeto) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1),
         Total = sum(n))
#end section 4.6

#create table with exhaustively collected verbs
antipassive_exh <- antipassive %>%
  filter(Verbo %in% c("acordar", "agarrar", "aprovechar", "cambiar", "disfrutar", "entender", "encargar", "mudar", "olvidar", "recordar", "referir"))

#Check number of examples
nrow(antipassive_exh) #number of examples
#proportion of reflexive uses
antipassive_exh %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
antipassive_exh %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
antipassive_exh %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#Plot with RM proportion by verb
##create table with proportions
antipassive_verbs <- antipassive_exh %>%
  count(Verbo, Pron_reflexivo) %>%
  group_by(Verbo) %>%
  mutate(total=sum(n), prop=(n/total*100)) %>%
  filter(total > 4) %>% 
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) 

##Create ordered vector with the relevant distinct verbs
antipassive_verbs_unique <- antipassive_verbs %>%
  filter(Pron_reflexivo == "RM") %>% 
  arrange(desc(prop)) %>% 
  select(Verbo)

##Order column Verbo by frequency for the plot
antipassive_verbs$Verbo <- factor(antipassive_verbs$Verbo, levels = antipassive_verbs_unique$Verbo) 

##create barplot
ggplot(antipassive_verbs, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Antipassive verbs", x="Verb", y="Presence of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("antipassive_verbs.png", height = 3) #saves the last plot

##create table
antipassive_map_verbs <- antipassive_exh %>%
  group_by(COSERID, Verbo, Pron_reflexivo) %>%
  summarize(n()) %>%
  spread(Pron_reflexivo, `n()`) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = Si + No) %>%
  filter(Verbo %in% c("cambiar", "entender", "recordar")) %>%
  ungroup() %>%
  group_by(Verbo) %>%
  mutate(Rango1 = range(Total)[1], Rango2 = range(Total)[2])

##join coordinates
antipassive_map_verbs <- inner_join(antipassive_map_verbs, enclaves_coser_todo)

##load map of Spain
spain <- map_data('world', "spain")

##create mao
ggplot(spain, aes(long, lat)) +
  geom_map(map=spain, aes(map_id=region), fill="white", color="black") +
  geom_scatterpie(data = antipassive_map_verbs, 
                  aes(Longitude, Latitude, r = sqrt(Total)/7),
                  cols = c("No", "Si"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("No", "Si"),
    labels = c("No RM", "RM"),
    values = c("No" = "white",
               "Si" = "black")
  ) +
  facet_wrap(~ Verbo, nrow=2) +
  labs(title = "Presence of the RM in selected antipassive verbs") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(0.86, 0.12),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.background = element_rect(colour = "black"))

##save map
ggsave("antipassive_verbs_map.png", width=10)


#Sección 3.2.6. Intransitive verbs

#create table
intransitive <- sintactica %>%
  filter(Tipo_sintactico == "Intransitivo") #"Intransitivo_Anticausativo" already included under anticausatives

#Check only those that show the RM at least once
intransitive_RM <- intransitive %>%
  filter(Pron_reflexivo == "Si")

#Check number of verbs that show the RM at least once
intransitive_RM %>% 
  distinct(Verbo) %>% 
  nrow() 

#Create table with exhaustively collected verbs
##Corpus E
intransitive_E <- intransitive %>%
  filter(Muestreo == "Exhaustivo") %>%
  filter(Verbo %in% c("estar", "salir", "volver", "ir", "venir", "morir", "vivir", "nacer", 
                               "dormir", "escapar", "valer", "andar", "llegar", "quedar", "entrar"))

##Whole corpus
intransitive_NE <- intransitive %>%
  filter(Verbo %in% c("pasar", "caer", "esperar", "marchar", "reir", 
                               "cagar", "montar", "arder", "crecer", "mear"))

##Verbs entrar, llegar, quedar
intransitive_entrar_llegar_quedar <- intransitive %>%
  filter(COSERID %in% c(716, 723, 728, 959, 1012, 1015, 1603, 2321, 3209, 3412, 3610, 4108, 4117, 4301, 4401, 4403, 4407, 4419, 4602, 4611, 4613, 4714)) %>%
  filter(Verbo %in% c("llegar", "entrar", "quedar"))

##Joni all together
intransitive_exhaustive <- rbind(intransitive_E, intransitive_NE, intransitive_entrar_llegar_quedar)

#Check number of examples
nrow(intransitive_exhaustive) #number of examples
#proportion of reflexive uses
intransitive_exhaustive %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
intransitive_exhaustive %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
intransitive_exhaustive %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#Create plots with RM proportion by verbs
##create tables with proportions
intransitive_verbs <- intransitive_exhaustive %>%
  count(Verbo, Pron_reflexivo) %>%
  group_by(Verbo) %>%
  mutate(total=sum(n), prop=(n/total*100)) %>% 
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM"))

##Create two plots, for verbs most frequently reflexive and verbs least frequently reflexive
###Find the cutpoint
intransitive_verbs_RM <- intransitive_verbs %>% #we select rows with the RM
  filter(Pron_reflexivo == "RM")
median(intransitive_verbs_RM$prop) #find the median value of the prop of RM

###First plot (frequently marked verbs)
####create table with all values greater than or equal to the median and arrange them
verbs_intr_RMfreq <- intransitive_verbs_RM %>%
  filter(prop >= 8.108108) %>%
  arrange(desc(prop))

####Create ordered vector with the relevant distinct verbs
verbs_intr_RMfreq <- unique(verbs_intr_RMfreq$Verbo) 

####Create table filtering with the verbs above
intransitive_verbs_freq <- intransitive_verbs %>% 
  filter(Verbo %in% verbs_intr_RMfreq)

####Order column Verbo by frequency for the plot
intransitive_verbs_freq$Verbo <- factor(intransitive_verbs_freq$Verbo, levels = verbs_intr_RMfreq) 

####Create barplot
ggplot(intransitive_verbs_freq, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Intransitive verbs I", x="Verb", y="Presence of the RM", fill = "Reflexive marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####save plot
ggsave("intransitive_verbs_freq.png", height = 4) #saves the last plot

###Second plot (unfrequently marked verbs)
####create table with all values lower than the median and arrange them
verbs_intr_RMunfreq <- intransitive_verbs_RM %>% 
  filter(prop < 8.108108)%>%
  arrange(desc(prop))

####Create ordered vector with the relevant distinct verbs
verbs_intr_RMunfreq <- unique(verbs_intr_RMunfreq$Verbo) 

####Create table filtering with the verbs above
intransitive_verbs_unfreq <- intransitive_verbs %>% 
  filter(Verbo %in% verbs_intr_RMunfreq)

####Order column Verbo by frequency for the plot
intransitive_verbs_unfreq$Verbo <- factor(intransitive_verbs_unfreq$Verbo, levels = verbs_intr_RMunfreq) 

####Create barplot
ggplot(intransitive_verbs_unfreq, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Intransitive verbs II", x="Verb", y="Presence of the RM", fill = "Reflexive marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####Save plot
ggsave("intransitive_verbs_unfreq.png", height = 4) #saves the last plot

#Crete map
##Create table with RM proportion by verb and place
intransitive_exhaustive_map_freq <- intransitive_exhaustive %>%
  group_by(COSERID, Verbo, Pron_reflexivo) %>%
  summarize(n()) %>%
  rename(n=`n()`) %>%
  spread(Pron_reflexivo, n) %>%
  replace_na(list(No = 0, Si = 0)) %>% 
  rename(RM = Si, No_RM = No) %>%
  mutate(Total = RM + No_RM, Prob_RM = RM/Total) %>%
  ungroup()

##Calculate cutpoint
summary(intransitive_exhaustive_map_freq$Total) #the median is 4, I'll choose those over 9 cases

##Create table with RM proportion by verb and place and select rows with more than 9 total occurrences
###Create table with number of verbs with more than 9 tokens per place
intransitive_exhaustive_map_verbs <- intransitive_exhaustive_map_freq %>%
  filter(Total > 9) %>%
  group_by(COSERID) %>%
  count(COSERID)
###Create table with sum of RM proportion of all verbs with more than 9 tokens per place
intransitive_exhaustive_map_probs <- intransitive_exhaustive_map_freq %>%
  filter(Total > 9) %>%
  group_by(COSERID) %>%
  summarise(sum = sum(Prob_RM))
###Join tables, calculate the mean RM proportion per place and join coordinates
intransitive_map <- full_join(intransitive_exhaustive_map_verbs, intransitive_exhaustive_map_probs) %>%
  mutate(mean_RM = sum/n) %>%
  left_join(enclaves_coser_todo)
###write table for creating map with QGIS
write_csv(intransitive_map, "intransitive_map.csv")

#Section 3.2.7. Non-reversible verbs
#Create table
nonreversible <- sintactica %>%
  filter(Tipo_sintactico == "Irreversible") 

#Check number of examples
nrow(nonreversible) #number of examples
#table RM (examples)
nonreversible %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1))

#Check number of verbs
nonreversible %>% 
  distinct(Verbo) %>% 
  nrow() 
#table RM (verbs)
nonreversible %>%
  group_by(Pron_reflexivo) %>% 
  distinct(Verbo) %>% 
  count(Pron_reflexivo) 

#Create table with number and proportion of verbs by semantic type
nonreversible %>% 
  count(Tipo_semantico, Verbo) %>%
  select(Tipo_semantico, Verbo) %>%
  count(Tipo_semantico) %>%
  mutate(percentage = round(n/sum(n)*100, 1))

#Create table with verbs by semantic type
nonreversible %>% 
  count(Tipo_semantico, Verbo) %>% 
  print(n = Inf)

