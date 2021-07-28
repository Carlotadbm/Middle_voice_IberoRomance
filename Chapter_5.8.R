#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 5
#Section 5.8. Pasar

#load library
library(tidyverse)

#load table
full_table <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t")

#clean table
full_table <- full_table %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>% 
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory")) 

##create table with pasar
pasar <- full_table %>%
  filter(Tipo_sintactico %in% c("Intransitivo", "Intransitivo_Anticausativo")) %>% 
  filter(Verbo == "pasar") %>%
  filter(Tipo_semantico == "Espontaneo")

#Total frequencies
##RM probability
pasar %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1), 
         total = sum(n))

##RM probability by area
pasar %>% 
  group_by(Area_dialecta) %>% 
  count(Pron_reflexivo) %>% 
  mutate(Percentage = round(n/sum(n)*100, 1), 
         Total = sum(n))

#Focussing now on the rest of the territory
##clean table
pasar <- pasar %>%
  filter(Area_dialecta == "Rest of the territory")

#Typical meanings depending on the subject
##Event
pasar %>%
  filter(Aspectualidades == "Evento") %>%
  group_by(Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100)

##Feelings, emotions or sickness,
pasar %>%
  filter(Aspectualidades == "Sensacion") %>%
  group_by(Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100)

##Food
pasar %>%
  filter(Aspectualidades == "Comida") %>%
  group_by(Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = n/sum(n)*100)

##Period of time
pasar %>%
  filter(Aspectualidades == "Tiempo") %>%
  group_by(Sdo_tipic_refl) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), Perc = round(n/sum(n)*100, 0))

#Plot with proportion of RM by subject
##create table
pasar_subj <- pasar %>%
  mutate(Aspectualidades = ifelse(Aspectualidades == "Comida", "Food", 
                                  ifelse(Aspectualidades == "Evento", "Event",
                                         ifelse(Aspectualidades == "Sensacion", "Feeling", "Time")))) %>%
  group_by(Aspectualidades) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n)*100)

##relevel Aspectualidades
pasar_subj$Aspectualidades <- factor(pasar_subj$Aspectualidades, levels = c("Feeling", "Food", "Time", "Event"))

##create plot
ggplot(pasar_subj, aes(x=Aspectualidades,y=prop, group=Pron_reflexivo)) + geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM and type of subject with spontaneous \"pasar(se)\"", x="Type of subject", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("pasar_subj.png", height = 6, width = 9) #saves the last plot
