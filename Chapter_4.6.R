#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 4
#Section 4.6. Animacy of the subject
#Animacy for section 5.3: line 128

#load library
library(tidyverse)

#read table
animacy <- read_delim("Subtabla_anticausativos2019.csv", delim=";")

#clean table
animacy <- animacy %>% 
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Inanimado", "Evento", "Impersonal", "Vehiculo"), 
                                   "Inanimate", "Animate")) %>% 
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "No", "No RM", "RM")) %>% 
  mutate(Area_dialectal = ifelse(Area_dialectal == "Noroeste", "Northwest", "Rest of the territory"))


#Barplot by area
##create table with RM probabilities by subject animacy and area
antic_animacy <- animacy %>%
  count(Area_dialectal, Animacion_sujeto, Pron_reflexivo) %>%
  group_by(Area_dialectal, Animacion_sujeto) %>%
  mutate(total=sum(n), 
         prop=(n/total*100))

##create plot
ggplot(antic_animacy, aes(x=Animacion_sujeto,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM with anticausative verbs depending \non the animacy of the subject by geographical area", x="Animacy of the subject", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialectal)

##save plot
ggsave("antic_animacy.png", height = 4, width = 6) #saves the last plot


#Dotplots of the correlation (faceted by area)
#grouped by verb, selected those who have more than 9 occurrences

##create table with RM probabilities by verb and area
animacy_RM_vbs <- animacy %>%
  group_by(Area_dialectal, Verbo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n)  %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = `No RM` + RM, RM_prob = RM/Total) %>%
  filter(Total > 9) %>%
  ungroup()

##create table with probabilities of having an animate subject by verb and area
animacy_Anim_vbs <- animacy %>%
  group_by(Area_dialectal, Verbo) %>%
  count(Animacion_sujeto) %>%
  spread(Animacion_sujeto, n)  %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = Animate + Inanimate, Anim_prob = Animate/Total) %>%
  filter(Total > 9) %>%
  ungroup()

##join tables
animacy_cor_vbs <- full_join(animacy_Anim_vbs, animacy_RM_vbs, by=c("Verbo", "Area_dialectal"))


##create plot
ggplot(animacy_cor_vbs, aes(Anim_prob, RM_prob)) + 
  geom_point() + 
  labs(title="Correlation between the probability of the RM and the probability of having \nan animate subject by verb", x="Probability of an animate subject", y="Probability of the RM") + 
  facet_wrap(c("Area_dialectal"))

##save plot
ggsave("animacy_cor_vbs.png", width = 9, height = 7) #saves the last plot


#Boxplots by areas
##create table with RM probabilities by verb and area
animacy_boxplot <- animacy %>%
  group_by(Area_dialectal, Animacion_sujeto, Verbo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  mutate(Total = RM + `No RM`, RM_prob = RM/Total) %>%
  filter(Total > 9) %>%
  ungroup()

##check number of verbs in each area and category
animacy_boxplot %>%
  group_by(Area_dialectal) %>%
  count(Animacion_sujeto)

##create boxplot
ggplot(animacy_boxplot, aes(x=Animacion_sujeto, y=RM_prob)) + 
  geom_boxplot() +
  labs(title="Relative frequency of the RM by verb depending on \nanimacy of the subject and geographical area", x="Animacy of the subject", y="Relative frequency of the RM") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0,1) + 
  facet_wrap(c("Area_dialectal"))

##save plot
ggsave("animacy_boxplot.png", width = 7, height = 6) #saves the last plot

#calculate central summary statistics for each subsample
##Northwest, Animate
animacy_boxplot %>% 
  filter(Area_dialectal == "Northwest", Animacion_sujeto == "Animate") %>% 
  summary()
  
##Northwest, Inanimate
animacy_boxplot %>% 
  filter(Area_dialectal == "Northwest", Animacion_sujeto == "Inanimate") %>% 
  summary()

##Rest of the territory, Animate
animacy_boxplot %>% 
  filter(Area_dialectal == "Rest of the territory", Animacion_sujeto == "Animate") %>% 
  summary()

##Rest of the territory, Inanimate
animacy_boxplot %>% 
  filter(Area_dialectal == "Rest of the territory", Animacion_sujeto == "Inanimate") %>% 
  summary()

#Section 5.3
#Dotplots with facets
animacy_boxplot %>% 
  ggplot(aes(x=RM_prob)) + 
  geom_dotplot(binwidth=0.0224) + 
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title="Relative frequency of the RM by intransitive verb depending on \nanimacy of the subject and geographical area", x="Relative frequency of the RM") + 
  facet_wrap(c("Animacion_sujeto", "Area_dialectal"))

#save plot
ggsave("animacy_dotplot.png", width = 6, height = 7) #saves the last plot


