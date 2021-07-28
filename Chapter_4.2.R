#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 4
#Section 4.2

#load library
library(tidyverse)

#read table
anticausative <- read_delim("Subtabla_anticausativos2019.csv", delim=";")

#create table with frequencies by verb and region
anticausative_freqRM <- anticausative %>%
  group_by(Area_dialectal, Verbo) %>%
  count(Pron_reflexivo) %>%
  spread(Pron_reflexivo, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  rename(RM = Si, No_RM = No) %>%
  mutate(Total = RM + No_RM, RM_prob = RM/Total) %>%
  filter(Total > 9) %>%
  ungroup() %>% 
  mutate(Area_dialectal = ifelse(Area_dialectal == "Noroeste", "Northwest", "Rest of the territory"))


#create boxplot
ggplot(anticausative_freqRM, aes(x=Area_dialectal, y=RM_prob)) + 
  geom_boxplot() +
  labs(title="Relative frequency of the RM in anticausative verbs", x="Geographical area", y="Relative frequency of the RM") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0,1)
#summary statistics: see line 64 & 118
#number of verbs: see line 66 & 120

#save plot
ggsave("anticausative_boxplot.png", width = 7, height = 4) #saves the last plot


#create dotplot
ggplot(anticausative_freqRM, aes(x=RM_prob)) + 
  geom_dotplot(binwidth=0.0224) + 
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title="Relative frequency of the RM by verb", x="Relative frequency of the RM") +
  facet_wrap(c("Area_dialectal"))

#save plot
ggsave("anticausative_dotplot.png", width = 7, height = 4.5) #saves the last plot

#Plot ecdf (empirical cumulative distribution)
ggplot(anticausative_freqRM, aes(RM_prob)) + stat_ecdf(geom = "step") +
  labs(title="Empirical cumulative distribution of the probability of the RM \nin anticausative verbs", x="Relative frequency of the RM", y="Frequency of verbs") + 
  facet_wrap(c("Area_dialectal"))

#save plot
ggsave("anticausative_rest_ecdf.png", width = 7, height = 4.5) #saves the last plot


#Section 4.2.1 (Rest of the territory)
#clean table: only rest of the territory
anticausative_freqRM_resto <- anticausative_freqRM %>%
  filter(Area_dialectal == "Rest of the territory")

#summary statistics (reported in section 4.2)
summary(anticausative_freqRM_resto$RM_prob)
#number of verbs (reported in section 4.2)
nrow(anticausative_freqRM_resto)

#"Group A" verbs (RM less than 40%)
##generate table
anticausative_freqRM_resto_grupoA <- anticausative_freqRM_resto %>%
  filter(RM_prob >= 0.4) %>%
  gather(Pron_reflexivo, casos, RM, No_RM) %>%
  arrange(RM_prob)

##check unique verbs
anticausative_freqRM_resto_grupoA %>% 
  distinct(Verbo) %>% 
  print(n = Inf)

#"Group B" verbs (RM more than 40%)
##arrange verbs by probability
anticausative_freqRM_resto_grupoB <- anticausative_freqRM_resto %>%
  arrange(desc(RM_prob))
##order column according to that arrangement
anticausative_freqRM_resto_grupoB_verbos <- anticausative_freqRM_resto_grupoB$Verbo
anticausative_freqRM_resto_grupoB$Verbo <- factor(anticausative_freqRM_resto_grupoB$Verbo, levels = anticausative_freqRM_resto_grupoB_verbos) 

##create table
anticausative_freqRM_resto_grupoB <- anticausative_freqRM_resto_grupoB %>%
  filter(RM_prob < 0.5) %>%
  gather(Pron_reflexivo, casos, RM, No_RM) %>%
  arrange(desc(RM_prob)) %>%
  mutate(Pron_reflexivo = replace(Pron_reflexivo, Pron_reflexivo == "No_RM", "No RM")) 

##check unique verbs
anticausative_freqRM_resto_grupoB %>% 
  distinct(Verbo) %>% 
  print(n = Inf)

##create plot 
ggplot(anticausative_freqRM_resto_grupoB, aes(x=Verbo, y=casos, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Class B anticausatives in the rest of the territory", x="Verb", y="Presence of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = casos), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##save plot
ggsave("anticausative_verbos_resto_grupoB.png", height = 4) #saves the last plot


#Section 4.2.2

#clean table: only Northwest
anticausative_freqRM_NW <- anticausative_freqRM %>%
  filter(Area_dialectal == "Northwest")
#summary statistics (reported in section 4.2)
summary(anticausative_freqRM_NW$RM_prob)
#number of verbs (reported in section 4.2)
nrow(anticausative_freqRM_NW)

##arrange verbs by probability
anticausative_freqRM_NW_all <- anticausative_freqRM_NW %>%
  arrange(desc(RM_prob))
##order column according to that arrangement
anticausative_freqRM_NW_all_verbos <- anticausative_freqRM_NW_all$Verbo
anticausative_freqRM_NW_all$Verbo <- factor(anticausative_freqRM_NW_all$Verbo, levels = anticausative_freqRM_NW_all_verbos) 

##create table
anticausative_freqRM_NW_all <- anticausative_freqRM_NW_all %>%
  gather(Pron_reflexivo, casos, RM, No_RM) %>%
  arrange(desc(RM_prob)) %>%
  mutate(Pron_reflexivo = replace(Pron_reflexivo, Pron_reflexivo == "No_RM", "No RM"))

##create plot
ggplot(anticausative_freqRM_NW_all, aes(x=Verbo,y=casos, group=Pron_reflexivo)) + geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Anticausatives verbs in the Northwest", x="Verb", y="Presence of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = casos), position = position_fill(vjust = .5)) + theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("anticausative_verbos_NW_all.png", height = 4) #saves the last plot

#Comparison of verbs by area
##create table with proportion of RM by verb and area
anticausative_freqRM_comparison <- anticausative_freqRM %>%
  mutate(RM_prob = round(RM_prob*100, 1)) %>% 
  select(Area_dialectal, Verbo, RM_prob) %>% 
  spread(Area_dialectal, RM_prob) %>% 
  filter(!is.na(Northwest)) %>% 
  mutate(Northwest = str_c(Northwest, "%"), 
         `Rest of the territory` = str_c(`Rest of the territory`, "%"))
##number of verbs considered
nrow(anticausative_freqRM_comparison)

##write table
write_delim(anticausative_freqRM_comparison, "anticausative_freqRM_comparison.csv", delim = "\t")

