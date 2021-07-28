#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 4
#Section 4.4. The aspectual properties of the predicate

#load library
library(tidyverse)

#load table
anticausative <- read_delim("Subtabla_anticausativos2019.csv", delim=";")

#poner a + inf (or similar)
##create table
atelic_inf <- anticausative %>%
  filter(Tipo_perifrasis == "Atelica", Tiempo_verbal == "infinitivo", Clase_aspectual == "Durativo")

##check involved periphrases
atelic_inf %>% 
  distinct(Perifrasis) %>% 
  print(n = Inf)

##check cases with RM
count(atelic_inf, Pron_reflexivo)

#tener + gerundio 
##create table
atelic_ger <- anticausative %>%
  filter(Tipo_perifrasis == "Atelica", Tiempo_verbal == "gerundio", Clase_aspectual == "Durativo")

##check involved periphrases
atelic_ger %>% 
  distinct(Perifrasis)
  
##check cases with RM
count(atelic_ger, Pron_reflexivo)

#poner or a similar verb + a que + inflected verb in the subjunctive
##create table
atelic <- anticausative %>%
  filter(Tipo_perifrasis == "Atelica", Clase_aspectual == "Durativo") %>% 
  mutate(Tiempo_verbal = ifelse(Tiempo_verbal %in% c("gerundio", "infinitivo"), "non-finite", "finite"))

#calculate probability of RM by type and region
atelic %>% 
  filter(Area_dialectal == "Resto") %>% 
  group_by(Tiempo_verbal) %>% 
  count(Pron_reflexivo) %>% 
  mutate(percentage = round(n/sum(n)*100, 1), 
         total = sum(n))

#Fisher test with the data form the rest of the territory
##generate table
atelic_resto <- atelic %>% 
  filter(Area_dialectal == "Resto") %>% 
  mutate(Tiempo_verbal = ifelse(Tiempo_verbal == "finite", "Si", "No")) #Rename variables so that positive cases: finite & RM; non-finite & No RM

##Check whether there are duplicate places (i.e. the sample is not independent)
###create logical vector (duplicated / not duplicate)
duplicate_locations <- duplicated(atelic_resto$COSERID)
###sum TRUE values
sum(duplicate_locations, na.rm = TRUE)
###histogram by COSERID
hist(count(atelic_resto,COSERID)$n) 
###summary by COSERID
summary(tibble(count(atelic_resto,COSERID)$n)) #most places have between 1-3 occurrences

##create function to generate random samples that remove duplicates but keep all variables
#dplyr functions: https://medium.com/optima-blog/writing-your-own-dplyr-functions-a1568720db0d
random_duplicates <- function (df, group_col) {
  df %>% group_by(.dots = lazyeval::lazy(group_col)) %>% #The lazyeval part makes the function read a column name with no quotes, tidyverse style
    sample_n(1)
}

##Create 1000 permutations, from which we get the estimate (odd-ratio)
ft_perm_atelic_resto <- sapply(1:1000, function (y) {
  
  # Randomly chose one data point for each duplicate location
  atelic_resto_random1 <- random_duplicates(atelic_resto, COSERID) 
  
  # Compute fisher test statistics
  ft <- fisher.test(table(atelic_resto_random1$Pron_reflexivo, atelic_resto_random1$Tiempo_verbal))
  # Extract p-values from results
  
  estimate <- ft$estimate
  return(estimate)
}) 

##summary of results
summary(ft_perm_atelic_resto)

#Comparison of all periphrasis 
##Clean table: Select non-finite forms only in the rest of the territory and remove causative periphrases
periphrases <- anticausative %>%
  filter(Tiempo_verbal %in% c("infinitivo", "gerundio"), Area_dialectal == "Resto", Tipo_perifrasis != "Causativa") %>% 
  mutate(Tipo_perifrasis = ifelse(Tipo_perifrasis == "Atelica2", "Atelic", 
                                  ifelse(Tipo_perifrasis == "Telica", "Telic", 
                                         ifelse(Tipo_perifrasis == "Atelica", "poner + inf. + a", 
                                                ifelse(Tipo_perifrasis == "Casi_atelica", "Similar to poner + inf. + a", 
                                                       ifelse(Tipo_perifrasis == "Indiferente", "Non-restricted", Tipo_perifrasis))))))


##Create table for plot (RM probability by periphrases)
periphrases_graph <- periphrases %>%
  filter(!is.na(Tipo_perifrasis)) %>%
  count(Tipo_perifrasis, Pron_reflexivo) %>%
  group_by(Tipo_perifrasis) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

##reorder levels
periphrases_graph$Tipo_perifrasis <- factor(periphrases_graph$Tipo_perifrasis, 
                                            levels = c("Non-restricted", "Atelic", "Telic", 
                                                       "Similar to poner + inf. + a", "poner + inf. + a"))
##create plot
ggplot(periphrases_graph, aes(x=Tipo_perifrasis, y=prop, group=Pron_reflexivo)) + geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in anticausative infinitives or gerunds \ndepending on the aspectual characteristics of the periphrases", x="Type of periphrasis", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##save plot
ggsave("antic_periphrases.png", height = 6, width = 8) #saves the last plot 


