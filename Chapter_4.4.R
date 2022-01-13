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
atelic_resto %>% 
  count(COSERID, sort = T)
###histogram by COSERID
hist(count(atelic_resto,COSERID)$n) 
###summary by COSERID
summary(tibble(count(atelic_resto,COSERID)$n)) #most places have between 1-3 occurrences

### Bootstrapping
#### Hierarchical bootstrap procedure to estimate the variability of a measure of association estimated from clustered data
#### The data consist of multiple interviews, each providing a variable number of observations

B = 1000
test.stat <- numeric(B)
test.stat[1] <- fisher.test(atelic_resto$Pron_reflexivo, atelic_resto$Tiempo_verbal)$estimate  # odds ratio from the sample

####transform into factors
atelic_resto$Pron_reflexivo <- as.factor(atelic_resto$Pron_reflexivo)
atelic_resto$Tiempo_verbal <- as.factor(atelic_resto$Tiempo_verbal)

for (k in 2:B) {
  # resample interviews
  interview.boot <- sample(atelic_resto$COSERID,replace=TRUE)
  # resample indices of individual observations from each resampled interview
  ID.boot <- unlist(sapply(interview.boot, function(i) sample(which(atelic_resto$COSERID==i),replace=TRUE) ) )
  # Compute the statistic of interest from the data subsetted by the resampled indices
  test.stat[k] <- fisher.test(atelic_resto$Pron_reflexivo[ID.boot],atelic_resto$Tiempo_verbal[ID.boot])$estimate
}

#### Summary of results
hist(test.stat)
summary(test.stat)
quantile(test.stat,probs=c(0.025,0.975))

#### Estimate and confidence interval calculated from original data, ignoring the clustering (too narrow)
fisher.test(atelic_resto$Pron_reflexivo, atelic_resto$Tiempo_verbal)

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


