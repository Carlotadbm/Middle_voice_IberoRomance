#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 27th 2021
#Chapter 6
#Section 6.5. Consumption verbs

#load library
library(tidyverse)

#load table
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") 

#clean table
sintactica <- sintactica %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Animado", "Humano"), "Animate", "Inanimate"))

#create table with transitive consumption verbs
##verbs in subcorpus E
transitive_exh_E <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo %in% c("comer", "tomar"), 
         Muestreo == "Exhaustivo")

##verbs in corpus NE
transitive_exh_NE <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo == "beber")

##join tables
transitive_exh <- rbind(transitive_exh_E, transitive_exh_NE)

##clean table
consumption <- transitive_exh %>%
  filter(!is.na(Aspectualidades), 
         !Aspectualidades %in% c("Absoluto", "Interrumpido", "coger")) %>%
  mutate(Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>% 
  mutate(Aspectualidades = ifelse(Aspectualidades %in% c("Pronombre", "SD", "nada", "que", "algo"), "Delimited", 
                                  ifelse(Aspectualidades %in% c("Desnudo", "Partitivo", "Cuantificador", "mucho", "poco", "menos", "mas"), "Non-delimited", 
                                         ifelse(Aspectualidades == "Nulo", "Null", Aspectualidades)))) %>%
  mutate(Area_dialecta = ifelse(Area_dialecta == "Noroeste", "Northwest", "Rest of the territory"))


#Plot frequency of the RM by verb and area
##create table
consumption_verbs <- consumption %>%
  group_by(Area_dialecta, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

##relevel Verbo
consumption_verbs$Verbo <- factor(consumption_verbs$Verbo, levels = c("tomar", "comer", "beber"))

##create plot
ggplot(consumption_verbs, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in selected transitive verbs of consumption \nby geographical area", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialecta, scales = "free_x")

##save plot
ggsave("consumption_verbs.png", height = 4, width = 6) #saves the last plot

#delimitation of the object
##create table
consumption_DO <- consumption %>% 
  filter(Area_dialecta == "Rest of the territory") %>%
  group_by(Aspectualidades, Verbo) %>%
  count(Pron_reflexivo) %>%
  mutate(Total=sum(n), prop = n/sum(n))

##relevel Verbo
consumption_DO$Verbo <- factor(consumption_DO$Verbo, levels = c("tomar", "comer", "beber"))

##create plot
ggplot(consumption_DO, aes(x=Aspectualidades,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in selected transitive verbs of consumption \nby delimitation of the object (in the rest of the territory)", x="Delimitation of the object", 
       y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Verbo, scales = "free_x")

##save plot
ggsave("consumption_DO.png", height = 5, width = 9) #saves the last plot

#Animacy of the subject (in all the territory)
consumption %>%
  count(Animacion_sujeto) %>%
  mutate(prop = round(n/sum(n)*100, 1))

#imperatives  
consumption %>%
  filter(Area_dialecta == "Rest of the territory") %>%
  mutate(Tiempo_verbal = ifelse(Tiempo_verbal %in% c("imperativo", "imperativo_(subjuntivo)"), "imperative", "other")) %>%
  count(Tiempo_verbal) %>%
  mutate(prop = round(n/sum(n)*100, 1))

#leer
sintactica %>%
  filter(Area_dialecta == "Resto", 
         Verbo == "leer") %>%
  count(Pron_reflexivo) %>%
  mutate(Total = sum(n), prop = round(n/sum(n)*100, 0))

#Mixed model
consumption1 <- consumption %>% 
  filter(Area_dialecta != "Northwest") %>% 
  filter(Aspectualidades != "Null")
  
##Transform columns into factor (first level is reference level)
consumption1$Pron_reflexivo <- factor(consumption1$Pron_reflexivo, levels = c("No RM", "RM"))
consumption1$Aspectualidades <- factor(consumption1$Aspectualidades, levels = c("Non-delimited", "Delimited"))
consumption1$Verbo <- factor(consumption1$Verbo)
consumption1$COSERID <- factor(consumption1$COSERID)

##calculate model
consumption_model <- glmer(Pron_reflexivo ~ Aspectualidades + (1|Verbo) + (1|COSERID), 
                     family = "binomial", data = consumption1)

##calculate model summary statistics
summary(consumption_model)
range(resid(consumption_model))
hist(resid(consumption_model)) #normal distribution?

##tidy model
consumption_model_tidy <- tidy(consumption_model, exponentiate = F, conf.int = T) #statistic es el z-value

##write model
write_delim(consumption_model_tidy, "6.5_consumption_model_tidy.csv", delim = "\t")
