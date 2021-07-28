#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 28th 2021
#Chapter 6
#Section 6.7. Verbs of cognition

#load libraries
library(tidyverse)

#load and clean table
sintactica <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Animado", "Humano"), "Animate", "Inanimate"))

#create table with exhaustively collected transitive verbs
##in subcorpus E
transitive_exh_E <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo %in% c("saber", "llevar", "comer", "ver", "dejar", "tomar", "estudiar","mirar", "ganar", 
                      "aprender", "pensar", "entender", "leer", "encontrar"), 
         Muestreo == "Exhaustivo")

##in corpus NE
transitive_exh_NE <- sintactica %>%
  filter(Tipo_sintactico %in% c("Transitivo", "Transitivo_tiempo", "Transitivo_path"), 
         Verbo %in% c("traer", "creer", "pasar",  "beber", "subir",  "recordar", "bajar", "imaginar", "dar_cuenta"))

##join them
transitive_exh <- rbind(transitive_exh_E, transitive_exh_NE)

#Check all cognition verbs that were attested with RM
##create table
cognition_RM <- sintactica %>%
  filter(Tipo_sintactico == "Transitivo", 
         Tipo_semantico == "Cognicion", 
         Pron_reflexivo == "Si")

##how many examples?
cognition_RM %>% 
  nrow()

##how many verbs?
cognition_RM %>% 
  distinct(Verbo) %>% 
  nrow()
##which verbs and how often?
cognition_RM %>% 
  count(Verbo, sort = T) %>% 
  print(n = Inf)

#recordar
transitive_exh %>%
  filter(Verbo == "recordar") %>%
  count(Pron_reflexivo) %>%
  mutate(Total = sum(n), percentage = round(n/Total*100, 0))

#Plot RM probability of frequent cognition verbs by verb and area
##create table
cognition_freq_verbs <- transitive_exh %>% 
  mutate(Aspectualidades = ifelse(is.na(Aspectualidades) & Verbo == "aprender", "Interrupted", Aspectualidades)) %>% 
  filter(!Aspectualidades %in% c("Absoluto", "Ambiguo", "Creg", "Interrupted")) %>% 
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest"), 
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>% 
  filter(Verbo %in% c("saber", "aprender", "creer", "imaginar", "estudiar", "dar_cuenta")) %>%
  mutate(Verbo = ifelse(Verbo == "dar_cuenta", "dar cuenta", Verbo)) %>%
  group_by(Area_dialecta, Verbo) %>%
  count(Verbo, Pron_reflexivo) %>%
  mutate(total=sum(n), prop=(n/total*100)) 

##relevel Verbo & Area_dialecta
cognition_freq_verbs$Verbo <- factor(cognition_freq_verbs$Verbo, levels = c("dar cuenta", "imaginar", "creer", "estudiar", "aprender", "saber"))
cognition_freq_verbs$Area_dialecta <- factor(cognition_freq_verbs$Area_dialecta, levels = c("Rest of the territory", "Northwest"))

##create plot
ggplot(cognition_freq_verbs, aes(x=Verbo,y=prop, group=Pron_reflexivo)) + 
  geom_col(aes(fill=Pron_reflexivo), position = "fill") + 
  labs(title="Frequency of the RM in selected transitive cognition verbs \nby geographical area", x="Verb", y="Frequency of the RM", fill="Reflexive Marker") +
  geom_text(aes(label = n), position = position_fill(vjust = .5)) + 
  theme_classic() +
  scale_fill_manual(values=c('darkgrey','lightgray')) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~ Area_dialecta, scales = "free_x")

##save plot
ggsave("cognition_verbs.png", height = 4, width = 6) #saves the last plot

#####Section 6.7.1
##saber: different meanings
sintactica %>%
  filter(Verbo == "saber", 
         Pron_reflexivo == "Si", 
         Area_dialecta == "Resto") %>%
  count(Sdo_tipic_refl, Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))

##aprender: different meanings
sintactica %>%
  filter(Verbo == "aprender", 
         Pron_reflexivo == "Si", 
         Area_dialecta == "Resto") %>%
  count(Sdo_tipic_refl, Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))

##estudiar: different meanings
sintactica %>%
  filter(Verbo == "estudiar", 
         Pron_reflexivo == "Si", 
         Area_dialecta == "Resto") %>%
  count(Sdo_tipic_refl, Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))

##estudiar & aprender (different meanings) in subcorpus E & rest of the territory in typically reflexive contexts
transitive_exh %>% 
  mutate(Aspectualidades = ifelse(is.na(Aspectualidades) & Verbo == "aprender", "Interrupted", Aspectualidades)) %>% 
  filter(!Aspectualidades %in% c("Absoluto", "Ambiguo", "Creg", "Interrupted")) %>% 
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest"), 
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>% 
  filter(Verbo %in% c("aprender", "estudiar")) %>%  
  filter(Area_dialecta == "Rest of the territory", 
         Sdo_tipic_refl == "S") %>% 
  group_by(Verbo) %>% 
  count(Sdo_tipic_refl, Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))

#####Section 6.7.2
##creer: RM probability
transitive_exh %>% 
  filter(!Aspectualidades %in% c("Absoluto", "Ambiguo", "Creg", "Interrupted")) %>% 
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest"), 
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>% 
  filter(Verbo %in% c("creer")) %>%  
  filter(Area_dialecta == "Rest of the territory") %>% 
  count(Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))

##creer: RM probability with "que"
transitive_exh %>% 
  filter(!Aspectualidades %in% c("Absoluto", "Ambiguo", "Creg", "Interrupted")) %>% 
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest"), 
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>% 
  filter(Verbo %in% c("creer")) %>%  
  filter(Area_dialecta == "Rest of the territory") %>% 
  group_by(Aspectualidades) %>% 
  count(Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))

##imaginar: RM probability
transitive_exh %>% 
  filter(!Aspectualidades %in% c("Absoluto", "Ambiguo", "Creg", "Interrupted")) %>% 
  mutate(Area_dialecta = ifelse(Area_dialecta == "Resto", "Rest of the territory", "Northwest"), 
         Pron_reflexivo = ifelse(Pron_reflexivo == "Si", "RM", "No RM")) %>% 
  filter(Verbo == "imaginar") %>%  
  filter(Area_dialecta == "Rest of the territory") %>% 
  count(Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))

##suponer: RM probability
sintactica %>%  
  filter(Verbo == "suponer") %>%  
  count(Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))

#####Section 6.7.3
##Check other verbs out
sintactica %>%
  filter(Tipo_sintactico == "Transitivo", 
         Tipo_semantico == "Cognicion", 
         str_detect(Verbo, "\\b(dar|echar|hacer)"),
         Verbo != "dar_cuenta") %>% 
  View()
#dar cuenta
transitive_exh %>% 
  filter(Verbo == "dar_cuenta") %>% 
  group_by(Area_dialecta) %>% 
  count(Pron_reflexivo) %>% 
  mutate(total = sum(n),
         percentage = round(n/total*100, 0))
