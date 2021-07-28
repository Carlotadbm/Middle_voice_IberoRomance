#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 28th 2021
#Chapter 6
#Section 6.8. Corporal process

#load library
library(tidyverse)

#load and clean table
corporal <- read_delim("VM_fusion_blanco_revisado_3.csv", delim="\t") %>%
  mutate(Animacion_sujeto = ifelse(Animacion_sujeto %in% c("Animal", "Animado", "Humano"), "Animate", "Inanimate")) %>%
  filter(Tipo_sintactico == "Transitivo", 
         Tipo_semantico == "Proceso_corporal",
         Verbo == "hacer")

##frequency of RM by region
corporal %>%
  group_by(Area_dialecta) %>% 
  count(Pron_reflexivo) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 0))

##frequency of RM by meaning
corporal %>%
  group_by(Sdo_tipic_refl) %>% 
  count(Pron_reflexivo) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 0))
