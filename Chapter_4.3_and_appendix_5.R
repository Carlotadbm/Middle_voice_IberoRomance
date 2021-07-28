#Carlota de Benito Moreno
#The middle voice in Ibero-Romance
#July 26th 2021
#Chapter 4
#Section 4.3
#And Appendix 5


#load library
library(tidyverse)

#load table
anticausative_ETT <- read_delim("verbosETT_tesis.csv", delim="\t")

#Calculate correlation
##clean table: remove no valid cases
anticausative_ETT <- anticausative_ETT %>%
  filter(!Valencia %in% "Nada") %>% #no valid valency change
  filter(!is.na(Valencia)) %>% #no valid valency change
  filter(!Verbo %in% c("descansar", "pesar", "enseñar")) #they only have unmarked cases

##check number of verbs
anticausative_ETT %>% 
  distinct(Verbo) %>% 
  nrow()

##clean table: group syntactic contexts on Transitive / Intransitive
anticausative_ETT <- mutate(anticausative_ETT, 
                  Transitividad = ifelse(Valencia %in% c("Pasiva se", "Causativo", "Reflexivo", "Absoluto"), "Transitivo",
                                               ifelse(Valencia %in% c("Se_impersonal", "Se – impersonal", "Régimen", "Se"), "Intransitivo",
                                                      ifelse(Valencia %in% c("Ambigua", "Auxiliar"), "Nada", Valencia)))) %>% 
  filter(Transitividad != "Nada")


##create table with transitivity probabilites
anticausative_ETT_TrIntr <- anticausative_ETT %>%
  group_by(Verbo) %>%
  count(Transitividad) %>%
  spread(Transitividad, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  rename(Transitive = Transitivo, Intransitive = Intransitivo) %>%
  mutate(Total = Transitive + Intransitive, Prob_trans = Transitive/Total, Type = "Transitive") %>%
  ungroup() 

##create table transitivity (to join later)
anticausative_ETT_TrIntr2 <- anticausative_ETT_TrIntr %>%
  select(Verbo, Total, Prob_trans, Type)

##create table with RM probabilities
anticausative_ETT_RM <- anticausative_ETT %>%
  filter(Valencia %in% c("Intransitivo", "Se")) %>%
  group_by(Verbo) %>%
  count(Valencia) %>%
  spread(Valencia, n) %>%
  mutate_if(is.numeric, ~(ifelse(is.na(.), 0, .))) %>%
  rename(RM = Se, No_RM = Intransitivo) %>%
  mutate(Total = RM + No_RM, Prob_RM = RM/Total, Type = "RM") %>%
  ungroup() %>%
  arrange(desc(Prob_RM))

##create table RM (to join later)
anticausative_ETT_RM2 <- anticausative_ETT_RM %>%
  select(Verbo, Total, Prob_RM, Type)

##join tables
anticausative_ETT_cor <- full_join(anticausative_ETT_TrIntr2, anticausative_ETT_RM2, by = "Verbo")

##plot distribution
ggplot(anticausative_ETT_cor, aes(Prob_trans, Prob_RM)) + 
  geom_point() + 
  labs(title="Correlation between presence of the RM and transitive uses by verb", x="Probability of transitive uses", y="Probability of the RM")

##save plot
ggsave("anticausative_ETT_cor.png", width = 7, height = 7) #saves the last plot


##Generate data for Appendix 5
##table with RM probability
anticausative_ETT_RM3 <- anticausative_ETT_RM %>%
  select(Verbo, No_RM, RM, Total, Prob_RM) %>%
  rename(Total_RM = Total)

##table with transitivity probability
anticausative_ETT_TrIntr3 <- anticausative_ETT_TrIntr %>%
  select(Verbo, Transitive, Intransitive, Total, Prob_trans) %>%
  rename(Total_TrIntr = Total)

##join tables
anticausative_ETT_table <- full_join(anticausative_ETT_TrIntr3, anticausative_ETT_RM3, by = "Verbo")

##write table
write_csv(anticausative_ETT_table, "anticausative_ETT_table.csv")
