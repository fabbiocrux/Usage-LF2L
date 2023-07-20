# Sujet : [catégorie usager] [type usager] [usage] Titre descriptif
## Par Categorie

LF2L.Calendar <-  
   LF2L.Calendar %>% mutate(Categorie = c(""))

## Mapping usaga par Categorie
LF2L.Calendar <- 
   LF2L.Calendar %>% 
   mutate(Categorie = case_when(
      str_detect(Summary, "\\[int\\]") ~ "Usage Interne",
      str_detect(Summary, "int") ~ "Usage Interne",
      str_detect(Summary, "\\[ext\\]") ~ "Usage Externe",
      str_detect(Summary, "ext") ~ "Usage Externe",
      TRUE ~ "Autre")
   ) 


# Categorie Usager total
Cat.usager <- list()
Cat.usager$total <- 
   LF2L.Calendar %>% 
   group_by(Categorie, Scholar.year) %>% 
   summarise(Events= n(), Heures=sum(Hours)) %>% 
   ggplot( aes(x=Scholar.year, y=Heures, fill=Categorie)) +
   geom_bar(stat = 'Identity' , position = position_dodge(width = 0.9)) +
   theme_fabio() +
   labs(title="Type d'usage du LF2L - Période 2019-2024",
        subtitle="Utilisant les TOUS les entrées de Zimbra",
        x = "Année scolaire",
        y = "Heures",
        caption= "Dernière mise à jour 19/07/2023")
   

# Categorie Usager Interne
## filtering ENSGSI ou ERPI

Cat.usager$interne  <- 
   LF2L.Calendar %>% filter(Categorie =="Usage Interne" ) %>% 
   mutate(Hours = as.numeric(Hours)) %>% 
   mutate(User = case_when(
      str_detect(Summary, "\\[ensgsi\\]") ~ "ENSGSI",
      str_detect(Summary, "ensgsi") ~ "ENSGSI",
      str_detect(Summary, "\\[erpi\\]") ~ "ERPI",
      str_detect(Summary, "erpi") ~ "ERPI",
      TRUE ~ Categorie)
   ) %>% 
   ggplot( aes(x=Scholar.year, y=Hours, fill=User)) +
   geom_bar(stat = 'Identity' ) +
   theme_fabio() +
   labs(title="Usage Interne du LF2L - Période 2019-2024",
        subtitle="Utilisant les entrées [int] de Zimbra",
        x = "Année scolaire",
        y = "Heures",
        caption =  paste0("Denière mise à jour: ", format(Sys.time(), '%d/%m/%Y') )
        ) 
#ggsave("Figures/2023/Usage-LF2L.jpg", height = 5, width = 12 )

# Categorie Usager Externe
## filtering ACA ou IND

Cat.usager$externe  <- 
   LF2L.Calendar %>% filter(Categorie =="Usage Externe" ) %>% 
   mutate(Hours = as.numeric(Hours)) %>% 
   mutate(User = case_when(
      str_detect(Summary, "\\[aca\\]") ~ "Académique",
      str_detect(Summary, "\\[ind\\]") ~ "Industriel",
      str_detect(Summary, "\\[asso\\]") ~ "Associatif",
      TRUE ~ 'Autre Usage Externe')
   ) %>% 
   ggplot( aes(x=Scholar.year, y=Hours, fill=User)) +
   geom_bar(stat = 'Identity' ) +
   theme_fabio() +
   labs(title="Usage Externe du LF2L - Période 2019-2024",
        subtitle="Utilisant les entrées [ext] de Zimbra",
        x = "Année scolaire",
        y = "Heures",
        caption =  paste0("Denière mise à jour: ", format(Sys.time(), '%d/%m/%Y') )
   ) 
#ggsave("Figures/2023/Usage-LF2L.jpg", height = 5, width = 12 )