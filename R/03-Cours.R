# Filtering several Cours
Courses <- list()

Courses$activity  <- 
   LF2L.Calendar %>% filter(Categorie =="Usage Interne" ) %>% 
   mutate(Hours = as.numeric(Hours)) %>% 
   mutate(Pedagogical.Activity = case_when(
      
      # INGEXYS / AIU
      str_detect(Summary, "\\[aiu\\]") ~ "Ateliers d'Innovation Urbaine (AIU, Ingexys)",
      str_detect(Summary, "aiu") ~ "Ateliers d'Innovation Urbaine (AIU, Ingexys)",
      str_detect(Summary, "ingexys p2") ~ "Ateliers d'Innovation Urbaine (AIU, Ingexys)",
      str_detect(Summary, "ingexys pétale 2") ~ "Usage Industriel",

      # INGEXYS / AIU
      str_detect(Summary, "\\[iuvtt\\]") ~ "Master IUVTT",
      str_detect(Summary, "iuvtt") ~ "Master IUVTT",
      
      # NYBI
      str_detect(Summary, "nybi") ~ "NYBI.CC",
      str_detect(Description, "nybi") ~ "NYBI.CC",
      
      # ENSGSI
      str_detect(Summary, "ci3") ~ "CI3",
      str_detect(Description, "ci3") ~ "CI3",
      str_detect(Summary, "ci6") ~ "CI6",
      str_detect(Description, "ci6") ~ "CI6",
      str_detect(Summary, "ci-6") ~ "CI6",      
      str_detect(Summary, "bachelor") ~ "Bachelor TIM",      
      
      # TELECOM
      str_detect(Summary, "big data") ~ "TELECOM",
      str_detect(Description, "mom1") ~ "TELECOM",
      str_detect(Summary, "mom2") ~ "TELECOM",
      str_detect(Description, "iamd") ~ "TELECOM",
      str_detect(Summary, "ci-6") ~ "TELECOM",      
      
      TRUE ~ 'Autre Usage Interne')
   ) %>% 
   ggplot( aes(x=Scholar.year, y=Hours, fill=Pedagogical.Activity)) +
   geom_bar(stat = 'Identity' ) +
   theme_fabio() +
   theme(legend.position="bottom") +
   #coord_flip() +
   #coord_cartesian(ylim = c(0,1000)) +
   labs(title="Usage Pedagogique du LF2L - Période 2019-2024",
        subtitle="Utilisant les entrées [int] de Zimbra",
        x = "Année scolaire",
        y = "Heures",
        caption =  paste0("Denière mise à jour: ", format(Sys.time(), '%d/%m/%Y') )
   ) 


#ggsave("Figures/2023/Usage-LF2L.jpg", height = 5, width = 12 )




