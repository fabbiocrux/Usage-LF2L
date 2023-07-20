# Filtering several Cours
Usage <- list()

Usage$activity  <- 
   LF2L.Calendar %>% filter(Categorie !="Autre" ) %>% 
   mutate(Usage = case_when(
      
      # INGEXYS / AIU
      str_detect(Summary, "\\[reu\\]") ~ "[Reu] Reunion",
      str_detect(Summary, "\\[vis\\]") ~ "[VIS] Visite",
      str_detect(Summary, "\\[for\\]") ~ "[FOR] Formation",
      str_detect(Summary, "\\[formu\\]") ~ "[FORMU] Formation Mutualisé",
      str_detect(Summary, "\\[pro\\]") ~ "[PRO] Prototypage",
      str_detect(Summary, "\\[exp\\]") ~ "[EXP] Expérimentation",
      str_detect(Summary, "\\[cre\\]") ~ "[EXP] Créativité",
      str_detect(Summary, "\\[sem\\]") ~ "[EXP] Séminaire",
      str_detect(Summary, "\\[aut\\]") ~ "[AUT] Autre",
      TRUE ~ 'Autre')
   ) %>% 
   ggplot( aes(x=Scholar.year, y=Hours, fill=Usage)) +
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
   ) +
   scale_fill_brewer(palette="Paired")

