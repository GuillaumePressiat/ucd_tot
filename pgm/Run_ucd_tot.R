
version = '00491'

#setwd('P:/Commun/PG/pgm/ucd_tot')
source('pgm/UCD_TOT.R', encoding = 'utf-8')

ucd_tot_ <- readr::read_lines('sources/UCD_TOT_' %+% version)

ucd_tot <- dplyr::tibble(l = ucd_tot_) %>%
  dplyr::mutate(balise_7 = l %s% .7) %>%
  dplyr::mutate(balise_6 = l %s% .6) %>%
  dplyr::mutate(titre = case_when(balise_7 == '1010101' ~ '1010101',
                                  TRUE ~ '')) %>%
  dplyr::mutate(code_ucd = case_when(balise_7 == '1010101' ~ l %s% '14.20')) %>%
  tidyr::fill(code_ucd)


further <- ucd_tot %>%
  filter(!is.na(code_ucd), balise_6 %s% .3 != '999') %>%
  group_by(balise_6) %>% 
  group_split()

names(further) <- further %>% 
  purrr::map_chr(function(x){x[1]$l[1] %s% .6})

rubriques_niveau_ucd <- names(further) %>% 
  .[. %s% '.3' %in% c('101', '102')]

ucd_tot_tibble <- rubriques_niveau_ucd %>% 
  purrr::map(function(x)map_balise(ucd_tot, x)) %>% 
  purrr::reduce(left_join, by = "code_ucd")

ucd_tot_table <- ucd_tot_tibble %>% 
  sjlabelled::set_label(dictionnaire_variables$libelle_variable) %>% 
  select(code_ucd, everything())


ucd_histo_prix <- map_balise(ucd_tot, '120010')

ucd_histo_validite <- map_balise(ucd_tot, '110010')

ucd_tot_table <- ucd_tot_table %>% 
  select(-code_ucd13) %>% 
  mutate(code_ucd_precedent = ifelse(code_ucd_precedent == "0000000000000", "", code_ucd_precedent),
         code_ucd_suivant = ifelse(code_ucd_suivant == "0000000000000", "", code_ucd_suivant))

readr::write_tsv(ucd_tot_table, 'results/ucd_tot_table_' %+% version %+% '.tsv')

readr::write_tsv(dictionnaire_variables,   'dictionnaires/dictionnaire_variables.tsv')

readr::write_tsv(ucd_histo_prix, 'results/ucd_tot_histo_prix_' %+% version %+% '.tsv')

# xlsx::write.xlsx(ucd_tot_table, 'results/ucd_tot_table.xlsx', sheetName = 'ucd_tot')
# xlsx::write.xlsx(dictionnaire_variables, 'ucd_tot_table.xlsx', sheetName = 'dictionnaire_variables', append = TRUE)
# 
# xlsx::write.xlsx(ucd_histo_prix, 'ucd_histo_prix.xlsx', sheetName = 'ucd_histo_prix', append = FALSE)
# xlsx::write.xlsx(ucd_histo_validite, 'ucd_histo_prix.xlsx', sheetName = 'ucd_histo_validite', append = TRUE)
