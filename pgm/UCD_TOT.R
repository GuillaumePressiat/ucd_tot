

library(stringfix)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

dictionnaire_rubriques <-
  tibble::tribble(
    ~'balise', ~'libelle_rubrique',
    "000", "Début de fichier",
    "001", "Données Table TB01 (Codification Taux)",
    "002", "Données Table TB02 (Taux / Nature de Prestation)",
    "003", "Données Table TB03 (Codification TVA)",
    "004", "Données Table TB04 (Marge Forfait)",
    "005", "Données Table TB05 (Marge %)",
    "006", "Données Table TB06 (Marge TTC)",
    "007", "Données Table TB07 (EMI)",
    "101", "Informations Générales pour un code UCD",
    "102", "Informations Techniques du code UCD",
    "110", "Historiques des Périodes de validité du code UCD",
    "120", "Historiques Tarifaires du code UCD",
    "130", "Historiques Taux du code UCD",
    "140", "Historiques Majorations DOM du code UCD",
    "150", "Historiques Coûts supplémentaires du code UCD",
    "160", "Historiques Top Établissement du code UCD",
    "170", "Historiques Quantités Max du code UCD",
    "180", "Historiques des PECP du code UCD",
    "201", "Informations Générales du CIP Lié à l'UCD",
    "210", "Historiques des périodes de lien",
    "299", "Fin de code CIP",
    "199", "Fin de code UCD",
    "999", "Fin de fichier")

dictionnaire_variables <- 
  tibble::tribble(
                       ~variable,                                                                                                                              ~libelle_variable,                                                                                                                         ~commentaire_modalites, ~rubrique,
                    "code_ucd13",                                                                                                                                     "Code UCD",                                                                                                                                             "",       101,
            "code_ucd_precedent",                                                                                                                           "Code UCD Précédent",                                                                                         "Ce champ permet de chaîner l\'historique du Code UCD",       101,
              "code_ucd_suivant",                                                                                                                             "Code UCD Suivant",                                                                                         "Ce champ permet de chaîner l\'historique du Code UCD",       101,
               "top_inscription",                                                  "Identifie les circuits de délivrance : double (ville et hôpital) ou uniquement à l\'Hôpital",  "Valeurs : 0 = Non Renseigné ; 1 = UCD délivrée sur circuits Assurés et Collectivités ; 2 = UCD délivrée sur circuit Collectivité uniquement",       101,
                   "laboratoire",                                                                                                      "Libellé court du Laboratoire exploitant",                                                                                                                                             "",       101,
               "date_source_doc",                                                                        "Date de parution de la dernière source documentaire relative à l\'UCD",                                                                                                                                             "",       101,
               "type_source_doc",                                                                         "Libellé du Type de la dernière source documentaire relative à l\'UCD",                                                                                                                                             "",       101,
                      "date_maj",                                                                                        "Date de la dernière Mise à Jour des données de l\'UCD",                                                                                                                                             "",       101,
                      "code_ucd",                                                                                                                    "Code UCD sur 7 caractères",                                                                                   "Ajout de notre part, non présent dans la norme d\'échanges",       101,
              "libelle_long_ucd",                                                                                                 "Libellé long de la désignation du médicament",                                                                                                                                             "",       101,
               "conditionnement",                                                                                                "Libellé long du Conditionnement du Médicament",                                                                                                                                             "",       101,
                   "observation",                                                                                                 "Libellé long de l\'observation du Médicament",                                                                                                                                             "",       101,
                          "etat",                                                                                                                                         "État",                                                                          "Valeurs : D = Commercialisé ; NC = Non Commercialisé ; S = Supprimé",       102,
                            "rh",                                                                                                   "Réserve hospitalière (CSP R.5121-82 à -83)",                                                                                                                  "Valeurs : O = Oui ; N = Non",       102,
                            "ph",                                                                                              "Prescription Hospitalière (CSP R.5121-84 à -86)",                                                                                                                  "Valeurs : O = Oui ; N = Non",       102,
                           "pih",                                                                                     "Prescription Initiale Hospitalière (CSP R.5121-87 à -89)",                                                                                                                  "Valeurs : O = Oui ; N = Non",       102,
                            "sp",                                                                                                "Surveillance Particulière(CSP R.5121-93 à-95)",                                                                                                                  "Valeurs : O = Oui ; N = Non",       102,
                            "ps",                                                                                        "Prescription par un Spécialiste (CSP R.5121-90 à -92)",                                                                                                                  "Valeurs : O = Oui ; N = Non",       102,
      "libelle_court_specialite",                                                                                                   "Libellé court de la spécialité du code UCD",                                                                                                                                             "",       102,
              "libelle_long_rcp",                                                                                                              "Libellé long du RCP du Code UCD",                                                                                                 "RCP : Résumé des caractéristiques du produit",       102,
                    "code_forme",                                                                                                      "Code interne de la Forme Pharmaceutique",                                                                                                                                             "",       102,
                 "libelle_forme",                                                                                                           "Libellé de la Forme Pharmaceutique",                                                                                                                                             "",       102,
         "code_forme_complement",                                                                                   "Code interne du Premier complément de Forme Pharmaceutique",                                                                                                                                             "",       102,
      "libelle_forme_complement",  "Concaténation des libellés des compléments de forme pharmaceutique dans la limite de 40 caractères. (Avec suppression des blancs inutiles).",                                                                                         "Qualitatif(s) complémentaires(s) précisant la Forme.",       102,
                        "dosage",                                                                                                       "Dosage de la première substance active",                                                                                                                                             "",       102,
                  "unite_dosage",                                                                                              "Unité de dosage de la première substance active",                                                                                                                                             "",       102,
           "voie_administration",                                                                                                                       "Voie d\'administration",                                                                                                                                             "",       102,
                      "code_atc",                                                                                                                        "Code de la classe ATC",                                                                                                                                             "",       102,
                   "libelle_atc",                                                                                                                     "Libellé de la classe ATC",                                                                                                                                             "",       102,
                   "code_ephmra",                                                                                                                     "Code de la classe EPHMRA",                                                                                                                                             "",       102,
                "libelle_ephmra",                                                                                                                  "Libellé de la classe EPHMRA",                                                                                                                                             "",       102
     )

extrait_ucd.b101010 <- function(df, lib){
  df %>% 
    mutate(l = l %s% '8.129') %>% 
    tidyr::separate(l, into = c('code_ucd', 'code_ucd_precedent', 'code_ucd_suivant',
                                'top_inscription', 'laboratoire', 'date_source_doc', 
                                'type_source_doc', 'date_maj'), 
                    sep = c(13, 26, 39, 
                            40, 70, 78, 
                            103, 111)) %>%
    mutate_all(stringr::str_trim) %>%
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%
    select(-starts_with('balise'), - titre) %>%
    rename(code_ucd13 = code_ucd) %>%
    mutate(code_ucd = code_ucd13 %s% '7.13')

}

# extrait_ucd.b101010(further['101010'][[1]])

extrait_ucd.b101020 <- function(df){
  df %>%
    mutate(libelle_long_ucd = l %s% '8.129' %>% stringr::str_trim()) %>%
    select(code_ucd, libelle_long_ucd)
}

# extrait_ucd.b101020(further['101020'][[1]])

extrait_ucd.b101030 <- function(df){
  df %>%
    mutate(conditionnement = l %s% '8.129' %>% stringr::str_trim()) %>% 
    arrange(code_ucd, balise_7) %>%
    select(code_ucd, conditionnement) %>%
    group_by(code_ucd) %>%
    summarise(conditionnement = conditionnement %c% '')
}

# extrait_ucd.b101030(further['101030'][[1]])

extrait_ucd.b101040 <- function(df){
  df %>% 
    mutate(observation = l %s% '8.129' %>% stringr::str_trim()) %>%
    arrange(code_ucd, balise_7) %>% 
    select(code_ucd, observation) %>%
    group_by(code_ucd) %>%
    summarise(observation = observation %c% '')
}

# extrait_ucd.b101040(further['101040'][[1]])

extrait_ucd.b102010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.129') %>% 
    tidyr::separate(l, 
                    into = c('etat', 'rh', 'ph', 'pih', 'sp', 'ps', 'libelle_court_specialite'),
                    sep = c(2, 3, 4, 5, 6, 7, 43)) %>%
    mutate_all(stringr::str_trim) %>% 
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b102010(further['102010'][[1]])

extrait_ucd.b102020 <- function(df){
  df %>% 
    mutate(libelle_long_rcp = l %s% '8.108' %>% stringr::str_trim()) %>%
    arrange(code_ucd, balise_7) %>% 
    select(code_ucd, libelle_long_rcp) %>%
    group_by(code_ucd) %>%
    summarise(libelle_long_rcp = libelle_long_rcp %c% '')
  
}

# extrait_ucd.b102020(further['102020'][[1]])

extrait_ucd.b102030 <- function(df){
  df %>% 
  mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('code_forme', 'libelle_forme', 
                             'code_forme_complement', 'libelle_forme_complement'),
                    sep = c(10, 50, 60, 100)) %>%
    mutate_all(stringr::str_trim) %>% 
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b102030(further['102030'][[1]])

extrait_ucd.b102040 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('dosage', 'unite_dosage', 'voie_administration'),
                    sep = c(17, 34, 124)) %>%
    mutate_all(stringr::str_trim) %>% 
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b102040(further['102040'][[1]])

extrait_ucd.b102050 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('code_atc', 'libelle_atc'),
                    sep = c(10, 87)) %>%
    mutate_all(stringr::str_trim) %>% 
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b102050(further['102050'][[1]])

extrait_ucd.b102060 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('code_ephmra', 'libelle_ephmra'),
                    sep = c(10, 87)) %>%
    mutate_all(stringr::str_trim) %>% 
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b102060(further['102060'][[1]])

extrait_ucd.b110010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('date_debut', 'date_fin', 'top_liste', 
                             'date_source_doc', 'type_source_doc', 'motif_fin'),
                    sep = c(8, 16, 17, 25, 50, 60)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%  
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b110010(further['110010'][[1]]) %>% View
# df <- further['120010'][[1]]
extrait_ucd.b120010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('date_application', 'top_liste', 'prix_fht', 'prix_ttc', 
                             'top_sur_facture', 'type_marge', 'code_marge', 'marge_ttc',
                             'date_source_doc', 'type_source_doc'),
                    sep = c(8, 9, 17, 24, 25, 26, 27, 34, 42, 67)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%  
    mutate_at(vars(ends_with('fht')), function(x){as.integer(x) / 1000}) %>%  
    mutate_at(vars(ends_with('ttc')), function(x){as.integer(x) / 100}) %>%  
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b120010(further['120010'][[1]]) %>% View

# df <- further['130010'][[1]]
extrait_ucd.b130010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('date_application', 'top_liste', 'code_taux', 'taux', 
                             'date_source_doc', 'type_source_doc'),
                    sep = c(8, 9, 10, 13, 21, 46)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%  
    mutate(taux = as.integer(taux)) %>% 
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b130010(further['130010'][[1]]) %>% View

# df <- further['140010'][[1]]
extrait_ucd.b140010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('date_application', 'top_liste', 'code_ctom', 'majo_ctom', 
                             'date_source_doc', 'type_source_doc'),
                    sep = c(8, 9, 12, 16, 24, 49)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%  
    mutate(majo_ctom = as.integer(majo_ctom) / 1000) %>% 
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b140010(further['140010'][[1]]) %>% View

# df <- further['150010'][[1]]
extrait_ucd.b150010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('date_debut', 'top_liste', 'date_fin',
                             'date_source_doc', 'type_source_doc'),
                    sep = c(8, 9, 17, 25, 50)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%  
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b150010(further['150010'][[1]]) %>% View

# df <- further['160010'][[1]]
extrait_ucd.b160010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('date_debut', 'top_liste', 'code_etablissement', 'date_fin',
                             'date_source_doc', 'type_source_doc'),
                    sep = c(8, 9, 10, 18, 26, 49)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%  
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b160010(further['160010'][[1]]) %>% View


# df <- further['170010'][[1]]
extrait_ucd.b170010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('date_debut', 'top_liste', 'type_alerte',
                             'quantite_max', 'date_fin',
                             'date_source_doc', 'type_source_doc'),
                    sep = c(8, 9, 12, 18, 26, 49)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate(quantite_max = as.integer(quantite_max)) %>% 
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%  
    select(- starts_with('balise'), -titre)
}

# extrait_ucd.b170010(further['170010'][[1]]) %>% View

# df <- further['180010'][[1]]
extrait_ucd.b180010 <- function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    tidyr::separate(l, 
                    into = c('date_debut', 'top_liste', 'pecp',
                             'date_fin', 
                             'date_source_doc', 'type_source_doc'),
                    sep = c(8, 9, 11, 19, 27, 52)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate_at(vars(starts_with('date')), lubridate::ymd) %>%  
    select(- starts_with('balise'), -titre)
}

extrait_ucd.b000 <-  function(df){
  df %>% 
    mutate(l = l %s% '4.128') %>%
    tidyr::separate(l,
                    into = c('type_emetteur', 'numero_emmetteur', 
                             'programme_emmetteur','date_fin', 
                             'type_destinataire', 'numero_destinataire',
                             'programme_destinataire', 'reste_ligne'),
                    sep = c(2, 16, 22, 24, 38, 44, 46, 49)) %>%
    mutate_all(stringr::str_trim)
}
                    
# extrait_ucd.b000(ucd_tot %>% filter(l %s% .3 == '000'))

# df <- ucd_tot %>% filter(l %s% .3 == '001')
extrait_ucd.b001 <-  function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    mutate(l = stringr::str_extract_all(l, '.{20}')) %>% 
    pull(l) %>% 
    unlist() %>%
    .[1:5] %>%
    tibble(l = .) %>%
    tidyr::separate(l,
                    into = c('code_taux', 'date_debut', 
                             'date_fin','taux'),
                    sep = c(1, 9, 17, 20)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate(taux = as.integer(taux))
}

# df <- ucd_tot %>% filter(l %s% .3 == '002')

extrait_ucd.b002 <-  function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    mutate(l = stringr::str_extract_all(l, '.{20}')) %>% 
    pull(l) %>% 
    unlist() %>%
    .[1:5] %>%
    tibble(l = .) %>%
    tidyr::separate(l,
                    into = c('code_taux', 'date_debut', 
                             'date_fin','nature_prestation'),
                    sep = c(1, 9, 17, 20)) %>%
    mutate_all(stringr::str_trim)
}

# df <- ucd_tot %>% filter(l %s% .3 == '003')

extrait_ucd.b003 <-  function(df){
  df %>% 
    mutate(l = l %s% '8.128') %>%
    mutate(l = stringr::str_extract_all(l, '.{21}')) %>% 
    pull(l) %>% 
    unlist() %>%
    .[1:5] %>%
    tibble(l = .) %>%
    tidyr::separate(l,
                    into = c('code_tva', 'date_debut', 
                             'date_fin','coefficient_tva'),
                    sep = c(1, 9, 17, 21)) %>%
    mutate_all(stringr::str_trim) %>% 
    mutate(coefficient_tva = as.integer(coefficient_tva) / 1000)
}

# extrait_ucd.b000(ucd_tot %>% filter(l %s% .3 == '000'))

extrait_ucd <- function(df){
  UseMethod('extrait_ucd')
}

map_balise <- function(ucd_tot, bal){
  temp <- ucd_tot %>% 
    filter(balise_6 == bal)
  f <- get('extrait_ucd.b' %+% bal)
  f(temp)
}

