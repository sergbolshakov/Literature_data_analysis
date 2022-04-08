library(magrittr)

# Data Preparation =============================================================

# Import raw data --------------------------------------------------------------

db_all_data <- readr::read_tsv("data/Data.tsv", guess_max = 200000)

db_all_references <- readr::read_tsv("data/References.tsv", guess_max = 2000)

geography <- readr::read_tsv("data/Geography.tsv", guess_max = 100)

# Filter data ------------------------------------------------------------------

data <-
  db_all_data %>%
  dplyr::filter(
    !group %in% c("agaricoid",
                  "boletoid",
                  "gasteroid"),
    sector == "Europe",
    is.na(higherGeography) | higherGeography != "Northern Caucasus",
    !stateProvince %in% c("Crimea",
                          "Sevastopol",
                          "Chelyabinsk Oblast",
                          "Sverdlovsk Oblast",
                          "Yamalo-Nenets Autonomous Okrug",
                          "ZZ Urals"),
    !grepl("Weinmann", bibliographicCitation),
    !grepl("Fries", bibliographicCitation),
  ) %>%
  dplyr::left_join(db_all_references) %>% 
  dplyr::filter(!taxonomicStatus %in% c("doubtful",
                                        "ambiguous name",
                                        "absent name",
                                        "#N/A"))

# Custom grid of European Russia regions ---------------------------------------

european_russia <- data.frame(
  row = c(1, 1, 
          2, 2, 2, 
          3, 3, 3, 3, 3, 3, 
          4, 4, 4, 4, 4, 4, 4, 4, 
          5, 5, 5, 5, 5, 5, 5, 5, 
          6, 6, 6, 6, 6, 6, 6, 
          7, 7, 7, 7, 7, 7, 
          8, 8, 8, 8, 
          9, 9, 9),
  col = c(4, 10, 
          4, 5, 10, 
          3, 4, 5, 6, 8, 10, 
          1, 2, 3, 4, 5, 6, 8, 9, 
          3, 4, 5, 6, 7, 8, 9, 10, 
          4, 5, 6, 7, 8, 9, 10, 
          3, 4, 5, 6, 7, 8, 
          4, 5, 6, 7, 
          6, 7, 8),
  code = c("MUR", "NEN", 
           "KR", "ARK", "KO", 
           "SPE", "LEN", "VLG", "KOS", "KIR", "PER", 
           "KGD", "PSK", "NGR", "TVE", "YAR", "IVA", "ME", "UD", 
           "SMO", "MOS", "MOW", "VLA", "NIZ", "CU", "TA", "BA", 
           "KLU", "TUL", "RYA", "MO", "ULY", "SAM", "ORE", 
           "BRY", "ORL", "LIP", "TAM", "PNZ", "SAR", 
           "KRS", "BEL", "VOR", "VGG", 
           "ROS", "KL", "AST"),
  name = c("Murmansk Oblast", "Nenets Autonomous Okrug", 
           "Karelia", "Arkhangelsk Oblast", "Komi Republic", 
           "Saint Petersburg", "Leningrad Oblast", "Vologda Oblast", 
           "Kostroma Oblast", "Kirov Oblast", "Perm Krai", 
           "Kaliningrad Oblast", "Pskov Oblast", "Novgorod Oblast", 
           "Tver Oblast", "Yaroslavl Oblast", "Ivanovo Oblast", 
           "Mari El Republic", "Udmurt Republic", 
           "Smolensk Oblast", "Moscow", "Moscow Oblast", "Vladimir Oblast", 
           "Nizhny Novgorod Oblast", "Chuvash Republic", "Tatarstan", "Bashkortostan", 
           "Kaluga Oblast", "Tula Oblast", "Ryazan Oblast", "Mordovia", 
           "Ulyanovsk Oblast", "Samara Oblast", "Orenburg Oblast", 
           "Bryansk Oblast", "Oryol Oblast", "Lipetsk Oblast", "Tambov Oblast", 
           "Penza Oblast", "Saratov Oblast", 
           "Kursk Oblast", "Belgorod Oblast", "Voronezh Oblast", "Volgograd Oblast", 
           "Rostov Oblast", "Kalmykia", "Astrakhan Oblast"),
  stringsAsFactors = FALSE
)
