library(magrittr)

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

# Study History ----------------------------------------------------------------

data_history <- 
  data %>%
  dplyr::select(bibliographicCitation,
                PublicationYear,
                acceptedNameUsage,
                stateProvince) %>%
  dplyr::distinct() %>%
  dplyr::arrange(PublicationYear,
                 stateProvince,
                 acceptedNameUsage)

history_publications <- 
  data_history %>%
  dplyr::mutate(publications_accum = cumsum(!duplicated(bibliographicCitation))) %>%
  dplyr::group_by(PublicationYear) %>%
  dplyr::mutate(publications_per_year = dplyr::n_distinct(bibliographicCitation)) %>%
  dplyr::select(PublicationYear, publications_accum, publications_per_year) %>%
  dplyr::slice_max(publications_accum, n = 1) %>% 
  dplyr::distinct()

history_species <- 
  data_history %>%
  dplyr::mutate(species_accum = cumsum(!duplicated(acceptedNameUsage))) %>%
  dplyr::group_by(PublicationYear) %>%
  dplyr::mutate(species_per_year = dplyr::n_distinct(acceptedNameUsage)) %>%
  dplyr::select(PublicationYear, species_accum, species_per_year) %>%
  dplyr::slice_max(species_accum, n = 1) %>% 
  dplyr::distinct()

history_regions <- 
  data_history %>%
  dplyr::filter(!grepl("ZZ", stateProvince)) %>%
  dplyr::mutate(regions_accum = cumsum(!duplicated(stateProvince))) %>%
  dplyr::group_by(PublicationYear) %>%
  dplyr::mutate(regions_per_year = dplyr::n_distinct(stateProvince)) %>%
  dplyr::select(PublicationYear, regions_accum, regions_per_year) %>%
  dplyr::slice_max(regions_accum, n = 1) %>% 
  dplyr::distinct()

# Plots

plot_history_publications <- 
  ggplot2::ggplot(history_publications) +
  ggplot2::geom_col(ggplot2::aes(x = PublicationYear,
                                 y = publications_per_year),
                    width = 1,
                    color = "darkblue",
                    fill = "lightblue") +
  ggplot2::scale_x_continuous(
    breaks = c(1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold", size = 10))

plot_history_species <- 
  ggplot2::ggplot(history_species) +
  ggplot2::geom_col(ggplot2::aes(x = PublicationYear,
                                 y = species_per_year),
                    width = 1,
                    color = "darkblue",
                    fill = "lightblue") +
  ggplot2::scale_x_continuous(
    breaks = c(1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold", size = 10))

plot_history_publications_species <- 
  ggplot2::ggplot() +
  ggplot2::geom_line(
    data = history_publications,
    mapping = ggplot2::aes(x = PublicationYear,
                           y = publications_accum),
    color = "darkblue") +
  ggplot2::geom_line(
    data = history_species,
    mapping = ggplot2::aes(x = PublicationYear,
                           y = species_accum),
    color = "red") +
  ggplot2::scale_x_continuous(
    breaks = c(1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold", size = 10))

plot_history_regions <- 
  ggplot2::ggplot(history_regions) +
  ggplot2::geom_col(ggplot2::aes(x = PublicationYear,
                                 y = regions_per_year),
                    width = 1,
                    color = "darkblue",
                    fill = "lightblue") +
  ggplot2::geom_line(ggplot2::aes(x = PublicationYear,
                                  y = regions_accum),
                     color = "red") +
  ggplot2::scale_x_continuous(
    breaks = c(1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                            face = "bold", size = 10))

cowplot::plot_grid(plot_history_publications,
                   plot_history_species,
                   plot_history_publications_species,
                   plot_history_regions,
                   labels = "auto",
                   label_fontfamily = "Times New Roman",
                   ncol = 2, nrow = 2)

ggplot2::ggsave("output/history.jpg",
                width = 219,
                height = 170,
                units = "mm",
                dpi = 1200,
                bg = "white")

# Statistics

history_publications %>% 
  dplyr::mutate(century = ceiling(PublicationYear / 100)) %>%
  dplyr::group_by(century) %>% 
  dplyr::summarize(mean = mean(publications_per_year))

history_species %>% 
  dplyr::mutate(century = ceiling(PublicationYear / 100)) %>%
  dplyr::group_by(century) %>% 
  dplyr::summarize(mean = mean(species_per_year))

data_history %>%
  dplyr::mutate(twenty = floor(PublicationYear / 20) * 20) %>%
  dplyr::group_by(twenty, bibliographicCitation) %>%
  dplyr::summarize(species = dplyr::n_distinct(acceptedNameUsage)) %>% 
  dplyr::arrange(twenty, desc(species)) %>%
  dplyr::top_n(5) %>% 
  flextable::flextable() %>%
  flextable::autofit()

history_regions %>% 
  dplyr::mutate(century = ceiling(PublicationYear / 100)) %>%
  dplyr::group_by(century) %>% 
  dplyr::summarize(mean = mean(regions_per_year))
