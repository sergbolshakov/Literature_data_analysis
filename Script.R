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

# Geographic Distribution ------------------------------------------------------

unique_species <- 
  data %>%
  dplyr::select(stateProvince,
                bibliographicCitation,
                acceptedNameUsage) %>%
  dplyr::filter(!grepl("ZZ", stateProvince)) %>%
  dplyr::arrange(acceptedNameUsage, stateProvince) %>%
  dplyr::group_by(acceptedNameUsage) %>%
  dplyr::mutate(count_regions = dplyr::n_distinct(stateProvince)) %>%
  dplyr::filter(count_regions == 1) %>%
  dplyr::group_by(stateProvince) %>%
  dplyr::summarize(unique_species = dplyr::n_distinct(acceptedNameUsage))

distribution <- 
  data %>%
  dplyr::select(stateProvince,
                bibliographicCitation,
                acceptedNameUsage) %>%
  dplyr::filter(!grepl("ZZ", stateProvince)) %>%
  dplyr::group_by(stateProvince) %>%
  dplyr::summarize(
    species = dplyr::n_distinct(acceptedNameUsage),
    percentage = round(species / length(unique(data$acceptedNameUsage)) * 100, digits = 2),
    publications = dplyr::n_distinct(bibliographicCitation)
  ) %>%
  dplyr::left_join(unique_species) %>% 
  dplyr::mutate(unique_species = replace(unique_species, is.na(unique_species), 0))

# Plots

plot_distribution_species <- 
  ggplot2::ggplot(distribution,
                  ggplot2::aes(x = species)) +
  ggplot2::geom_histogram(boundary = 0,
                          binwidth = 100,
                          fill = "lightblue",
                          color = "darkblue") +
  ggplot2::scale_y_continuous(limits = c(0, 12),
                              breaks = seq(0, 12, 2)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 900, 100)) +
  ggplot2::geom_text(stat = "bin",
                     binwidth = 100,
                     ggplot2::aes(label = ..count..),
                     boundary = 0,
                     vjust = -1,
                     family = "Times New Roman",
                     size = 2) +
  ggplot2::labs(x = "Number of species", y = "Number of regions") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = -10, l = 10)),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = -10, b = 10)),
    text = ggplot2::element_text(family = "Times New Roman",
                                 face = "bold", size = 10))

plot_distribution_regions <- 
  data %>%
  dplyr::select(stateProvince,
                bibliographicCitation,
                acceptedNameUsage) %>%
  dplyr::filter(!grepl("ZZ", stateProvince)) %>%
  dplyr::group_by(acceptedNameUsage) %>%
  dplyr::summarize(regions = dplyr::n_distinct(stateProvince)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = regions)) +
  ggplot2::geom_histogram(boundary = 0,
                          binwidth = 5,
                          fill = "lightblue",
                          color = "darkblue") +
  ggplot2::scale_y_continuous(limits = c(0, 600),
                              breaks = seq(0, 600, 100)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 50, 5)) +
  ggplot2::geom_text(stat = "bin",
                     binwidth = 5,
                     ggplot2::aes(label = ..count..),
                     boundary = 0,
                     vjust = -1,
                     family = "Times New Roman",
                     size = 2) +
  ggplot2::labs(x = "Number of regions", y = "Number of species") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = -10, l = 10)),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = -10, b = 10)),
    text = ggplot2::element_text(family = "Times New Roman",
                                 face = "bold", size = 10))


cowplot::plot_grid(plot_distribution_species,
                   plot_distribution_regions,
                   labels = "auto",
                   label_fontfamily = "Times New Roman",
                   ncol = 2, nrow = 1)

ggplot2::ggsave("output/distribution.jpg",
                width = 219,
                height = 82,
                units = "mm",
                dpi = 1200,
                bg = "white")

# Statistics

median(distribution$species)

data %>%
  dplyr::select(stateProvince, acceptedNameUsage) %>%
  dplyr::filter(!grepl("ZZ", stateProvince)) %>%
  dplyr::group_by(acceptedNameUsage) %>%
  dplyr::summarize(regions = dplyr::n_distinct(stateProvince)) %>% 
  dplyr::filter(regions == 1) %>% 
  dplyr::count()

# Output summary table

flextable::set_flextable_defaults(font.family = "Times New Roman",
                                  font.size = 10,
                                  font.color = "black",
                                  text.align = 'left',
                                  padding.bottom = 3,
                                  padding.top = 3,
                                  padding.left = 0,
                                  padding.right = 0)

distribution %>% 
  dplyr::left_join(geography,
                   by = c("stateProvince" = "alt_name_en")) %>% 
  dplyr::select(stateProvince,
                code = "iso_3166-2_ru",
                species,
                percentage,
                unique_species,
                publications) %>% 
  flextable::flextable() %>% 
  flextable::set_header_labels(stateProvince = "Administrative regions",
                               code = "Code",
                               species = "Number of accepted species",
                               percentage = "% total accepted species",
                               unique_species = "Number of “unique” species",
                               publications = "Number of publications") %>% 
  flextable::set_table_properties(layout = "autofit") %>% 
  flextable::save_as_docx(path = "output/regions_summary.docx")
