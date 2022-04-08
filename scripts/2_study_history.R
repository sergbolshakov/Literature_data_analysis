library(magrittr)

# Study History ================================================================

# Prepare data -----------------------------------------------------------------

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

# Plots ------------------------------------------------------------------------

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

ggplot2::ggsave("output/history_plots.jpg",
                width = 219,
                height = 170,
                units = "mm",
                dpi = 1200,
                bg = "white")

# Statistics -------------------------------------------------------------------

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

# Tile grid map ----------------------------------------------------------------

data_history %>%
  dplyr::mutate(twenty = floor((PublicationYear - 1) / 20) * 20) %>%
  dplyr::group_by(stateProvince, twenty) %>% 
  dplyr::summarize(species = dplyr::n_distinct(acceptedNameUsage)) %>% 
  ggplot2::ggplot(ggplot2::aes(x = twenty,
                               y = species)) +
  ggplot2::geom_col(fill = "darkblue") +
  ggplot2::labs(x = "", y = "Number of species") +
  ggplot2::scale_y_log10() +
  ggplot2::scale_x_continuous(breaks = c(1900, 2000),
                              labels = c("1900s", "2000s")) +
  geofacet::facet_geo(~ stateProvince,
                      grid = european_russia,
                      label = "code") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Times New Roman",
                                              face = "bold"),
                 axis.text.y = ggplot2::element_text(size = 6),
                 axis.text.x = ggplot2::element_text(size = 6))

ggplot2::ggsave("output/history_regions.jpg",
                width = 219,
                height = 170,
                units = "mm",
                dpi = 1200,
                bg = "white")

# Statistics -------------------------------------------------------------------

data_history %>% 
  dplyr::filter(stateProvince == "Smolensk Oblast") %>% 
  dplyr::group_by(PublicationYear) %>% 
  dplyr::summarize(species = dplyr::n_distinct(acceptedNameUsage))

data_history %>% 
  dplyr::filter(stateProvince == "Smolensk Oblast",
                dplyr::between(PublicationYear, "1893", "1898")) %>% 
  dplyr::summarize(species = dplyr::n_distinct(acceptedNameUsage))

data_history %>% 
  dplyr::filter(stateProvince == "Moscow") %>% 
  dplyr::group_by(PublicationYear) %>% 
  dplyr::summarize(species = dplyr::n_distinct(acceptedNameUsage))

data_history %>% 
  dplyr::filter(stateProvince == "Moscow",
                dplyr::between(PublicationYear, "1897", "1906")) %>% 
  dplyr::summarize(species = dplyr::n_distinct(acceptedNameUsage))

get_region_stat <- 
  function(region, century) {
    species_century <- 
      data_history %>% 
      dplyr::filter(stateProvince == region,
                    PublicationYear <= century) %>% 
      dplyr::summarize(species_century = dplyr::n_distinct(acceptedNameUsage))
    species_total <- 
      data_history %>% 
      dplyr::filter(stateProvince == region) %>% 
      dplyr::summarize(species_total = dplyr::n_distinct(acceptedNameUsage))
    dplyr::bind_cols(species_century, species_total)
  }

get_region_stat("Smolensk Oblast", 1900)
get_region_stat("Smolensk Oblast", 2000)
get_region_stat("Moscow", 1900)
get_region_stat("Moscow", 2000)

get_region_stat("Ivanovo Oblast", 2000)
get_region_stat("Penza Oblast", 2000)
get_region_stat("Saratov Oblast", 2000)
get_region_stat("Tambov Oblast", 2000)
