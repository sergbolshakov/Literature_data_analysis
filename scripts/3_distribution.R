library(magrittr)

# Geographic Distribution ======================================================

# Prepare data -----------------------------------------------------------------

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
    percentage = round(species / length(unique(data$acceptedNameUsage)) * 100,
                       digits = 2),
    publications = dplyr::n_distinct(bibliographicCitation)
  ) %>%
  dplyr::left_join(unique_species) %>% 
  dplyr::mutate(unique_species = replace(unique_species,
                                         is.na(unique_species),
                                         0))

# Histograms -------------------------------------------------------------------

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

# Statistics -------------------------------------------------------------------

length(unique(data$acceptedNameUsage))

median(distribution$species)

data %>%
  dplyr::select(stateProvince, acceptedNameUsage) %>%
  dplyr::filter(!grepl("ZZ", stateProvince)) %>%
  dplyr::group_by(acceptedNameUsage) %>%
  dplyr::summarize(regions = dplyr::n_distinct(stateProvince)) %>% 
  #View()
  dplyr::filter(regions == 1) %>% 
  dplyr::count()

# Output summary table ---------------------------------------------------------

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
