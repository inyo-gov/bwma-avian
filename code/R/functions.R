library(dplyr)
library(lubridate)

# Function to clean data
clean_data <- function(data) {
  data %>%
    clean_names() %>%
    select(date, year, season, unit, subunit, family, species, code, total)
}

# Function to create lookup table for subunits
create_lookup_table <- function() {
  data.frame(
    entered_subunit = c("TH05", "TH06", "TH07", "TH08", "TH09", "TH10", "W12", "W13", "W14",
                        "WAG1", "WAG2", "WAG3", "WAG4", "WAG5", "TH11", "W11", "WAG6",
                        "WIN11", "WIN12", "WIN13", "WIN14", "WS1", "WS2", "W7", "W9",
                        "TH5", "TH7", "TH8", "TH9", "SW2", "WAG2W", "WIN09", "WAG2w",
                        "SW1", "NA", "W09", "TH03", "THxx", "WAG2west", "WAG5west",
                        "WAG4west", "WAG6west"),
    correct_subunit = c("TH5", "TH6", "TH7", "TH8", "TH9", "TH10", "W12", "W13", "W14",
                        "WAG1", "WAG2", "WAG3", "WAG4", "WAG5", "TH11", "W11", "WAG6",
                        "W11", "W12", "W13", "W14", "SW1", "SW2", "W7", "W9",
                        "TH5", "TH7", "TH8", "TH9", "SW2", "WAG2", "W9", "WAG2",
                        "SW1", NA, "W9", "TH3", NA, "WAG2", "WAG5", "WAG4", "WAG6")
  )
}

# Function to correct subunits
correct_subunits <- function(data, lookup_table) {
  data %>%
    left_join(lookup_table, by = c("subunit" = "entered_subunit")) %>%
    mutate(subunit = ifelse(is.na(correct_subunit), subunit, correct_subunit)) %>%
    select(-correct_subunit)
}

# Function to extract month and create real season variable
add_season_and_month <- function(data) {
  data %>%
    mutate(
      month = month(ymd(date)),
      real_season = case_when(
        month %in% c(12, 1, 2) ~ "Winter",
        month %in% c(3, 4, 5) ~ "Spring",
        month %in% c(6, 7, 8) ~ "Summer",
        month %in% c(9, 10, 11) ~ "Fall",
        TRUE ~ NA_character_
      )
    )
}

# Function to create survey number and label
add_survey_info <- function(data) {
  data %>%
    mutate(survey_number = case_when(
      real_season == "Fall" & year == 2021 ~ 1,
      real_season == "Winter" & year == 2021 & month %in% c(12) ~ 2,
      real_season == "Winter" & year == 2022 & month %in% c(1, 2) ~ 2,
      real_season == "Spring" & year == 2022 ~ 3,
      real_season == "Fall" & year == 2022 ~ 4,
      real_season == "Winter" & year == 2022 & month %in% c(12) ~ 5,
      real_season == "Winter" & year == 2023 & month %in% c(1, 2) ~ 5,
      real_season == "Spring" & year == 2023 ~ 6,
      real_season == "Fall" & year == 2023 ~ 7,
      real_season == "Winter" & year == 2023 & month %in% c(12) ~ 8,
      real_season == "Winter" & year == 2024 & month %in% c(1, 2) ~ 8,
      real_season == "Spring" & year == 2024 ~ 9,
      TRUE ~ NA_real_
    ),
    survey_label = case_when(
      survey_number == 1 ~ "Fall 2021",
      survey_number == 2 ~ "Winter 2021-22",
      survey_number == 3 ~ "Spring 2022",
      survey_number == 4 ~ "Fall 2022",
      survey_number == 5 ~ "Winter 2022-23",
      survey_number == 6 ~ "Spring 2023",
      survey_number == 7 ~ "Fall 2023",
      survey_number == 8 ~ "Winter 2023-24",
      survey_number == 9 ~ "Spring 2024",
      TRUE ~ NA_character_
    ))
}

# Function to process data
process_data <- function(data) {
  data %>%
    group_by(year, unit, subunit, family, real_season, survey_number, survey_label) %>%
    summarize(total = sum(total, na.rm = TRUE))
}

# Function to plot data by family
plot_bird_data <- function(data, flooding_units, family_name, ncol = 5, fig_width = 12, fig_height = 8) {
  data <- data %>% filter(family == family_name)
  plot_title <- paste("Bird Numbers by Survey and Subbasin (2021-2024) - Family:", family_name)

  ggplot(data = data) +
    geom_sf(data = flooding_units, fill = "lightgray", color = "white") +  # Background map
    geom_point(aes(geometry = geometry, size = total, color = family), stat = "sf_coordinates") +
    facet_wrap(~survey_number + survey_label, ncol = ncol) +
    theme_minimal() +
    labs(title = plot_title,
         size = "Total Birds",
         color = "Bird Family") +
    theme(legend.position = "none",  # Remove the legend
          axis.title.x = element_blank(),  # Remove x axis label
          axis.title.y = element_blank(),  # Remove y axis label
          axis.text = element_blank(),  # Remove axis text
          axis.ticks = element_blank(),  # Remove axis ticks
          strip.text = element_text(size = 12))  # Increase facet label size
}
