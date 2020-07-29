library("dplyr")
library("forcats")
library("readr")
library("purrr")


dir_raw_201909 <- file.path("data", "raw", "201909")
dir_raw_202001 <- file.path("data", "raw", "202001")
dir_processed <- file.path("data", "processed")


collate_data <- function(files, date_label) {
  d <- map_df(files, read_csv, col_types = cols(
    .default = col_character(),
    trial_index = col_double(),
    time_elapsed = col_double(),
    list = col_double(),
    rt = col_double(),
    presentation_start_time = col_double(),
    id = col_double(),
    study = col_logical()),
    na = "null"
  )
  
  d %>%
    filter(trial_type == "html-image-mc-response") %>%
    mutate(city = gsub("\\.\\/res\\/([a-z]*?)\\.csv", "\\1", materials),
           correct = as.logical(correct),
           correct = ifelse(is.na(correct), FALSE, correct), # Treat missing response as incorrect
           date = as.POSIXct(presentation_start_time / 1000, origin = "1970-01-01"),
           list = paste(date_label, list, sep = "_"),
           id = paste(city, id, sep = "_")) %>% 
    select(-value, -trial_type, -internal_node_id, -materials, -view_history, -browser_info, -image) %>%
    group_by(subject) %>%
    mutate(start_time = presentation_start_time - min(presentation_start_time)) %>%
    ungroup()
}

auto_to_auth0 <- read_csv(file.path(dir_raw_202001, "auto_to_auth0.csv"))

# Learning sessions -------------------------------------------------------

# Each learning session has a CSV file with "safehouses" in its name
files_learn_201909 <- list.files(dir_raw_201909, "safehouses", full.names = TRUE)
learn_201909 <- collate_data(files_learn_201909, "201909")

# Replace subject tokens with anonymised IDs ------------------------------

subjects <- learn_201909 %>%
  distinct(subject) %>%
  left_join(auto_to_auth0, by = c("subject" = "auto_id")) %>%
  mutate(subject = ifelse(is.na(auth0_id), subject, auth0_id)) %>%
  select(-auth0_id) %>%
  mutate(subject = as.factor(subject),
         subj = fct_anon(subject, prefix = "subj"),
         subject = as.character(subject))

learn_201909 <- left_join(learn_201909, subjects, by = "subject") %>%
  mutate(subject = subj) %>%
  select(-subj)

# Export to CSV -----------------------------------------------------------

write_csv(learn_201909, file.path(dir_processed, "learn_201909.csv"))

write_csv(subjects, file.path(dir_processed, "subject_IDs.csv"))