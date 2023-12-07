# C. Savonen
# A first quick stab for collecting metrics on ITN happenings.

authorize("google")

accounts <- get_ga_user()

## Collect google analytics
fhdsl_stats_list <- all_ga_metrics(account_id = accounts$id[1])
itcr_stats_list <- all_ga_metrics(account_id = accounts$id[2])

all_data <- dplyr::bind_rows(fhdsl_stats_list$metrics ,itcr_stats_list$metrics)

# Collect coursera numbers by hand -- there's no API
coursera_numbers <- c(
  "AI in Software" = 72,
  "Leadership Course" = 605,
  "Documentation and Usability Course" = 275,
  "Computing" = 893,
  "Reproducibility" = 347,
  "Advanced Reproducibility in Cancer Informatics" = 255
) %>%
  data.frame() %>%
  tibble::rownames_to_column("website") %>%
  dplyr::rename("total_coursera_enrollments"= ".")

# Collect leanpub numbers by hand -- their API is limited
leanpub_numbers <- c(
  "AI in Software" = 6,
  "Leadership Course" = 32,
  "Documentation and Usability Course" = 25,
  "Computing" = 19,
  "Reproducibility" = 12,
  "Advanced Reproducibility in Cancer Informatics" = 15,
  "Choosing Genomic Tools" = 11
) %>%
  data.frame() %>%
  tibble::rownames_to_column("website") %>%
  dplyr::rename("total_leanpub_enrollees"= ".")

# Filter out non ITCR websites
itcr_data <- all_data %>%
  dplyr::filter(!(website %in% c("hutchdatasci", "whoiswho", "MMDS", "FH Cluster 101"))) %>%
  dplyr::left_join(coursera_numbers) %>%
  dplyr::left_join(leanpub_numbers)

readr::write_tsv(itcr_data, "itcr_website_metrics.tsv")

# Collect loqui data
loqui_usage <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1G_HTU-bv2k5txExP8EH3ScUfGqtW1P3syThD84Z-g9k/edit#gid=0")
loqui_usage %>% count(email)

## Collect slido info
itcr_drive_id <- "https://drive.google.com/drive/folders/0AJb5Zemj0AAkUk9PVA"
itcr_slido_data <- get_slido_files(itcr_drive_id)

poll_data <- itcr_slido_data$`Polls-per-user` %>%
  janitor::clean_names()

as.numeric(c(poll_data$how_likely_would_you_be_to_recommend_this_workshop, poll_data$how_likely_would_you_be_to_recommend_this_workshop_2)) %>%
  ggplot2::qplot(geom = "bar") +
  ggplot2::geom_bar(fill = "#CBC3E3") +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "How likely would you be to recommend this workshop?")
  ggplot2::ggsave("itn_workshop_rec.png", width = 4, height = 2)

  poll_data <- poll_data %>%
    dplyr::filter(how_likely_are_you_to_use_what_you_learned_in_your_daily_work %in% c("Extremely likely", "Likely", "Not very likely", "Somewhat likely", "Very likely"))

  poll_data$how_likely_are_you_to_use_what_you_learned_in_your_daily_work <- factor(poll_data$how_likely_are_you_to_use_what_you_learned_in_your_daily_work, levels = c("Not very likely",  "Somewhat likely", "Likely", "Very likely", "Extremely likely"))

   ggplot2::ggplot(poll_data, ggplot2::aes(x = how_likely_are_you_to_use_what_you_learned_in_your_daily_work)) +
    ggplot2::geom_bar(stat = "count", fill = "#CBC3E3") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
    ggplot2::labs(title = "How likely are you to use what you learned in your daily work?") +
    ggplot2::xlab("")
   ggplot2::ggsave("itn_relevance.png", width = 4, height = 2)

itcr_slido_data$JoinedParticipants %>% dplyr::count(event_name)


library(udpipe)
library(textrank)
library(wordcloud)
## First step: Take the Spanish udpipe model and annotate the text. Note: this takes about 3 minutes
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

results <- udpipe_annotate(ud_model, x = poll_data$what_did_you_like_most_about_the_workshop) %>%
  as.data.frame(results) %>%
  dplyr::filter(upos %in% c("NOUN", "ADJ", "ADV")) %>%
  dplyr::count(lemma)

wordcloud(words = results$lemma, freq=results$n)

rec_results <- udpipe_annotate(ud_model, x = poll_data$please_share_any_recommendations_you_have_for_improvements) %>%
  as.data.frame(results) %>%
  dplyr::filter(upos %in% c("NOUN", "ADJ", "ADV")) %>%
  dplyr::count(lemma)

wordcloud(words = rec_results$lemma, freq=results$n)


# Get unique attendees
unique_emails <-
  unique(c(itcr_slido_data$`Polls-per-user`$`Please submit your email so we can log your attendance`,
           itcr_slido_data$`Polls-per-user`$`What's your email?`,
           itcr_slido_data$`Polls-per-user`$`What is your email?`))

length(unique_emails)

### PLOTS

TODO: this plot should be wrapped up into a function and also made more prettier
## Bookdown
ggplot2::ggplot(itcr_data, ggplot2::aes(x = reorder(website, -activeUsers), y = activeUsers)) +
  ggplot2::geom_bar(stat = "identity", fill = "#89CFF0") +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
  ggplot2::xlab("") +
  ggplot2::geom_text(ggplot2::aes(label = activeUsers), size = 3, vjust = - 1) +
  ggplot2::ylim(c(0, 5500))

## Coursera
ggplot2::ggplot(itcr_data, ggplot2::aes(x = reorder(website, -total_coursera_enrollments), y = total_coursera_enrollments)) +
  ggplot2::geom_bar(stat = "identity", fill = "#89CFF0", na.rm = TRUE) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
  ggplot2::xlab("") +
  ggplot2::ylab("Coursera enrollments") +
  ggplot2::geom_text(ggplot2::aes(label = total_coursera_enrollments), size = 3, vjust = - 1, na.rm = TRUE) +
  ggplot2::ylim(c(0, 1200))

## Leanpub
ggplot2::ggplot(itcr_data, ggplot2::aes(x = reorder(website, -total_leanpub_enrollees), y = total_leanpub_enrollees)) +
  ggplot2::geom_bar(stat = "identity", fill = "#89CFF0", na.rm = TRUE) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust=1)) +
  ggplot2::xlab("") +
  ggplot2::ylab("Leanpub enrollments") +
  ggplot2::geom_text(ggplot2::aes(label = total_leanpub_enrollees), size = 3, vjust = - 1, na.rm = TRUE) +
  ggplot2::ylim(c(0, 40))

## Sum the learners up
sum(itcr_data$activeUsers, na.rm = TRUE)
sum(itcr_data$total_coursera_enrollments, na.rm = TRUE)
sum(itcr_data$total_leanpub_enrollees, na.rm = TRUE)
