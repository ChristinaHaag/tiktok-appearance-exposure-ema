#######################################################################################
#                                                                                     #
#   Exposure to ideal appearance content on TikTok and body satisfaction              #
#   in female adolescents                                                             #
#                                                                                     #
#   METADATA EXPOSURE 2: daily exposure in the MORNING window (morning prompt ->      #
#     03:00 next day) for H2b (beauty filter) and H2c (beauty content label)          #
#                                                                                     #
#   Second registration. Two exposure measures from the TikTok metadata, each a       #
#   property of the whole video, both built here from the metadata file:              #
#     beauty_filter         at least one effect applied to the video is on the        #
#                           reviewed list of beauty filters (Input_data/              #
#                           beauty_filter_effect_list_reviewed.txt, one name per line)#
#     beauty_content_label  at least one of TikTok's content labels for the video is  #
#                           one of the six beauty labels set below                    #
#   Every watched second of a flagged video counts. Videos without metadata are       #
#   substituted with the same three content-probability tiers as the computer-vision  #
#   measure. Same participant-day dataset, window and output layout as                #
#   02_build_computer_vision_exposure_morning.R; only the counting step differs.      #
#                                                                                     #
#   Reads: Processed_data/participant_day_dataset_morning.rds,                        #
#          Processed_data/tiktok_viewing_events_with_watch_duration.rds,              #
#          Input_data/processed_metadata.csv (columns video_id,                       #
#            video_effect_stickers, video_diversification_labels),                    #
#          Input_data/beauty_filter_effect_list_reviewed.txt  (the list after the     #
#            verification review; the preregistered candidates are in                 #
#            beauty_filter_effect_list_preregistered_candidates.txt)                  #
#   Writes to Processed_data: daily_exposure_morning_beauty_filter.rds                #
#                             daily_exposure_morning_beauty_content_label.rds         #
#   Both keep the layout of the computer-vision exposure files; the exposure column   #
#   is beauty_filter_seconds / beauty_content_seconds.                                #
#                                                                                     #
#######################################################################################


# Load packages
library(dplyr); library(tidyr); library(lubridate)

# Disable scientific notation of small values
options(scipen = 999)

# Folders
data_dir  <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"
input_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Input_data"

# The six beauty content labels (exactly as TikTok writes them)
beauty_labels <- c("Beauty", "Beauty & Style", "Beauty & Care", "Outfit", "Nail Art", "Hair")



#######################################################################################################
#######################################################################################################
#######################################################################################################
# LOAD DATA

### The participant-day dataset (one row per participant and study day, with the morning window, sses_am and the lag covariates)
participant_days <- readRDS(file.path(data_dir, "participant_day_dataset_morning.rds"))
participant_days$id <- as.character(participant_days$id)
stopifnot(!anyNA(participant_days$window_start), !anyNA(participant_days$window_end))
nrow(participant_days)   # 924


### Viewing events with their estimated watch duration
events <- readRDS(file.path(data_dir, "tiktok_viewing_events_with_watch_duration.rds")) %>% as.data.frame()
events$id <- as.character(events$id)
stopifnot(!anyNA(events$viewing_event_duration_sec))

# Study day of each event: a day runs from 03:00 UTC (= 04:00 BST) to 02:59 the next
# calendar day, so events between 00:00 and 03:00 belong to the previous date
events$date <- as_date(events$tiktok_datetime - hours(3))


### The video metadata: all columns as text, so the 19-digit video ids are not turned into numbers
metadata <- read.csv(file.path(input_dir, "processed_metadata.csv"), colClasses = "character", na.strings = character(0))
nrow(metadata)
stopifnot(all(c("video_id", "video_effect_stickers", "video_diversification_labels") %in% names(metadata)))   # the three columns used below
stopifnot(!any(duplicated(metadata$video_id)))                 # one row per video
stopifnot(all(grepl("^[0-9]+$", metadata$video_id)))           # ids as plain digit strings


### The reviewed list of beauty filters, one name per line
beauty_filter_list <- trimws(readLines(file.path(input_dir, "beauty_filter_effect_list_reviewed.txt"), warn = FALSE))
beauty_filter_list <- beauty_filter_list[beauty_filter_list != ""]
length(beauty_filter_list)

#######################################################################################################
#######################################################################################################
#######################################################################################################
# VIDEO-LEVEL FLAGS FROM THE METADATA (one row per video, TRUE / FALSE / NA each)

# Rule for both flags, for every video that has a metadata row: at least one match -> TRUE;
# otherwise FALSE -- including an empty effect / label field, which TikTok leaves empty when no
# effect was applied or no label assigned. Only a watched video WITHOUT a metadata row has no
# flag (NA after the join below) and is substituted like an unclassified video (the case the
# registration's substitution rule describes). Names are matched ignoring case and spaces.

### The two text columns; "NA" written as text means the same as an empty cell
metadata$effect_text <- trimws(metadata$video_effect_stickers)
metadata$label_text  <- trimws(metadata$video_diversification_labels)
metadata$effect_text[is.na(metadata$effect_text) | metadata$effect_text == "NA"] <- ""
metadata$label_text[is.na(metadata$label_text)   | metadata$label_text  == "NA"] <- ""

sum(metadata$effect_text == "")                 # scraped videos with no effect applied
sum(metadata$effect_text == "[]")               # videos with no effect applied
sum(!metadata$effect_text %in% c("", "[]"))     # videos with effects listed
sum(metadata$label_text == "")                  # scraped videos with no label assigned
sum(metadata$label_text == "[]")
sum(!metadata$label_text %in% c("", "[]"))


### Beauty filter: one row per effect applied to a video, matched against the reviewed list
# The effect names are the values after 'name': in the text, in single quotes -- or in
# double quotes when the name itself contains an apostrophe (e.g. "Mom's Lashes")
names_single_quoted <- regmatches(metadata$effect_text, gregexpr("'name': '[^']*'", metadata$effect_text))
names_double_quoted <- regmatches(metadata$effect_text, gregexpr("'name': \"[^\"]*\"", metadata$effect_text))

video_effects <- rbind(
  data.frame(video_id    = rep(metadata$video_id, lengths(names_single_quoted)),
             effect_name = sub("'$", "", sub("^'name': '", "", unlist(names_single_quoted)))),
  data.frame(video_id    = rep(metadata$video_id, lengths(names_double_quoted)),
             effect_name = sub("\"$", "", sub("^'name': \"", "", unlist(names_double_quoted)))))
nrow(video_effects)                             # effects applied, summed over videos
stopifnot(all(metadata$video_id[!metadata$effect_text %in% c("", "[]")] %in% video_effects$video_id))   # every effect list was read

video_effects$on_list <- tolower(trimws(video_effects$effect_name)) %in% tolower(beauty_filter_list)
table(video_effects$on_list)

# The beauty filters found, with the number of videos each flags (Supplement), and the listed effects never seen
video_effects %>% subset(on_list) %>% group_by(effect_name = tolower(trimws(effect_name))) %>%
  summarise(n_videos = n_distinct(video_id)) %>% arrange(desc(n_videos)) %>% as.data.frame()
beauty_filter_list[!(tolower(beauty_filter_list) %in% tolower(trimws(video_effects$effect_name)))]

videos_with_beauty_filter <- unique(video_effects$video_id[video_effects$on_list])
metadata$beauty_filter <- metadata$video_id %in% videos_with_beauty_filter
table(metadata$beauty_filter, useNA = "ifany")


### Beauty content label: one row per label of a video, matched against the six beauty labels
# The labels are the quoted strings inside the brackets: remove brackets and quotes, split at the commas
label_lists  <- strsplit(gsub("\\[|\\]|'", "", metadata$label_text), ",")
video_labels <- data.frame(video_id = rep(metadata$video_id, lengths(label_lists)),
                           label    = trimws(unlist(label_lists)))
video_labels <- subset(video_labels, label != "")
nrow(video_labels)                              # labels, summed over videos
stopifnot(all(metadata$video_id[!metadata$label_text %in% c("", "[]")] %in% video_labels$video_id))   # every label list was read

video_labels$is_beauty <- tolower(video_labels$label) %in% tolower(beauty_labels)
table(video_labels$is_beauty)

# Each beauty label with the number of videos it flags (Supplement), and the beauty labels never seen
video_labels %>% subset(is_beauty) %>% group_by(label = tolower(label)) %>%
  summarise(n_videos = n_distinct(video_id)) %>% arrange(desc(n_videos)) %>% as.data.frame()
beauty_labels[!(tolower(beauty_labels) %in% tolower(video_labels$label))]

videos_with_beauty_label <- unique(video_labels$video_id[video_labels$is_beauty])
metadata$beauty_content_label <- metadata$video_id %in% videos_with_beauty_label
table(metadata$beauty_content_label, useNA = "ifany")


### The flags table, and how many of the watched videos have a metadata row (the rest are substituted below)
video_metadata_flags <- metadata[, c("video_id", "beauty_filter", "beauty_content_label")]
stopifnot(!anyNA(video_metadata_flags$beauty_filter), !anyNA(video_metadata_flags$beauty_content_label))   # every scraped video has both flags
table(beauty_filter = video_metadata_flags$beauty_filter, beauty_content_label = video_metadata_flags$beauty_content_label, useNA = "ifany")
mean(unique(events$video_id) %in% video_metadata_flags$video_id)   # watched videos with a metadata row



#######################################################################################################
#######################################################################################################
#######################################################################################################
# VIEWING EVENTS INSIDE EACH DAY'S WINDOW

# Join every event to its study day and keep those between the morning prompt start (or the
# substituted start, see window_start_source) and 03:00 UTC the next day
events_in_window <- participant_days %>%
                    select(id, date, window_start, window_end, donation_covered) %>%
                    inner_join(events, by = c("id", "date"), relationship = "one-to-many") %>%
                    subset(tiktok_datetime > window_start & tiktok_datetime < window_end) %>%
                    mutate(event = row_number())
nrow(events_in_window)




#######################################################################################################
#######################################################################################################
#######################################################################################################
# DAILY EXPOSURE: BEAUTY FILTER (H2b)

# A video counts with ALL its watched seconds when its metadata says it uses a beauty filter
# (the flag is a property of the whole video). No stills are involved: a video is classified
# when its flag is known, and substituted when it is not (Methods 4.6, 4.11).

FLAG_COLUMN <- "beauty_filter"
OUT_NAME    <- "daily_exposure_morning_beauty_filter.rds"


### Viewing events with the video's flag; available = the flag is known
flagged_events <- events_in_window %>%
                  left_join(video_metadata_flags %>% select(video_id, flag = all_of(FLAG_COLUMN)), by = "video_id") %>%
                  mutate(available = !is.na(flag))
table(flagged_events$available)   # available = metadata known; unavailable = substituted below
table(flagged_events$flag, useNA = "ifany")


### Classified videos: seconds watched and qualifying seconds per participant-day
seconds_per_day_available_videos <- flagged_events %>%
                 subset(available) %>%
                 group_by(id, date) %>%
                 summarise(watched_seconds_available = sum(viewing_event_duration_sec),
                           beauty_filter_seconds_available = sum(viewing_event_duration_sec[flag]),
                           .groups = "drop") %>%
                 mutate(day_proportion = if_else(watched_seconds_available > 0, beauty_filter_seconds_available / watched_seconds_available, NA_real_))


### Content-probability substitution for videos without metadata (Methods 4.6, 4.11)
# Tier 1: the participant's own proportion of qualifying seconds among her classified videos that day
# Tier 2: her proportion across her whole donation period (only when nothing was classified that day)
# Tier 3: the sample proportion (only when a participant has no classified videos at all)
participant_proportion <- seconds_per_day_available_videos %>%
                          group_by(id) %>%
                          summarise(participant_proportion = if_else(sum(watched_seconds_available) > 0,
                                                                     sum(beauty_filter_seconds_available) / sum(watched_seconds_available), NA_real_))
sample_proportion <- sum(seconds_per_day_available_videos$beauty_filter_seconds_available) / sum(seconds_per_day_available_videos$watched_seconds_available)
sample_proportion   # share of watched seconds in flagged videos, among videos with metadata

events_to_substitute <- flagged_events %>% subset(!available)
nrow(events_to_substitute)

seconds_per_day_unavailable_videos <- events_to_substitute %>%
                    left_join(seconds_per_day_available_videos %>% select(id, date, day_proportion), by = c("id", "date")) %>%
                    left_join(participant_proportion, by = "id") %>%
                    mutate(tier            = case_when(!is.na(day_proportion)         ~ "1_day",
                                                       !is.na(participant_proportion) ~ "2_participant",
                                                       TRUE                           ~ "3_sample"),
                           proportion_used = coalesce(day_proportion, participant_proportion, sample_proportion),
                           beauty_filter_seconds_substituted = viewing_event_duration_sec * proportion_used)
table(seconds_per_day_unavailable_videos$tier)

seconds_per_day_unavailable_videos <- seconds_per_day_unavailable_videos %>%
                    group_by(id, date) %>%
                    summarise(beauty_filter_seconds_substituted = sum(beauty_filter_seconds_substituted),
                              watched_seconds_substituted = sum(viewing_event_duration_sec),
                              n_unavailable_videos = n(),
                              .groups = "drop")


### Daily exposure on the full participant-day dataset
# Days with no events in the window are non-use days (exposure 0, as registered);
# days outside the donation period are NA (multiply imputed later)
exposure_seconds_per_participant_day <- participant_days %>%
         left_join(seconds_per_day_available_videos,        by = c("id", "date")) %>%
         left_join(seconds_per_day_unavailable_videos, by = c("id", "date")) %>%
         mutate(across(c(watched_seconds_available, beauty_filter_seconds_available, watched_seconds_substituted, beauty_filter_seconds_substituted, n_unavailable_videos),
                       ~ replace_na(.x, 0)),
                watched_seconds = watched_seconds_available + watched_seconds_substituted,
                beauty_filter_seconds = beauty_filter_seconds_available + beauty_filter_seconds_substituted,
                beauty_filter_seconds = if_else(donation_covered, beauty_filter_seconds, NA_real_),
                watched_seconds = if_else(donation_covered, watched_seconds, NA_real_))

# Checks
stopifnot(nrow(exposure_seconds_per_participant_day) == nrow(participant_days))
stopifnot(all(exposure_seconds_per_participant_day$beauty_filter_seconds <= exposure_seconds_per_participant_day$watched_seconds + 1e-9, na.rm = TRUE))

# Numbers for the manuscript
sum(is.na(exposure_seconds_per_participant_day$beauty_filter_seconds))                                       # uncovered days (16)
sum(exposure_seconds_per_participant_day$beauty_filter_seconds == 0, na.rm = TRUE)                           # days with no qualifying exposure
mean(exposure_seconds_per_participant_day$beauty_filter_seconds / 60, na.rm = TRUE)                          # mean daily exposure in minutes
median(exposure_seconds_per_participant_day$beauty_filter_seconds / 60, na.rm = TRUE)                        # median
table(exposure_seconds_per_participant_day$window_start_source)                                         # observed vs substituted window starts
sum(exposure_seconds_per_participant_day$beauty_filter_seconds_substituted, na.rm = TRUE) / sum(exposure_seconds_per_participant_day$beauty_filter_seconds, na.rm = TRUE)   # share of exposure from substitution

# Save
saveRDS(as.data.frame(exposure_seconds_per_participant_day), file.path(data_dir, OUT_NAME))




#######################################################################################################
#######################################################################################################
#######################################################################################################
# DAILY EXPOSURE: BEAUTY CONTENT LABEL (H2c)

# A video counts with ALL its watched seconds when its metadata says it carries a beauty content label
# (the flag is a property of the whole video). No stills are involved: a video is classified
# when its flag is known, and substituted when it is not (Methods 4.6, 4.11).

FLAG_COLUMN <- "beauty_content_label"
OUT_NAME    <- "daily_exposure_morning_beauty_content_label.rds"


### Viewing events with the video's flag; available = the flag is known
flagged_events <- events_in_window %>%
                  left_join(video_metadata_flags %>% select(video_id, flag = all_of(FLAG_COLUMN)), by = "video_id") %>%
                  mutate(available = !is.na(flag))
table(flagged_events$available)   # available = metadata known; unavailable = substituted below
table(flagged_events$flag, useNA = "ifany")


### Classified videos: seconds watched and qualifying seconds per participant-day
seconds_per_day_available_videos <- flagged_events %>%
                 subset(available) %>%
                 group_by(id, date) %>%
                 summarise(watched_seconds_available = sum(viewing_event_duration_sec),
                           beauty_content_seconds_available = sum(viewing_event_duration_sec[flag]),
                           .groups = "drop") %>%
                 mutate(day_proportion = if_else(watched_seconds_available > 0, beauty_content_seconds_available / watched_seconds_available, NA_real_))


### Content-probability substitution for videos without metadata (Methods 4.6, 4.11)
# Tier 1: the participant's own proportion of qualifying seconds among her classified videos that day
# Tier 2: her proportion across her whole donation period (only when nothing was classified that day)
# Tier 3: the sample proportion (only when a participant has no classified videos at all)
participant_proportion <- seconds_per_day_available_videos %>%
                          group_by(id) %>%
                          summarise(participant_proportion = if_else(sum(watched_seconds_available) > 0,
                                                                     sum(beauty_content_seconds_available) / sum(watched_seconds_available), NA_real_))
sample_proportion <- sum(seconds_per_day_available_videos$beauty_content_seconds_available) / sum(seconds_per_day_available_videos$watched_seconds_available)
sample_proportion   # share of watched seconds in flagged videos, among videos with metadata

events_to_substitute <- flagged_events %>% subset(!available)
nrow(events_to_substitute)

seconds_per_day_unavailable_videos <- events_to_substitute %>%
                    left_join(seconds_per_day_available_videos %>% select(id, date, day_proportion), by = c("id", "date")) %>%
                    left_join(participant_proportion, by = "id") %>%
                    mutate(tier            = case_when(!is.na(day_proportion)         ~ "1_day",
                                                       !is.na(participant_proportion) ~ "2_participant",
                                                       TRUE                           ~ "3_sample"),
                           proportion_used = coalesce(day_proportion, participant_proportion, sample_proportion),
                           beauty_content_seconds_substituted = viewing_event_duration_sec * proportion_used)
table(seconds_per_day_unavailable_videos$tier)

seconds_per_day_unavailable_videos <- seconds_per_day_unavailable_videos %>%
                    group_by(id, date) %>%
                    summarise(beauty_content_seconds_substituted = sum(beauty_content_seconds_substituted),
                              watched_seconds_substituted = sum(viewing_event_duration_sec),
                              n_unavailable_videos = n(),
                              .groups = "drop")


### Daily exposure on the full participant-day dataset
# Days with no events in the window are non-use days (exposure 0, as registered);
# days outside the donation period are NA (multiply imputed later)
exposure_seconds_per_participant_day <- participant_days %>%
         left_join(seconds_per_day_available_videos,        by = c("id", "date")) %>%
         left_join(seconds_per_day_unavailable_videos, by = c("id", "date")) %>%
         mutate(across(c(watched_seconds_available, beauty_content_seconds_available, watched_seconds_substituted, beauty_content_seconds_substituted, n_unavailable_videos),
                       ~ replace_na(.x, 0)),
                watched_seconds = watched_seconds_available + watched_seconds_substituted,
                beauty_content_seconds = beauty_content_seconds_available + beauty_content_seconds_substituted,
                beauty_content_seconds = if_else(donation_covered, beauty_content_seconds, NA_real_),
                watched_seconds = if_else(donation_covered, watched_seconds, NA_real_))

# Checks
stopifnot(nrow(exposure_seconds_per_participant_day) == nrow(participant_days))
stopifnot(all(exposure_seconds_per_participant_day$beauty_content_seconds <= exposure_seconds_per_participant_day$watched_seconds + 1e-9, na.rm = TRUE))

# Numbers for the manuscript
sum(is.na(exposure_seconds_per_participant_day$beauty_content_seconds))                                       # uncovered days (16)
sum(exposure_seconds_per_participant_day$beauty_content_seconds == 0, na.rm = TRUE)                           # days with no qualifying exposure
mean(exposure_seconds_per_participant_day$beauty_content_seconds / 60, na.rm = TRUE)                          # mean daily exposure in minutes
median(exposure_seconds_per_participant_day$beauty_content_seconds / 60, na.rm = TRUE)                        # median
table(exposure_seconds_per_participant_day$window_start_source)                                         # observed vs substituted window starts
sum(exposure_seconds_per_participant_day$beauty_content_seconds_substituted, na.rm = TRUE) / sum(exposure_seconds_per_participant_day$beauty_content_seconds, na.rm = TRUE)   # share of exposure from substitution

# Save
saveRDS(as.data.frame(exposure_seconds_per_participant_day), file.path(data_dir, OUT_NAME))
