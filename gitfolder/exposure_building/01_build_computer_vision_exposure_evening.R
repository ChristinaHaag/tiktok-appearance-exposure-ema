#######################################################################################
#                                                                                     #
#   Exposure to ideal appearance content on TikTok and body satisfaction              #
#   in female adolescents                                                             #
#                                                                                     #
#   EXPOSURE 1: daily exposure time, evening window (03:00 UTC -> evening prompt)     #
#                                                                                     #
#   Exposure definition of the paper (Methods 4.10, "Confirmatory exposure            #
#   definition"): a still counts when it shows at least one female-presenting person  #
#   with minimal/tight clothing + posing of any kind + thin body build                #
#   (spine column thin_sexualised_female_body_still == "yes").                        #
#                                                                                     #
#   Manuscript: Methods 4.5 (daily exposure time), 4.6 (missing data),                #
#               4.11 (content-probability substitution)                               #
#                                                                                     #
#   Reads: participant_day_dataset_evening.rds,                                       #
#     tiktok_viewing_events_with_watch_duration.rds (Processed_data),                 #
#         SPINE_watched_stills_Scout_labelled.parquet (1_Data/Processed_data)         #
#   Writes to Processed_data: daily_exposure_evening_clothing_anyposing_thin.rds      #
#                                                                                     #
#######################################################################################


# Load packages
library(arrow); library(dplyr); library(tidyr); library(lubridate)

# Disable scientific notation of small values
options(scipen = 999)

# Folders
data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"
spine_file <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data/SPINE_watched_stills_Scout_labelled.parquet"

# Presentation duration of one slideshow image
SLIDE_SEC <- 3L



#######################################################################################################
#######################################################################################################
#######################################################################################################
# LOAD DATA

### The participant-day dataset (one row per participant and study day, with the H1 exposure window)
grid <- readRDS(file.path(data_dir, "participant_day_dataset_evening.rds"))
grid$id <- as.character(grid$id)
stopifnot(!anyNA(grid$window_end))
nrow(grid)   # 924


### Viewing events with their estimated watch duration
events <- readRDS(file.path(data_dir, "tiktok_viewing_events_with_watch_duration.rds")) %>% as.data.frame()
events$id <- as.character(events$id)
stopifnot(!anyNA(events$viewing_event_duration_sec))

# Study day of each event: a day runs from 03:00 UTC (= 04:00 BST) to 02:59 the next
# calendar day, so events between 00:00 and 03:00 belong to the previous date
events$date <- as_date(events$tiktok_datetime - hours(3))


### Still-level content labels (one row per still of every watched video)
stills <- read_parquet(spine_file)
nrow(stills)

# Check the label vocabulary the definitions rely on
stopifnot(all(stills$content_type %in% c("video", "slide")))
stopifnot(all(stills$seconds_of_presentation[stills$content_type == "video"] == 1L))
stopifnot(all(stills$seconds_of_presentation[stills$content_type == "slide"] == SLIDE_SEC))
stopifnot(all(stills$clothing_still_label %in% c("revealing", "not revealing", "indeterminate", "none", "refused")))
stopifnot(all(stills$any_posing_still_label %in% c("posing", "not posing", "indeterminate", "none", "refused")))
stopifnot(all(stills$sexual_posing_still_label %in% c("sexually suggestive", "not sexually suggestive", "indeterminate", "none", "refused")))
stopifnot(all(stills$thin_sexualised_female_body_still %in% c("yes", "no")))

# Second at which each still starts playing (still numbers are 0-based; slides advance every 3 s)
stopifnot(min(stills$still_number) == 0L)
stills$still_sec0 <- ifelse(stills$content_type == "slide", stills$still_number * SLIDE_SEC, stills$still_number)

table(stills$thin_sexualised_female_body_still)



#######################################################################################################
#######################################################################################################
#######################################################################################################
# VIEWING EVENTS INSIDE EACH DAY'S WINDOW

# Join every event to its study day and keep those between window start (03:00) and window end
# (evening prompt start, or the substituted end on days without an evening prompt)
events_in_window <- grid %>%
                    select(id, date, window_start, window_end, donation_covered) %>%
                    inner_join(events, by = c("id", "date"), relationship = "one-to-many") %>%
                    subset(tiktok_datetime > window_start & tiktok_datetime < window_end) %>%
                    mutate(available = video_id %in% unique(stills$video_id),
                           event     = row_number())

nrow(events_in_window)
table(events_in_window$available)   # available = downloaded and classified; unavailable = substituted below



#######################################################################################################
#######################################################################################################
#######################################################################################################
# DAILY EXPOSURE: CLOTHING + ANY POSING + THIN (the paper's definition)

LABEL_COLUMN <- "thin_sexualised_female_body_still"
POSITIVE     <- "yes"
OUT_NAME     <- "daily_exposure_evening_clothing_anyposing_thin.rds"


### Classified videos: seconds watched and qualifying seconds per event
# A still counts in full when its start second falls inside the watched duration
# (a partially shown last slide counts fully; stills beyond the watched duration do not)
seconds_per_video_viewing <- events_in_window %>%
              subset(available) %>%
              select(event, id, date, video_id, viewing_event_duration_sec) %>%
              inner_join(stills %>% select(video_id, seconds_of_presentation, still_sec0, label = all_of(LABEL_COLUMN)),
                         by = "video_id", relationship = "many-to-many") %>%
              subset(still_sec0 < viewing_event_duration_sec) %>%
              group_by(event, id, date) %>%
              summarise(watched_seconds = sum(seconds_of_presentation),
                        thin_sexualised_female_bodies_seconds = sum(seconds_of_presentation[label == POSITIVE]),
                        .groups = "drop")

# Per participant-day: watched seconds and qualifying seconds from classified videos
seconds_per_day_available_videos <- seconds_per_video_viewing %>%
                 group_by(id, date) %>%
                 summarise(watched_seconds_available = sum(watched_seconds),
                           thin_sexualised_female_bodies_seconds_available = sum(thin_sexualised_female_bodies_seconds),
                           .groups = "drop") %>%
                 mutate(day_proportion = if_else(watched_seconds_available > 0, thin_sexualised_female_bodies_seconds_available / watched_seconds_available, NA_real_))


### Content-probability substitution for videos that could not be classified (Methods 4.6, 4.11)
# Tier 1: the participant's own proportion of qualifying seconds among her classified videos that day
# Tier 2: her proportion across her whole donation period (only when nothing was classified that day)
# Tier 3: the sample proportion (only when a participant has no classified videos at all)
participant_proportion <- seconds_per_day_available_videos %>%
                          group_by(id) %>%
                          summarise(participant_proportion = if_else(sum(watched_seconds_available) > 0,
                                                                     sum(thin_sexualised_female_bodies_seconds_available) / sum(watched_seconds_available), NA_real_))
sample_proportion <- sum(seconds_per_day_available_videos$thin_sexualised_female_bodies_seconds_available) / sum(seconds_per_day_available_videos$watched_seconds_available)

# Events to substitute: unavailable videos, plus available videos whose stills all start beyond
# the watched duration (nothing to count -- treated like an unclassified video)
events_to_substitute <- bind_rows(events_in_window %>% subset(!available),
                         events_in_window %>% subset(available) %>% anti_join(seconds_per_video_viewing, by = "event"))
nrow(events_to_substitute)

seconds_per_day_unavailable_videos <- events_to_substitute %>%
                    left_join(seconds_per_day_available_videos %>% select(id, date, day_proportion), by = c("id", "date")) %>%
                    left_join(participant_proportion, by = "id") %>%
                    mutate(tier            = case_when(!is.na(day_proportion)         ~ "1_day",
                                                       !is.na(participant_proportion) ~ "2_participant",
                                                       TRUE                           ~ "3_sample"),
                           proportion_used = coalesce(day_proportion, participant_proportion, sample_proportion),
                           thin_sexualised_female_bodies_seconds_substituted = viewing_event_duration_sec * proportion_used)
table(seconds_per_day_unavailable_videos$tier)

seconds_per_day_unavailable_videos <- seconds_per_day_unavailable_videos %>%
                    group_by(id, date) %>%
                    summarise(thin_sexualised_female_bodies_seconds_substituted = sum(thin_sexualised_female_bodies_seconds_substituted),
                              watched_seconds_substituted = sum(viewing_event_duration_sec),
                              n_unavailable_videos = n(),
                              .groups = "drop")


### Daily exposure on the full grid
# Days with no events in the window are non-use days (exposure 0, as registered);
# days outside the donation period are NA (multiply imputed later)
exposure_seconds_per_participant_day <- grid %>%
         left_join(seconds_per_day_available_videos,    by = c("id", "date")) %>%
         left_join(seconds_per_day_unavailable_videos, by = c("id", "date")) %>%
         mutate(across(c(watched_seconds_available, thin_sexualised_female_bodies_seconds_available, watched_seconds_substituted, thin_sexualised_female_bodies_seconds_substituted, n_unavailable_videos),
                       ~ replace_na(.x, 0)),
                watched_seconds = watched_seconds_available + watched_seconds_substituted,
                thin_sexualised_female_bodies_seconds = thin_sexualised_female_bodies_seconds_available + thin_sexualised_female_bodies_seconds_substituted,
                thin_sexualised_female_bodies_seconds = if_else(donation_covered, thin_sexualised_female_bodies_seconds, NA_real_),
                watched_seconds = if_else(donation_covered, watched_seconds, NA_real_))

# Checks
stopifnot(nrow(exposure_seconds_per_participant_day) == nrow(grid))
stopifnot(all(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds <= exposure_seconds_per_participant_day$watched_seconds, na.rm = TRUE))

# Numbers for the manuscript (Results 2.1)
sum(is.na(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds))                                          # uncovered days (16)
sum(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds == 0, na.rm = TRUE)                              # days with no qualifying exposure
mean(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds / 60, na.rm = TRUE)                             # mean daily exposure in minutes (0.73)
median(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds / 60, na.rm = TRUE)                           # median (0.42)
mean(exposure_seconds_per_participant_day$watched_seconds / 60, na.rm = TRUE)             # mean watched minutes per day (85.1)
sum(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds_substituted, na.rm = TRUE) / sum(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds, na.rm = TRUE)   # share of exposure from substitution (29.0%)
table(exposure_seconds_per_participant_day$day_index[!is.na(exposure_seconds_per_participant_day$watched_seconds) & exposure_seconds_per_participant_day$watched_seconds > 0])   # days with use, by study day

# Save
saveRDS(as.data.frame(exposure_seconds_per_participant_day), file.path(data_dir, OUT_NAME))
