#######################################################################################
#                                                                                     #
#   Exposure to ideal appearance content on TikTok and body satisfaction              #
#   in female adolescents                                                             #
#                                                                                     #
#   EXPOSURE 2: daily exposure in the MORNING window (morning prompt -> 03:00 next    #
#     day): start-of-day body satisfaction (morning prompt) -> exposure from the      #
#     morning prompt until 02:59 UTC the next calendar day ("03:59 BST")              #
#                                                                                     #
#   The window, the substituted starts on days without an answered morning prompt,    #
#   the start-of-day body satisfaction and the "most recent EMA occasion" covariates  #
#   come from participant_day_dataset_morning.rds (script 05) -- the same file the    #
#   metadata morning builder reads -- so every morning analysis uses one window.      #
#   This script adds the exposure, exactly as 01_build_computer_vision_exposure_      #
#   evening.R does for the evening window.                                            #
#                                                                                     #
#   Reads: participant_day_dataset_morning.rds,                                       #
#          tiktok_viewing_events_with_watch_duration.rds (Processed_data),            #
#          SPINE_watched_stills_Scout_labelled.parquet (1_Data/Processed_data)        #
#   Writes to Processed_data: daily_exposure_morning_clothing_anyposing_thin.rds      #
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

### The participant-day dataset for the morning window (one row per participant and study day):
# window_start (the morning prompt, or a substituted start), window_end (03:00 UTC the next day),
# start-of-day body satisfaction (sses_am), the "most recent EMA occasion" covariates (lag_*),
# donation coverage -- all built by 01_preprocessing/05_build_participant_day_dataset_morning.R,
# the same file the metadata morning builder reads
grid <- readRDS(file.path(data_dir, "participant_day_dataset_morning.rds"))
grid$id <- as.character(grid$id)
stopifnot(!anyNA(grid$window_start), !anyNA(grid$window_end), all(grid$window_start < grid$window_end))
nrow(grid)   # 924
table(grid$window_start_source)                                # observed morning prompts vs substituted starts
colSums(!is.na(grid[grep("^lag_", names(grid))]))              # days with a most-recent-occasion record


### Viewing events with their estimated watch duration; study day as in 01_build_computer_vision_exposure_evening.R
events <- readRDS(file.path(data_dir, "tiktok_viewing_events_with_watch_duration.rds")) %>% as.data.frame()
events$id   <- as.character(events$id)
events$date <- as_date(events$tiktok_datetime - hours(3))
stopifnot(!anyNA(events$viewing_event_duration_sec))


### Still-level content labels (same checks and derived columns as in 01_build_computer_vision_exposure_evening.R)
stills <- read_parquet(spine_file)
stopifnot(all(stills$content_type %in% c("video", "slide")))
stopifnot(all(stills$seconds_of_presentation[stills$content_type == "video"] == 1L))
stopifnot(all(stills$seconds_of_presentation[stills$content_type == "slide"] == SLIDE_SEC))
stopifnot(all(stills$thin_sexualised_female_body_still %in% c("yes", "no")))
stopifnot(min(stills$still_number) == 0L)
stills$still_sec0 <- ifelse(stills$content_type == "slide", stills$still_number * SLIDE_SEC, stills$still_number)



#######################################################################################################
#######################################################################################################
#######################################################################################################
# DAILY EXPOSURE IN THE MORNING WINDOW: CLOTHING + ANY POSING + THIN
# (identical computation to 01_build_computer_vision_exposure_evening.R, with the morning windows)

LABEL_COLUMN <- "thin_sexualised_female_body_still"
POSITIVE     <- "yes"
OUT_NAME     <- "daily_exposure_morning_clothing_anyposing_thin.rds"


### Viewing events inside each day's window
events_in_window <- grid %>%
                    select(id, date, window_start, window_end, donation_covered) %>%
                    inner_join(events, by = c("id", "date"), relationship = "one-to-many") %>%
                    subset(tiktok_datetime > window_start & tiktok_datetime < window_end) %>%
                    mutate(available = video_id %in% unique(stills$video_id),
                           event     = row_number())
table(events_in_window$available)


### Classified videos: seconds watched and qualifying seconds per event
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

seconds_per_day_available_videos <- seconds_per_video_viewing %>%
                 group_by(id, date) %>%
                 summarise(watched_seconds_available = sum(watched_seconds),
                           thin_sexualised_female_bodies_seconds_available = sum(thin_sexualised_female_bodies_seconds),
                           .groups = "drop") %>%
                 mutate(day_proportion = if_else(watched_seconds_available > 0, thin_sexualised_female_bodies_seconds_available / watched_seconds_available, NA_real_))


### Content-probability substitution (tiers as in 01_build_computer_vision_exposure_evening.R)
participant_proportion <- seconds_per_day_available_videos %>%
                          group_by(id) %>%
                          summarise(participant_proportion = if_else(sum(watched_seconds_available) > 0,
                                                                     sum(thin_sexualised_female_bodies_seconds_available) / sum(watched_seconds_available), NA_real_))
sample_proportion <- sum(seconds_per_day_available_videos$thin_sexualised_female_bodies_seconds_available) / sum(seconds_per_day_available_videos$watched_seconds_available)

events_to_substitute <- bind_rows(events_in_window %>% subset(!available),
                         events_in_window %>% subset(available) %>% anti_join(seconds_per_video_viewing, by = "event"))

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


### Daily exposure on the full participant-day dataset (morning window)
exposure_seconds_per_participant_day <- grid %>%
         left_join(seconds_per_day_available_videos,    by = c("id", "date")) %>%
         left_join(seconds_per_day_unavailable_videos, by = c("id", "date")) %>%
         mutate(across(c(watched_seconds_available, thin_sexualised_female_bodies_seconds_available, watched_seconds_substituted, thin_sexualised_female_bodies_seconds_substituted, n_unavailable_videos),
                       ~ replace_na(.x, 0)),
                watched_seconds = watched_seconds_available + watched_seconds_substituted,
                thin_sexualised_female_bodies_seconds = thin_sexualised_female_bodies_seconds_available + thin_sexualised_female_bodies_seconds_substituted,
                thin_sexualised_female_bodies_seconds = if_else(donation_covered, thin_sexualised_female_bodies_seconds, NA_real_),
                watched_seconds = if_else(donation_covered, watched_seconds, NA_real_))

stopifnot(nrow(exposure_seconds_per_participant_day) == nrow(grid))
stopifnot(all(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds <= exposure_seconds_per_participant_day$watched_seconds, na.rm = TRUE))

sum(is.na(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds))               # uncovered days (16)
sum(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds == 0, na.rm = TRUE)   # days with no qualifying exposure
sum(!is.na(exposure_seconds_per_participant_day$sses_am) & !is.na(exposure_seconds_per_participant_day$thin_sexualised_female_bodies_seconds))   # complete-case days of the morning analysis (manuscript: 615)

# Save
saveRDS(as.data.frame(exposure_seconds_per_participant_day), file.path(data_dir, OUT_NAME))
