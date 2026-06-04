# ============================================================
# Summarize Species Manual ID counts by Site + Year + Month
#
# Handles:
#   - Comma-separated multi-species entries (e.g. "LASCIN,MYLU")
#     → each species gets its own count
#   - FB (Feeding Buzz) and SC (Social Call) treated as
#     their own categories, placed as the last 2 columns
#     in the wide output
#
# Input:  GUANO_Complete_Metadata_test.csv (or full dataset)
# Output: species_summary_by_site_month_year.csv  (wide format)
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)

# ---- 1. Load data ------------------------------------------

dat <- read_csv(
  "Data/GUANO_Complete_Metadata_test.csv",
  show_col_types = FALSE
)

# ---- 2. Parse timestamp ------------------------------------

dat <- dat %>%
  mutate(
    ts_parsed = ymd_hms(Timestamp, tz = "UTC"),
    Year      = year(ts_parsed),
    Month     = month(ts_parsed, label = TRUE, abbr = FALSE)  # e.g. "June"
  )

# ---- 3. Expand comma-separated species ---------------------
# Split "LASCIN, MYLU" into one row per species, trimming whitespace.
# This increases row count where multiple species were identified.

dat_expanded <- dat %>%
  filter(
    !is.na(Site),
    !is.na(ts_parsed),
    !is.na(`Species Manual ID`),
  ) %>%
  mutate(species_list = str_split(`Species Manual ID`, ",")) %>%
  unnest(species_list) %>%
  mutate(species_list = str_trim(species_list)) %>%  # remove stray whitespace
  filter(species_list != "")                          # drop any empty tokens

# ---- 4. Separate regular species from FB / SC --------------

species_regular <- dat_expanded %>%
  filter(!species_list %in% c("FB", "SC"))

species_fb_sc <- dat_expanded %>%
  filter(species_list %in% c("FB", "SC"))

# ---- 5. Count regular species ------------------------------

summary_regular <- species_regular %>%
  count(Site, Year, Month, species_list, name = "n_recordings") %>%
  pivot_wider(
    names_from  = species_list,
    values_from = n_recordings,
    values_fill = 0
  )

# ---- 6. Count FB and SC ------------------------------------

summary_fb_sc <- species_fb_sc %>%
  count(Site, Year, Month, species_list, name = "n_recordings") %>%
  pivot_wider(
    names_from  = species_list,
    values_from = n_recordings,
    values_fill = 0
  ) %>%
  # Ensure both columns exist even if one call type is absent in data
  {
    if (!"FB" %in% names(.)) mutate(., FB = 0L) else .
  } %>%
  {
    if (!"SC" %in% names(.)) mutate(., SC = 0L) else .
  }

# ---- 7. Join and place FB / SC as last 2 columns -----------

summary_wide <- summary_regular %>%
  left_join(summary_fb_sc, by = c("Site", "Year", "Month")) %>%
  # Replace NAs from the join (sites/months with no FB or SC)
  mutate(
    FB = replace_na(FB, 0),
    SC = replace_na(SC, 0)
  ) %>%
  arrange(Site, Year, Month)

# Move Noise and NoID after Month, FB and SC last
summary_wide <- summary_wide %>%
  select(
    Site, Year, Month,
    any_of(c("Noise", "NoID")),   # first after Month (if they exist)
    -FB, -SC,                      # exclude FB/SC from their current position
    everything(),                  # remaining species columns
    FB, SC                         # FB/SC last
  )

# ---- 8. Save -----------------------------------------------

write_csv(summary_wide, "species_summary_by_site_month_year.csv")

message("Done. Rows written: ", nrow(summary_wide))
print(summary_wide)