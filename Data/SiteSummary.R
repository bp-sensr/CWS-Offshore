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
library(lubridate)
library(hms)

# ---- 1. Load data ------------------------------------------
#this is the raw 
df <- read_csv(
  "Data/GUANO_Complete_Metadata.csv",
  show_col_types = FALSE
)

# ---- 2. Get year and month ------------------------------------

df <- df %>%
  extract(
    filename,
    into = c("date_str", "time_str"),
    regex = "_(\\d{8})_(\\d{6})",   # grab yyyymmdd_hhmmss wherever it sits; ignore trailing junk
    remove = FALSE
  ) %>%
  mutate(
    datetime = ymd_hms(paste(date_str, time_str)),
    Date  = as_date(datetime),
    Time  = as_hms(datetime),
    year  = year(datetime),
    month = month(datetime)
  ) %>%
  select(-date_str, -time_str)

# ---- 3. Expand comma-separated species ---------------------
# Split "LASCIN, MYLU" into one row per species, trimming whitespace.
# This increases row count where multiple species were identified.

dat_expanded <- df %>%
  filter(
    !is.na(Site),
    !is.na(Time)
  ) %>%
  mutate(
    # Use the manual ID when present; otherwise fall back to the auto ID
    species_source = if_else(
      is.na(`Species Manual ID`) | str_trim(`Species Manual ID`) == "",
      `Species Auto ID`,
      `Species Manual ID`
    )
  ) %>%
  mutate(species_list = str_split(species_source, ",")) %>%
  unnest(species_list) %>%
  mutate(species_list = str_trim(species_list)) %>%
  filter(species_list != "", !is.na(species_list))

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

id_cols <- c("Site", "Year", "Month")   # pinned at the front

# Groups to force to the end, in the order you want them to appear
end_targets <- c("40kMyo", "HIGH FREQUENCY", "LOW FREQUENCY",
                 "NoID", "FB", "SC", "NOISE")

all_cols <- names(summary_wide)

# Resolve targets to the real column names (case-insensitive), keeping your order
end_cols <- all_cols[match(tolower(end_targets), tolower(all_cols))]
end_cols <- end_cols[!is.na(end_cols)]        # drop any not present

# Everything else (not id, not end) gets alphabetized
species_cols <- sort(setdiff(all_cols, c(id_cols, end_cols)))

summary_wide <- summary_wide %>%
  select(all_of(c(id_cols, species_cols, end_cols)))


summary_wide <- summary_wide %>%
  mutate(Noise = NOISE + Noise) %>%   # add the two together
  select(-NOISE) %>%                  # drop the uppercase version
  relocate(Noise, .after = last_col()) 
# ---- 8. Save -----------------------------------------------

write_csv(summary_wide, "Data/species_summary_by_site_month_year.csv")

message("Done. Rows written: ", nrow(summary_wide))
print(summary_wide)