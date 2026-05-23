# ================================================================================================================================================================
# Does public funding drive electric vehicle charger rollout? Evidence from English local authorities between 2020-2025 and implications for equitable allocation.
# ================================================================================================================================================================
#
#
# OVERVIEW
# --------
# This script reproduces the main variables found in our study, and is the basis for all analysis. All figures included in the study were derived from this code, but excluded for conciseness
# All data sources need to be downloaded from Appendix Table 1
# =============================================================================
# Defining working directory
# =============================================================================
#Set working directory to personal device
DATA <- 
OUT  <- 

dir.create(file.path(OUT, "tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "panel"),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "models"),  showWarnings = FALSE, recursive = TRUE)
# =============================================================================
# Packages download
# =============================================================================

required_pkgs <- c(
  "tidyverse", "fixest", "plm", "modelsummary", "sf", "spdep",
  "lpSolve", "lmtest", "janitor", "readxl", "readODS", "lubridate",
  "broom", "scales", "zoo", "car", "rvest"
)

for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =============================================================================
# LA matching
# =============================================================================

# This code standardises LA names for matching across DfT, ONS, NGED, IMD,
# and LEVI datasets. Strips title prefixes, such as 'Borough of', suffixes such as 'council', and punctuation, then
# removes spaces.
clean_la_name <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\blondon borough of\\b",   "") |>
    str_replace_all("\\bthe royal borough of\\b","") |>
    str_replace_all("\\broyal borough of\\b",    "") |>
    str_replace_all("\\bcity of\\b",             "") |>
    str_replace_all("\\bborough of\\b",          "") |>
    str_replace_all("\\bcouncil\\b",             "") |>
    str_replace_all("\\bborough\\b",             "") |>
    str_replace_all("\\bcounty\\b",              "") |>
    str_replace_all("\\bcity\\b",                "") |>
    str_replace_all("\\bdistrict\\b",            "") |>
    str_replace_all("\\bmetropolitan\\b",        "") |>
    str_replace_all("\\bdc\\b",                  "") |>
    str_replace_all("\\blbc\\b",                 "") |>
    str_replace_all("\\bmbc\\b",                 "") |>
    str_replace_all("[[:punct:]]",               "") |>
    str_squish()
}
# =============================================================================
# Data processing
# =============================================================================
# -----------------------------------------------------------------------------
# Data processing for the dependent variable (charger count)
# -----------------------------------------------------------------------------
# manually defines charger dates
charger_dates <- c(
  "Oct-19", "Jan-20", "Apr-20", "Jul-20", "Oct-20",
  "Jan-21", "Apr-21", "Jul-21", "Oct-21",
  "Jan-22", "Apr-22", "Jul-22", "Oct-22",
  "Jan-23", "Apr-23", "Jul-23", "Oct-23",
  "Jan-24", "Apr-24", "Jul-24", "Oct-24",
  "Jan-25", "Apr-25", "Jul-25"
)
# reads raw data file
chargers_raw <- read_ods(
  paste0(DATA, "electric-vehicle-public-charging-infrastructure-statistics-july-2025__1_.ods"),
  sheet = "1a", skip = 2, col_names = FALSE
)
# assigns column names
names(chargers_raw) <- c("la_code", "la_name", charger_dates)
# data cleaning, also removes non-English rows and non-LA rows
chargers <- chargers_raw |>
  slice(-1) |>                                              
  filter(str_starts(la_code, "E0[6-9]")) |>                 
  pivot_longer(-c(la_code, la_name),
               names_to = "period", values_to = "n_total") |>
  mutate(
    n_total   = suppressWarnings(as.numeric(
      str_replace_all(n_total, "[,c\\[\\]-]", ""))),
    period_dt = parse_date_time(period, "b-y"),                                 # converts date strings into a real date, extracting year and quarter
    year      = year(period_dt),
    quarter   = quarter(period_dt)
  ) |>
  filter(!is.na(period_dt), !is.na(n_total),
         year >= 2020, year <= 2025,
         !(year == 2025 & quarter == 4)) |>
  group_by(la_code, la_name, year, quarter) |>                                  # this makes sure there's only one observation per LA per quarter, keeping the larger value if more than one is present
  summarise(n_total = max(n_total, na.rm = TRUE), .groups = "drop") |>
  arrange(la_code, year, quarter) |>                                            # this calculates new charger installed per quarter by subtracting last quarter's stock from current quarter
  group_by(la_code) |>
  mutate(new_chargers = pmax(0, n_total - dplyr::lag(n_total, 1, default = 0))) |> # pmax sets negative values to zero
  ungroup()

# creates lookup table for LAs, used to match LAs across datasets
la_name_lookup <- chargers |>                                
  distinct(la_code, la_name) |>
  mutate(name_clean = clean_la_name(la_name))

# -----------------------------------------------------------------------------
# Data processing for the variable lagged EV stock
# -----------------------------------------------------------------------------
# This code includes all plug-in vehicles (BEV, Plug-in petrol, Plug-in diesel)

# reads the data file
ev_raw <- read_ods(
  paste0(DATA, "veh0142__1_.ods"),
  sheet = "VEH0142", skip = 4
) |>
  janitor::clean_names()
# filters the data to only include battery electric and plug-in cars
ev_stock <- ev_raw |>
  filter(body_type == "Cars",
         fuel %in% c("BATTERY ELECTRIC",
                     "PLUG-IN HYBRID ELECTRIC (DIESEL)",
                     "PLUG-IN HYBRID ELECTRIC (PETROL)"),
         keepership == "Total",                                                 # keeps total number of vehicles, rather than separating public and private ownership
         str_starts(ons_code, "E0[6-9]")) |>                                    # keeps only English LAs
  rename(la_code = ons_code) |>                                                 # renames LA codes so they match rest of the data
  mutate(across(matches("^x\\d{4}_q\\d$"), as.character)) |>
  pivot_longer(matches("^x\\d{4}_q\\d$"),
               names_to = "period", values_to = "ev_count") |>
  mutate(                                                                       # reshapes data
    year     = as.integer(str_extract(period, "(?<=x)\\d{4}")),
    quarter  = as.integer(str_extract(period, "(?<=q)\\d")),
    ev_count = suppressWarnings(as.numeric(
      str_replace_all(ev_count, "[,\\[\\]c-]", "")))
  ) |>
  filter(!is.na(ev_count)) |>
  group_by(la_code, year, quarter) |>                                           
  summarise(ev_stock = sum(ev_count, na.rm = TRUE), .groups = "drop") |>
  arrange(la_code, year, quarter) |>                                            # cleans EV count values and converts them to numbers 
  group_by(la_code) |>
  mutate(ev_stock_lag1 = dplyr::lag(ev_stock, 1)) |>                            # creates lagged EV stock variable
  ungroup()
# -----------------------------------------------------------------------------
# Population variable
# -----------------------------------------------------------------------------

# extracts population data from excel file
pop_2011_2022 <- read_xlsx(
  paste0(DATA, "myebtablesenglandwales20112022v3.xlsx"),
  sheet = "MYEB2 (2023 Geography)", skip = 1
) |>
  clean_names() |>
  filter(str_starts(ladcode23, "E"), !str_starts(ladcode23, "E12")) |>          # removes Welsh regions and English regions, rather than LAs
  pivot_longer(matches("^population_\\d{4}$"),                                  # reshapes data
               names_to = "year_str", values_to = "pop") |>
  group_by(ladcode23, year_str) |>
  summarise(population = sum(pop, na.rm = TRUE), .groups = "drop") |>           # sums population for each LA quarter
  mutate(year = as.integer(str_extract(year_str, "\\d{4}"))) |>
  rename(la_code = ladcode23) |>
  select(la_code, year, population)
# reads 2023 file
pop_2023 <- read_xlsx(paste0(DATA, "mye23tablesew.xlsx"),
                       sheet = "MYE2 - Persons", skip = 7) |>
  clean_names() |>
  filter(str_starts(code, "E"), !str_starts(code, "E12")) |>
  select(la_code = code, population = all_ages) |>
  mutate(year = 2023L, population = as.numeric(population))
# reads 2024 file
pop_2024 <- read_xlsx(paste0(DATA, "mye24tablesew.xlsx"),
                       sheet = "MYE2 - Persons", skip = 7) |>
  clean_names() |>
  filter(str_starts(code, "E"), !str_starts(code, "E12")) |>
  select(la_code = code, population = all_ages) |>
  mutate(year = 2024L, population = as.numeric(population))
# combines all years
population_ts <- bind_rows(pop_2011_2022, pop_2023, pop_2024) |>
  arrange(la_code, year)

# -----------------------------------------------------------------------------
# Variable construction for main independent variable
# -----------------------------------------------------------------------------
# removes non-LA rows
regional_aggregate_rows <- c(
  "North East", "North West", "Yorkshire and The Humber",
  "Yorkshire And The Humber", "Yorkshire and the Humber",
  "East Midlands", "West Midlands", "East of England",
  "London", "South East", "South West",
  "Wales", "Scotland", "Northern Ireland", "Total"
)
# loads and cleans excel file
orcs_annual <- read_ods(
    paste0(DATA, "electric-vehicle-charging-device-grant-scheme-statistics-july-2025.ods"),
    sheet = "9", skip = 2
  ) |>
  clean_names() |>
  rename_with(~ str_replace(.x, "_note_\\d+", "")) |>
  rename_with(~ str_replace(.x, "financial_year_ending_", "")) |>
  rename(la_name_orcs = region_local_authority_or_council) |>
  filter(!la_name_orcs %in% regional_aggregate_rows,
         !is.na(la_name_orcs)) |>
  pivot_longer(matches("^fye_\\d{4}"),
               names_to = "year_str", values_to = "orcs_grant_gbp") |>
  mutate(                                                                       # convert from FYE to calendar year
    year_fye       = as.integer(str_extract(year_str, "\\d{4}")),
    year           = year_fye - 1,
    orcs_grant_gbp = replace_na(orcs_grant_gbp, 0)
  ) |>
  filter(orcs_grant_gbp > 0) |>                                                 # any missing values set to zero                                  
  select(la_name_orcs, year, orcs_grant_gbp)

# create mapping tables for ORCS allocation
post_2019_orcs_mapping <- tribble(
  ~la_name_orcs,                       ~la_code,
  "Harrogate Borough Council",          "E06000065",
  "Craven District Council",            "E06000065",
  "Ryedale District Council",           "E06000065",
  "Hambleton District Council",         "E06000065",
  "Richmondshire District Council",     "E06000065",
  "Scarborough Borough Council",        "E06000065",
  "Selby District Council",             "E06000065",
  "Somerset West and Taunton Council",  "E06000066",
  "Mendip District Council",            "E06000066",
  "South Somerset District Council",    "E06000066",
  "Sedgemoor District Council",         "E06000066"
)

county_to_districts <- tribble(
  ~county_name,                    ~district_codes,
  "Cambridgeshire County Council", c("E07000008","E07000009","E07000010",
                                     "E07000011","E07000012"),
  "Derbyshire County Council",     c("E07000032","E07000033","E07000034",
                                     "E07000035","E07000036","E07000037",
                                     "E07000038","E07000039","E07000040"),
  "Essex County Council",          c("E07000066","E07000067","E07000068",
                                     "E07000069","E07000070","E07000071",
                                     "E07000072","E07000073","E07000074",
                                     "E07000075","E07000076","E07000077"),
  "Hampshire County Council",      c("E07000084","E07000085","E07000086",
                                     "E07000087","E07000088","E07000089",
                                     "E07000090","E07000091","E07000092",
                                     "E07000093","E07000094"),
  "Kent County Council",           c("E07000105","E07000106","E07000107",
                                     "E07000108","E07000109","E07000110",
                                     "E07000111","E07000112","E07000113",
                                     "E07000114","E07000115","E07000116"),
  "Norfolk County Council",        c("E07000143","E07000144","E07000145",
                                     "E07000146","E07000147","E07000148",
                                     "E07000149"),
  "Oxfordshire County Council",    c("E07000178","E07000179","E07000180",
                                     "E07000181","E07000200"),
  "Surrey County Council",         c("E07000207","E07000208","E07000209",
                                     "E07000210","E07000211","E07000212",
                                     "E07000213","E07000214","E07000215",
                                     "E07000216","E07000217"),
  "Warwickshire County Council",   c("E07000218","E07000219","E07000220",
                                     "E07000221","E07000222"),
  "West Sussex County Council",    c("E07000223","E07000224","E07000225",
                                     "E07000226","E07000227","E07000228",
                                     "E07000229")
) |>
  unnest(district_codes)

# sums ORCS funding for direct LA name matches
direct_orcs <- orcs_annual |>
  mutate(name_clean = clean_la_name(la_name_orcs)) |>
  inner_join(la_name_lookup, by = "name_clean", relationship = "many-to-many") |>
  group_by(la_code, year) |>
  summarise(direct_grant_gbp = sum(orcs_grant_gbp), .groups = "drop")

# sums ORCS funding for post-2019 LA reorgansation structure
post_2019_orcs <- orcs_annual |>
  inner_join(post_2019_orcs_mapping, by = "la_name_orcs") |>
  group_by(la_code, year) |>
  summarise(post_2019_grant_gbp = sum(orcs_grant_gbp), .groups = "drop")

# sums county-allocated ORCS funding by population-weight
district_pops_2024 <- population_ts |>
  filter(year == 2024) |>
  select(la_code, population)

county_pop_weights <- county_to_districts |>
  left_join(district_pops_2024, by = c("district_codes" = "la_code")) |>
  group_by(county_name) |>
  mutate(pop_share = population / sum(population, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(pop_share))

county_orcs <- orcs_annual |>
  inner_join(county_pop_weights, by = c("la_name_orcs" = "county_name"),
             relationship = "many-to-many") |>
  mutate(allocated_grant_gbp = orcs_grant_gbp * pop_share,
         la_code             = district_codes) |>
  group_by(la_code, year) |>
  summarise(allocated_grant_gbp = sum(allocated_grant_gbp, na.rm = TRUE),
            .groups = "drop")
# combines all funding streams
combined_orcs <- direct_orcs |>
  full_join(county_orcs,    by = c("la_code", "year")) |>
  full_join(post_2019_orcs, by = c("la_code", "year")) |>
  mutate(
    direct_grant_gbp    = replace_na(direct_grant_gbp,    0),
    allocated_grant_gbp = replace_na(allocated_grant_gbp, 0),
    post_2019_grant_gbp = replace_na(post_2019_grant_gbp, 0),
    total_orcs_gbp      = direct_grant_gbp + allocated_grant_gbp + post_2019_grant_gbp
  )

# spreads annual grants across four quarters and calculates cumulative funding
all_la_quarters <- chargers |>
  distinct(la_code, year, quarter) |>
  arrange(la_code, year, quarter)

orcs_quarterly <- combined_orcs |>
  crossing(quarter = 1:4) |>
  mutate(orcs_q = total_orcs_gbp / 4) |>
  select(la_code, year, quarter, orcs_q)

orcs_cumulative <- all_la_quarters |>
  left_join(orcs_quarterly, by = c("la_code", "year", "quarter")) |>
  mutate(orcs_q = replace_na(orcs_q, 0)) |>
  arrange(la_code, year, quarter) |>
  group_by(la_code) |>
  mutate(orcs_cumulative_gbp = cumsum(orcs_q)) |>
  ungroup() |>
  select(la_code, year, quarter, orcs_cumulative_gbp)
# -----------------------------------------------------------------------------------------
# NGED grid headroom variable construction (East Midlands, West Midlands, South West only)
# -----------------------------------------------------------------------------------------
# reads file
nged_proportion <- read_csv(paste0(DATA, "ev_capacity_map_2.csv"),
                             show_col_types = FALSE) |>
  clean_names() |>
  filter(!is.na(lower_tier_local_authority),
         !is.na(capacity_description)) |>
  mutate(has_capacity = capacity_description %in%                               # creates a falsifiable variable, marking a substation as having available capacity if it falls into the top 2 categories
                         c("Extensive Capacity Available",
                           "Capacity Available")) |>
  group_by(lower_tier_local_authority) |>                                       # groups susbtations to LAs
  summarise(n_substations      = n(),
            n_with_capacity    = sum(has_capacity),
            grid_capacity_prop = n_with_capacity / n_substations,
            .groups = "drop") |>
  rename(la_name = lower_tier_local_authority)

nged_matched <- nged_proportion |>                                              # matching LAs to panel
  mutate(name_clean = clean_la_name(la_name)) |>
  inner_join(la_name_lookup, by = "name_clean", relationship = "many-to-many") |>
  distinct(la_code, .keep_all = TRUE) |>
  select(la_code, grid_capacity_prop, n_substations)
# -----------------------------------------------------------------------------
# IMD variable construction
# -----------------------------------------------------------------------------
# maps pre-2019 LA codes to current ones
post_2019_imd_mapping <- tribble(
  ~la_code,    ~old_codes,
  "E06000060", c("E07000004","E07000005","E07000006","E07000007"),
  "E06000061", c("E07000150","E07000152","E07000153","E07000156"),
  "E06000062", c("E07000151","E07000154","E07000155"),
  "E06000063", c("E07000026","E07000028","E07000029"),
  "E06000064", c("E07000027","E07000030","E07000031"),
  "E06000065", c("E07000164","E07000165","E07000166","E07000167",
                 "E07000168","E07000169"),
  "E06000066", c("E07000187","E07000188","E07000189","E07000246")
) |>
  unnest(old_codes)
# reads and cleans data 
imd_pre <- read_xlsx(
    paste0(DATA, "File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"),
    sheet = "IMD2019", skip = 0
  ) |>
  clean_names() |>
  group_by(local_authority_district_code_2019) |>
  summarise(imd_avg_rank = mean(index_of_multiple_deprivation_imd_rank,         # calculates average IMD score
                                 na.rm = TRUE),
            .groups = "drop") |>
  rename(la_code = local_authority_district_code_2019) |>
  filter(!is.na(la_code))

# same code function, but post-2019
imd_post_2019 <- post_2019_imd_mapping |>
  left_join(imd_pre, by = c("old_codes" = "la_code")) |>
  group_by(la_code) |>
  summarise(imd_avg_rank = mean(imd_avg_rank, na.rm = TRUE),
            .groups = "drop") |>
  filter(!is.na(imd_avg_rank))
# combines pre and post 2019
imd <- bind_rows(imd_pre, imd_post_2019) |>
  distinct(la_code, .keep_all = TRUE) |>
  mutate(imd_score = scale(max(imd_avg_rank) - imd_avg_rank + 1)[, 1])          # inverts and z-standardises IMD scores

# -----------------------------------------------------------------------------
# LA to region ONS lookup table
# -----------------------------------------------------------------------------
# read file, cleans table
region_lookup <- read_csv(
    paste0(DATA, "Local_Authority_District_to_Region__December_2024__Lookup_in_EN.csv"),
    show_col_types = FALSE
  ) |>
  clean_names() |>
  select(la_code = lad24cd, region = rgn24nm) |>
  distinct()

# =============================================================================
# Panel construction
# =============================================================================
#
# This section joins all data sources, EV stock, ORCS, grid headroom, population, LA code, and IMD, forward-filling 2025 population from 2024 mid-year estimates and rescaling funding to per-£10,000 and EV stock to per-1,000 vehicles for coefficient interpretability 
panel <- chargers |>
  left_join(ev_stock |> select(la_code, year, quarter, ev_stock_lag1),          
            by = c("la_code", "year", "quarter")) |>
  left_join(orcs_cumulative,                       by = c("la_code", "year", "quarter")) |>
  left_join(nged_matched |> select(la_code, grid_capacity_prop), by = "la_code") |>
  left_join(population_ts,                         by = c("la_code", "year")) |>
  left_join(region_lookup,                         by = "la_code") |>
  left_join(imd |> select(la_code, imd_score),     by = "la_code") |>
  mutate(orcs_cumulative_gbp = replace_na(orcs_cumulative_gbp, 0),
         panel_id            = la_code,
         time_id             = year * 10 + quarter) |>
  distinct(la_code, year, quarter, .keep_all = TRUE) |>
  filter(!is.na(ev_stock_lag1)) |>
  group_by(la_code) |>
  arrange(year, quarter) |>
  mutate(population = na.locf(population, na.rm = FALSE)) |>      # forward-fill 2025
  ungroup() |>
  mutate(orcs_per_10k = orcs_cumulative_gbp / 10000,
         ev_per_1k    = ev_stock_lag1       / 1000,
         quarter_num  = (year - 2019) * 4 + quarter - 3)          # creates a continuous quarter number

# spatial lag construction (queen contiguity)
la_shp_panel <- st_read(paste0(DATA, "LAD_MAY_2024_UK_BFE.shp"), quiet = TRUE) |>
  filter(str_starts(LAD24CD, "E"),
         LAD24CD %in% unique(panel$la_code)) |>
  arrange(LAD24CD)

W_queen   <- nb2listw(poly2nb(la_shp_panel, queen = TRUE),
                      style = "W", zero.policy = TRUE)
time_vals <- sort(unique(panel$time_id))

# computes spatial lag of total chargers for any given spatial weights matrix
compute_spatial_lag <- function(W) {
  lags <- vector("list", length(time_vals))
  for (q in seq_along(time_vals)) {
    snapshot <- panel |>
      filter(time_id == time_vals[q]) |>
      arrange(la_code)
    snapshot <- snapshot[match(la_shp_panel$LAD24CD, snapshot$la_code), ]       
    n_vec <- snapshot$n_total          
    n_vec[is.na(n_vec)] <- 0
    lags[[q]] <- data.frame(
      la_code = la_shp_panel$LAD24CD,
      time_id = time_vals[q],
      nbr_lag = lag.listw(W, n_vec, zero.policy = TRUE)                         # calculates neighbour charger value and stores it
    )
  }
  do.call(rbind, lags)
}
# adds spatial value to panel and joins them
panel <- panel |>
  left_join(compute_spatial_lag(W_queen) |>
              rename(neighbour_chargers = nbr_lag),
            by = c("la_code", "time_id")) |>
  filter(!is.na(neighbour_chargers))
# saves final analysis panel to prevent re-running entire script
saveRDS(panel, file.path(OUT, "panel/panel_main.rds"))
# =============================================================================
# Main model construction
# =============================================================================
#
# M1: Two-way FE on the full panel
m1 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers
            | panel_id + time_id,
            data = panel, cluster = ~panel_id)
# M3: LA FE only with log population 
m3 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + log(population) +
              neighbour_chargers
            | panel_id,
            data = panel, cluster = ~panel_id)
# M4: Poisson PPML with population offset 
m4 <- fepois(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers +
               offset(log(population))
             | panel_id + time_id,
             data    = panel |> filter(!is.na(population), population > 0),
             cluster = ~panel_id)
# M5: M1 re-estimated excluding London 
m5 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers
            | panel_id + time_id,
            data    = panel |> filter(region != "London"),
            cluster = ~panel_id)
# M_London: within-London only
m_london <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers
                  | panel_id + time_id,
                  data    = panel |> filter(region == "London"),
                  cluster = ~panel_id)

main_models <- list(m1 = m1, m3 = m3, m4 = m4, m5 = m5, m_london = m_london)
saveRDS(main_models, file.path(OUT, "models/main_models.rds"))
# =============================================================================
# Interaction model construction
# =============================================================================
# create NGED subsample
panel_nged <- panel |>
  filter(!is.na(grid_capacity_prop)) |>
  mutate(grid_capacity_z = scale(grid_capacity_prop)[, 1],                      # z standardises grid capacity, with 0 meaning average capacity, positive values meaning above average, and negative values indicating below average
         grid_high       = grid_capacity_prop >
                            median(grid_capacity_prop, na.rm = TRUE))
# estimates the standard two-way FE model, but only for NGED subsample
m_nged_1 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers
                  | panel_id + time_id,
                  data = panel_nged, cluster = ~panel_id)
# continuous grid headroom interaction model
m_nged_2 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers +
                    orcs_per_10k:grid_capacity_z
                  | panel_id + time_id,
                  data = panel_nged, cluster = ~panel_id)
# binary grid headroom interaction model
m_nged_3 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers +
                    orcs_per_10k:grid_high
                  | panel_id + time_id,
                  data = panel_nged, cluster = ~panel_id)

# create the IMD sample
panel_imd <- panel |>
  filter(!is.na(imd_score)) |>
  mutate(imd_high = imd_score > median(imd_score, na.rm = TRUE))                # creates IMD variable
# estimates the standard two-way FE model, but only for IMD
m_imd_1 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers +
                   orcs_per_10k:imd_score
                 | panel_id + time_id,
                 data = panel_imd, cluster = ~panel_id)
# continuous IMD interaction
m_imd_2 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers +
                   orcs_per_10k:imd_high
                 | panel_id + time_id,
                 data = panel_imd, cluster = ~panel_id)
# binary IMD interaction
m_imd_3 <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers +
                   orcs_per_10k:imd_score
                 | panel_id + time_id,
                 data    = panel_imd |> filter(region != "London"),
                 cluster = ~panel_id)

interaction_models <- list(
  m_nged_1 = m_nged_1, m_nged_2 = m_nged_2, m_nged_3 = m_nged_3,
  m_imd_1  = m_imd_1,  m_imd_2  = m_imd_2,  m_imd_3  = m_imd_3
)
saveRDS(interaction_models, file.path(OUT, "models/interaction_models.rds"))

# =============================================================================
# Dynamic robustness checks
# =============================================================================
# event study
# find first ORCS funded quarter for each LA
first_orcs <- panel |>
  filter(orcs_cumulative_gbp > 0) |>
  group_by(la_code) |>
  summarise(first_orcs_quarter = min(quarter_num), .groups = "drop")
# joining LA's first funded quarter to rest of panel
panel_es <- panel |>
  left_join(first_orcs, by = "la_code") |>
  mutate(event_time = if_else(is.na(first_orcs_quarter),
                               NA_integer_,
                               as.integer(quarter_num - first_orcs_quarter))) |>
  filter(!is.na(event_time), event_time >= -6, event_time <= 6)
# estimates the event study model
m_es <- feols(new_chargers ~ i(event_time, ref = -1) + ev_per_1k + neighbour_chargers
              | panel_id + time_id,
              data = panel_es, cluster = ~panel_id)

# identify pre-treatment leads (Wald test - parallel trends)
es_pre_terms <- broom::tidy(m_es) |>
  filter(str_detect(term, "event_time")) |>
  mutate(event_time = as.numeric(str_extract(term, "(?<=::)-?\\d+"))) |>
  filter(!is.na(event_time), event_time < -1) |>
  pull(term)

wald_pre <- if (length(es_pre_terms) > 0) {
  linearHypothesis(m_es, es_pre_terms, vcov = vcov(m_es))
} else NULL

# distributed lag model (estimates 9 quarterly flow lags)
panel_dl <- panel |>
  arrange(la_code, year, quarter) |>
  group_by(la_code) |>
  mutate(orcs_flow    = orcs_cumulative_gbp -                                   # creates lagged funding flows
                         dplyr::lag(orcs_cumulative_gbp, 1, default = 0),
         orcs_flow_l1 = dplyr::lag(orcs_flow, 1, default = 0),
         orcs_flow_l2 = dplyr::lag(orcs_flow, 2, default = 0),
         orcs_flow_l3 = dplyr::lag(orcs_flow, 3, default = 0),
         orcs_flow_l4 = dplyr::lag(orcs_flow, 4, default = 0),
         orcs_flow_l5 = dplyr::lag(orcs_flow, 5, default = 0),
         orcs_flow_l6 = dplyr::lag(orcs_flow, 6, default = 0),
         orcs_flow_l7 = dplyr::lag(orcs_flow, 7, default = 0),
         orcs_flow_l8 = dplyr::lag(orcs_flow, 8, default = 0)) |>
  ungroup()
# estimates distributed lags model
m_dl <- feols(new_chargers ~ orcs_flow + orcs_flow_l1 + orcs_flow_l2 +
                orcs_flow_l3 + orcs_flow_l4 + orcs_flow_l5 + orcs_flow_l6 +
                orcs_flow_l7 + orcs_flow_l8 + ev_per_1k + neighbour_chargers
              | panel_id + time_id,
              data = panel_dl, cluster = ~panel_id)

# Arellano-Bond dynamic panel
panel_plm <- pdata.frame(panel, index = c("panel_id", "time_id"))

m_ab <- tryCatch({
  pgmm(new_chargers ~ lag(new_chargers, 1) + ev_per_1k + orcs_per_10k +
         neighbour_chargers | lag(new_chargers, 2:3),
       data           = panel_plm,
       effect         = "individual",
       model          = "twosteps",
       transformation = "d")
})

dynamic_models <- list(
  m_es     = m_es,
  m_dl     = m_dl,
  m_ab     = m_ab,
  wald_pre = wald_pre
)
saveRDS(dynamic_models, file.path(OUT, "models/dynamic_models.rds"))

# =============================================================================
# Diagnostic tests
# =============================================================================
# recreate M1 using plm
fe_mod <- plm(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers,
              data = panel_plm, model = "within", effect = "twoways")
# estimate random FE for comparison
re_mod <- plm(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers,
              data = panel_plm, model = "random", effect = "twoways")
# estimate a rest-of-England FE model
fe_mod_rest <- plm(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers,
                   data   = pdata.frame(panel |> filter(region != "London"),
                                        index = c("panel_id", "time_id")),
                   model  = "within", effect = "twoways")

diagnostics <- list(
  hausman           = phtest(fe_mod, re_mod),
  bg_serial         = pbgtest(fe_mod, order = 2),
  pesaran_cd        = pcdtest(fe_mod, test = "cd"),
  bp_hetero         = bptest(new_chargers ~ ev_per_1k + orcs_per_10k +
                              neighbour_chargers, data = panel),
  driscoll_kraay    = coeftest(fe_mod,
                               vcov = vcovSCC(fe_mod, type = "HC3", maxlag = 4)),
  driscoll_kraay_m5 = coeftest(fe_mod_rest,
                               vcov = vcovSCC(fe_mod_rest, type = "HC3", maxlag = 4))
)
saveRDS(diagnostics, file.path(OUT, "models/diagnostics.rds"))

# =============================================================================
# Spatial weights robustness
# =============================================================================
#
# Re-estimates M5 under three alternative weights schemes.
coords  <- st_centroid(la_shp_panel) |> st_coordinates()
# k nearest neighbour
W_knn   <- nb2listw(knn2nb(knearneigh(coords, k = 5)),
                    style = "W", zero.policy = TRUE)
# 50km distance
W_dist  <- nb2listw(dnearneigh(coords, 0, 50000),
                    style = "W", zero.policy = TRUE)

panel_knn <- panel |>
  select(-neighbour_chargers) |>
  left_join(compute_spatial_lag(W_knn) |>
              rename(neighbour_chargers = nbr_lag),
            by = c("la_code", "time_id")) |>
  filter(!is.na(neighbour_chargers))

panel_dist <- panel |>
  select(-neighbour_chargers) |>
  left_join(compute_spatial_lag(W_dist) |>
              rename(neighbour_chargers = nbr_lag),
            by = c("la_code", "time_id")) |>
  filter(!is.na(neighbour_chargers))

m5_queen <- m5
m5_knn   <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers
                  | panel_id + time_id,
                  data    = panel_knn  |> filter(region != "London"),
                  cluster = ~panel_id)
m5_dist  <- feols(new_chargers ~ ev_per_1k + orcs_per_10k + neighbour_chargers
                  | panel_id + time_id,
                  data    = panel_dist |> filter(region != "London"),
                  cluster = ~panel_id)

spatial_robustness <- list(m5_queen = m5_queen, m5_knn = m5_knn, m5_dist = m5_dist)
saveRDS(spatial_robustness, file.path(OUT, "models/spatial_robustness.rds"))
# =============================================================================
# Creating coefficient tables
# =============================================================================

coef_labels <- c(
  "ev_per_1k"                    = "EV Stock (lagged, per 1,000 vehicles)",
  "orcs_per_10k"                 = "Cumulative ORCS Funding (£10,000s)",
  "neighbour_chargers"           = "Neighbour Charger Count",
  "log(population)"              = "Population (log)",
  "orcs_per_10k:grid_capacity_z" = "Funding × Grid Capacity (z)",
  "orcs_per_10k:grid_highTRUE"   = "Funding × Grid High (binary)",
  "orcs_per_10k:imd_score"       = "Funding × IMD Score (z)",
  "orcs_per_10k:imd_highTRUE"    = "Funding × IMD High (binary)"
)

# creates reusable wrapper to avoid repetition
write_table <- function(model_list, filename) {
  modelsummary(
    model_list,
    coef_map = coef_labels, fmt = 4,
    stars    = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
    gof_omit = "AIC|BIC|Log.Lik|RMSE|Std.Errors|FE",
    output   = file.path(OUT, "tables", filename)
  )
}

write_table(list("M1: Two-Way FE"   = m1,
                 "M3: LA FE Only"   = m3,
                 "M4: Poisson PPML" = m4,
                 "M5: Excl. London" = m5),
            "table3_main_models.html")

write_table(list("M_NGED_1 Baseline"   = m_nged_1,
                 "M_NGED_2 Continuous" = m_nged_2,
                 "M_NGED_3 Binary"     = m_nged_3),
            "table5_grid_interaction.html")

write_table(list("M_IMD_1 Continuous"  = m_imd_1,
                 "M_IMD_2 Binary"      = m_imd_2,
                 "M_IMD_3 Excl London" = m_imd_3),
            "table6_imd_interaction.html")

write_table(list("M1 Baseline"     = m1,
                 "M5 Excl. London" = m5,
                 "M_London"        = m_london,
                 "M5 KNN5"         = m5_knn,
                 "M5 IDW"          = m5_dist),
            "table7_london_rest.html")
# =============================================================================
# Optimisation
# =============================================================================
# convert M5 coefficient to per-pound productivity rate
beta_pound <- coef(m5)["orcs_per_10k"] / 10000

baseline_opt <- panel |>
  group_by(la_code, region, imd_score) |>
  summarise(orcs_cumulative_gbp = max(orcs_cumulative_gbp, na.rm = TRUE),
            .groups = "drop") |>
  filter(!is.na(imd_score), region != "London")

# apply reusable wrapper
solve_optimisation <- function(budget_gbp, cap_gbp, use_het = FALSE) {
  n       <- nrow(baseline_opt)
  weights <- pmax(baseline_opt$imd_score + 3, 1)
# choose productivity number
  if (use_het) {
    beta_main  <- coef(m_imd_1)["orcs_per_10k"]            / 10000
    beta_intxn <- coef(m_imd_1)["orcs_per_10k:imd_score"]  / 10000
    beta_la    <- beta_main + beta_intxn * baseline_opt$imd_score
  } else {
    beta_la    <- rep(beta_pound, n)
  }

  result <- lp(
    direction   = "max",
    objective.in = weights * beta_la,
    const.mat   = rbind(matrix(rep(1, n), nrow = 1), diag(n)),
    const.dir   = c("<=", rep("<=", n)),
    const.rhs   = c(budget_gbp, rep(cap_gbp, n))
  )

  list(
    allocation     = result$solution,
    total_chargers = sum(beta_la * result$solution) / 4,    # quarterly conversion
    n_funded       = sum(result$solution > 0),
    mean_imd       = if (sum(result$solution) > 0) {
      weighted.mean(baseline_opt$imd_score, result$solution)
    } else NA_real_
  )
}

# various budget models at fixed £5M per-LA cap
budgets <- c(10e6, 25e6, 50e6, 100e6, 250e6)
sweep_results <- map_dfr(budgets, function(b) {
  res <- solve_optimisation(b, cap_gbp = 5e6)
  data.frame(budget_gbp     = b,
             budget_label   = paste0("£", b/1e6, "M"),
             n_funded       = res$n_funded,
             total_chargers = res$total_chargers,
             mean_imd       = res$mean_imd,
             la_code        = baseline_opt$la_code,
             allocation     = res$allocation)
})

sweep_summary <- sweep_results |>
  group_by(budget_label) |>
  summarise(n_funded       = first(n_funded),
            total_chargers = first(total_chargers),
            mean_imd       = first(mean_imd),
            .groups = "drop")

# optimal 100m allocation
optimal_100m <- sweep_results |>
  filter(budget_label == "£100M") |>
  select(la_code, allocation)
# cap sensitivity check at fixed £100M budget
cap_sensitivity <- map_dfr(c(2e6, 5e6, 10e6, 20e6), function(cap) {
  res <- solve_optimisation(100e6, cap_gbp = cap)
  data.frame(cap_label      = paste0("£", cap/1e6, "M"),
             n_funded       = res$n_funded,
             total_chargers = res$total_chargers,
             mean_imd       = res$mean_imd)
})

# M5 coefficient vs IMD coefficient comparison
het_sweep <- map_dfr(budgets, function(b) {
  res_lin <- solve_optimisation(b, 5e6, use_het = FALSE)
  res_het <- solve_optimisation(b, 5e6, use_het = TRUE)
  data.frame(budget_gbp      = b,
             budget_label    = paste0("£", b/1e6, "M"),
             linear_chargers = res_lin$total_chargers,
             het_chargers    = res_het$total_chargers,
             gain_pct        = (res_het$total_chargers / res_lin$total_chargers - 1) * 100)
})

# -----------------------------------------------------------------------------
# LEVI comparison
# -----------------------------------------------------------------------------
# obtain LEVI values
levi_url <- paste0("https://www.gov.uk/government/publications/",
                    "local-ev-infrastructure-levi-funding-amounts/",
                    "local-electric-vehicle-infrastructure-levi-funding-amounts-capital")
levi_tables <- read_html(levi_url) |> html_table()

levi_la_raw <- levi_tables[[3]] |>
  rename(la_name_levi   = `Local authority`,
         allocation_str = `Indicative allocation`) |>
  mutate(levi_amount_gbp = as.numeric(str_replace_all(allocation_str, "[£,]", ""))) |>
  filter(!is.na(levi_amount_gbp))

levi_combined_raw <- levi_tables[[2]] |>
  rename(la_name_levi   = `Combined authority (CA)`,
         allocation_str = `Indicative allocation`) |>
  mutate(levi_amount_gbp = as.numeric(str_replace_all(allocation_str, "[£,]", ""))) |>
  filter(!is.na(levi_amount_gbp))

# LEVI county mapping
levi_county_mapping <- tribble(
  ~county_name,        ~district_codes,
  "Cambridgeshire",     c("E07000008","E07000009","E07000010","E07000011","E07000012"),
  "Cornwall & Scilly",  c("E06000052","E06000053"),
  "Cumbria",            c("E06000063","E06000064"),
  "Derbyshire",         c("E07000032","E07000033","E07000034","E07000035","E07000036",
                          "E07000037","E07000038","E07000039","E07000040"),
  "Devon",              c("E07000040","E07000041","E07000042","E07000043","E07000044",
                          "E07000045","E07000046","E07000047"),
  "East Sussex",        c("E07000061","E07000062","E07000063","E07000064","E07000065",
                          "E07000066"),
  "Essex",              c("E07000066","E07000067","E07000068","E07000069","E07000070",
                          "E07000071","E07000072","E07000073","E07000074","E07000075",
                          "E07000076","E07000077"),
  "Gloucestershire",    c("E07000078","E07000079","E07000080","E07000081","E07000082",
                          "E07000083"),
  "Hampshire",          c("E07000084","E07000085","E07000086","E07000087","E07000088",
                          "E07000089","E07000090","E07000091","E07000092","E07000093",
                          "E07000094"),
  "Hertfordshire",      c("E07000095","E07000096","E07000098","E07000099","E07000102",
                          "E07000103","E07000240","E07000241","E07000242","E07000243"),
  "Kent",               c("E07000105","E07000106","E07000107","E07000108","E07000109",
                          "E07000110","E07000111","E07000112","E07000113","E07000114",
                          "E07000115","E07000116"),
  "Lancashire",         c("E07000117","E07000118","E07000119","E07000120","E07000121",
                          "E07000122","E07000123","E07000124","E07000125","E07000126",
                          "E07000127","E07000128"),
  "Leicestershire",     c("E07000129","E07000130","E07000131","E07000132","E07000133",
                          "E07000134","E07000135"),
  "Lincolnshire",       c("E07000136","E07000137","E07000138","E07000139","E07000140",
                          "E07000141","E07000142"),
  "Norfolk",            c("E07000143","E07000144","E07000145","E07000146","E07000147",
                          "E07000148","E07000149"),
  "North Yorkshire",    c("E06000065"),
  "Nottinghamshire",    c("E07000170","E07000171","E07000172","E07000173","E07000174",
                          "E07000175","E07000176"),
  "Oxfordshire",        c("E07000178","E07000179","E07000180","E07000181","E07000200"),
  "Somerset",           c("E06000066"),
  "Staffordshire",      c("E07000192","E07000193","E07000194","E07000195","E07000196",
                          "E07000197","E07000198","E07000199"),
  "Suffolk",            c("E07000200","E07000202","E07000203","E07000244","E07000245"),
  "Surrey",             c("E07000207","E07000208","E07000209","E07000210","E07000211",
                          "E07000212","E07000213","E07000214","E07000215","E07000216",
                          "E07000217"),
  "Warwickshire",       c("E07000218","E07000219","E07000220","E07000221","E07000222"),
  "West Sussex",        c("E07000223","E07000224","E07000225","E07000226","E07000227",
                          "E07000228","E07000229"),
  "Wiltshire",          c("E06000054"),
  "Worcestershire",     c("E07000234","E07000235","E07000236","E07000237","E07000238",
                          "E07000239")
) |>
  unnest(district_codes)

# combined authority constituent mapping
levi_ca_mapping <- tribble(
  ~ca_name,                                              ~district_codes,
  "Cambridgeshire and Peterborough Combined Authority",   c("E07000008","E07000009","E07000010",
                                                             "E07000011","E07000012","E06000031"),
  "Greater Manchester CA",                                 c("E08000001","E08000002","E08000003",
                                                             "E08000004","E08000005","E08000006",
                                                             "E08000007","E08000008","E08000009","E08000010"),
  "Liverpool City Region CA",                              c("E08000011","E08000012","E08000013",
                                                             "E08000014","E08000015","E06000006"),
  "North East CA",                                         c("E08000037","E08000021","E08000022",
                                                             "E08000023","E08000024","E06000005","E06000047"),
  "South Yorkshire CA",                                    c("E08000016","E08000017","E08000018","E08000019"),
  "Tees Valley CA",                                        c("E06000001","E06000002","E06000003",
                                                             "E06000004","E06000057"),
  "West Midlands CA",                                      c("E08000025","E08000026","E08000027",
                                                             "E08000028","E08000029","E08000030","E08000031"),
  "West of England CA",                                    c("E06000022","E06000023","E06000025"),
  "West Yorkshire CA",                                     c("E08000032","E08000033","E08000034",
                                                             "E08000035","E08000036")
) |>
  unnest(district_codes)

# direct LA matches
levi_direct <- levi_la_raw |>
  mutate(name_clean = clean_la_name(la_name_levi)) |>
  inner_join(la_name_lookup, by = "name_clean", relationship = "many-to-many") |>
  distinct(la_name_levi, .keep_all = TRUE) |>
  select(la_code, levi_amount_gbp)

# county allocation (population-weighted)
levi_county_alloc <- levi_la_raw |>
  inner_join(
    levi_county_mapping |>
      left_join(district_pops_2024, by = c("district_codes" = "la_code")) |>
      group_by(county_name) |>
      mutate(pop_share = population / sum(population, na.rm = TRUE)) |>
      ungroup() |>
      filter(!is.na(pop_share)),
    by = c("la_name_levi" = "county_name")
  ) |>
  mutate(la_code = district_codes,
         allocated_levi_gbp = levi_amount_gbp * pop_share) |>
  group_by(la_code) |>
  summarise(allocated_levi_gbp = sum(allocated_levi_gbp, na.rm = TRUE),
            .groups = "drop")

# combined authority allocation (population weighted)
levi_ca_alloc <- levi_combined_raw |>
  inner_join(
    levi_ca_mapping |>
      left_join(district_pops_2024, by = c("district_codes" = "la_code")) |>
      group_by(ca_name) |>
      mutate(pop_share = population / sum(population, na.rm = TRUE)) |>
      ungroup() |>
      filter(!is.na(pop_share)),
    by = c("la_name_levi" = "ca_name")
  ) |>
  mutate(la_code = district_codes,
         allocated_levi_gbp = levi_amount_gbp * pop_share) |>
  group_by(la_code) |>
  summarise(allocated_levi_gbp = sum(allocated_levi_gbp, na.rm = TRUE),
            .groups = "drop")

levi_total <- bind_rows(
    levi_direct |> rename(allocated_levi_gbp = levi_amount_gbp),
    levi_county_alloc,
    levi_ca_alloc
  ) |>
  group_by(la_code) |>
  summarise(levi_allocation_gbp = sum(allocated_levi_gbp, na.rm = TRUE),
            .groups = "drop")

# three-way comparison dataset (LEVI, ORCS, optimal)
comparison <- panel |>
  group_by(la_code, region, imd_score) |>
  summarise(orcs_cumulative_gbp = max(orcs_cumulative_gbp, na.rm = TRUE),
            .groups = "drop") |>
  filter(!is.na(imd_score)) |>
  left_join(levi_total,   by = "la_code") |>
  left_join(optimal_100m |> rename(optimal_allocation = allocation),
            by = "la_code") |>
  mutate(levi_allocation_gbp = replace_na(levi_allocation_gbp, 0),
         optimal_allocation  = replace_na(optimal_allocation,  0))

regional_summary <- comparison |>
  group_by(region) |>
  summarise(orcs_total    = sum(orcs_cumulative_gbp,   na.rm = TRUE),
            levi_total    = sum(levi_allocation_gbp,   na.rm = TRUE),
            optimal_total = sum(optimal_allocation,    na.rm = TRUE),
            .groups = "drop") |>
  mutate(orcs_pct    = orcs_total    / sum(orcs_total)    * 100,
         levi_pct    = levi_total    / sum(levi_total)    * 100,
         optimal_pct = optimal_total / sum(optimal_total) * 100) |>
  select(region, orcs_pct, levi_pct, optimal_pct) |>
  mutate(across(where(is.numeric), ~round(., 1))) |>
  arrange(desc(optimal_pct))
