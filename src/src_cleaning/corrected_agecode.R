library(tidyverse)

# 1. Read the raw text lines exactly as they exist in the file 
# (This catches any weird hidden SSB metadata rows at the top)
cat("=== RAW TEXT LINES ===\n")
raw_text <- readLines("07459_20260712-204505.csv", n = 5, encoding = "latin1")
print(raw_text)

# 2. Load the file using the standard SSB delimiter and encoding
raw_diagnostic <- read_delim(
  "07459_20260712-204505.csv",
  delim = ";",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

# 3. Print the exact column names R sees
cat("\n=== EXACT COLUMN NAMES ===\n")
print(colnames(raw_diagnostic))

# 4. Print the data types and structure (Wide vs. Long check)
cat("\n=== DATA STRUCTURE ===\n")
glimpse(raw_diagnostic)

# 5. Print the first 5 rows of actual data
cat("\n=== FIRST 5 ROWS ===\n")
print(head(raw_diagnostic, 5))



library(tidyverse)

# =============================================================================
# Script: Build exact continuous median_age_clean.csv from 1-year bins (07459)
# Output: 2848 rows, columns: municipality_code, year, median_age
# =============================================================================

# 1. Load files (using skip = 1 to bypass the title string)
raw <- read_delim(
  "07459_20260712-204505.csv",
  delim = ";",
  skip = 1,
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

ssb <- read_csv("ssb_municipality_list.csv",
                col_types = cols(municipality_code = col_character()))

cat("Raw dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")

# 2. Fix column names based on the diagnostic
names(raw) <- c("region", "sex", "age", paste0("persons_", 2016:2023))

# 3. Extract municipality code and exact numeric age
raw_clean <- raw %>%
  mutate(
    municipality_code = str_extract(region, "(?<=K-)\\d+"),
    # This pulls just the number from "0 years", "1 year", "100 years or older"
    age_num = as.numeric(str_extract(age, "^\\d+")) 
  ) %>%
  filter(!is.na(municipality_code))

# 4. Apply your exact code remapping framework
code_map <- c(
  '3001'='3101','3002'='3103','3003'='3105','3004'='3107',
  '3005'='3301','3006'='3303','3007'='3305','3011'='3110',
  '3012'='3124','3013'='3122','3014'='3118','3015'='3116',
  '3016'='3120','3017'='3112','3018'='3114','3019'='3216',
  '3020'='3207','3021'='3218','3022'='3214','3023'='3212',
  '3024'='3201','3025'='3203','3026'='3220','3027'='3222',
  '3028'='3224','3029'='3205','3030'='3230','3031'='3228',
  '3032'='3232','3033'='3234','3034'='3209','3035'='3236',
  '3036'='3238','3037'='3240','3038'='3242','3039'='3226',
  '3040'='3322','3041'='3310','3042'='3312','3043'='3314',
  '3044'='3316','3045'='3318','3046'='3320','3047'='3324',
  '3048'='3326','3049'='3328','3050'='3330','3051'='3332',
  '3052'='3334','3053'='3336','3054'='3338',
  '3801'='3901','3802'='3903','3803'='3905','3804'='3907',
  '3805'='3909','3806'='3911','3807'='4001','3808'='4003',
  '3811'='4005','3812'='4010','3813'='4012','3814'='4014',
  '3815'='4016','3816'='4018','3817'='4020','3818'='4022',
  '3819'='4024','3820'='4026','3821'='4028','3822'='4030',
  '3823'='4032','3824'='4034','3825'='4036',
  '1508'='1507','1580'='1576',
  '5501'='5401','5503'='5402','5510'='5411','5512'='5412',
  '5514'='5413','5516'='5414','5518'='5415','5520'='5416',
  '5522'='5417','5524'='5418','5526'='5419','5528'='5420',
  '5530'='5421','5532'='5422','5534'='5423','5536'='5424',
  '5538'='5425','5540'='5426','5542'='5427','5544'='5428',
  '5546'='5429','5601'='5403','5603'='5406','5605'='5444',
  '5607'='5405','5610'='5437','5612'='5430','5614'='5432',
  '5616'='5433','5618'='5434','5620'='5435','5622'='5436',
  '5624'='5438','5626'='5439','5628'='5441','5630'='5440',
  '5632'='5443','5634'='5404','5636'='5442'
)

raw_mapped <- raw_clean %>%
  mutate(
    municipality_code = ifelse(
      municipality_code %in% names(code_map),
      code_map[municipality_code],
      municipality_code
    )
  ) %>%
  filter(municipality_code %in% ssb$municipality_code)

cat("After filtering - unique municipalities:", n_distinct(raw_mapped$municipality_code), "\n")

# 5. Pivot wide years to long format and sum males/females together
long_pop <- raw_mapped %>%
  pivot_longer(
    cols = starts_with("persons_"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.numeric(str_remove(year, "persons_")),
    # Remove any invisible spaces from population counts just in case
    population = as.numeric(str_replace_all(population, "\\s", "")) 
  ) %>%
  # Sum the population of both sexes for each specific age, in each town, in each year
  group_by(municipality_code, year, age_num) %>%
  summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

# 6. The continuous demographic median function
# This uses linear interpolation inside the exact 1-year bracket
calc_demographic_median <- function(ages, pops) {
  if(sum(pops, na.rm = TRUE) == 0) return(NA_real_)
  
  # Ensure sorted by age
  ord <- order(ages)
  ages <- ages[ord]
  pops <- pops[ord]
  
  cum_pop <- cumsum(pops)
  target <- sum(pops) / 2
  
  # Find the exact age bucket the median person falls into
  median_idx <- which(cum_pop >= target)[1]
  
  # If it's the very first bucket (age 0), handle safely
  if (median_idx == 1) {
    return(ages[1] + (target / pops[1]))
  }
  
  # Linear interpolation for the true decimal
  age_bucket <- ages[median_idx]
  pop_in_bucket <- pops[median_idx]
  pop_before <- cum_pop[median_idx - 1]
  
  exact_median <- age_bucket + ((target - pop_before) / pop_in_bucket)
  return(exact_median)
}

# 7. Apply the math to build the final panel
result <- long_pop %>%
  group_by(municipality_code, year) %>%
  summarise(
    median_age = calc_demographic_median(age_num, population),
    .groups = "drop"
  ) %>%
  arrange(municipality_code, year)

# =============================================================================
# VERIFICATION BLOCK
# =============================================================================
cat("\n=== VERIFICATION ===\n")
cat("Rows:", nrow(result), "| Expected 2848 |",
    ifelse(nrow(result)==2848, "PASS", "FAIL"), "\n")
cat("Municipalities:", n_distinct(result$municipality_code), "| Expected 356 |",
    ifelse(n_distinct(result$municipality_code)==356, "PASS", "FAIL"), "\n")
cat("Duplicates:", sum(duplicated(result[,c("municipality_code","year")])),
    "| Expected 0 |",
    ifelse(sum(duplicated(result[,c("municipality_code","year")]))==0, "PASS", "FAIL"), "\n")
cat("Nulls:", sum(is.na(result$median_age)), "\n")

oslo <- result %>% filter(municipality_code=="0301", year==2023)
cat("Oslo 2023 exact continuous median age:", round(oslo$median_age, 2), "\n")
cat("Median age range:", round(min(result$median_age, na.rm=TRUE), 2),
    "to", round(max(result$median_age, na.rm=TRUE), 2), "\n")

write_csv(result, "median_age_clean.csv")
cat("\nSaved: median_age_clean.csv\n")


# Patch the 20 missing values by borrowing the nearest year's age 
# within that specific municipality's timeline
result_patched <- result %>%
  group_by(municipality_code) %>%
  fill(median_age, .direction = "downup") %>%
  ungroup()

# Verify the patch worked
cat("Nulls after patch:", sum(is.na(result_patched$median_age)), "\n")

# Save the pristine file
write_csv(result_patched, "median_age_clean.csv")
cat("\nSaved: median_age_clean.csv\n")



# =============================================================================
# ADVERSARIAL STRESS TEST FOR MEDIAN AGE PANEL DATA
# =============================================================================

cat("--- 1. PANEL BALANCE AND DIMENSIONALITY ---\n")
expected_rows <- 356 * 8
actual_rows <- nrow(result_patched)
cat("Expected Rows:", expected_rows, "| Actual Rows:", actual_rows, "\n")
if(actual_rows == expected_rows) cat("PASS: Panel is perfectly balanced.\n") else cat("FAIL: Missing or extra rows!\n")

# Check if every single municipality has exactly 8 years of data
years_per_muni <- result_patched %>% 
  count(municipality_code) %>% 
  filter(n != 8)
if(nrow(years_per_muni) == 0) cat("PASS: Every municipality has exactly 8 consecutive years.\n") else cat("FAIL: Broken time series found!\n")


cat("\n--- 2. LOGICAL BOUNDS AND ANOMALY DETECTION ---\n")
# Check for physically impossible median ages
rogue_ages <- result_patched %>% filter(median_age < 18 | median_age > 80)
if(nrow(rogue_ages) == 0) cat("PASS: All median ages fall within a logical human span.\n") else cat("FAIL: Outliers found!\n")

# Check the distribution profile for massive unnatural jumps over time
max_1year_jump <- result_patched %>%
  group_by(municipality_code) %>%
  arrange(year) %>%
  mutate(jump = abs(median_age - lag(median_age))) %>%
  filter(!is.na(jump)) %>%
  ungroup() %>%
  filter(jump > 2) # A change of >2 years of median age in 12 months is highly anomalous

if(nrow(max_1year_jump) == 0) cat("PASS: Year-over-year shifts are smoothly continuous.\n") else cat("WARNING: Volatile volatility detected!\n")


cat("\n--- 3. SPATIAL WEIGHTS MATRIX SYNC CHECK ---\n")
# Ensure every single code in your age dataset exists in your master master list
unmatched_codes <- result_patched %>% 
  filter(!municipality_code %in% ssb$municipality_code)
if(nrow(unmatched_codes) == 0) cat("PASS: 100% of spatial codes match your ssb_municipality_list.\n") else cat("FAIL: Rogue codes found!\n")


cat("\n--- 4. DATA DENSITY DISTRIBUTION ---\n")
summary(result_patched$median_age)