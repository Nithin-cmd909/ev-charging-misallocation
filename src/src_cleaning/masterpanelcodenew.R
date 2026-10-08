library(tidyverse)

# =============================================================================
# MASTER PANEL — STAGE 1: RAW MERGE ONLY
#
# Merges all 8 corrected input files. Builds NO constructed variables
# (no ev_share, no logodds, no asinh, no density, no logs). That is Stage 2,
# done separately once this stage is verified clean.
#
# Inputs (all corrected/verified tonight, except area which is static):
#   ev_registrations_clean.csv   -> ev_count, fleet_total
#   nobil_chargers_clean.csv     -> station-level, aggregated here
#   income_clean.csv             -> income_after_tax_median      [FIXED]
#   median_age_clean.csv         -> median_age                    [FIXED]
#   population_clean.csv         -> population                    [FIXED]
#   housing_clean.csv            -> detached, total_dwellings     [FIXED]
#   municipality_area_clean.csv  -> area_km2
#   ssb_municipality_list.csv    -> municipality_name, master grid
#
# Output: master_panel_raw_merge.csv (2848 rows, RAW variables only)
# =============================================================================

ev <- read_csv("ev_registrations_clean.csv", col_types = cols(municipality_code = col_character()))
nobil <- read_csv("nobil_chargers_clean.csv", col_types = cols(municipality_code = col_character()))
income <- read_csv("income_clean.csv", col_types = cols(municipality_code = col_character()))
age <- read_csv("median_age_clean.csv", col_types = cols(municipality_code = col_character()))
pop <- read_csv("population_clean.csv", col_types = cols(municipality_code = col_character()))
housing <- read_csv("housing_clean.csv", col_types = cols(municipality_code = col_character()))
area <- read_csv("municipality_area_clean.csv", col_types = cols(municipality_code = col_character()))
ssb <- read_csv("ssb_municipality_list.csv", col_types = cols(municipality_code = col_character()))

# DEFENSIVE FIX: zero-pad municipality_code in EVERY input file, not just NOBIL.
# Any file that has ever passed through Excel/Sheets can silently lose leading
# zeros (e.g. "0301" -> "301"), which causes a left_join to fail SILENTLY --
# no error, just an NA for that municipality. This just happened to Oslo in
# housing_clean.csv. Padding everything here makes the merge immune to it.
ev      <- ev      %>% mutate(municipality_code = str_pad(municipality_code, 4, pad = "0"))
income  <- income  %>% mutate(municipality_code = str_pad(municipality_code, 4, pad = "0"))
age     <- age     %>% mutate(municipality_code = str_pad(municipality_code, 4, pad = "0"))
pop     <- pop     %>% mutate(municipality_code = str_pad(municipality_code, 4, pad = "0"))
housing <- housing %>% mutate(municipality_code = str_pad(municipality_code, 4, pad = "0"))
area    <- area    %>% mutate(municipality_code = str_pad(municipality_code, 4, pad = "0"))
ssb     <- ssb     %>% mutate(municipality_code = str_pad(municipality_code, 4, pad = "0"))

cat("=== INPUT FILE DIMENSIONS ===\n")
cat("ev:", nrow(ev), "rows |", n_distinct(ev$municipality_code), "munis\n")
cat("nobil:", nrow(nobil), "rows (station-level, not muni-year)\n")
cat("income:", nrow(income), "rows |", n_distinct(income$municipality_code), "munis\n")
cat("age:", nrow(age), "rows |", n_distinct(age$municipality_code), "munis\n")
cat("pop:", nrow(pop), "rows |", n_distinct(pop$municipality_code), "munis\n")
cat("housing:", nrow(housing), "rows |", n_distinct(housing$municipality_code), "munis\n")
cat("area:", nrow(area), "rows |", n_distinct(area$municipality_code), "munis\n")
cat("ssb:", nrow(ssb), "rows\n\n")

# Zero-pad NOBIL codes (known, documented fix)
nobil <- nobil %>%
  mutate(municipality_code = str_pad(as.character(municipality_code), 4, pad = "0"))
cat("NOBIL Oslo stations after zero-pad fix:", sum(nobil$municipality_code=="0301"), "\n\n")

# Full grid: every municipality x every year, 2016-2023
full_grid <- expand_grid(
  municipality_code = ssb$municipality_code,
  year = 2016:2023
)
cat("Full grid:", nrow(full_grid), "rows | Expected", 356*8, "\n\n")

# Aggregate NOBIL to muni-year cumulative kW
charger_access <- map_dfr(2016:2023, function(yr) {
  nobil %>%
    filter(install_year <= yr) %>%
    group_by(municipality_code) %>%
    summarise(charger_kw_cumulative = sum(charging_kw_numeric, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = yr)
})
charger_access <- full_grid %>%
  left_join(charger_access, by = c("municipality_code", "year")) %>%
  mutate(charger_kw_cumulative = replace_na(charger_kw_cumulative, 0))

# ---- THE MERGE (raw variables only, nothing derived) -----------------------

master_raw <- full_grid %>%
  left_join(ev %>% select(municipality_code, year, ev_count, fleet_total),
            by = c("municipality_code", "year")) %>%
  left_join(charger_access, by = c("municipality_code", "year")) %>%
  left_join(income %>% select(municipality_code, year, income_after_tax_median),
            by = c("municipality_code", "year")) %>%
  left_join(age %>% select(municipality_code, year, median_age),
            by = c("municipality_code", "year")) %>%
  left_join(pop %>% select(municipality_code, year, population),
            by = c("municipality_code", "year")) %>%
  left_join(housing %>% select(municipality_code, year, detached, total_dwellings),
            by = c("municipality_code", "year")) %>%
  left_join(area %>% select(municipality_code, area_km2), by = "municipality_code") %>%
  left_join(ssb %>% select(municipality_code, municipality_name), by = "municipality_code")

cat("=== STAGE 1 MERGE COMPLETE ===\n")
cat("Rows:", nrow(master_raw), "| Expected 2848 |",
    ifelse(nrow(master_raw)==2848,"PASS","FAIL"), "\n")
cat("Municipalities:", n_distinct(master_raw$municipality_code), "| Expected 356 |",
    ifelse(n_distinct(master_raw$municipality_code)==356,"PASS","FAIL"), "\n")
cat("Duplicates (code,year):", sum(duplicated(master_raw[,c("municipality_code","year")])),
    "| Expected 0 |\n\n")

cat("=== NULL COUNTS, EVERY COLUMN ===\n")
print(colSums(is.na(master_raw)))
cat("\n(Expect exactly 20 NAs in ev_count/fleet_total/income/age/population/detached/\n")
cat(" total_dwellings -- the 5 known excluded municipalities, 2016-2019. Anything\n")
cat(" else non-zero here means something upstream still needs checking.)\n\n")

cat("=== WHICH ROWS HAVE THE NULLS? ===\n")
null_rows <- master_raw %>% filter(if_any(everything(), is.na))
cat("Distinct municipalities with any NA:", n_distinct(null_rows$municipality_code), "\n")
print(null_rows %>% distinct(municipality_code, municipality_name))
cat("\n(Should be exactly: 1806 Narvik, 1875 Hamaroy, 5055 Heim, 5056 Hitra, 5059 Orkland)\n\n")

cat("=== SPOT CHECKS ===\n")
for (code in c("0301","4601","5001","1103","3201")) {
  r <- master_raw %>% filter(municipality_code==code, year==2023)
  cat(sprintf("%s (%s) 2023: ev=%s fleet=%s kw=%.1f income=%s pop=%s age=%s detached/%s\n",
              code, r$municipality_name, r$ev_count, r$fleet_total, r$charger_kw_cumulative,
              r$income_after_tax_median, r$population, round(r$median_age,1),
              paste0(r$detached,"/",r$total_dwellings)))
}

cat("\n=== HALDEN 2016 CHECK (should be REAL 453000 income, from tonight's income fix) ===\n")
halden <- master_raw %>% filter(municipality_code=="3101", year==2016)
cat("Halden 2016 income:", halden$income_after_tax_median, "| Expected 453000 |\n")

write_csv(master_raw, "master_panel_raw_merge.csv")
cat("\nSaved: master_panel_raw_merge.csv\n")
cat("STOP HERE. Verify this output before moving to constructed variables.\n")





library(tidyverse)

# =============================================================================
# MASTER PANEL — STAGE 2: CONSTRUCTED VARIABLES
#
# Takes the verified Stage 1 output (master_panel_raw_merge.csv) and builds
# every derived variable. Nothing here touches the raw inputs -- if a number
# looks wrong, the bug is in this stage, not in Stage 1, which is already
# confirmed clean.
#
# Outcome variable: ev_share_sqrt replaces the old clamped ev_logodds.
#   - No floor/ceiling needed (sqrt is defined at 0).
#   - Verified tonight as the only transform (of raw, asinh, log1p, sqrt,
#     clamped logit) where BOTH the direct effect and the spillover effect
#     are stable and significant across weight matrices, without an
#     arbitrary clamp.
#   - Known limitation, disclosed: a linear model on sqrt(share) does not
#     structurally guarantee predictions stay in [0,1]. This does not affect
#     in-sample estimation (all observed shares are 0-44%, well behaved),
#     but MUST be handled with explicit clipping in the counterfactual
#     reallocation script, the same way the old pipeline already did
#     (p_cur <- pmin(pmax(p_cur, 0.001), 0.999)).
#
# Required: master_panel_raw_merge.csv (Stage 1 output)
# Output: master_panel_final.csv
# =============================================================================

master <- read_csv("master_panel_raw_merge.csv", col_types = cols(municipality_code = col_character()))

cat("Input rows:", nrow(master), "| Expected 2848\n\n")

master <- master %>%
  mutate(
    
    # ---- OUTCOME: replaces ev_share_wins / ev_logodds ----------------------
    ev_share_raw  = ifelse(fleet_total == 0 | is.na(fleet_total), NA, ev_count / fleet_total),
    ev_share_sqrt = ifelse(is.na(ev_share_raw), NA, sqrt(ev_share_raw)),
    
    # ---- TREATMENT: unchanged, already verified clean ----------------------
    charger_access_asinh = asinh(charger_kw_cumulative),
    
    # ---- CONTROLS: unchanged formulas, now built on corrected inputs -------
    pop_density     = population / area_km2,
    log_pop_density = ifelse(population == 0 | is.na(population), NA_real_, log(pop_density)),
    share_detached  = ifelse(total_dwellings == 0 | is.na(total_dwellings), NA,
                             detached / total_dwellings),
    log_income      = log(income_after_tax_median)
  )

cat("=== STAGE 2 CONSTRUCTION COMPLETE ===\n\n")

# ---- Verification: recompute everything independently and compare ---------

cat("=== INDEPENDENT RECOMPUTATION CHECK ===\n")
check <- master %>%
  mutate(
    chk_share = ifelse(fleet_total==0, NA, ev_count/fleet_total),
    chk_sqrt  = ifelse(is.na(chk_share), NA, sqrt(chk_share)),
    chk_asinh = asinh(charger_kw_cumulative),
    chk_dens  = population/area_km2,
    chk_ldens = ifelse(population==0, NA, log(chk_dens)),
    chk_det   = ifelse(total_dwellings==0, NA, detached/total_dwellings),
    chk_linc  = log(income_after_tax_median)
  )
cat("ev_share_sqrt matches:", all.equal(check$ev_share_sqrt, check$chk_sqrt), "\n")
cat("charger_access_asinh matches:", all.equal(check$charger_access_asinh, check$chk_asinh), "\n")
cat("pop_density matches:", all.equal(check$pop_density, check$chk_dens), "\n")
cat("log_pop_density matches:", isTRUE(all.equal(check$log_pop_density, check$chk_ldens)), "\n")
cat("share_detached matches:", isTRUE(all.equal(check$share_detached, check$chk_det)), "\n")
cat("log_income matches:", all.equal(check$log_income, check$chk_linc), "\n\n")

# ---- Sanity checks on the new outcome variable specifically ----------------

cat("=== ev_share_sqrt SANITY ===\n")
cat("Range:", round(min(master$ev_share_sqrt, na.rm=TRUE),4), "to",
    round(max(master$ev_share_sqrt, na.rm=TRUE),4), "\n")
cat("Any NaN (would indicate negative input to sqrt):",
    sum(is.nan(master$ev_share_sqrt)), "| Expected 0\n")
cat("Zeros (municipalities with truly zero EVs):", sum(master$ev_share_sqrt==0, na.rm=TRUE), "\n\n")

cat("=== NULL COUNTS, ALL COLUMNS ===\n")
print(colSums(is.na(master)))
cat("\n(Constructed variables should show NAs ONLY for the 5 known excluded\n")
cat(" municipalities pre-2020, propagated from their missing raw inputs.\n")
cat(" No NEW nulls should appear anywhere.)\n\n")

cat("=== SPOT CHECKS ===\n")
for (code in c("0301","4601","5001","1103","3201","1151")) {
  r <- master %>% filter(municipality_code==code, year==2023)
  cat(sprintf("%s (%s): share=%.4f sqrt_share=%.4f asinh_kw=%.3f log_inc=%.3f log_dens=%.3f detached_share=%.3f\n",
              code, r$municipality_name, r$ev_share_raw, r$ev_share_sqrt,
              r$charger_access_asinh, r$log_income, r$log_pop_density, r$share_detached))
}

cat("\n=== BOUNDS REMINDER ===\n")
cat("ev_share_sqrt is used in a LINEAR spatial model. In-sample this is fine\n")
cat("(all values below are well within [0,1] before the transform). BUT the\n")
cat("counterfactual reallocation script MUST clip predicted shares back to\n")
cat("[0,1] (e.g. via pmin/pmax) after every simulation step, same as the\n")
cat("original pipeline already did for the old outcome variable. This is a\n")
cat("known, disclosed limitation of any linear model on a bounded outcome.\n\n")

write_csv(master, "master_panel_final.csv")
cat("Saved: master_panel_final.csv\n")
cat("\nThis is the FINAL panel. All downstream scripts (SDM estimation,\n")
cat("multiplier, misallocation index, counterfactual) should now be re-run\n")
cat("against THIS file, using ev_share_sqrt as the outcome.\n")




library(tidyverse)
library(scales)

# =============================================================================
# FULL DIAGNOSTIC — master_panel_final.csv
#
# Produces:
#   table_diag_1_descriptives.csv   (all key variables, full sample)
#   table_diag_2_analysis_sample.csv (same, restricted to 351-muni balanced panel)
#   table_diag_3_correlations.csv    (correlation matrix of controls + outcome)
#   fig_diag_1_national_trends.png
#   fig_diag_2_outcome_distributions.png  (raw share vs sqrt share side by side)
#   fig_diag_3_control_distributions.png
#   fig_diag_4_charger_concentration.png
#   fig_diag_5_outcome_vs_treatment_scatter.png
#
# Required: master_panel_final.csv
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))

cat("=============================================================\n")
cat("1. STRUCTURE\n")
cat("=============================================================\n")
cat("Rows:", nrow(master), "| Municipalities:", n_distinct(master$municipality_code),
    "| Years:", paste(range(master$year), collapse="-"), "\n\n")

# ---- Balanced analysis sample (same filter used throughout tonight) -------

core <- c("ev_share_sqrt","charger_access_asinh","log_income","log_pop_density",
          "share_detached","median_age")
clean <- master %>% drop_na(all_of(core))
balanced <- clean %>% count(municipality_code) %>% filter(n==8) %>% pull(municipality_code)
analysis <- clean %>% filter(municipality_code %in% balanced)

cat("Balanced analysis sample:", n_distinct(analysis$municipality_code), "municipalities |",
    nrow(analysis), "rows (expect 351 / 2808)\n")
dropped <- setdiff(unique(master$municipality_code), balanced)
cat("Dropped:", paste(dropped, collapse=", "), "\n\n")

# =============================================================================
# 2. DESCRIPTIVE TABLES
# =============================================================================

desc_row <- function(x, label, unit="") {
  x <- x[!is.na(x)]
  tibble(Variable=label, Unit=unit, Mean=mean(x), SD=sd(x), Min=min(x),
         P25=quantile(x,.25), Median=median(x), P75=quantile(x,.75), Max=max(x),
         Skewness = mean((x-mean(x))^3)/sd(x)^3)
}

cat("=============================================================\n")
cat("2. DESCRIPTIVE STATISTICS (full panel, 2848 rows)\n")
cat("=============================================================\n")
desc_full <- bind_rows(
  desc_row(master$ev_share_raw*100, "EV share", "%"),
  desc_row(master$ev_share_sqrt, "EV share (sqrt)", "sqrt"),
  desc_row(master$charger_kw_cumulative, "Charging capacity", "kW"),
  desc_row(master$charger_access_asinh, "Charging capacity (asinh)", "asinh kW"),
  desc_row(master$income_after_tax_median, "Median income", "NOK"),
  desc_row(master$pop_density, "Population density", "per km2"),
  desc_row(master$share_detached*100, "Detached dwellings", "%"),
  desc_row(master$median_age, "Median age", "years"),
  desc_row(master$population, "Population", "persons")
)
print(desc_full %>% mutate(across(where(is.numeric), ~round(.x,3))), n=20, width=Inf)
write_csv(desc_full, "table_diag_1_descriptives.csv")

cat("\n=============================================================\n")
cat("2b. DESCRIPTIVE STATISTICS (balanced analysis sample, 351 munis)\n")
cat("=============================================================\n")
desc_bal <- bind_rows(
  desc_row(analysis$ev_share_raw*100, "EV share", "%"),
  desc_row(analysis$ev_share_sqrt, "EV share (sqrt)", "sqrt"),
  desc_row(analysis$charger_access_asinh, "Charging capacity (asinh)", "asinh kW"),
  desc_row(analysis$log_income, "Log income", ""),
  desc_row(analysis$log_pop_density, "Log pop density", ""),
  desc_row(analysis$share_detached*100, "Detached dwellings", "%"),
  desc_row(analysis$median_age, "Median age", "years")
)
print(desc_bal %>% mutate(across(where(is.numeric), ~round(.x,3))), width=Inf)
write_csv(desc_bal, "table_diag_2_analysis_sample.csv")

cat("\n(Skewness near 0 = symmetric. |Skew| > 1 = notably skewed.\n")
cat(" Compare 'EV share' vs 'EV share (sqrt)' skewness directly --\n")
cat(" this is the empirical justification for the transformation.)\n\n")

# =============================================================================
# 3. CORRELATION MATRIX
# =============================================================================

cat("=============================================================\n")
cat("3. CORRELATION MATRIX (analysis sample)\n")
cat("=============================================================\n")
corr_vars <- analysis %>% select(ev_share_sqrt, charger_access_asinh, log_income,
                                 log_pop_density, share_detached, median_age)
corr_mat <- cor(corr_vars, use="complete.obs")
print(round(corr_mat, 3))
write_csv(as_tibble(corr_mat, rownames="variable"), "table_diag_3_correlations.csv")
cat("\n(Watch for any control correlated with the outcome above ~0.7 --\n")
cat(" could indicate a control is too close to a proxy for the outcome.\n")
cat(" Also watch controls correlated with EACH OTHER above ~0.8 -- possible\n")
cat(" multicollinearity, e.g. log_income vs log_pop_density.)\n\n")

# =============================================================================
# 4. NATIONAL TRENDS
# =============================================================================

national <- analysis %>% group_by(year) %>% summarise(
  ev_share_nat = 100*sum(ev_count)/sum(fleet_total),
  kw_total = sum(charger_kw_cumulative),
  .groups="drop")

scale_factor <- max(national$kw_total)/max(national$ev_share_nat)
p1 <- ggplot(national, aes(x=year)) +
  geom_col(aes(y=kw_total/scale_factor), fill="grey80", width=0.6) +
  geom_line(aes(y=ev_share_nat), linewidth=1.1) +
  geom_point(aes(y=ev_share_nat), size=2) +
  scale_y_continuous(name="EV share (%)",
                     sec.axis=sec_axis(~.*scale_factor/1000, name="Charging capacity (MW)")) +
  scale_x_continuous(breaks=2016:2023) +
  labs(x=NULL, caption="Analysis sample, 351 municipalities.") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())
ggsave("fig_diag_1_national_trends.png", p1, width=7, height=4.5, dpi=300)
cat("Saved fig_diag_1_national_trends.png\n")

# =============================================================================
# 5. OUTCOME DISTRIBUTION: raw share vs sqrt share (the key justification plot)
# =============================================================================

dist_data <- analysis %>% select(ev_share_raw, ev_share_sqrt) %>%
  pivot_longer(everything(), names_to="var", values_to="value") %>%
  mutate(var = factor(var, levels=c("ev_share_raw","ev_share_sqrt"),
                      labels=c("EV share (raw)", "EV share (sqrt) -- final outcome")))

p2 <- ggplot(dist_data, aes(x=value)) +
  geom_histogram(bins=40, fill="grey50", colour="white", linewidth=0.1) +
  facet_wrap(~var, scales="free", ncol=2) +
  labs(x=NULL, y="Municipality-years",
       caption="Left: raw share, right-skewed with a mass near zero.\nRight: sqrt transform, the final outcome variable -- more symmetric, no clamp required.") +
  theme_minimal(base_size=10) +
  theme(panel.grid.minor=element_blank(), strip.text=element_text(face="bold"))
ggsave("fig_diag_2_outcome_distributions.png", p2, width=7, height=4, dpi=300)
cat("Saved fig_diag_2_outcome_distributions.png\n")

# =============================================================================
# 6. CONTROL DISTRIBUTIONS
# =============================================================================

ctrl_data <- analysis %>% select(log_income, log_pop_density, share_detached, median_age) %>%
  pivot_longer(everything(), names_to="var", values_to="value") %>%
  mutate(var = recode(var, log_income="Log income", log_pop_density="Log pop. density",
                      share_detached="Share detached", median_age="Median age"))

p3 <- ggplot(ctrl_data, aes(x=value)) +
  geom_histogram(bins=35, fill="grey50", colour="white", linewidth=0.1) +
  facet_wrap(~var, scales="free", ncol=2) +
  labs(x=NULL, y="Municipality-years") +
  theme_minimal(base_size=10) +
  theme(panel.grid.minor=element_blank(), strip.text=element_text(face="bold"))
ggsave("fig_diag_3_control_distributions.png", p3, width=7, height=5, dpi=300)
cat("Saved fig_diag_3_control_distributions.png\n")


# =============================================================================
# 7. CHARGER CONCENTRATION (2023)
# =============================================================================
conc <- analysis %>% filter(year==2023) %>% arrange(desc(charger_kw_cumulative)) %>%
  mutate(rank=row_number(), cum_kw=cumsum(charger_kw_cumulative),
         cum_share=100*cum_kw/sum(charger_kw_cumulative), pct_munis=100*rank/n())

# Reference points at the top 10% and top 25% cuts (values cited in the text)
share_at_10 <- conc$cum_share[which(conc$pct_munis >= 10)[1]]
share_at_25 <- conc$cum_share[which(conc$pct_munis >= 25)[1]]

p4 <- ggplot(conc, aes(x=pct_munis, y=cum_share)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey60") +
  # guide lines marking the cited concentration points
  geom_segment(aes(x=10, xend=10, y=0, yend=share_at_10),
               linetype="dotted", colour="grey40") +
  geom_segment(aes(x=0, xend=10, y=share_at_10, yend=share_at_10),
               linetype="dotted", colour="grey40") +
  geom_segment(aes(x=25, xend=25, y=0, yend=share_at_25),
               linetype="dotted", colour="grey40") +
  geom_segment(aes(x=0, xend=25, y=share_at_25, yend=share_at_25),
               linetype="dotted", colour="grey40") +
  geom_line(linewidth=1.1) +
  annotate("text", x=12, y=share_at_10-4,
           label=sprintf("Top 10%%: %.1f%%", share_at_10), hjust=0, size=3, colour="grey30") +
  annotate("text", x=27, y=share_at_25-4,
           label=sprintf("Top 25%%: %.1f%%", share_at_25), hjust=0, size=3, colour="grey30") +
  scale_x_continuous(labels=label_percent(scale=1)) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(x="Municipalities, ranked by capacity (cumulative %)",
       y="Cumulative share of national capacity",
       caption="2023, analysis sample.") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())
ggsave("fig_diag_4_charger_concentration.png", p4, width=6.5, height=4.5, dpi=300)
cat("Saved fig_diag_4_charger_concentration.png\n")


# =============================================================================
# 8. OUTCOME vs TREATMENT SCATTER (raw relationship, before any model)
# =============================================================================

p5 <- ggplot(analysis %>% filter(year==2023), aes(x=charger_access_asinh, y=ev_share_sqrt)) +
  geom_point(alpha=0.5, size=1.8) +
  geom_smooth(method="lm", se=TRUE, colour="black", linewidth=0.8) +
  labs(x="Charging capacity (asinh kW)", y="EV share (sqrt)",
       caption="2023, analysis sample. Raw bivariate relationship, no controls, for reference only.") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())
ggsave("fig_diag_5_outcome_vs_treatment_scatter.png", p5, width=6.5, height=4.5, dpi=300)
cat("Saved fig_diag_5_outcome_vs_treatment_scatter.png\n")

cat("\n=============================================================\n")
cat("DONE. 3 tables, 5 figures saved.\n")
cat("=============================================================\n")






library(tidyverse)
library(scales)

# =============================================================================
# FULL DIAGNOSTIC — master_panel_final.csv
#
# Produces:
#   table_diag_1_descriptives.csv   (all key variables, full sample)
#   table_diag_2_analysis_sample.csv (same, restricted to 351-muni balanced panel)
#   table_diag_3_correlations.csv    (correlation matrix of controls + outcome)
#   fig_diag_1_national_trends.png
#   fig_diag_2_outcome_distributions.png  (raw share vs sqrt share side by side)
#   fig_diag_3_control_distributions.png
#   fig_diag_4_charger_concentration.png
#   fig_diag_5_outcome_vs_treatment_scatter.png
#
# Required: master_panel_final.csv
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))

cat("=============================================================\n")
cat("1. STRUCTURE\n")
cat("=============================================================\n")
cat("Rows:", nrow(master), "| Municipalities:", n_distinct(master$municipality_code),
    "| Years:", paste(range(master$year), collapse="-"), "\n\n")

# ---- Balanced analysis sample (same filter used throughout tonight) -------

core <- c("ev_share_sqrt","charger_access_asinh","log_income","log_pop_density",
          "share_detached","median_age")
clean <- master %>% drop_na(all_of(core))
balanced <- clean %>% count(municipality_code) %>% filter(n==8) %>% pull(municipality_code)
analysis <- clean %>% filter(municipality_code %in% balanced)

cat("Balanced analysis sample:", n_distinct(analysis$municipality_code), "municipalities |",
    nrow(analysis), "rows (expect 351 / 2808)\n")
dropped <- setdiff(unique(master$municipality_code), balanced)
cat("Dropped:", paste(dropped, collapse=", "), "\n\n")

# =============================================================================
# 2. DESCRIPTIVE TABLES
# =============================================================================

desc_row <- function(x, label, unit="") {
  x <- x[!is.na(x)]
  tibble(Variable=label, Unit=unit, Mean=mean(x), SD=sd(x), Min=min(x),
         P25=quantile(x,.25), Median=median(x), P75=quantile(x,.75), Max=max(x),
         Skewness = mean((x-mean(x))^3)/sd(x)^3)
}

cat("=============================================================\n")
cat("2. DESCRIPTIVE STATISTICS (full panel, 2848 rows)\n")
cat("=============================================================\n")
desc_full <- bind_rows(
  desc_row(master$ev_share_raw*100, "EV share", "%"),
  desc_row(master$ev_share_sqrt, "EV share (sqrt)", "sqrt"),
  desc_row(master$charger_kw_cumulative, "Charging capacity", "kW"),
  desc_row(master$charger_access_asinh, "Charging capacity (asinh)", "asinh kW"),
  desc_row(master$income_after_tax_median, "Median income", "NOK"),
  desc_row(master$pop_density, "Population density", "per km2"),
  desc_row(master$share_detached*100, "Detached dwellings", "%"),
  desc_row(master$median_age, "Median age", "years"),
  desc_row(master$population, "Population", "persons")
)
print(desc_full %>% mutate(across(where(is.numeric), ~round(.x,3))), n=20, width=Inf)
write_csv(desc_full, "table_diag_1_descriptives.csv")

cat("\n=============================================================\n")
cat("2b. DESCRIPTIVE STATISTICS (balanced analysis sample, 351 munis)\n")
cat("=============================================================\n")
desc_bal <- bind_rows(
  desc_row(analysis$ev_share_raw*100, "EV share", "%"),
  desc_row(analysis$ev_share_sqrt, "EV share (sqrt)", "sqrt"),
  desc_row(analysis$charger_access_asinh, "Charging capacity (asinh)", "asinh kW"),
  desc_row(analysis$log_income, "Log income", ""),
  desc_row(analysis$log_pop_density, "Log pop density", ""),
  desc_row(analysis$share_detached*100, "Detached dwellings", "%"),
  desc_row(analysis$median_age, "Median age", "years")
)
print(desc_bal %>% mutate(across(where(is.numeric), ~round(.x,3))), width=Inf)
write_csv(desc_bal, "table_diag_2_analysis_sample.csv")

cat("\n(Skewness near 0 = symmetric. |Skew| > 1 = notably skewed.\n")
cat(" Compare 'EV share' vs 'EV share (sqrt)' skewness directly --\n")
cat(" this is the empirical justification for the transformation.)\n\n")

# =============================================================================
# 3. CORRELATION MATRIX
# =============================================================================

cat("=============================================================\n")
cat("3. CORRELATION MATRIX (analysis sample)\n")
cat("=============================================================\n")
corr_vars <- analysis %>% select(ev_share_sqrt, charger_access_asinh, log_income,
                                 log_pop_density, share_detached, median_age)
corr_mat <- cor(corr_vars, use="complete.obs")
print(round(corr_mat, 3))
write_csv(as_tibble(corr_mat, rownames="variable"), "table_diag_3_correlations.csv")
cat("\n(Watch for any control correlated with the outcome above ~0.7 --\n")
cat(" could indicate a control is too close to a proxy for the outcome.\n")
cat(" Also watch controls correlated with EACH OTHER above ~0.8 -- possible\n")
cat(" multicollinearity, e.g. log_income vs log_pop_density.)\n\n")

# =============================================================================
# 4. NATIONAL TRENDS
# =============================================================================

national <- analysis %>% group_by(year) %>% summarise(
  ev_share_nat = 100*sum(ev_count)/sum(fleet_total),
  kw_total = sum(charger_kw_cumulative),
  .groups="drop")

scale_factor <- max(national$kw_total)/max(national$ev_share_nat)
p1 <- ggplot(national, aes(x=year)) +
  geom_col(aes(y=kw_total/scale_factor), fill="grey80", width=0.6) +
  geom_line(aes(y=ev_share_nat), linewidth=1.1) +
  geom_point(aes(y=ev_share_nat), size=2) +
  scale_y_continuous(name="EV share (%)",
                     sec.axis=sec_axis(~.*scale_factor/1000, name="Charging capacity (MW)")) +
  scale_x_continuous(breaks=2016:2023) +
  labs(x=NULL, caption="Analysis sample, 351 municipalities.") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())
ggsave("fig_diag_1_national_trends.png", p1, width=7, height=4.5, dpi=300)
cat("Saved fig_diag_1_national_trends.png\n")

# =============================================================================
# 5. OUTCOME DISTRIBUTION: raw share vs sqrt share (the key justification plot)
# =============================================================================

dist_data <- analysis %>% select(ev_share_raw, ev_share_sqrt) %>%
  pivot_longer(everything(), names_to="var", values_to="value") %>%
  mutate(var = factor(var, levels=c("ev_share_raw","ev_share_sqrt"),
                      labels=c("EV share (raw)", "EV share (sqrt) -- final outcome")))

p2 <- ggplot(dist_data, aes(x=value)) +
  geom_histogram(bins=40, fill="grey50", colour="white", linewidth=0.1) +
  facet_wrap(~var, scales="free", ncol=2) +
  labs(x=NULL, y="Municipality-years",
       caption="Left: raw share, right-skewed with a mass near zero.\nRight: sqrt transform, the final outcome variable -- more symmetric, no clamp required.") +
  theme_minimal(base_size=10) +
  theme(panel.grid.minor=element_blank(), strip.text=element_text(face="bold"))
ggsave("fig_diag_2_outcome_distributions.png", p2, width=7, height=4, dpi=300)
cat("Saved fig_diag_2_outcome_distributions.png\n")

# =============================================================================
# 6. CONTROL DISTRIBUTIONS
# =============================================================================

ctrl_data <- analysis %>% select(log_income, log_pop_density, share_detached, median_age) %>%
  pivot_longer(everything(), names_to="var", values_to="value") %>%
  mutate(var = recode(var, log_income="Log income", log_pop_density="Log pop. density",
                      share_detached="Share detached", median_age="Median age"))

p3 <- ggplot(ctrl_data, aes(x=value)) +
  geom_histogram(bins=35, fill="grey50", colour="white", linewidth=0.1) +
  facet_wrap(~var, scales="free", ncol=2) +
  labs(x=NULL, y="Municipality-years") +
  theme_minimal(base_size=10) +
  theme(panel.grid.minor=element_blank(), strip.text=element_text(face="bold"))
ggsave("fig_diag_3_control_distributions.png", p3, width=7, height=5, dpi=300)
cat("Saved fig_diag_3_control_distributions.png\n")

# =============================================================================
# 7. CHARGER CONCENTRATION (2023)
# =============================================================================

conc <- analysis %>% filter(year==2023) %>% arrange(desc(charger_kw_cumulative)) %>%
  mutate(rank=row_number(), cum_kw=cumsum(charger_kw_cumulative),
         cum_share=100*cum_kw/sum(charger_kw_cumulative), pct_munis=100*rank/n())

p4 <- ggplot(conc, aes(x=pct_munis, y=cum_share)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey60") +
  geom_line(linewidth=1.1) +
  scale_x_continuous(labels=label_percent(scale=1)) +
  scale_y_continuous(labels=label_percent(scale=1)) +
  labs(x="Municipalities, ranked by capacity (cumulative %)",
       y="Cumulative share of national capacity",
       caption="2023, analysis sample.") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())
ggsave("fig_diag_4_charger_concentration.png", p4, width=6.5, height=4.5, dpi=300)
cat("Saved fig_diag_4_charger_concentration.png\n")





# =============================================================================
# 8. OUTCOME vs TREATMENT SCATTER (raw relationship, before any model)
# =============================================================================

p5 <- ggplot(analysis %>% filter(year==2023), aes(x=charger_access_asinh, y=ev_share_sqrt)) +
  geom_point(alpha=0.5, size=1.8) +
  geom_smooth(method="lm", se=TRUE, colour="black", linewidth=0.8) +
  labs(x="Charging capacity (asinh kW)", y="EV share (sqrt)",
       caption="2023, analysis sample. Raw bivariate relationship, no controls, for reference only.") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())
ggsave("fig_diag_5_outcome_vs_treatment_scatter.png", p5, width=6.5, height=4.5, dpi=300)
cat("Saved fig_diag_5_outcome_vs_treatment_scatter.png\n")

cat("\n=============================================================\n")
cat("DONE. 3 tables, 5 figures saved.\n")
cat("=============================================================\n")


library(tidyverse)

# =============================================================================
# MISSING NUMBERS FOR SECTION 3.4 — concentration figures, final panel
# Required: master_panel_final.csv
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))

core <- c("ev_share_sqrt","charger_access_asinh","log_income","log_pop_density","share_detached","median_age")
d_clean <- master %>% drop_na(all_of(core))
bal <- d_clean %>% count(municipality_code) %>% filter(n==8) %>% pull(municipality_code)
analysis <- d_clean %>% filter(municipality_code %in% bal)

conc <- analysis %>% filter(year==2023) %>% arrange(desc(charger_kw_cumulative)) %>%
  mutate(rank=row_number(), cum_kw=cumsum(charger_kw_cumulative),
         cum_share=100*cum_kw/sum(charger_kw_cumulative))

n <- nrow(conc)
top10_share <- conc$cum_share[floor(0.10*n)]
top25_share <- conc$cum_share[floor(0.25*n)]
zero_2023 <- sum(conc$charger_kw_cumulative==0)

cat("=== CONCENTRATION, 2023, FINAL PANEL ===\n")
cat("Top 10% of municipalities hold:", round(top10_share,1), "% of national capacity\n")
cat("Top 25% of municipalities hold:", round(top25_share,1), "% of national capacity\n")
cat("Municipalities with zero capacity in 2023:", zero_2023, "of", n, "\n\n")

cat("=== POPULATION vs CAPACITY INEQUALITY, 2023 ===\n")
gini <- function(x) {
  x <- sort(x); n <- length(x); cum <- cumsum(x)
  (n+1-2*sum(cum)/cum[length(cum)])/n
}
cat("Gini, charging capacity:", round(gini(conc$charger_kw_cumulative),3), "\n")
cat("Gini, population:", round(gini(conc$population),3), "\n")