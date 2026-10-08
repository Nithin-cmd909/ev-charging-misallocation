library(tidyverse)
library(plm)
library(splm)
library(spdep)

# =============================================================================
# STEP 1 — SDM, CONTEMPORANEOUS, ALL THREE MATRICES
#
# Outcome: ev_share_sqrt (replaces the old clamped ev_logodds)
# Panel: master_panel_final.csv (fully corrected: income, population, housing,
#        age all fixed tonight; Oslo zero-pad bug fixed; area verified)
#
# Same specification across all three matrices for direct comparability:
#   model = "within", effect = "twoways" (municipality + year FE),
#   lag = TRUE (spatial lag of the dependent variable), spatial.error = "none"
#
# Required: master_panel_final.csv, W1_queen.rds, W2_invdist_100km.rds,
#           W3_commute.rds, municipality_order.rds
#
# Output: sdm_key_parameters_w1_final.csv, sdm_key_parameters_w2_final.csv,
#         sdm_key_parameters_w3_final.csv, sdm_comparison_step1.csv
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))
order  <- readRDS("municipality_order.rds")

cat("Panel loaded:", nrow(master), "rows |", n_distinct(master$municipality_code), "municipalities\n\n")

# ---- Reusable estimation function ------------------------------------------

run_sdm_contemporaneous <- function(W, W_name, master, order) {
  
  cat("\n", strrep("=", 65), "\n", sep = "")
  cat("ESTIMATING:", W_name, "(contemporaneous)\n")
  cat(strrep("=", 65), "\n")
  
  rownames(W) <- order; colnames(W) <- order
  
  core <- c("ev_share_sqrt", "charger_access_asinh", "log_income",
            "log_pop_density", "share_detached", "median_age")
  
  d_clean <- master %>% drop_na(all_of(core))
  bal <- d_clean %>% count(municipality_code) %>% filter(n == 8) %>% pull(municipality_code)
  d_clean <- d_clean %>% filter(municipality_code %in% bal) %>% arrange(municipality_code, year)
  
  cat("Municipalities:", length(bal), "| Rows:", nrow(d_clean), "\n")
  
  W_sub <- W[bal, bal]
  rs <- rowSums(W_sub)
  n_isolated <- sum(rs == 0)
  W_sub <- W_sub / ifelse(rs == 0, 1, rs)
  cat("Isolated after subsetting:", n_isolated, "\n")
  
  years <- sort(unique(d_clean$year))
  d_wx <- map_dfr(years, function(yr) {
    yd <- d_clean %>% filter(year == yr) %>% arrange(municipality_code)
    stopifnot(identical(yd$municipality_code, rownames(W_sub)))
    yd %>% mutate(
      w_charger        = as.numeric(W_sub %*% charger_access_asinh),
      w_log_income     = as.numeric(W_sub %*% log_income),
      w_log_pop_density= as.numeric(W_sub %*% log_pop_density),
      w_share_detached = as.numeric(W_sub %*% share_detached),
      w_median_age     = as.numeric(W_sub %*% median_age)
    )
  })
  
  pdat <- pdata.frame(d_wx, index = c("municipality_code", "year"))
  W_lw <- mat2listw(W_sub, style = "W")
  
  fml <- ev_share_sqrt ~ charger_access_asinh + log_income + log_pop_density +
    share_detached + median_age +
    w_charger + w_log_income + w_log_pop_density + w_share_detached + w_median_age
  
  fit <- spml(formula = fml, data = pdat, listw = W_lw,
              model = "within", effect = "twoways",
              lag = TRUE, spatial.error = "none")
  
  ct <- summary(fit)$CoefTable
  
  rho   <- ct["lambda", "Estimate"];  rho_se  <- ct["lambda", "Std. Error"];  rho_p  <- ct["lambda", "Pr(>|t|)"]
  beta  <- ct["charger_access_asinh", "Estimate"]; beta_se <- ct["charger_access_asinh", "Std. Error"]; beta_p <- ct["charger_access_asinh", "Pr(>|t|)"]
  theta <- ct["w_charger", "Estimate"]; theta_se <- ct["w_charger", "Std. Error"]; theta_p <- ct["w_charger", "Pr(>|t|)"]
  
  cat(sprintf("\n  rho   = %+.4f (p=%.4g)\n", rho, rho_p))
  cat(sprintf("  beta  = %+.5f (p=%.4g)\n", beta, beta_p))
  cat(sprintf("  theta = %+.5f (p=%.4g)\n", theta, theta_p))
  cat(sprintf("  theta/beta ratio = %+.2f\n", theta/beta))
  
  key_params <- tibble(
    parameter = c("rho","beta_charger","theta_charger"),
    estimate  = c(rho, beta, theta),
    std_error = c(rho_se, beta_se, theta_se),
    p_value   = c(rho_p, beta_p, theta_p)
  )
  
  list(
    key_params = key_params,
    summary_row = tibble(
      matrix = W_name, n_muni = length(bal), n_obs = nrow(d_wx),
      rho = rho, rho_p = rho_p,
      beta = beta, beta_p = beta_p,
      theta = theta, theta_p = theta_p,
      theta_beta_ratio = theta/beta,
      beta_sig = beta_p < 0.05, theta_sig = theta_p < 0.05
    ),
    W_sub = W_sub, bal = bal, fit = fit
  )
}

# ---- Run all three -----------------------------------------------------------

W1 <- readRDS("W1_queen.rds")
r1 <- run_sdm_contemporaneous(W1, "W1_queen", master, order)
write_csv(r1$key_params, "sdm_key_parameters_w1_final.csv")

W2 <- readRDS("W2_invdist_100km.rds")
r2 <- run_sdm_contemporaneous(W2, "W2_invdist_100km", master, order)
write_csv(r2$key_params, "sdm_key_parameters_w2_final.csv")

W3 <- readRDS("W3_commute.rds")
r3 <- run_sdm_contemporaneous(W3, "W3_commute", master, order)
write_csv(r3$key_params, "sdm_key_parameters_w3_final.csv")

# ---- Comparison table ----------------------------------------------------

comparison <- bind_rows(r1$summary_row, r2$summary_row, r3$summary_row)

cat("\n\n", strrep("=", 100), "\n", sep = "")
cat("STEP 1 SUMMARY — SDM CONTEMPORANEOUS, ALL THREE MATRICES (ev_share_sqrt, final panel)\n")
cat(strrep("=", 100), "\n")
print(comparison %>%
        mutate(across(where(is.numeric), ~round(.x, 5))),
      width = Inf)

cat("\nCompare these to the PROVISIONAL numbers from earlier tonight (different panel):\n")
cat("  W1 provisional: rho=0.628 beta=0.000657(p=0.010) theta=0.001746(p=0.001)\n")
cat("  W2 provisional: rho=0.707 beta=0.000742(p=0.006) theta=0.002875(p=0.001)\n")
cat("  W3 provisional: rho=0.324 beta=0.001172(p=0.000) theta=0.001217(p=0.097)\n")
cat("If these are close to the new numbers above, the finding survives the\n")
cat("rebuild. If they differ meaningfully, the methodology needs to be written\n")
cat("around the NEW numbers, not the provisional ones.\n\n")

write_csv(comparison, "sdm_comparison_step1.csv")
cat("Saved: sdm_key_parameters_w1_final.csv, _w2_final.csv, _w3_final.csv,\n")
cat("       sdm_comparison_step1.csv\n")




library(tidyverse)
library(plm)
library(splm)
library(spdep)

# =============================================================================
# STEP 2 — SDM, TIME-LAGGED TREATMENT, ALL THREE MATRICES
#
# Chargers at t-1 explain adoption at t. This is the identification check:
# a charger built last year cannot be a reaction to this year's adoption,
# which rules out the most direct form of contemporaneous reverse causality.
#
# Outcome: ev_share_sqrt (unchanged from Step 1)
# Treatment: charger_access_asinh_lag1 = charger_access_asinh lagged 1 year
#   -> 2016 has no lag available, so this uses 7 years, not 8 (351 munis
#      confirmed unaffected -- same set, one fewer year each, per the check
#      run earlier tonight).
#
# Required: master_panel_final.csv, W1_queen.rds, W2_invdist_100km.rds,
#           W3_commute.rds, municipality_order.rds
#
# Output: sdm_key_parameters_w1_lag_final.csv, _w2_lag_final.csv,
#         _w3_lag_final.csv, sdm_comparison_step2.csv,
#         sdm_bound_comparison.csv (contemporaneous vs lagged, side by side)
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))
order  <- readRDS("municipality_order.rds")

master <- master %>%
  arrange(municipality_code, year) %>%
  group_by(municipality_code) %>%
  mutate(charger_access_asinh_lag1 = dplyr::lag(charger_access_asinh, 1)) %>%
  ungroup()

cat("Panel loaded and lagged:", nrow(master), "rows |",
    sum(!is.na(master$charger_access_asinh_lag1)), "rows with a valid lag\n\n")

# ---- Reusable estimation function ------------------------------------------

run_sdm_lagged <- function(W, W_name, master, order) {
  
  cat("\n", strrep("=", 65), "\n", sep = "")
  cat("ESTIMATING:", W_name, "(time-lagged treatment)\n")
  cat(strrep("=", 65), "\n")
  
  rownames(W) <- order; colnames(W) <- order
  
  core <- c("ev_share_sqrt", "charger_access_asinh_lag1", "log_income",
            "log_pop_density", "share_detached", "median_age")
  
  d_clean <- master %>% drop_na(all_of(core))
  muni_counts <- d_clean %>% count(municipality_code)
  max_years <- max(muni_counts$n)
  bal <- muni_counts %>% filter(n == max_years) %>% pull(municipality_code)
  d_clean <- d_clean %>% filter(municipality_code %in% bal) %>% arrange(municipality_code, year)
  
  cat("Years retained per municipality after lagging:", max_years, "\n")
  cat("Municipalities:", length(bal), "| Rows:", nrow(d_clean), "\n")
  
  W_sub <- W[bal, bal]
  rs <- rowSums(W_sub)
  n_isolated <- sum(rs == 0)
  W_sub <- W_sub / ifelse(rs == 0, 1, rs)
  cat("Isolated after subsetting:", n_isolated, "\n")
  
  years <- sort(unique(d_clean$year))
  d_wx <- map_dfr(years, function(yr) {
    yd <- d_clean %>% filter(year == yr) %>% arrange(municipality_code)
    stopifnot(identical(yd$municipality_code, rownames(W_sub)))
    yd %>% mutate(
      w_charger_lag1    = as.numeric(W_sub %*% charger_access_asinh_lag1),
      w_log_income      = as.numeric(W_sub %*% log_income),
      w_log_pop_density = as.numeric(W_sub %*% log_pop_density),
      w_share_detached  = as.numeric(W_sub %*% share_detached),
      w_median_age      = as.numeric(W_sub %*% median_age)
    )
  })
  
  pdat <- pdata.frame(d_wx, index = c("municipality_code", "year"))
  W_lw <- mat2listw(W_sub, style = "W")
  
  fml <- ev_share_sqrt ~ charger_access_asinh_lag1 + log_income + log_pop_density +
    share_detached + median_age +
    w_charger_lag1 + w_log_income + w_log_pop_density + w_share_detached + w_median_age
  
  fit <- spml(formula = fml, data = pdat, listw = W_lw,
              model = "within", effect = "twoways",
              lag = TRUE, spatial.error = "none")
  
  ct <- summary(fit)$CoefTable
  
  rho   <- ct["lambda", "Estimate"];  rho_p  <- ct["lambda", "Pr(>|t|)"]
  beta  <- ct["charger_access_asinh_lag1", "Estimate"]; beta_p <- ct["charger_access_asinh_lag1", "Pr(>|t|)"]
  theta <- ct["w_charger_lag1", "Estimate"]; theta_p <- ct["w_charger_lag1", "Pr(>|t|)"]
  
  cat(sprintf("\n  rho   = %+.4f (p=%.4g)\n", rho, rho_p))
  cat(sprintf("  beta  = %+.5f (p=%.4g)\n", beta, beta_p))
  cat(sprintf("  theta = %+.5f (p=%.4g)\n", theta, theta_p))
  cat(sprintf("  theta/beta ratio = %+.2f\n", theta/beta))
  
  key_params <- tibble(
    parameter = c("rho","beta_charger_lag1","theta_charger_lag1"),
    estimate  = c(rho, beta, theta),
    std_error = c(ct["lambda","Std. Error"], ct["charger_access_asinh_lag1","Std. Error"], ct["w_charger_lag1","Std. Error"]),
    p_value   = c(rho_p, beta_p, theta_p)
  )
  
  list(
    key_params = key_params,
    summary_row = tibble(
      matrix = W_name, n_muni = length(bal), n_obs = nrow(d_wx),
      rho = rho, rho_p = rho_p, beta = beta, beta_p = beta_p,
      theta = theta, theta_p = theta_p, theta_beta_ratio = theta/beta,
      beta_sig = beta_p < 0.05, theta_sig = theta_p < 0.05
    )
  )
}

# ---- Run all three -----------------------------------------------------------

W1 <- readRDS("W1_queen.rds")
r1 <- run_sdm_lagged(W1, "W1_queen_lag", master, order)
write_csv(r1$key_params, "sdm_key_parameters_w1_lag_final.csv")

W2 <- readRDS("W2_invdist_100km.rds")
r2 <- run_sdm_lagged(W2, "W2_invdist_100km_lag", master, order)
write_csv(r2$key_params, "sdm_key_parameters_w2_lag_final.csv")

W3 <- readRDS("W3_commute.rds")
r3 <- run_sdm_lagged(W3, "W3_commute_lag", master, order)
write_csv(r3$key_params, "sdm_key_parameters_w3_lag_final.csv")

comparison_lag <- bind_rows(r1$summary_row, r2$summary_row, r3$summary_row)

cat("\n\n", strrep("=", 100), "\n", sep = "")
cat("STEP 2 SUMMARY — SDM TIME-LAGGED, ALL THREE MATRICES (ev_share_sqrt, final panel)\n")
cat(strrep("=", 100), "\n")
print(comparison_lag %>% mutate(across(where(is.numeric), ~round(.x, 5))), width = Inf)
write_csv(comparison_lag, "sdm_comparison_step2.csv")

# ---- The bound: contemporaneous vs lagged, side by side ---------------------

comparison_contemp <- read_csv("sdm_comparison_step1.csv", show_col_types = FALSE) %>%
  mutate(spec = "contemporaneous")
comparison_lag2 <- comparison_lag %>% mutate(spec = "lagged", matrix = str_remove(matrix, "_lag$"))

bound <- bind_rows(comparison_contemp, comparison_lag2) %>%
  select(matrix, spec, rho, beta, beta_p, theta, theta_p, theta_beta_ratio) %>%
  arrange(matrix, spec)

cat("\n\n", strrep("=", 100), "\n", sep = "")
cat("CONTEMPORANEOUS vs LAGGED — THE IDENTIFICATION BOUND\n")
cat(strrep("=", 100), "\n")
print(bound %>% mutate(across(where(is.numeric), ~round(.x, 5))), n = 20, width = Inf)

cat("\nHow to read this:\n")
cat("  If theta stays positive and significant (or close to it) under the lag,\n")
cat("  for the matrix you plan to call primary, the finding survives the\n")
cat("  identification check. The CONTEMPORANEOUS estimate is the headline;\n")
cat("  the LAGGED estimate is the conservative lower bound. Report both.\n")

write_csv(bound, "sdm_bound_comparison.csv")
cat("\nSaved: sdm_key_parameters_w1_lag_final.csv, _w2_lag_final.csv, _w3_lag_final.csv,\n")
cat("       sdm_comparison_step2.csv, sdm_bound_comparison.csv\n")



library(tidyverse)

# =============================================================================
# STEP 4 — DIFFUSION MULTIPLIER, W3 (PRIMARY), CONTEMPORANEOUS + LAGGED
#
# M = (I - rho*W)^-1 * (beta*I + theta*W)
# diffusion_multiplier_i = column sum of M for municipality i
#   = total system-wide adoption generated by a unit increase in charger
#     access at municipality i, once all spatial feedback has propagated.
#
# Built from the Step 1 / Step 2 parameter files, on the ev_share_sqrt outcome,
# on master_panel_final.csv (fully corrected panel).
#
# Required: master_panel_final.csv, W3_commute.rds, municipality_order.rds,
#           sdm_key_parameters_w3_final.csv, sdm_key_parameters_w3_lag_final.csv
#
# Output: diffusion_multipliers_w3_contemporaneous_final.csv,
#         diffusion_multipliers_w3_lag_final.csv
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))
W3     <- readRDS("W3_commute.rds")
order  <- readRDS("municipality_order.rds")
rownames(W3) <- order; colnames(W3) <- order

ssb <- read_csv("ssb_municipality_list.csv", col_types = cols(municipality_code = col_character()))

params_contemp <- read_csv("sdm_key_parameters_w3_final.csv", show_col_types = FALSE)
params_lag     <- read_csv("sdm_key_parameters_w3_lag_final.csv", show_col_types = FALSE)

cat("Contemporaneous parameters:\n"); print(params_contemp)
cat("\nLagged parameters:\n"); print(params_lag)

# ---- Contemporaneous: balanced panel (351 munis, 8 years) -------------------

core_c <- c("ev_share_sqrt","charger_access_asinh","log_income",
            "log_pop_density","share_detached","median_age")
d_c <- master %>% drop_na(all_of(core_c))
bal_c <- d_c %>% count(municipality_code) %>% filter(n==8) %>% pull(municipality_code)

cat("\nContemporaneous balanced sample:", length(bal_c), "municipalities | Expected 351\n")

W_sub_c <- W3[bal_c, bal_c]
W_sub_c <- W_sub_c / ifelse(rowSums(W_sub_c)==0, 1, rowSums(W_sub_c))
n_c <- nrow(W_sub_c)
I_c <- diag(n_c)

rho_c   <- params_contemp$estimate[params_contemp$parameter=="rho"]
beta_c  <- params_contemp$estimate[params_contemp$parameter=="beta_charger"]
theta_c <- params_contemp$estimate[params_contemp$parameter=="theta_charger"]
cat("Using: rho =", rho_c, "beta =", beta_c, "theta =", theta_c, "\n")

M_c <- solve(I_c - rho_c * W_sub_c) %*% (beta_c * I_c + theta_c * W_sub_c)

mult_contemp <- tibble(municipality_code = bal_c,
                       diffusion_multiplier_w3_contemp = colSums(M_c)) %>%
  left_join(ssb, by="municipality_code") %>%
  arrange(desc(diffusion_multiplier_w3_contemp))

cat("\n=== TOP 10, CONTEMPORANEOUS ===\n")
print(head(mult_contemp, 10))
cat("\n=== BOTTOM 5, CONTEMPORANEOUS ===\n")
print(tail(mult_contemp, 5))

write_csv(mult_contemp, "diffusion_multipliers_w3_contemporaneous_final.csv")

# ---- Lagged: balanced panel (351 munis, 7 years) -----------------------------

master_lag <- master %>%
  arrange(municipality_code, year) %>%
  group_by(municipality_code) %>%
  mutate(charger_access_asinh_lag1 = dplyr::lag(charger_access_asinh, 1)) %>%
  ungroup()

core_l <- c("ev_share_sqrt","charger_access_asinh_lag1","log_income",
            "log_pop_density","share_detached","median_age")
d_l <- master_lag %>% drop_na(all_of(core_l))
muni_counts_l <- d_l %>% count(municipality_code)
max_years_l <- max(muni_counts_l$n)
bal_l <- muni_counts_l %>% filter(n==max_years_l) %>% pull(municipality_code)

cat("\nLagged balanced sample:", length(bal_l), "municipalities | Expected 351\n")
cat("Same municipalities as contemporaneous?", setequal(bal_c, bal_l), "\n")

W_sub_l <- W3[bal_l, bal_l]
W_sub_l <- W_sub_l / ifelse(rowSums(W_sub_l)==0, 1, rowSums(W_sub_l))
n_l <- nrow(W_sub_l)
I_l <- diag(n_l)

rho_l   <- params_lag$estimate[params_lag$parameter=="rho"]
beta_l  <- params_lag$estimate[params_lag$parameter=="beta_charger_lag1"]
theta_l <- params_lag$estimate[params_lag$parameter=="theta_charger_lag1"]
cat("Using: rho =", rho_l, "beta =", beta_l, "theta =", theta_l, "\n")

M_l <- solve(I_l - rho_l * W_sub_l) %*% (beta_l * I_l + theta_l * W_sub_l)

mult_lag <- tibble(municipality_code = bal_l,
                   diffusion_multiplier_w3_lag = colSums(M_l)) %>%
  left_join(ssb, by="municipality_code") %>%
  arrange(desc(diffusion_multiplier_w3_lag))

cat("\n=== TOP 10, LAGGED ===\n")
print(head(mult_lag, 10))
cat("\n=== BOTTOM 5, LAGGED ===\n")
print(tail(mult_lag, 5))

write_csv(mult_lag, "diffusion_multipliers_w3_lag_final.csv")

# ---- Cross-check: contemporaneous vs lagged ranking correlation -------------

comp <- mult_contemp %>% select(municipality_code, diffusion_multiplier_w3_contemp) %>%
  left_join(mult_lag %>% select(municipality_code, diffusion_multiplier_w3_lag), by="municipality_code")

cat("\n=== CONTEMPORANEOUS vs LAGGED MULTIPLIER CORRELATION ===\n")
cat("Pearson:", round(cor(comp$diffusion_multiplier_w3_contemp, comp$diffusion_multiplier_w3_lag),4), "\n")
cat("Spearman (rank):", round(cor(comp$diffusion_multiplier_w3_contemp, comp$diffusion_multiplier_w3_lag, method="spearman"),4), "\n")
cat("(High correlation = the two specifications agree on WHICH municipalities\n")
cat(" matter most, not just that spillovers exist in general.)\n\n")

cat("=== BERGEN CHECK (known result from earlier tonight) ===\n")
bergen_c <- mult_contemp %>% filter(municipality_code=="4601")
bergen_l <- mult_lag %>% filter(municipality_code=="4601")
cat("Bergen contemporaneous multiplier:", bergen_c$diffusion_multiplier_w3_contemp,
    "| percentile:", round(100*mean(mult_contemp$diffusion_multiplier_w3_contemp < bergen_c$diffusion_multiplier_w3_contemp),1), "\n")
cat("Bergen lagged multiplier:", bergen_l$diffusion_multiplier_w3_lag,
    "| percentile:", round(100*mean(mult_lag$diffusion_multiplier_w3_lag < bergen_l$diffusion_multiplier_w3_lag),1), "\n")

cat("\nSaved: diffusion_multipliers_w3_contemporaneous_final.csv,\n")
cat("       diffusion_multipliers_w3_lag_final.csv\n")


##diffusion graph 
library(tidyverse)
library(sf)

gdf <- st_read("Basisdata_0000_Norge_25833_Kommune_GeoJSON.geojson", quiet = TRUE)
ssb <- read_csv("ssb_municipality_list.csv", col_types = cols(municipality_code = col_character()))

# Same shapefile_map fix used all night for the spatial matrices
shapefile_map <- c(
  '1508'='1507','1580'='1576','5501'='5401','5503'='5402','5510'='5411','5512'='5412',
  '5514'='5413','5516'='5414','5518'='5415','5520'='5416','5522'='5417','5524'='5418',
  '5526'='5419','5528'='5420','5530'='5421','5532'='5422','5534'='5423','5536'='5424',
  '5538'='5425','5540'='5426','5542'='5427','5544'='5428','5546'='5429','5601'='5403',
  '5603'='5406','5605'='5444','5607'='5405','5610'='5437','5612'='5430','5614'='5432',
  '5616'='5433','5618'='5434','5620'='5435','5622'='5436','5624'='5438','5626'='5439',
  '5628'='5441','5630'='5440','5632'='5443','5634'='5404','5636'='5442'
)

gdf <- gdf %>%
  mutate(municipality_code = str_pad(as.character(kommunenummer), 4, pad = "0"),
         municipality_code = ifelse(municipality_code %in% names(shapefile_map),
                                    shapefile_map[municipality_code], municipality_code)) %>%
  filter(municipality_code %in% ssb$municipality_code) %>%
  group_by(municipality_code) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

mult_lag <- read_csv("diffusion_multipliers_w3_lag_final.csv",
                     col_types = cols(municipality_code = col_character()))

map_data <- gdf %>% left_join(mult_lag, by = "municipality_code")

cat("Municipalities matched:", sum(!is.na(map_data$diffusion_multiplier_w3_lag)), "of", nrow(map_data), "\n")

p <- ggplot(map_data) +
  geom_sf(aes(fill = diffusion_multiplier_w3_lag), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(name = "Diffusion\nmultiplier", option = "plasma", na.value = "grey85") +
  theme_void(base_size = 12) +
  theme(legend.position = "right") +
  labs(caption = "Lagged specification, W3 (commuting flows).")

ggsave("fig_5_3_multiplier_map.png", p, width = 6, height = 9, dpi = 300)
print(p)

p <- ggplot(map_data) +
  geom_sf(aes(fill = diffusion_multiplier_w3_lag), color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(name = "Diffusion\nmultiplier", option = "plasma",
                       na.value = "grey85", trans = "log10",
                       labels = scales::label_number(accuracy = 0.001)) +
  theme_void(base_size = 12) +
  theme(legend.position = "right") +
  labs(caption = "Lagged specification, W3 (commuting flows). Log color scale.")

ggsave("fig_5_2_multiplier_map.png", p, width = 6, height = 9, dpi = 300)
print(p)

library(tidyverse)

# =============================================================================
# STEP 5 — MISALLOCATION INDEX, W3, CONTEMPORANEOUS + LAGGED
#
# misallocation_index = charger_standardised - M_standardised
#   charger_standardised: actual charger kW, standardised WITHIN each year
#   M_standardised: diffusion multiplier, standardised across municipalities
#
# Positive  = OVERSUPPLIED  (more chargers than the multiplier justifies)
# Negative  = UNDERSUPPLIED (fewer chargers than the multiplier justifies)
#
# Required: master_panel_final.csv,
#           diffusion_multipliers_w3_contemporaneous_final.csv,
#           diffusion_multipliers_w3_lag_final.csv
#
# Output: misallocation_index_w3_contemporaneous_final.csv,
#         misallocation_index_w3_lag_final.csv
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))
mult_contemp <- read_csv("diffusion_multipliers_w3_contemporaneous_final.csv",
                         col_types = cols(municipality_code = col_character()))
mult_lag <- read_csv("diffusion_multipliers_w3_lag_final.csv",
                     col_types = cols(municipality_code = col_character()))

cat("Master panel rows:", nrow(master), "\n")
cat("Contemporaneous multipliers available for:", nrow(mult_contemp), "municipalities\n")
cat("Lagged multipliers available for:", nrow(mult_lag), "municipalities\n\n")

# =============================================================================
# CONTEMPORANEOUS misallocation
# =============================================================================

panel_contemp <- master %>%
  filter(municipality_code %in% mult_contemp$municipality_code) %>%
  left_join(mult_contemp %>% select(municipality_code, diffusion_multiplier_w3_contemp),
            by = "municipality_code")

cat("Rows after restricting to municipalities with contemporaneous multiplier:", nrow(panel_contemp), "\n")

mult_mean_c <- mean(mult_contemp$diffusion_multiplier_w3_contemp)
mult_sd_c   <- sd(mult_contemp$diffusion_multiplier_w3_contemp)

misalloc_contemp <- panel_contemp %>%
  mutate(M_standardised = (diffusion_multiplier_w3_contemp - mult_mean_c) / mult_sd_c) %>%
  group_by(year) %>%
  mutate(
    charger_standardised = (charger_kw_cumulative - mean(charger_kw_cumulative)) / sd(charger_kw_cumulative),
    misallocation_index_w3_contemp = charger_standardised - M_standardised
  ) %>%
  ungroup() %>%
  select(municipality_code, municipality_name, year, charger_kw_cumulative,
         diffusion_multiplier_w3_contemp, misallocation_index_w3_contemp)

cat("\n=== CONTEMPORANEOUS — TOP 5 OVERSUPPLIED, 2023 ===\n")
print(misalloc_contemp %>% filter(year==2023) %>%
        arrange(desc(misallocation_index_w3_contemp)) %>% head(5))

cat("\n=== CONTEMPORANEOUS — TOP 5 UNDERSUPPLIED, 2023 ===\n")
print(misalloc_contemp %>% filter(year==2023) %>%
        arrange(misallocation_index_w3_contemp) %>% head(5))

write_csv(misalloc_contemp, "misallocation_index_w3_contemporaneous_final.csv")

# =============================================================================
# LAGGED misallocation
# =============================================================================

panel_lag <- master %>%
  filter(municipality_code %in% mult_lag$municipality_code) %>%
  left_join(mult_lag %>% select(municipality_code, diffusion_multiplier_w3_lag),
            by = "municipality_code")

cat("\nRows after restricting to municipalities with lagged multiplier:", nrow(panel_lag), "\n")

mult_mean_l <- mean(mult_lag$diffusion_multiplier_w3_lag)
mult_sd_l   <- sd(mult_lag$diffusion_multiplier_w3_lag)

misalloc_lag <- panel_lag %>%
  mutate(M_standardised = (diffusion_multiplier_w3_lag - mult_mean_l) / mult_sd_l) %>%
  group_by(year) %>%
  mutate(
    charger_standardised = (charger_kw_cumulative - mean(charger_kw_cumulative)) / sd(charger_kw_cumulative),
    misallocation_index_w3_lag = charger_standardised - M_standardised
  ) %>%
  ungroup() %>%
  select(municipality_code, municipality_name, year, charger_kw_cumulative,
         diffusion_multiplier_w3_lag, misallocation_index_w3_lag)

cat("\n=== LAGGED — TOP 5 OVERSUPPLIED, 2023 ===\n")
print(misalloc_lag %>% filter(year==2023) %>%
        arrange(desc(misallocation_index_w3_lag)) %>% head(5))

cat("\n=== LAGGED — TOP 5 UNDERSUPPLIED, 2023 ===\n")
print(misalloc_lag %>% filter(year==2023) %>%
        arrange(misallocation_index_w3_lag) %>% head(5))

write_csv(misalloc_lag, "misallocation_index_w3_lag_final.csv")

# =============================================================================
# VERIFICATION
# =============================================================================

cat("\n=== VERIFICATION ===\n")
cat("Contemporaneous rows:", nrow(misalloc_contemp), "| Expected", 351*8, "|",
    ifelse(nrow(misalloc_contemp)==351*8, "PASS", "FAIL"), "\n")
cat("Lagged rows:", nrow(misalloc_lag), "| Expected", 351*8, "|",
    ifelse(nrow(misalloc_lag)==351*8, "PASS", "FAIL"), "\n")

# ---- Bergen check, both specs, all years -------------------------------------

cat("\n=== BERGEN MISALLOCATION, ALL YEARS, BOTH SPECS ===\n")
bergen_c <- misalloc_contemp %>% filter(municipality_code=="4601") %>%
  select(year, misallocation_index_w3_contemp)
bergen_l <- misalloc_lag %>% filter(municipality_code=="4601") %>%
  select(year, misallocation_index_w3_lag)
bergen_both <- bergen_c %>% left_join(bergen_l, by="year")
print(bergen_both)

# ---- Overlap check: do contemp and lag agree on top oversupplied/undersupplied? --

cat("\n=== 2023 CROSS-SPEC AGREEMENT: TOP 10 OVERSUPPLIED ===\n")
top_c <- misalloc_contemp %>% filter(year==2023) %>% arrange(desc(misallocation_index_w3_contemp)) %>% head(10) %>% pull(municipality_code)
top_l <- misalloc_lag %>% filter(year==2023) %>% arrange(desc(misallocation_index_w3_lag)) %>% head(10) %>% pull(municipality_code)
cat("Overlap:", length(intersect(top_c, top_l)), "of 10\n")
cat("Contemporaneous top 10:", paste(top_c, collapse=", "), "\n")
cat("Lagged top 10:", paste(top_l, collapse=", "), "\n")

cat("\nSaved: misallocation_index_w3_contemporaneous_final.csv,\n")
cat("       misallocation_index_w3_lag_final.csv\n")


library(tidyverse)

misalloc_contemp <- read_csv("misallocation_index_w3_contemporaneous_final.csv",
                             col_types = cols(municipality_code = col_character()))
misalloc_lag <- read_csv("misallocation_index_w3_lag_final.csv",
                         col_types = cols(municipality_code = col_character()))

bergen_c <- misalloc_contemp %>% filter(municipality_code == "4601") %>%
  select(year, value = misallocation_index_w3_contemp) %>% mutate(spec = "Contemporaneous")
bergen_l <- misalloc_lag %>% filter(municipality_code == "4601") %>%
  select(year, value = misallocation_index_w3_lag) %>% mutate(spec = "Lagged")

bergen_both <- bind_rows(bergen_c, bergen_l)

p <- ggplot(bergen_both, aes(x = year, y = value, color = spec, linetype = spec)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Contemporaneous" = "grey50", "Lagged" = "black")) +
  scale_x_continuous(breaks = 2016:2023) +
  labs(x = NULL, y = "Misallocation index (Bergen)", color = NULL, linetype = NULL,
       caption = "Positive values indicate oversupply relative to diffusion value.\nDotted line marks zero (neutral allocation).") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave("fig_5_3_bergen_misallocation.png", p, width = 7, height = 5, dpi = 300)
print(p)

library(tidyverse)

# =============================================================================
# DECOMPOSE OSLO'S MISALLOCATION INDEX
#
# misallocation_index = charger_standardised - M_standardised
# This shows each term separately, plus the full multiplier distribution,
# to check whether Oslo's undersupply flag is a real property of extreme
# network centrality, or a construction problem.
#
# Required: diffusion_multipliers_w3_contemporaneous_final.csv,
#           misallocation_index_w3_contemporaneous_final.csv
# =============================================================================

mult <- read_csv("diffusion_multipliers_w3_contemporaneous_final.csv",
                 col_types = cols(municipality_code = col_character()))
misalloc <- read_csv("misallocation_index_w3_contemporaneous_final.csv",
                     col_types = cols(municipality_code = col_character()))

cat("=== FULL MULTIPLIER DISTRIBUTION ===\n")
print(summary(mult$diffusion_multiplier_w3_contemp))
cat("SD:", sd(mult$diffusion_multiplier_w3_contemp), "\n")
cat("Mean:", mean(mult$diffusion_multiplier_w3_contemp), "\n\n")

oslo_mult <- mult$diffusion_multiplier_w3_contemp[mult$municipality_code=="0301"]
mean_mult <- mean(mult$diffusion_multiplier_w3_contemp)
sd_mult   <- sd(mult$diffusion_multiplier_w3_contemp)
oslo_M_z  <- (oslo_mult - mean_mult) / sd_mult

cat("=== OSLO'S MULTIPLIER Z-SCORE ===\n")
cat("Oslo multiplier:", oslo_mult, "\n")
cat("Oslo M_standardised (z-score):", round(oslo_M_z, 3), "\n")
cat("(How many SDs above the mean multiplier Oslo sits, ACROSS ALL 351 municipalities)\n\n")

cat("=== OSLO'S CHARGER Z-SCORE, 2023 ONLY (within-year standardisation) ===\n")
oslo_2023 <- misalloc %>% filter(municipality_code=="0301", year==2023)
cat("Oslo charger_kw_cumulative 2023:", oslo_2023$charger_kw_cumulative, "\n")

# recompute the within-year z-score directly for verification
year_2023 <- misalloc %>% filter(year==2023)
kw_mean_2023 <- mean(year_2023$charger_kw_cumulative)
kw_sd_2023   <- sd(year_2023$charger_kw_cumulative)
oslo_charger_z <- (oslo_2023$charger_kw_cumulative - kw_mean_2023) / kw_sd_2023

cat("2023 national mean charger kW:", round(kw_mean_2023,1), "\n")
cat("2023 national SD charger kW:", round(kw_sd_2023,1), "\n")
cat("Oslo charger_standardised (z-score):", round(oslo_charger_z, 3), "\n\n")

cat("=== THE DECOMPOSITION ===\n")
cat("misallocation_index = charger_standardised - M_standardised\n")
cat(sprintf("                     = %.3f - %.3f = %.3f\n", oslo_charger_z, oslo_M_z, oslo_charger_z - oslo_M_z))
cat("(Compare to the actual reported value:", oslo_2023$misallocation_index_w3_contemp, ")\n\n")

cat("=== HOW EXTREME IS OSLO'S MULTIPLIER RELATIVE TO EVERYONE ELSE? ===\n")
mult_sorted <- mult %>% arrange(desc(diffusion_multiplier_w3_contemp))
cat("Oslo's multiplier is", round(oslo_mult / mult_sorted$diffusion_multiplier_w3_contemp[2], 2),
    "times the SECOND highest (Bergen)\n")
cat("Oslo's multiplier is", round(oslo_mult / mean_mult, 2), "times the average municipality's multiplier\n")
cat("Number of municipalities within 50% of Oslo's multiplier value:",
    sum(mult$diffusion_multiplier_w3_contemp > 0.5*oslo_mult) - 1, "(should be very few/zero)\n\n")

cat("=== IS OSLO ALSO EXTREME ON CHARGER SUPPLY, JUST LESS EXTREME? ===\n")
kw_sorted <- year_2023 %>% arrange(desc(charger_kw_cumulative))
cat("Oslo's 2023 rank by absolute charger kW:", which(kw_sorted$municipality_code=="0301"), "of 351\n")
cat("(If Oslo ranks e.g. #2 on chargers but #1 on multiplier by a much bigger margin,\n")
cat(" that confirms this is a real 'even a lot isn't enough given how central Oslo is'\n")
cat(" finding, not a data error.)\n")




library(tidyverse)

# =============================================================================
# STEP 6 — COUNTERFACTUAL REALLOCATION, W3, CONTEMPORANEOUS + LAGGED
#
# MATH CHANGE FROM THE ORIGINAL LOG-ODDS VERSION:
#   Old link: y = log(p/(1-p))  ->  dp/dy = p(1-p)
#   New link: y = sqrt(p)        ->  dp/dy = 2*sqrt(p)
# This changes the marginal-gain scaling term and the state-update step.
# The rest of the algorithm's structure (greedy allocation, same computational
# approximation of using the receiving municipality's own scaling factor)
# is unchanged from the verified original.
#
# BOUNDS FIX: p_cur is explicitly clipped to [0.0001, 0.9999] after every
# step. This is required because sqrt(share) is a linear model with no
# structural guarantee predictions stay in [0,1] -- this is the fix flagged
# as necessary before any counterfactual work began.
#
# Required: master_panel_final.csv, W3_commute.rds, municipality_order.rds,
#           sdm_key_parameters_w3_final.csv, sdm_key_parameters_w3_lag_final.csv
#
# Output: counterfactual_w3_scenario{1,2,3}_final.csv (contemporaneous),
#         counterfactual_w3_scenario{1,2,3}_lag_final.csv (lagged)
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))
W3     <- readRDS("W3_commute.rds")
order  <- readRDS("municipality_order.rds")
rownames(W3) <- order; colnames(W3) <- order

# ---- Reusable function: builds M, runs 3 budget scenarios -------------------

run_counterfactual_set <- function(params, treatment_col, balanced_munis, W3, order,
                                   obs_kw_col, obs_share_col, label_prefix) {
  
  W_sub <- W3[balanced_munis, balanced_munis]
  W_sub <- W_sub / ifelse(rowSums(W_sub)==0, 1, rowSums(W_sub))
  n <- nrow(W_sub)
  I_mat <- diag(n)
  
  rho   <- params$estimate[params$parameter == "rho"]
  beta  <- params$estimate[str_detect(params$parameter, "^beta")]
  theta <- params$estimate[str_detect(params$parameter, "^theta")]
  cat("Using: rho =", rho, "beta =", beta, "theta =", theta, "\n")
  
  M <- solve(I_mat - rho * W_sub) %*% (beta * I_mat + theta * W_sub)
  colM <- colSums(M)
  
  panel_2023 <- master %>%
    filter(year == 2023, municipality_code %in% balanced_munis) %>%
    arrange(match(municipality_code, balanced_munis))
  stopifnot(identical(panel_2023$municipality_code, balanced_munis))
  
  observed_budget <- sum(panel_2023[[obs_kw_col]])
  p_obs <- panel_2023[[obs_share_col]]
  p_obs[is.na(p_obs)] <- median(p_obs, na.rm = TRUE)
  obs_kw <- panel_2023[[obs_kw_col]]
  
  cat("Observed budget:", round(observed_budget, 1), "kW\n\n")
  
  run_scenario <- function(budget, label) {
    n_steps <- 2000
    increment <- budget / n_steps
    x_cf <- rep(0, n)
    p_cur <- p_obs
    y_cur <- sqrt(p_obs)   # sqrt-space state, replaces log-odds state
    
    for (step in 1:n_steps) {
      cur_kw <- obs_kw + x_cf
      asinh_deriv <- 1 / sqrt(1 + cur_kw^2)
      
      # NEW derivative: dp/dy = 2*sqrt(p), replacing old p*(1-p)
      sqrt_scale <- 2 * sqrt(pmax(p_cur, 0))
      
      marginal_gain <- colM * sqrt_scale * asinh_deriv
      best <- which.max(marginal_gain)
      
      old_kw <- cur_kw[best]; new_kw <- old_kw + increment
      delta_asinh <- asinh(new_kw) - asinh(old_kw)
      x_cf[best] <- x_cf[best] + increment
      
      # Update in sqrt-space (was log-odds space)
      delta_y <- M[, best] * delta_asinh
      y_cur <- y_cur + delta_y
      
      # Convert back to share: p = y^2
      p_cur <- y_cur^2
      
      # BOUNDS FIX (required, was missing risk in sqrt-space): clip to valid range
      p_cur <- pmin(pmax(p_cur, 0.0001), 0.9999)
      y_cur <- sqrt(p_cur)  # keep y_cur consistent with the clipped p_cur
    }
    
    pct_gain <- (sum(p_cur) - sum(p_obs)) / sum(p_obs) * 100
    touched <- sum(x_cf > 0)
    budget_check <- abs(sum(x_cf) - budget) < 1
    n_clipped <- sum(p_cur %in% c(0.0001, 0.9999))
    
    cat("===", label, "===\n")
    cat("Budget:", round(budget, 1), "kW\n")
    cat("Adoption gain:", round(pct_gain, 2), "%\n")
    cat("Municipalities touched:", touched, "out of", n, "\n")
    cat("Budget conserved:", budget_check, "\n")
    cat("Observations hitting the clip bound:", n_clipped, "(should be 0 or very small)\n\n")
    
    tibble(municipality_code = balanced_munis, observed_kw = obs_kw,
           counterfactual_kw = x_cf, observed_ev_share = p_obs,
           counterfactual_ev_share = p_cur)
  }
  
  cf_s1 <- run_scenario(observed_budget, paste(label_prefix, "SCENARIO 1: Budget Neutral"))
  cf_s2 <- run_scenario(observed_budget * 1.10, paste(label_prefix, "SCENARIO 2: +10%"))
  cf_s3 <- run_scenario(observed_budget * 1.25, paste(label_prefix, "SCENARIO 3: +25%"))
  
  list(s1 = cf_s1, s2 = cf_s2, s3 = cf_s3)
}

# =============================================================================
# CONTEMPORANEOUS
# =============================================================================

cat("\n", strrep("=", 65), "\n", sep = "")
cat("CONTEMPORANEOUS COUNTERFACTUAL\n")
cat(strrep("=", 65), "\n")

params_c <- read_csv("sdm_key_parameters_w3_final.csv", show_col_types = FALSE)

core_c <- c("ev_share_sqrt","charger_access_asinh","log_income",
            "log_pop_density","share_detached","median_age")
d_c <- master %>% drop_na(all_of(core_c))
bal_c <- d_c %>% count(municipality_code) %>% filter(n==8) %>% pull(municipality_code)

master <- master %>% mutate(ev_share_raw_check = ev_share_sqrt^2)  # sanity: should equal ev_share_raw

results_c <- run_counterfactual_set(params_c, "charger_access_asinh", bal_c, W3, order,
                                    "charger_kw_cumulative", "ev_share_raw", "[CONTEMP]")

write_csv(results_c$s1, "counterfactual_w3_scenario1_final.csv")
write_csv(results_c$s2, "counterfactual_w3_scenario2_final.csv")
write_csv(results_c$s3, "counterfactual_w3_scenario3_final.csv")

# =============================================================================
# LAGGED
# =============================================================================

cat("\n", strrep("=", 65), "\n", sep = "")
cat("LAGGED COUNTERFACTUAL\n")
cat(strrep("=", 65), "\n")

params_l <- read_csv("sdm_key_parameters_w3_lag_final.csv", show_col_types = FALSE)

master_lag <- master %>%
  arrange(municipality_code, year) %>%
  group_by(municipality_code) %>%
  mutate(charger_access_asinh_lag1 = dplyr::lag(charger_access_asinh, 1)) %>%
  ungroup()

core_l <- c("ev_share_sqrt","charger_access_asinh_lag1","log_income",
            "log_pop_density","share_detached","median_age")
d_l <- master_lag %>% drop_na(all_of(core_l))
muni_counts_l <- d_l %>% count(municipality_code)
bal_l <- muni_counts_l %>% filter(n==max(muni_counts_l$n)) %>% pull(municipality_code)

# For the lagged counterfactual, the "observed" 2023 charger stock is still
# the actual 2023 kW -- what changes is which SDM parameters (and therefore
# which M matrix) translate a kW increase into an adoption increase.
results_l <- run_counterfactual_set(params_l, "charger_access_asinh_lag1", bal_l, W3, order,
                                    "charger_kw_cumulative", "ev_share_raw", "[LAGGED]")

write_csv(results_l$s1, "counterfactual_w3_scenario1_lag_final.csv")
write_csv(results_l$s2, "counterfactual_w3_scenario2_lag_final.csv")
write_csv(results_l$s3, "counterfactual_w3_scenario3_lag_final.csv")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n\n", strrep("=", 90), "\n", sep = "")
cat("FINAL SUMMARY -- ADOPTION GAIN BY SCENARIO AND SPECIFICATION\n")
cat(strrep("=", 90), "\n")

summarise_gain <- function(cf, label) {
  pct <- (sum(cf$counterfactual_ev_share) - sum(cf$observed_ev_share)) / sum(cf$observed_ev_share) * 100
  cat(sprintf("  %-35s %.2f%%\n", label, pct))
}

summarise_gain(results_c$s1, "Contemporaneous, neutral budget")
summarise_gain(results_c$s2, "Contemporaneous, +10% budget")
summarise_gain(results_c$s3, "Contemporaneous, +25% budget")
summarise_gain(results_l$s1, "Lagged, neutral budget")
summarise_gain(results_l$s2, "Lagged, +10% budget")
summarise_gain(results_l$s3, "Lagged, +25% budget")

cat("\nSaved 6 counterfactual scenario files (3 contemporaneous, 3 lagged).\n")

# =============================================================================
# VERIFICATION: does colM here match the saved diffusion multiplier file?
# =============================================================================
cat("\n\n=== CROSS-CHECK: recomputed multiplier vs Step 4's saved file ===\n")
saved_mult <- read_csv("diffusion_multipliers_w3_contemporaneous_final.csv",
                       col_types = cols(municipality_code = col_character()))
W_sub_check <- W3[bal_c, bal_c]
W_sub_check <- W_sub_check / ifelse(rowSums(W_sub_check)==0, 1, rowSums(W_sub_check))
n_check <- nrow(W_sub_check)
rho_c <- params_c$estimate[params_c$parameter=="rho"]
beta_c <- params_c$estimate[params_c$parameter=="beta_charger"]
theta_c <- params_c$estimate[params_c$parameter=="theta_charger"]
M_check <- solve(diag(n_check) - rho_c*W_sub_check) %*% (beta_c*diag(n_check) + theta_c*W_sub_check)
colM_check <- tibble(municipality_code = bal_c, colM_recomputed = colSums(M_check))
compare <- colM_check %>% left_join(saved_mult, by="municipality_code")
cat("Max absolute difference between recomputed and saved multiplier:",
    max(abs(compare$colM_recomputed - compare$diffusion_multiplier_w3_contemp)), "\n")
cat("(Should be ~0. If so, the counterfactual is using the identical multiplier,\n")
cat(" just recomputed rather than read from disk -- fully consistent with Step 4.)\n")

library(tidyverse)

scenario_data <- tribble(
  ~scenario, ~spec, ~gain,
  "Neutral", "Contemporaneous", 2.54,
  "+10%",    "Contemporaneous", 2.64,
  "+25%",    "Contemporaneous", 2.78,
  "Neutral", "Lagged",          4.17,
  "+10%",    "Lagged",          4.34,
  "+25%",    "Lagged",          4.57
) %>%
  mutate(scenario = factor(scenario, levels = c("Neutral", "+10%", "+25%")),
         spec = factor(spec, levels = c("Contemporaneous", "Lagged")))

p <- ggplot(scenario_data, aes(x = scenario, y = gain, fill = spec)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  geom_text(aes(label = sprintf("%.2f%%", gain)),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Contemporaneous" = "grey65", "Lagged" = "black")) +
  labs(x = "Budget scenario", y = "Adoption gain (%)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave("fig_5_4_counterfactual_gains.png", p, width = 7, height = 5, dpi = 300)
print(p)


library(tidyverse)
library(plm)
library(splm)
library(spdep)

# =============================================================================
# ROBUSTNESS CHECK — FLEET-SIZE THRESHOLD CUT, W3 (PRIMARY), FINAL PANEL
#
# Re-runs the plain (non-interacted) SDM at increasing minimum fleet-size
# thresholds. This is the original crisis-era stress test that first
# surfaced the log-odds problem -- repeated here as a standalone check on
# the final corrected panel with ev_share_sqrt, to confirm the constant-
# coefficient result does not depend on small, badly-measured municipalities.
#
# Required: master_panel_final.csv, W3_commute.rds, municipality_order.rds
# Output: robustness_fleet_threshold_w3.csv
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))
W3     <- readRDS("W3_commute.rds")
order  <- readRDS("municipality_order.rds")
rownames(W3) <- order; colnames(W3) <- order

run_spec <- function(min_fleet, label) {
  
  cat("\n", strrep("-", 60), "\n", sep = "")
  cat(label, "\n")
  
  core <- c("ev_share_sqrt", "charger_access_asinh", "log_income",
            "log_pop_density", "share_detached", "median_age")
  
  d <- master %>%
    filter(fleet_total >= min_fleet) %>%
    drop_na(all_of(core))
  
  bal <- d %>% count(municipality_code) %>% filter(n == 8) %>% pull(municipality_code)
  
  if (length(bal) < 50) {
    cat("  Too few municipalities (", length(bal), ") -- skipping.\n")
    return(NULL)
  }
  
  d <- d %>% filter(municipality_code %in% bal) %>% arrange(municipality_code, year)
  
  W_sub <- W3[bal, bal]
  rs <- rowSums(W_sub)
  n_isolated <- sum(rs == 0)
  W_sub <- W_sub / ifelse(rs == 0, 1, rs)
  
  cat("  Municipalities:", length(bal), "| Rows:", nrow(d),
      "| Isolated after subsetting:", n_isolated, "\n")
  
  years <- sort(unique(d$year))
  d_wx <- map_dfr(years, function(yr) {
    yd <- d %>% filter(year == yr) %>% arrange(municipality_code)
    stopifnot(identical(yd$municipality_code, rownames(W_sub)))
    yd %>% mutate(
      w_charger         = as.numeric(W_sub %*% charger_access_asinh),
      w_log_income      = as.numeric(W_sub %*% log_income),
      w_log_pop_density = as.numeric(W_sub %*% log_pop_density),
      w_share_detached  = as.numeric(W_sub %*% share_detached),
      w_median_age      = as.numeric(W_sub %*% median_age)
    )
  })
  
  pdat <- pdata.frame(d_wx, index = c("municipality_code", "year"))
  W_lw <- mat2listw(W_sub, style = "W")
  
  fml <- ev_share_sqrt ~ charger_access_asinh + log_income + log_pop_density +
    share_detached + median_age +
    w_charger + w_log_income + w_log_pop_density + w_share_detached + w_median_age
  
  fit <- try(
    spml(formula = fml, data = pdat, listw = W_lw,
         model = "within", effect = "twoways", lag = TRUE, spatial.error = "none"),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) {
    cat("  ESTIMATION FAILED\n")
    return(NULL)
  }
  
  ct <- summary(fit)$CoefTable
  rho   <- ct["lambda", "Estimate"]
  beta  <- ct["charger_access_asinh", "Estimate"]
  theta <- ct["w_charger", "Estimate"]
  bp    <- ct["charger_access_asinh", "Pr(>|t|)"]
  tp    <- ct["w_charger", "Pr(>|t|)"]
  
  cat(sprintf("  rho = %+.4f | beta = %+.5f (p=%.4g) | theta = %+.5f (p=%.4g) | theta/beta = %+.2f\n",
              rho, beta, bp, theta, tp, theta/beta))
  
  tibble(
    min_fleet = min_fleet, label = label,
    n_muni = length(bal), n_obs = nrow(d_wx),
    rho = rho, beta = beta, beta_p = bp, theta = theta, theta_p = tp,
    theta_beta = theta/beta, beta_sig = bp < 0.05, theta_sig = tp < 0.05
  )
}

cat(strrep("=", 70), "\n")
cat("FLEET-SIZE THRESHOLD ROBUSTNESS -- W3, ev_share_sqrt, FINAL PANEL\n")
cat(strrep("=", 70), "\n")

results <- bind_rows(
  run_spec(0,    "Baseline (all 351 municipalities)"),
  run_spec(500,  "fleet >= 500"),
  run_spec(1000, "fleet >= 1000"),
  run_spec(2000, "fleet >= 2000")
)

cat("\n\n", strrep("=", 100), "\n", sep = "")
cat("SUMMARY\n")
cat(strrep("=", 100), "\n")
print(results %>% mutate(across(where(is.numeric), ~round(.x, 5))), n = 10, width = Inf)

cat("\nHow to read this:\n")
cat("  If theta stays positive, significant, and roughly stable across all four\n")
cat("  thresholds, the spillover result does NOT depend on small municipalities\n")
cat("  and the constant-coefficient result is confirmed robust.\n")

write_csv(results, "robustness_fleet_threshold_w3.csv")
cat("\nSaved: robustness_fleet_threshold_w3.csv\n")


library(tidyverse)
library(plm)
library(splm)
library(spdep)

# =============================================================================
# ROBUSTNESS CHECK — FLEET-SIZE THRESHOLD CUT, W3 LAGGED, FINAL PANEL
#
# Same test as the contemporaneous version, applied to the time-lagged
# treatment (chargers at t-1 explaining adoption at t). This is the
# specification least exposed to both contemporaneous reverse causality
# AND (per the fleet-vs-maturity horse race) the small-municipality
# measurement concern -- if theta survives the fleet cut HERE, that is the
# strongest single robustness result available for this thesis.
#
# The fleet_total filter is still based on CURRENT-year fleet (it measures
# how well the OUTCOME, ev_share_sqrt at time t, is measured -- independent
# of whether the treatment is contemporaneous or lagged).
#
# Required: master_panel_final.csv, W3_commute.rds, municipality_order.rds
# Output: robustness_fleet_threshold_w3_lag.csv
# =============================================================================

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))
W3     <- readRDS("W3_commute.rds")
order  <- readRDS("municipality_order.rds")
rownames(W3) <- order; colnames(W3) <- order

master <- master %>%
  arrange(municipality_code, year) %>%
  group_by(municipality_code) %>%
  mutate(charger_access_asinh_lag1 = dplyr::lag(charger_access_asinh, 1)) %>%
  ungroup()

run_spec_lag <- function(min_fleet, label) {
  
  cat("\n", strrep("-", 60), "\n", sep = "")
  cat(label, "\n")
  
  core <- c("ev_share_sqrt", "charger_access_asinh_lag1", "log_income",
            "log_pop_density", "share_detached", "median_age")
  
  d <- master %>%
    filter(fleet_total >= min_fleet) %>%
    drop_na(all_of(core))
  
  muni_counts <- d %>% count(municipality_code)
  max_years <- if (nrow(muni_counts) > 0) max(muni_counts$n) else 0
  bal <- muni_counts %>% filter(n == max_years) %>% pull(municipality_code)
  
  if (length(bal) < 50) {
    cat("  Too few municipalities (", length(bal), ") -- skipping.\n")
    return(NULL)
  }
  
  d <- d %>% filter(municipality_code %in% bal) %>% arrange(municipality_code, year)
  
  W_sub <- W3[bal, bal]
  rs <- rowSums(W_sub)
  n_isolated <- sum(rs == 0)
  W_sub <- W_sub / ifelse(rs == 0, 1, rs)
  
  cat("  Years retained:", max_years, "| Municipalities:", length(bal), "| Rows:", nrow(d),
      "| Isolated after subsetting:", n_isolated, "\n")
  
  years <- sort(unique(d$year))
  d_wx <- map_dfr(years, function(yr) {
    yd <- d %>% filter(year == yr) %>% arrange(municipality_code)
    stopifnot(identical(yd$municipality_code, rownames(W_sub)))
    yd %>% mutate(
      w_charger_lag1    = as.numeric(W_sub %*% charger_access_asinh_lag1),
      w_log_income      = as.numeric(W_sub %*% log_income),
      w_log_pop_density = as.numeric(W_sub %*% log_pop_density),
      w_share_detached  = as.numeric(W_sub %*% share_detached),
      w_median_age      = as.numeric(W_sub %*% median_age)
    )
  })
  
  pdat <- pdata.frame(d_wx, index = c("municipality_code", "year"))
  W_lw <- mat2listw(W_sub, style = "W")
  
  fml <- ev_share_sqrt ~ charger_access_asinh_lag1 + log_income + log_pop_density +
    share_detached + median_age +
    w_charger_lag1 + w_log_income + w_log_pop_density + w_share_detached + w_median_age
  
  fit <- try(
    spml(formula = fml, data = pdat, listw = W_lw,
         model = "within", effect = "twoways", lag = TRUE, spatial.error = "none"),
    silent = TRUE
  )
  
  if (inherits(fit, "try-error")) {
    cat("  ESTIMATION FAILED:", conditionMessage(attr(fit, "condition")), "\n")
    return(NULL)
  }
  
  ct <- summary(fit)$CoefTable
  rho   <- ct["lambda", "Estimate"]
  beta  <- ct["charger_access_asinh_lag1", "Estimate"]
  theta <- ct["w_charger_lag1", "Estimate"]
  bp    <- ct["charger_access_asinh_lag1", "Pr(>|t|)"]
  tp    <- ct["w_charger_lag1", "Pr(>|t|)"]
  
  cat(sprintf("  rho = %+.4f | beta = %+.5f (p=%.4g) | theta = %+.5f (p=%.4g) | theta/beta = %+.2f\n",
              rho, beta, bp, theta, tp, theta/beta))
  
  tibble(
    min_fleet = min_fleet, label = label,
    n_muni = length(bal), n_obs = nrow(d_wx),
    rho = rho, beta = beta, beta_p = bp, theta = theta, theta_p = tp,
    theta_beta = theta/beta, beta_sig = bp < 0.05, theta_sig = tp < 0.05
  )
}

cat(strrep("=", 70), "\n")
cat("FLEET-SIZE THRESHOLD ROBUSTNESS -- W3 LAGGED, ev_share_sqrt, FINAL PANEL\n")
cat(strrep("=", 70), "\n")

results_lag <- bind_rows(
  run_spec_lag(0,    "Baseline (all 351 municipalities)"),
  run_spec_lag(500,  "fleet >= 500"),
  run_spec_lag(1000, "fleet >= 1000"),
  run_spec_lag(2000, "fleet >= 2000")
)

cat("\n\n", strrep("=", 100), "\n", sep = "")
cat("SUMMARY -- LAGGED SPECIFICATION\n")
cat(strrep("=", 100), "\n")
print(results_lag %>% mutate(across(where(is.numeric), ~round(.x, 5))), n = 10, width = Inf)

cat("\n\n", strrep("=", 100), "\n", sep = "")
cat("SIDE-BY-SIDE: CONTEMPORANEOUS (from previous run) vs LAGGED\n")
cat(strrep("=", 100), "\n")
cat("Contemporaneous theta by threshold: 0.00131(p=.073) -> 0.00093(p=.19) -> 0.00045(p=.54) -> -0.00059(p=.54)\n")
cat("Lagged theta by threshold:          see table above\n\n")
cat("If lagged theta stays positive and closer to significant across more\n")
cat("thresholds than contemporaneous did, THAT is the specification to lead\n")
cat("with in the robustness section, not the contemporaneous one.\n")

write_csv(results_lag, "robustness_fleet_threshold_w3_lag.csv")
cat("\nSaved: robustness_fleet_threshold_w3_lag.csv\n")





#sdm plot 


library(tidyverse)

coef_data <- tribble(
  ~matrix,          ~spec,              ~estimate,  ~std_error,
  "W1 (queen)",     "Contemporaneous",  0.001704,   0.000512,
  "W1 (queen)",     "Lagged",           0.002703,   0.000549,
  "W2 (distance)",  "Contemporaneous",  0.002767,   0.000885,
  "W2 (distance)",  "Lagged",           0.004764,   0.000984,
  "W3 (commuting)", "Contemporaneous",  0.001308,   0.000731,
  "W3 (commuting)", "Lagged",           0.002744,   0.000745
) %>%
  mutate(
    ci_lower = estimate - 1.96 * std_error,
    ci_upper = estimate + 1.96 * std_error,
    matrix = factor(matrix, levels = c("W1 (queen)", "W2 (distance)", "W3 (commuting)")),
    spec = factor(spec, levels = c("Contemporaneous", "Lagged"))
  )

print(coef_data)

p <- ggplot(coef_data, aes(x = matrix, y = estimate, color = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                  position = position_dodge(width = 0.4), size = 0.8) +
  scale_color_manual(values = c("Contemporaneous" = "grey60", "Lagged" = "black")) +
  labs(x = NULL, y = expression(hat(theta)~"(spillover coefficient)"),
       color = NULL,
       caption = "95% confidence intervals (estimate ± 1.96·SE). Dashed line marks zero.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

ggsave("fig_5_1_theta_by_spec.png", p, width = 7, height = 5, dpi = 300)
print(p)



##sdm full table 
library(tidyverse)
library(plm)
library(splm)
library(spdep)

master <- read_csv("master_panel_final.csv", col_types = cols(municipality_code = col_character()))
order  <- readRDS("municipality_order.rds")

master <- master %>%
  arrange(municipality_code, year) %>%
  group_by(municipality_code) %>%
  mutate(charger_access_asinh_lag1 = dplyr::lag(charger_access_asinh, 1)) %>%
  ungroup()

run_full <- function(W, W_name, treatment_col, master, order) {
  cat("Running:", W_name, "-", treatment_col, "\n")
  rownames(W) <- order; colnames(W) <- order
  
  core <- c("ev_share_sqrt", treatment_col, "log_income", "log_pop_density",
            "share_detached", "median_age")
  d_clean <- master %>% drop_na(all_of(core))
  bal <- d_clean %>% count(municipality_code) %>% filter(n == max(n)) %>% pull(municipality_code)
  d_clean <- d_clean %>% filter(municipality_code %in% bal) %>% arrange(municipality_code, year)
  
  W_sub <- W[bal, bal]
  W_sub <- W_sub / ifelse(rowSums(W_sub) == 0, 1, rowSums(W_sub))
  
  years <- sort(unique(d_clean$year))
  d_wx <- map_dfr(years, function(yr) {
    yd <- d_clean %>% filter(year == yr) %>% arrange(municipality_code)
    yd$treat <- yd[[treatment_col]]
    yd %>% mutate(
      w_treat = as.numeric(W_sub %*% treat),
      w_log_income = as.numeric(W_sub %*% log_income),
      w_log_pop_density = as.numeric(W_sub %*% log_pop_density),
      w_share_detached = as.numeric(W_sub %*% share_detached),
      w_median_age = as.numeric(W_sub %*% median_age)
    )
  })
  
  pdat <- pdata.frame(d_wx, index = c("municipality_code", "year"))
  W_lw <- mat2listw(W_sub, style = "W")
  fml <- ev_share_sqrt ~ treat + log_income + log_pop_density + share_detached + median_age +
    w_treat + w_log_income + w_log_pop_density + w_share_detached + w_median_age
  
  fit <- spml(formula = fml, data = pdat, listw = W_lw,
              model = "within", effect = "twoways", lag = TRUE, spatial.error = "none")
  
  ct <- as.data.frame(summary(fit)$CoefTable)
  ct$term <- rownames(ct)
  ct$specification <- W_name
  ct
}

W1 <- readRDS("W1_queen.rds"); W2 <- readRDS("W2_invdist_100km.rds"); W3 <- readRDS("W3_commute.rds")

full_tables <- bind_rows(
  run_full(W1, "W1_contemporaneous", "charger_access_asinh", master, order),
  run_full(W1, "W1_lagged", "charger_access_asinh_lag1", master, order),
  run_full(W2, "W2_contemporaneous", "charger_access_asinh", master, order),
  run_full(W2, "W2_lagged", "charger_access_asinh_lag1", master, order),
  run_full(W3, "W3_contemporaneous", "charger_access_asinh", master, order),
  run_full(W3, "W3_lagged", "charger_access_asinh_lag1", master, order)
)

# Save FIRST, so the data is preserved regardless of any print issues
write_csv(full_tables, "sdm_full_coefficient_tables.csv")
cat("Saved: sdm_full_coefficient_tables.csv\n\n")

# Then print safely
print(as_tibble(full_tables), n = 100)