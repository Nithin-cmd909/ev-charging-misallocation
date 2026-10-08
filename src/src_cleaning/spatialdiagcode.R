library(tidyverse)

# =============================================================================
# SPATIAL MATRIX VALIDATION — W1, W2, W3
#
# Strategy: test the actual constructed files for both structural correctness
# AND content plausibility (do the real neighbor/commuting relationships make
# geographic sense), since structural checks alone missed real bugs earlier
# tonight (housing/population frozen values, income remapping gaps all passed
# row-count checks). Only dig into the construction scripts if something here
# looks wrong.
#
# Required: W1_queen.rds, W2_invdist_100km.rds, W3_commute.rds,
#           municipality_order.rds, ssb_municipality_list.csv
# =============================================================================

ssb <- read_csv("ssb_municipality_list.csv", col_types = cols(municipality_code = col_character()))
order <- readRDS("municipality_order.rds")

get_name <- function(code) {
  n <- ssb$municipality_name[ssb$municipality_code == code]
  if (length(n) == 0) return(code)
  n
}

top_n_neighbors <- function(W, code, order, n = 8) {
  idx <- which(order == code)
  if (length(idx) == 0) return(NULL)
  weights <- W[idx, ]
  names(weights) <- order
  top <- sort(weights[weights > 0], decreasing = TRUE)
  head(top, n)
}

validate_matrix <- function(W, name, order) {
  cat("\n=============================================================\n")
  cat(name, "\n")
  cat("=============================================================\n")
  
  cat("Dimensions:", dim(W)[1], "x", dim(W)[2], "| Expected 356 x 356 |",
      ifelse(all(dim(W)==356), "PASS", "FAIL"), "\n")
  cat("Diagonal sum:", sum(diag(W)), "| Expected 0 |",
      ifelse(sum(diag(W))==0, "PASS", "FAIL"), "\n")
  
  rs <- rowSums(W)
  cat("Row sums range:", round(min(rs),6), "to", round(max(rs),6),
      "| Expected all ~1 (row-standardised) |\n")
  cat("Rows NOT summing to 1 (tolerance 1e-6):", sum(abs(rs-1) > 1e-6), "\n")
  
  cat("Isolated municipalities (row sum = 0):", sum(rs == 0), "\n")
  if (sum(rs==0) > 0) {
    isolated_codes <- order[rs==0]
    cat("  Codes:", paste(isolated_codes, collapse=", "), "\n")
    cat("  Names:", paste(sapply(isolated_codes, get_name), collapse=", "), "\n")
  }
  
  cat("Any negative weights (should be impossible):", sum(W < 0), "\n")
  cat("Any NA/NaN in matrix:", sum(is.na(W)), "\n")
  
  cat("\nSymmetric (informative, not required for row-standardised W):",
      isTRUE(all.equal(W, t(W), tolerance=1e-6)), "\n")
}

# =============================================================================
# LOAD AND VALIDATE EACH MATRIX
# =============================================================================

cat("Municipality order vector length:", length(order), "\n")

# ---- W1: Queen contiguity ---------------------------------------------------
W1 <- readRDS("W1_queen.rds")
rownames(W1) <- order; colnames(W1) <- order
validate_matrix(W1, "W1 -- QUEEN CONTIGUITY", order)

# ---- W2: Inverse distance, 100km cutoff -------------------------------------
W2 <- readRDS("W2_invdist_100km.rds")
if (is.null(rownames(W2))) { rownames(W2) <- order; colnames(W2) <- order }
validate_matrix(W2, "W2 -- INVERSE DISTANCE (100km cutoff)", order)

# ---- W3: Commuting flows -----------------------------------------------------
W3 <- readRDS("W3_commute.rds")
if (is.null(rownames(W3))) { rownames(W3) <- order; colnames(W3) <- order }
validate_matrix(W3, "W3 -- COMMUTING FLOWS", order)

# =============================================================================
# CONTENT PLAUSIBILITY CHECKS -- does this match real Norwegian geography?
# =============================================================================

cat("\n\n=============================================================\n")
cat("CONTENT CHECK 1 -- W1 (queen contiguity): known bordering municipalities\n")
cat("=============================================================\n")
cat("Real fact: Oslo (0301) genuinely shares borders with Baerum (3201),\n")
cat("Lorenskog (3029... check current code), Nittedal, Nordre Follo, etc.\n")
cat("Bergen (4601) borders Alver, Oygarden, Askoy, Bjornafjorden, Vaksdal.\n\n")

for (code in c("0301","4601","5001")) {
  cat(get_name(code), "(", code, ") W1 neighbours (all get equal weight if row-standardised):\n")
  nb <- top_n_neighbors(W1, code, order, n=10)
  if (!is.null(nb)) {
    for (i in seq_along(nb)) cat("  ", get_name(names(nb)[i]), "(", names(nb)[i], ")\n")
  }
  cat("\n")
}

cat("=============================================================\n")
cat("CONTENT CHECK 2 -- W3 (commuting): known commuter-belt relationships\n")
cat("=============================================================\n")
cat("Real fact: Baerum, Asker, Lillestrom/Lorenskog area, and Drammen have\n")
cat("very heavy real-world commuting INTO Oslo. If W3 is correct, Oslo's top\n")
cat("commuting destinations (or these municipalities' top ties TO Oslo)\n")
cat("should reflect this.\n\n")

cat("Oslo (0301) top 10 W3 commuting ties:\n")
nb <- top_n_neighbors(W3, "0301", order, n=10)
for (i in seq_along(nb)) cat("  ", get_name(names(nb)[i]), "(", names(nb)[i], "): weight =", round(nb[i],4), "\n")

cat("\nBaerum (3201) top 10 W3 commuting ties (expect Oslo near/at top):\n")
nb <- top_n_neighbors(W3, "3201", order, n=10)
for (i in seq_along(nb)) cat("  ", get_name(names(nb)[i]), "(", names(nb)[i], "): weight =", round(nb[i],4), "\n")

cat("\n=============================================================\n")
cat("CONTENT CHECK 3 -- W2 (inverse distance): nearest neighbours by weight\n")
cat("=============================================================\n")
cat("Real fact: Oslo's geographically NEAREST municipalities are Baerum,\n")
cat("Nittedal, Lorenskog, Nordre Follo -- all within ~20km. These should\n")
cat("dominate Oslo's W2 weights (inverse distance = closer gets more weight).\n\n")

cat("Oslo (0301) top 10 W2 nearest-weighted municipalities:\n")
nb <- top_n_neighbors(W2, "0301", order, n=10)
for (i in seq_along(nb)) cat("  ", get_name(names(nb)[i]), "(", names(nb)[i], "): weight =", round(nb[i],4), "\n")

cat("\n=============================================================\n")
cat("CONTENT CHECK 4 -- Cross-matrix comparison for a well-known case\n")
cat("=============================================================\n")
cat("Baerum should appear as a HIGH-weight neighbour of Oslo in ALL THREE\n")
cat("matrices (it borders Oslo, is geographically closest, AND has heavy\n")
cat("commuting ties). If it's missing or negligible in any one matrix,\n")
cat("that matrix likely has a construction problem.\n\n")

for (mat_name in c("W1","W2","W3")) {
  W <- get(mat_name)
  idx_oslo <- which(order == "0301")
  idx_baerum <- which(order == "3201")
  w <- W[idx_oslo, idx_baerum]
  rank <- rank(-W[idx_oslo,])[idx_baerum]
  cat(sprintf("  %s: Oslo->Baerum weight = %.5f  (rank #%d out of 355 possible neighbours)\n",
              mat_name, w, rank))
}

cat("\n=============================================================\n")
cat("DONE. Paste the full output.\n")
cat("=============================================================\n")