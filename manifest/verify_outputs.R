#!/usr/bin/env Rscript
# =============================================================================
# verify_outputs.R : check the shipped outputs against the single source of truth
# =============================================================================
#
# Run from the package root:
#     Rscript manifest/verify_outputs.R
#
# It performs four families of checks and exits non-zero if ANY fails:
#   1. Registry integrity  -- outputs/key_numbers.csv has the right schema and no
#      duplicate keys.
#   2. Headline values     -- every value in verification/expected/headline_values.csv
#      matches the registry (this is what a Track-A rerun is checked against).
#   3. Structure           -- the declaration sets nest (FDX <= FDR <= {p>0.5}).
#   4. Disclosure          -- the shipped aggregate layer carries no provider-
#      identifying column, the expected number of tracts is suppressed, and the
#      canonical figures are present.
#
# It needs only base R plus utils; no restricted data and no spatial toolchain.
# =============================================================================

fails <- character(0)
ok <- function(cond, msg) {
  status <- isTRUE(cond)
  cat(sprintf("  [%s] %s\n", if (status) "PASS" else "FAIL", msg))
  if (!status) fails[[length(fails) + 1L]] <<- msg
  invisible(status)
}
num <- function(x) suppressWarnings(as.numeric(x))

# ---- 1. Registry integrity --------------------------------------------------
cat("Registry integrity\n")
kn_path <- "outputs/key_numbers.csv"
ok(file.exists(kn_path), "outputs/key_numbers.csv exists")
kn <- utils::read.csv(kn_path, colClasses = "character", check.names = FALSE)
ok(identical(names(kn),
             c("key", "value", "unit", "source_script", "computed_at", "note")),
   "key_numbers.csv has the expected schema")
ok(!anyDuplicated(kn$key), "key_numbers.csv has no duplicate keys")
val <- function(k) { v <- kn$value[kn$key == k]; if (length(v) == 1L) v else NA_character_ }

# ---- 2. Headline values match the registry ----------------------------------
cat("\nHeadline values (expected vs registry)\n")
exp <- utils::read.csv("verification/expected/headline_values.csv",
                       colClasses = "character")
for (i in seq_len(nrow(exp))) {
  k <- exp$key[i]; e <- num(exp$expected[i]); got <- num(val(k))
  ok(!is.na(got) && abs(got - e) <= 1e-9 * max(1, abs(e)),
     sprintf("%-28s expected %s, registry %s", k, exp$expected[i], val(k)))
}

# ---- 3. Structure: the declaration sets nest --------------------------------
cat("\nStructure\n")
fdx <- num(val("n_desert_fdx")); fdr <- num(val("n_desert_fdr_q10"))
p50 <- num(val("n_prob_gt_50"))
ok(fdx <= fdr, sprintf("FDX (%g) <= FDR (%g)", fdx, fdr))
ok(fdr <= p50, sprintf("FDR (%g) <= tracts with p>0.50 (%g)", fdr, p50))
surv <- num(val("n_point690_surviving_fdr")); p690 <- num(val("n_p01_desert_e2sfca"))
ok(surv <= p690, sprintf("point deserts surviving FDR (%g) <= P01 point deserts (%g)", surv, p690))

# ---- 4. Disclosure and artifacts --------------------------------------------
cat("\nDisclosure and artifacts\n")
tr <- utils::read.csv("data/derived/tract_results.csv",
                      colClasses = c(GEOID = "character", county_fips = "character"))
banned <- c("facility_id", "provider_id", "latitude", "longitude", "address",
            "street", "lat", "lon", "lng", "mdlat", "mdlong")
ok(length(intersect(tolower(names(tr)), banned)) == 0L,
   "tract_results.csv carries no provider-identifying column")
ok(nrow(tr) == 1436L, "tract_results.csv has all 1,436 tracts")
ok(sum(!as.logical(tr$display_allowed)) == 46L,
   "exactly 46 tracts are display-suppressed")
ok(sum(is.na(tr$desert_probability)) == 46L,
   "suppressed tracts have no published desert probability")
co <- utils::read.csv("data/derived/county_results.csv",
                      colClasses = c(county_fips = "character"))
ok(nrow(co) == 67L, "county_results.csv has all 67 counties")
ok(sum(co$county_fdr_desert %in% c(TRUE, "TRUE", "true")) ==
     num(val("n_desert_counties_weighted")),
   "declared counties match the registry")
figs <- file.path("outputs", "figures",
                  paste0(c("F1_desert_probability", "F2_fdr_declarations",
                           "F3_fdx_declarations", "F4_point_fdr_reclassification",
                           "F5_county_declarations", "F6_sensitivity_triangulation"),
                         ".png"))
ok(all(file.exists(figs)), "all six canonical figures are present")

# ---- Summary ----------------------------------------------------------------
cat("\n")
if (length(fails)) {
  cat(sprintf("VERIFICATION FAILED: %d check(s) did not pass.\n", length(fails)))
  quit(status = 1L)
}
cat("VERIFICATION PASSED: all checks succeeded.\n")
