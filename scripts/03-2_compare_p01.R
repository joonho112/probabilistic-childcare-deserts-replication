#!/usr/bin/env Rscript

# P07 Step 3.2 -- Cross-classify P01 deterministic deserts against P07 declarations.
#
# Purpose:
#   Quantifies what the uncertainty-aware P07 analysis changes relative to the
#   deterministic P01 point-estimate map: how many point deserts survive, how
#   many are set aside, and how many new tracts enter. This is the headline
#   "does accounting for uncertainty matter" comparison.
#
# Method:
#   A 2x2 cross-classification on the common 1,409-tract universe: P01 point
#   E2SFCA desert (coverage < 0.33) x P07 FDR declaration. Reported cells:
#   500 both, 190 point-only (dropped but NOT exonerated -- their median
#   posterior desert probability is 0.689), 12 FDR-only (newly entered), 707
#   neither. Survival: 500/690 = 72.5% of point deserts are FDR-declared.
#   Disclosure guards: the four cells must sum to 1,409 tracts and 294,417
#   children, and every displayed cell must hold >= 10 tracts.
#
# Reads:
#   data/analytic/03_fdr_declarations.rds   (P07 FDR/FDX tract flags)
#   <P01>/02_deserts_classified.rds         (P01 point desert flags)
#   <P01>/outputs/key_numbers.csv           (P01 anchor: 690 point deserts)
#
# Writes:
#   data/analytic/03_p01_comparison.rds        (tracts + cross table + headline)
#   outputs/tables/03_p01_fdr_cross.csv
#   outputs/tables/03_p01_fdr_headline.csv
#   outputs/key_numbers.csv                    (appended single-source-of-truth numbers)

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))

declarations <- readRDS(p07_path("data", "analytic", "03_fdr_declarations.rds"))
point_sf <- read_p01("02_deserts_classified.rds")
p01_ssot <- read_p01("key_numbers.csv", "outputs")
tracts <- declarations$tracts

# Look up a single value in the P01 single-source-of-truth key_numbers table,
# asserting exactly one match so a renamed or duplicated key fails loudly.
p01_value <- function(key) {
  hit <- p01_ssot$value[p01_ssot$key == key]
  assert_that(length(hit) == 1L, paste0("Missing/duplicate P01 SSOT key: ", key))
  as.numeric(hit)
}
# Anchor the comparison to P01's published 690-tract point-desert count.
p01_desert_total <- p01_value("desert_tracts_e2sfca")
assert_that(p01_desert_total == 690L, "P01 SSOT point-desert anchor changed.")

# Align P01 point-desert flags to the P07 tract order on the common 1,409-tract universe.
point <- point_sf$is_desert_e2sfca[match(tracts$GEOID, point_sf$GEOID)]
assert_that(!anyNA(point) && sum(point) == p01_desert_total, "P01 flags failed common-universe join.")
tracts$point_desert <- point
# Assign each tract to one of the four cross-classification cells
# (both / point-only / FDR-only / neither) from the two binary flags.
tracts$comparison_class <- ifelse(
  point & tracts$fdr_desert, "both_point_and_fdr",
  ifelse(
    point & !tracts$fdr_desert, "point_only",
    ifelse(!point & tracts$fdr_desert, "fdr_only", "neither")
  )
)
tracts$comparison_class <- factor(
  tracts$comparison_class,
  levels = c("both_point_and_fdr", "point_only", "fdr_only", "neither")
)

cross <- tracts |>
  dplyr::group_by(.data$comparison_class) |>
  dplyr::summarise(
    n_tracts = dplyr::n(),
    children = sum(.data$demand_mean),
    child_pct = 100 * .data$children / sum(tracts$demand_mean),
    high_cv_tracts = sum(.data$high_cv_flag),
    high_cv_pct = 100 * mean(.data$high_cv_flag),
    median_desert_probability = stats::median(.data$desert_probability),
    .groups = "drop"
  )

# Headline survival counts: point deserts P07 still declares, new FDR-only entries,
# and point deserts P07 sets aside.
surviving <- sum(tracts$point_desert & tracts$fdr_desert)
new_fdr <- sum(!tracts$point_desert & tracts$fdr_desert)
point_only <- sum(tracts$point_desert & !tracts$fdr_desert)
pct_surviving <- 100 * surviving / p01_desert_total
fdx_point_surviving <- sum(tracts$point_desert & tracts$fdx_desert)

assert_that(sum(cross$n_tracts) == 1409L, "P01/FDR four-cell tract total failed.")
assert_that(sum(cross$children) == 294417, "P01/FDR four-cell child total failed.")
assert_that(surviving + point_only == p01_desert_total, "P01 desert partition failed.")
assert_that(surviving + new_fdr == sum(tracts$fdr_desert), "FDR desert partition failed.")
# Disclosure guard: every displayed cross-tab cell must hold at least 10 tracts.
assert_that(all(cross$n_tracts >= 10L), "A public comparison cell violates the minimum cell-size rule.")

headline <- data.frame(
  metric = c(
    "P01 point deserts", "P07 FDR deserts", "P07 FDX deserts",
    "P01 deserts surviving FDR", "P01 deserts removed by FDR",
    "FDR additions vs P01", "P01 survival percent", "P01 deserts surviving FDX"
  ),
  value = c(
    p01_desert_total, sum(tracts$fdr_desert), sum(tracts$fdx_desert),
    surviving, point_only, new_fdr, pct_surviving, fdx_point_surviving
  ),
  unit = c("tracts", "tracts", "tracts", "tracts", "tracts", "tracts", "percent", "tracts"),
  stringsAsFactors = FALSE
)

save_analytic(
  list(
    tracts = tracts,
    cross = cross,
    headline = headline,
    metadata = list(
      universe = "1,409 nonzero-demand tracts",
      p01_ssot_key = "desert_tracts_e2sfca",
      p01_ssot_value = p01_desert_total,
      child_total = sum(tracts$demand_mean),
      comparison = "P01 point E2SFCA <0.33 versus P07 FDR q=0.10"
    )
  ),
  "03_p01_comparison.rds"
)
utils::write.csv(cross, p07_path("outputs", "tables", "03_p01_fdr_cross.csv"), row.names = FALSE, na = "")
utils::write.csv(headline, p07_path("outputs", "tables", "03_p01_fdr_headline.csv"), row.names = FALSE, na = "")

append_key_numbers(data.frame(
  key = c(
    "n_point690_surviving_fdr", "n_fdr_new_vs_point", "pct_point_surviving",
    "n_point690_surviving_fdx"
  ),
  value = c(surviving, new_fdr, pct_surviving, fdx_point_surviving),
  unit = c("tracts", "tracts", "percent", "tracts"),
  source_script = "scripts/03-2_compare_p01.R",
  note = c(
    "Intersection of P01 point deserts and P07 FDR q=0.10 declarations.",
    "P07 FDR declarations among P01 point non-deserts.",
    "Intersection divided by the P01 SSOT 690 point deserts.",
    "Intersection of P01 point deserts and P07 FDX headline declarations."
  ), stringsAsFactors = FALSE
))

cat("Step 3.2 P01 point comparison: PASS\n")
