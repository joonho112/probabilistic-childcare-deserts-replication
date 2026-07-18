# ============================================================================
# fct_io.R -- Shared paths, validation, and single-source-of-truth (SSOT)
#             helpers for the P07 replication package.
# ----------------------------------------------------------------------------
# Purpose
#   Centralizes every filesystem concern so the numbered scripts never hard-code
#   a path or re-implement I/O. Two roots are resolved once, here: this package
#   (P07_CODEBASE_ROOT) and the read-only companion package P01, whose analytic
#   artifacts P07 consumes but never writes.
#
# Roots and resolution
#   P07_CODEBASE_ROOT / P01_CODEBASE_ROOT come from environment variables when
#   set, otherwise fall back to the working directory and a sibling
#   "codebase-P01" folder. normalizePath(mustWork = TRUE) forces every path to
#   exist, turning a mis-launch into an immediate, legible error.
#
# Single source of truth (SSOT)
#   Every headline number in the manuscript is written exactly once into
#   outputs/key_numbers.csv (schema: key, value, unit, source_script,
#   computed_at, note) via append_key_number()/append_key_numbers(), and read
#   back by key_value(). One authoritative value per key means the paper and the
#   code cannot silently drift apart.
#
# Safety
#   read_p01() and save_analytic() reject any filename that is not a bare
#   basename (basename(f) == f), blocking directory-traversal reads/writes into
#   or out of the protected P01/P07 trees. file_sha256() hashes files for the
#   provenance ledger.
#
# Key functions
#   p07_path()             build an absolute path inside this package
#   assert_that()          minimal scalar-condition guard used throughout P07
#   read_p01()             traversal-guarded reader for read-only P01 artifacts
#   save_analytic()        write a versioned .rds under data/{analytic,interim}
#   read_key_numbers()     load + schema-check the SSOT registry
#   append_key_number(s)   upsert one/many rows into the SSOT registry
#   key_value()            look up a single SSOT value by key
#   file_sha256()          SHA-256 digest of a file for provenance
# ============================================================================

P07_CODEBASE_ROOT <- normalizePath(
  Sys.getenv("P07_CODEBASE_ROOT", unset = getwd()),
  mustWork = TRUE
)

# Sentinel files confirm we were launched from the package root; otherwise stop
# with a legible message instead of failing deep inside a later step.
if (!all(file.exists(file.path(P07_CODEBASE_ROOT, c("README.md", "_run_order.md"))))) {
  stop("Run P07 scripts from the codebase-P07 root or set P07_CODEBASE_ROOT.", call. = FALSE)
}

P07_PROJECT_ROOT <- normalizePath(file.path(P07_CODEBASE_ROOT, ".."), mustWork = TRUE)
# Resolve the read-only companion package P07 consumes: env var if set, else a
# sibling "codebase-P01" directory. mustWork = TRUE makes a missing path fail now.
P01_CODEBASE_ROOT <- normalizePath(
  Sys.getenv("P01_CODEBASE_ROOT", unset = file.path(P07_PROJECT_ROOT, "codebase-P01")),
  mustWork = TRUE
)
P01_ANALYTIC <- file.path(P01_CODEBASE_ROOT, "data", "analytic")
P01_OUTPUTS <- file.path(P01_CODEBASE_ROOT, "outputs")

p07_path <- function(...) file.path(P07_CODEBASE_ROOT, ...)

# Minimal guard used throughout P07: the condition must be a single, non-NA TRUE
# or execution stops. Kept dependency-free on purpose.
assert_that <- function(condition, message) {
  if (length(condition) != 1L || is.na(condition) || !condition) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

# Traversal-guarded reader for the read-only P01 artifacts. `domain` selects the
# analytic vs outputs subtree; the basename == filename check (below) refuses any
# filename with directory separators, so callers cannot read outside P01.
read_p01 <- function(filename, domain = c("analytic", "outputs")) {
  domain <- match.arg(domain)
  assert_that(
    length(filename) == 1L && !is.na(filename) && basename(filename) == filename,
    "filename must be one basename without directory traversal."
  )
  root <- if (domain == "analytic") P01_ANALYTIC else P01_OUTPUTS
  path <- file.path(root, filename)
  assert_that(file.exists(path), paste0("Missing read-only P01 artifact: ", path))
  # Dispatch on file extension: .rds is unserialized, .csv is read as plain
  # character-friendly data, and anything else is an error.
  extension <- tolower(tools::file_ext(path))
  switch(
    extension,
    rds = readRDS(path),
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    stop("Unsupported P01 artifact type: ", extension, call. = FALSE)
  )
}

# Write a P07 analytic object as versioned .rds under data/{analytic,interim}.
# Same basename traversal guard as read_p01; refuses non-.rds filenames.
save_analytic <- function(object, filename, subdir = c("analytic", "interim")) {
  subdir <- match.arg(subdir)
  assert_that(
    length(filename) == 1L && !is.na(filename) && basename(filename) == filename,
    "filename must be one basename without directory traversal."
  )
  assert_that(tolower(tools::file_ext(filename)) == "rds", "Analytic objects must use .rds.")
  destination <- p07_path("data", subdir, filename)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  # version = 3 pins the serialization format for cross-version reproducibility.
  saveRDS(object, destination, version = 3)
  invisible(destination)
}

# SSOT registry schema. Every headline number lives in outputs/key_numbers.csv
# as one row keyed by name, so the manuscript and code cannot silently diverge.
key_number_schema <- c("key", "value", "unit", "source_script", "computed_at", "note")

# Load the SSOT registry, returning a correctly typed empty frame when the file
# does not yet exist, and asserting the schema and key-uniqueness invariants.
read_key_numbers <- function(path = p07_path("outputs", "key_numbers.csv")) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    return(stats::setNames(as.data.frame(matrix(nrow = 0L, ncol = 6L)), key_number_schema))
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  assert_that(identical(names(out), key_number_schema), "key_numbers.csv schema mismatch.")
  assert_that(!anyDuplicated(out$key), "key_numbers.csv contains duplicate keys.")
  out
}

# Upsert a single SSOT row. By default (replace = TRUE) an existing key is
# overwritten, so re-running a script updates rather than duplicates its numbers.
append_key_number <- function(key, value, unit, source_script, note = "", replace = TRUE) {
  assert_that(length(key) == 1L && !is.na(key) && nzchar(key), "key must be one non-empty value.")
  key_file <- p07_path("outputs", "key_numbers.csv")
  existing <- read_key_numbers(key_file)
  if (key %in% existing$key && !replace) {
    stop("Duplicate key_numbers key: ", key, call. = FALSE)
  }
  # Drop any prior row for this key (the upsert), then append the fresh one.
  existing <- existing[existing$key != key, , drop = FALSE]
  new_row <- data.frame(
    key = as.character(key), value = as.character(value), unit = as.character(unit),
    source_script = as.character(source_script),
    computed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    note = as.character(note), stringsAsFactors = FALSE
  )
  out <- rbind(existing, new_row)
  # Keep the registry sorted by key so its on-disk diff is stable across runs.
  out <- out[order(out$key), , drop = FALSE]
  utils::write.csv(out, key_file, row.names = FALSE, na = "", quote = TRUE)
  invisible(new_row)
}

# Vectorized wrapper: upsert a data frame of key-number rows one at a time.
append_key_numbers <- function(rows, replace = TRUE) {
  assert_that(all(key_number_schema[1:4] %in% names(rows)), "Key-number rows lack required columns.")
  if (!"note" %in% names(rows)) rows$note <- ""
  for (i in seq_len(nrow(rows))) {
    append_key_number(
      rows$key[i], rows$value[i], rows$unit[i], rows$source_script[i], rows$note[i], replace
    )
  }
  invisible(rows)
}

# Look up exactly one SSOT value by key (optionally coerced to numeric); asserts
# the key resolves to a single row.
key_value <- function(key, numeric = FALSE, path = p07_path("outputs", "key_numbers.csv")) {
  registry <- read_key_numbers(path)
  hit <- registry$value[registry$key == key]
  assert_that(length(hit) == 1L, paste0("Expected exactly one SSOT value for key: ", key))
  if (numeric) as.numeric(hit) else hit
}

# SHA-256 digest of a file's bytes, used to record artifact provenance.
file_sha256 <- function(path) {
  assert_that(file.exists(path), paste0("Cannot hash missing file: ", path))
  assert_that(requireNamespace("digest", quietly = TRUE), "Package 'digest' is required for SHA-256.")
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}
