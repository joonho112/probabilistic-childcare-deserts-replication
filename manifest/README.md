# `manifest/` — integrity and verification

Two tools let a reader confirm — not assume — that they have the files the
authors shipped and that a reproduction matched.

| File | What it does |
|---|---|
| `pipeline_manifest.csv` | A SHA-256 ledger of every shipped artifact (path, size, hash). Re-hash a file and compare it to the ledger to detect corruption or an accidental edit. |
| `verify_outputs.R` | Checks the shipped outputs against the single source of truth (`outputs/key_numbers.csv`) and the expected headline values in `verification/expected/headline_values.csv`: the registry schema, every headline number, the nesting of the declaration sets, small-cell suppression, and the absence of any provider-identifying column. Exits non-zero on any mismatch. |

Run the verification from the package root:

```sh
Rscript manifest/verify_outputs.R
```

Check file integrity against the ledger (example):

```sh
Rscript -e 'm <- read.csv("manifest/pipeline_manifest.csv"); \
  m$now <- vapply(m$path, function(p) if (file.exists(p)) \
    digest::digest(p, algo="sha256", file=TRUE) else NA_character_, ""); \
  print(m[which(m$sha256 != m$now), c("path","sha256","now")])'
```

An empty result means every shipped file matches the ledger. See
`../verification/reproduction-map.csv` for the claim-by-claim map from the paper's
numbers to the registry keys and the scripts that compute them.
