#!/usr/bin/env Rscript
# run_pipeline.R — run the full pipeline end to end.
# Usage: Rscript run_pipeline.R

steps <- c(
  "scripts/01_download_data.R",
  "scripts/02_build_characteristics.R",
  "scripts/03_fit_pca_benchmark.R",
  "scripts/04_fit_ff5_benchmark.R",
  "scripts/05_fit_autoencoder.R",
  "scripts/06_evaluate_models.R",
  "scripts/07_risk_decomposition.R"
)

for (step in steps) {
  message(sprintf("\n=== Running %s ===", step))
  source(step)
}

message("\nPipeline complete. See output/tables/ for results.")
