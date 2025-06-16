# main workflow for the project.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'process_genomics.R'))
source(here('analysis', 'script', 'create_lines.R'))
source(here('analysis', 'script', 'evaluate_lines.R'))
source(here('analysis', 'script', 'progression.R'))
source(here('analysis', 'script', 'build_cohort.R'))

# after this render the analysis/reports/astellus.qmd file, code to automate coming later on.
