# main workflow for the project.

library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

# Preprocessing scripts to help build the cohort:
source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'process_genomics.R'))
source(here('analysis', 'script', 'create_lines.R'))
source(here('analysis', 'script', 'evaluate_lines.R'))
source(here('analysis', 'script', 'progression.R'))
source(here('analysis', 'script', 'cpt_itherapy_timing.R'))
# Uses the outputs from all of those:
source(here('analysis', 'script', 'build_cohort.R'))
source(here('analysis', 'script', 'baseline_table.R'))
source(here('analysis', 'script', 'sites_of_met_at_index.R'))


# Render the feasibilty report:
quarto::quarto_render(
  input = here('analysis/report/cohort-build.qmd')
)
fs::file_move(
  path = 'analysis/report/cohort-build.html',
  new_path = 'output/cohort-build.html'
)

# Run the relevant survival scripts:
source(here('analysis', 'script', 'survival_main.R'))
# source(here('analysis', 'script', 'survival_verified_prog.R'))

source(here('analysis', 'script', 'build_cohort_no_kras.R'))
source(here('analysis', 'script', 'survival_geno.R'))

quarto::quarto_render(
  input = here('analysis/report/survival.qmd')
)
fs::file_move(
  path = 'analysis/report/survival.html',
  new_path = 'output/survival.html'
)

source(here('analysis', 'script', 'response.R'))
quarto::quarto_render(
  input = here('analysis/report/response.qmd')
)
fs::file_move(
  path = 'analysis/report/response.html',
  new_path = 'output/response.html'
)
