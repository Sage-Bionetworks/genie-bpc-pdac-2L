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

# Render the feasibilty report:
quarto::quarto_render(
  input = here('analysis/report/cohort-build.qmd')
)
fs::file_move(
  path = 'analysis/report/cohort-build.html',
  new_path = 'analysis/report/cohort-build.html'
)

# Run the relevant survival scripts:
source(here('analysis', 'script', 'survival_main.R'))
source(here('analysis', 'script', 'survival_verified_prog.R'))
