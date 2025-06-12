library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

maf <- data.table::fread(
  here('data-raw', 'main_genie', 'data_mutations_extended_12.1-public.txt')
)

clin_samp <- readr::read_tsv(
  comment = '#',
  here('data-raw', 'main_genie', 'data_clinical_sample_12.1-public.txt')
)

ca_ind <- readr::read_csv(
  here('data-raw', 'PANC', 'cancer_level_dataset_index.csv')
)

# very rough filter just to get a workable MAF size:
bpc_panc_samples <- clin_samp %>%
  filter(PATIENT_ID %in% ca_ind$record_id) %>%
  pull(SAMPLE_ID)

maf_relevant <- maf %>%
  filter(Tumor_Sample_Barcode %in% bpc_panc_samples) %>%
  filter(Hugo_Symbol %in% "KRAS") %>%
  filter(HGVSp_Short %in% "p.G12D")

readr::write_rds(
  maf_relevant,
  here('data', 'genomic', 'maf_rel.rds')
)

samp_kras_g12d <- maf_relevant %>%
  pull(Tumor_Sample_Barcode) %>%
  unique %>%
  sort

readr::write_rds(
  samp_kras_g12d,
  here('data', 'genomic', 'samp_kras_g12d.rds')
)
