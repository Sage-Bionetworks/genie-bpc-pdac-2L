# this is a non-reproducible file that I used to download a colleague's cbio outputs and compare them to mine (Shawn sweeney).

data <- readr::read_tsv(
  "/Users/apaynter/Downloads/20250616_panc_genie_bpc_clinical_data_US_PAAD_PAASC_1_cancer.tsv"
)

data$`Number of Cancers, Any Type`

count(data, `Number of Cancers, Any Type`)

data$Center


shawn_g12d_count <- readr::read_tsv(
  "/Users/apaynter/Downloads/20250616_panc_genie_bpc_clinical_data_KRASG12D_astellas.tsv"
)

count(shawn_g12d_count, `Patient ID`, sort = T)

flow_track <- readr::read_rds(
  here('data', 'flow_track.rds')
)

alex_g12d_record <- flow_track %>%
  filter(message %in% "KRAS G12D+ (ever)") %>%
  pull(dat) %>%
  .[[1]]

set.seed(26)

shawn_g12d_count %>%
  filter(!(`Patient ID` %in% alex_g12d_record$record_id)) %>%
  sample_n(3)

ca_ind %>%
  filter(record_id %in% 'GENIE-MSK-P-0019389') %>%
  glimpse

non_pdac_record_id <- cpt %>%
  filter(cpt_oncotree_code %in% c('PAAC', 'UCP')) %>%
  pull(record_id)

cpt %>%
  filter(record_id %in% non_pdac_record_id) %>%
  group_by(record_id) %>%
  summarize(
    oncotrees = paste(cpt_oncotree_code, collapse = ", ")
  ) %>%
  group_by(oncotrees) %>%
  sample_n(3, replace = T)
View(.)
