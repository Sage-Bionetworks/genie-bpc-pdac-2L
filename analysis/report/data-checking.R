---
title: "Astellas metastatic PDAC"
subtitle: "Data checking"
author: "Alex Paynter"
date: "`r format(Sys.Date(), '%d %b %Y')`"
editor_options:
  quarto:
  chunk_output_type: console
format:
  html:
  embed-resources: true
toc: true
toc-depth: 4
theme: sandstone 
execute:
  echo: false
include: false
warning: false
message: false
fig.width: 7
fig.height: 5
---
  

```{r}
library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)
```

## Introduction

This report lists out cases to be visually confirmed in cBioPortal.  The idea is that each link is a query on one person, and with a little bit of additional information (which drug is index) you can check that they meet the key entry and response criteria.

This report is intended for internal (Alex) use, but if you want to spend some time checking things over I'm fully supportive - reach out if you have problems.

```{r}
cohort <- readr::read_rds(
  here('data', 'cohort_prog_verified.rds')
)
resp <- readr::read_rds(
  here('data', 'response', 'responses.rds')
)

chk_dat <- coho

resp %>%
  mutate(
    chk_cat = case_when(
      resp_cat_conf ~ "con_res",
      resp_cat ~ 
```


## Section 1: Response strata 

Categories:

- Confirmed responders
- Unconfirmed responders
- Non-responders
- NE scans only
- No scans

## Confirmed responders

```{r}

```


```{r}
```

