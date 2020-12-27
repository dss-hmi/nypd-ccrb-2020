# This scripts reads in the raw survey data
# performs basic exploration of the raw data
# and stores data tweaks inherited by all subsequent reports
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console when working in RStudio
# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
library(tidyverse)
library(glue)
# ---- declare-globals ----------------------------------------------------
# source : https://github.com/new-york-civil-liberties-union/NYPD-Misconduct-Complaint-Database
path_input_clu <- ("./data-unshared/derived/dto_nyclu.rds")
path_input_pp <- ("./data-unshared/derived/dto_propublica.rds")
#set default ggplot theme
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- load-data -------------------------------------------------------------
ds0_clu <- readr::read_rds(path_input_clu)
ds0_pp  <- readr::read_rds(path_input_pp)

# ---- inspect-data -----------------------------------------------------------
skimr::skim(ds0_clu)
skimr::skim(ds0_pp)

ds0_clu %>% pryr::object_size()
ds0_pp %>% pryr::object_size()

ds0_clu %>% glimpse()
ds0_pp %>% glimpse()

compid <- 42835

d1 <- ds0_pp %>% filter(complaint_id == compid)
d2 <- ds0_clu %>% filter(complaint_id == compid)

ds1 <- left_join(
   ds0_pp %>% select(c("complaint_id","shield_no", "unique_mos_id"))
  ,ds0_clu %>% select(c("complaint_id","shield_no", "unique_mos_id"))
  , by = c("complaint_id")
)

ds1 %>% skimr::skim()

# How many complaints in common between these two databases?
intersect(unique(ds0_pp$complaint_id), unique(ds0_clu$complaint_id)) # A: 0, it seems the ids were scrambled in at least one file
# How many officers in each dataset?
mos_id_pp <- unique(ds0_pp$unique_mos_id); mos_id_pp %>% length() # Pro-Publica
mos_id_clu <- unique(ds0_clu$unique_id);mos_id_clu %>% length() # CLU
intersect(mos_id_pp, mos_id_clu) %>% length()# all MOS IDs in Pro-Publical can be found in CLU
setdiff(mos_id_pp, mos_id_clu) %>% length() # None in CLU that cannot be found in PP
setdiff(mos_id_clu, mos_id_pp) %>% length() # Those found only in CLU

# -----tweak-data -------------------------------------------------------------

ds1 <- ds0 %>%
  janitor::clean_names()
ds1 %>% glimpse()
# ---- survey-response -------------------------

# ---- save-to-disk --------------------------------------------------------

ds1 %>% readr::write_rds("./data-unshared/derived/dto.rds")

# ----- publisher --------------------
# path <- "./analysis/0-greeter/0-greeter.Rmd"
# rmarkdown::render(
#   input = path ,
#   output_format=c(
#     "html_document"
#     # ,"word_document"
#   ),
#   clean=TRUE
# )


