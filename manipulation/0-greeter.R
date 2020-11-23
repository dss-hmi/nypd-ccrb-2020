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
# path_input <- "../data/allegations_20200726939.csv"
path_input <- "https://raw.githubusercontent.com/rtrent/NYPDCivilianComplaintReviewBoard/master/data/allegations_20200726939.csv"
# see  https://github.com/rtrent/NYPDCivilianComplaintReviewBoard/blob/master/import.py  for some hints about the data, but we should consider working with DB directly ( https://github.com/rtrent/NYPDCivilianComplaintReviewBoard)

#set default ggplot theme
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- load-data -------------------------------------------------------------
ds0 <- readr::read_csv(path_input)
ds0 %>% glimpse()
# ---- inspect-data -----------------------------------------------------------
skimr::skim(ds0)
ds0 %>% pryr::object_size()
# -----tweak-data -------------------------------------------------------------

ds1 <- ds0
# ---- survey-response -------------------------

# ---- save-to-disk --------------------------------------------------------

ds1 %>% readr::write_rds("./data-unshared/derived/dto.rds")

# ----- publisher --------------------
path <- "./analysis/0-greeter/0-greeter.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)


