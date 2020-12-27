# This scripts reads in the raw survey data
# performs basic exploration of the raw data
# and stores data tweaks inherited by all subsequent reports
# rmarkdown::render('./analysis/1-first-look/1-first-look.R')
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console when working in RStudio
# ---- load-sources ------------------------------------------------------------
# knitr::opts_knit$set(base.dir = "../../")
source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
library(tidyverse)
library(glue)
library(scales)
# ---- declare-globals ----------------------------------------------------
# path_input <- "../../data-unshared/derived/dto.rds"
path_input <- "./data-unshared/derived/dto_propublica.rds"
# Taps into https://projects.propublica.org/nypd-ccrb/
# https://github.com/rtrent/NYPDCivilianComplaintReviewBoard)
#set default ggplot theme
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- load-data -------------------------------------------------------------
ds0 <- readr::read_rds(path_input)
# metadata <- readr::read_csv("../../data-public/metadata/better-labels.csv")
metadata <- readr::read_csv("./data-public/metadata/better-labels.csv")

ds0 %>% glimpse()
# ---- inspect-data -----------------------------------------------------------
skimr::skim(ds0)
ds0 %>% pryr::object_size()
# -----tweak-data -------------------------------------------------------------

ds1 <- ds0 %>%
  mutate(
    date_received = as.Date(paste0(year_received,"-",month_received,"-15"))
    ,date_closed = as.Date(paste0(year_closed,"-",month_closed,"-15"))
    ,mos_yob = (year_received - mos_age_incident)
    ,complainant_yob = (year_received - complainant_age_incident)
  ) %>%
  mutate(
    complainant_ethnicity = replace_na(complainant_ethnicity, "Unknown")
    ,mos_gender = fct_recode(mos_gender, "Female"="F", "Male"="M")
    ,complainant_gender = replace_na(complainant_gender,"(Missing)")
    ,complainant_gender = fct_recode(complainant_gender,
                                        "Female" = "Female"
                                        ,"Male" = "Male"
                                        ,"Other" = "Gender non-conforming"
                                        ,"Other" = "Transman (FTM)"
                                        ,"Other" = "Transwoman (MTF)"
                                        ,"(Missing)" = "Not described"
                                        )
  ) %>%
  mutate_at(
    c("complainant_ethnicity","mos_ethnicity"), factor
  )
  # select(-c("year_received","month_received","year_closed","month_closed"))

ds1 %>% glimpse()

ds2 <- ds1 %>%
  select(
  # MOS - Member of Service
    unique_mos_id            #  Unique ID of the officer
    ,mos_ethnicity           #
    ,mos_gender              #
    ,mos_age_incident        #
    ,mos_yob                 # Year of birth
    ,rank_now                # Officer's rank as of July 2020
    --
    ,rank_incident           #  --- at the time of the incident
    ,rank_abbrev_now         #
    ,rank_abbrev_incident    #
    ,command_now             # Officer's command assignment as of July 2020
    ,command_at_incident     #
    ,first_name
    ,last_name
    ,shield_no
  # Complainant
    ,complainant_ethnicity
    ,complainant_gender
    ,complainant_age_incident
    ,complainant_yob
  # Complaint
    ,complaint_id
    ,date_received
    ,date_closed
    # ,year_received
    # ,month_received
    # ,year_closed
    # ,month_closed
    ,fado_type               # Top-level category of complaint
    ,allegation              # Specific category of complaint
    ,precinct
    ,board_disposition       # Finding disposition
  # Contact ( between MOS and the complanant)
    ,contact_reason
    ,outcome_description     # Contact outcome
  )
# ds2 %>% glimpse()
# setdiff(names(ds1),names(ds2))
#
# ---- officer-1 -------------------------

# How many distinct officers and complaints represented in the data?
ds2 %>% summarize(
  n_officers = n_distinct(unique_mos_id)
  ,n_shield_ns = n_distinct(shield_no)
  ,n_complaints = n_distinct(complaint_id)
  )

# how many missing shield_no?
ds2 %>% filter(shield_no == 0L) %>%
  summarize(n_mos_with_missing_shield_no = n_distinct(unique_mos_id))
# Shield No is missing from 458 officers

# Are shield numbers unique?
ds2 %>% group_by(shield_no) %>%
  summarize(n_duplicate_shields = n_distinct(unique_mos_id)) %>%
  filter(n_duplicate_shields > 1) %>%
  arrange(desc(n_duplicate_shields))
# 197 Shield numbers are registered to more than 1 officer
/
ds2 %>% group_by(unique_mos_id) %>%
  summarize(n_duplicate_ids = n_distinct(shield_no)) %>%
  filter(n_duplicate_ids > 1) %>%
  arrange(desc(n_duplicate_ids))
# Each officer has no more than 1 shield number


# Rank of the officer (as of July 2020)
ds2 %>% group_by(rank_now) %>% count()
ds2 %>% group_by(rank_now, rank_abbrev_now) %>% count()
# Abbreviated rank offers greater detail

# Rank of the officer (at the time of the indident)
ds2 %>% group_by(rank_incident) %>% count()
ds2 %>% group_by(rank_incident, rank_abbrev_incident) %>% count()
# Abbreviated rank offers greater detail

# Ethnicity
ds2 %>% group_by(mos_ethnicity) %>% count()
ds2 %>% group_by(complainant_ethnicity) %>% count()

# Gender
ds2 %>% group_by(mos_gender) %>% count()
ds2 %>% group_by(complainant_gender) %>% count()





# ---- save-to-disk --------------------------------------------------------

# ds1 %>% readr::write_rds("./data-unshared/derived/dto.rds")

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


