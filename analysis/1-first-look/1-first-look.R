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

mos_rank_order <- c("Police Officer", "Detective", "Sergeant", "Lieutenant",
                    "Captain", "Deputy Inspector", "Inspector","Chief")

ethnicity5 <- c("White","Black","Hispanic","Other", "(Missing)")
gender4 <- c("Male","Female","Other","(Missing)")

counts_and_percent <- function(d,varnames){
  dt <- ds2 %>%
    group_by(.dots = varnames) %>%
    count() %>%
    ungroup() %>%
    mutate(pct = scales::percent(n/sum(n), accuracy=.1))
  return(dt)
}
# How to use
# counts_and_percent(ds2, c("rank_incident","rank_now")) %>% neat()


# ---- load-data -------------------------------------------------------------
ds0 <- readr::read_rds(path_input)
# metadata <- readr::read_csv("../../data-public/metadata/better-labels.csv")
metadata <- readr::read_csv("./data-public/metadata/better-labels.csv")

# ds0 %>% glimpse()
# ---- inspect-data -----------------------------------------------------------
skimr::skim(ds0)
ds0 %>% pryr::object_size()

# -----tweak-data-1 -------------------------------------------------------------

# ds1 - adjust the rows
ds1 <- ds0 %>%
  mutate(
    date_received = as.Date(paste0(year_received,"-",month_received,"-15"))
    ,date_closed = as.Date(paste0(year_closed,"-",month_closed,"-15"))
    ,mos_yob = (year_received - mos_age_incident)
    ,complainant_yob = (year_received - complainant_age_incident)
  ) %>%
  mutate(
    complainant_ethnicity = replace_na(complainant_ethnicity, "(Missing)")
    ,complainant_ethnicity5 = fct_recode(
      complainant_ethnicity,
      "Other" = "American Indian"
      ,"Other" = "Asian"
      ,"Other" = "Other Race"
      ,"(Missing)" = "Refused"
      ,"(Missing)" = "Unknown"
    ) %>% fct_relevel(ethnicity5)
    ,mos_gender = fct_recode(mos_gender, "Female"="F", "Male"="M")
    ,complainant_gender = replace_na(complainant_gender,"(Missing)")
    ,complainant_gender4 = fct_recode(complainant_gender,
                                        "Female" = "Female"
                                        ,"Male" = "Male"
                                        ,"Other" = "Gender non-conforming"
                                        ,"Other" = "Transman (FTM)"
                                        ,"Other" = "Transwoman (MTF)"
                                        ,"(Missing)" = "Not described"
                                        ) %>% fct_relevel(gender4)
    ,complainant_age_incident = ifelse(complainant_age_incident < 10,NA,complainant_age_incident)

  ) %>%
  mutate_at(
    c("complainant_ethnicity","mos_ethnicity"), factor
  ) %>%
   tidyr::separate(board_disposition,into = c("disposition", "penalty"), sep = " \\(", remove = FALSE) %>%
  mutate(
    penalty = str_remove(penalty, "\\)$")
    ,disposition = fct_relevel(disposition, "Exonerated","Unsubstantiated","Substantiated")
  ) %>%
  mutate(
    rank_now = fct_recode(
      rank_now,
      "Chief" = "Chiefs and other ranks"
    ) %>%
      fct_relevel(mos_rank_order)
    ,rank_incident = fct_recode(
      rank_incident,
      "Chief" = "Chiefs and other ranks"
    ) %>%
      fct_relevel(mos_rank_order)

  )

  # select(-c("year_received","month_received","year_closed","month_closed"))

# ds1 %>% group_by(board_disposition) %>% count() %>% arrange(desc(n))
# ds1 %>% group_by(disposition) %>% count()
# ds1 %>% group_by(penalty) %>% count()

# ds1 %>% glimpse()

# -----tweak-data-2 -------------------------------------------------------------

# ds2 - adjust the columns
ds2 <- ds1 %>%
  select(
  # MOS - Member of Service
    unique_mos_id            #  Unique ID of the officer
    ,mos_ethnicity           #
    ,mos_gender              #
    ,mos_age_incident        #
    ,mos_yob                 # Year of birth
    ,rank_now                # Officer's rank as of July 2020
    ,rank_incident           #  --- at the time of the incident
    ,rank_abbrev_now         # Greater granularity of rank_now
    ,rank_abbrev_incident    # Greater granularity of rank_incident
    ,command_now             # Officer's command assignment as of July 2020
    ,command_at_incident     # --- at the time of the incident
    ,first_name
    ,last_name
    ,shield_no
  # Complainant
    ,complainant_ethnicity
    ,complainant_ethnicity5   # collapsed into 5 categories
    ,complainant_gender       # original values
    ,complainant_gender4      # Collapsed into 5 categories
    ,complainant_age_incident # less than 10 converted to NA
    ,complainant_yob          # year of birth
  # Complaint
    ,complaint_id
    ,date_received           # transformed from year + month (15th of the month)
    ,date_closed             # transformed from year + month (15th of the month)
    ,fado_type               # Top-level category of complaint
    ,allegation              # Specific category of complaint
    ,precinct
    ,board_disposition       # Finding disposition
    ,disposition             # Decision        (part of board_disposition)
    ,penalty                 # Penalty applied (part of board_disposition)
  # Contact ( between MOS and the complanant)
    ,contact_reason
    ,outcome_description     # Contact outcome
  )

# ----- variable-groups ---------------------
metadata %>%
  group_by(entity) %>%
  summarize(n_variables = n_distinct(item_name)) %>%
  arrange(desc(entity)) %>%
  neat()

metadata %>%
  filter(entity == "officer") %>%
  arrange(desc(entity), item_name) %>%
  select(entity,item_name, item_description, item_label) %>%
  neat()

metadata %>%
  filter(entity == "complainant") %>%
  arrange(desc(entity), item_name) %>%
  select(entity,item_name, item_description, item_label) %>%
  neat()

metadata %>%
  filter(entity == "complaint") %>%
  arrange(desc(entity), item_name) %>%
  select(entity,item_name, item_description, item_label) %>%
  neat()

metadata %>%
  filter(entity == "contact") %>%
  arrange(desc(entity), item_name) %>%
  select(entity,item_name, item_description, item_label) %>%
  neat()
# ---- officer-1 -------------------------

# How many distinct officers and complaints represented in the data?
ds2 %>% summarize(
  n_officers = n_distinct(unique_mos_id)
  ,n_shield_ns = n_distinct(shield_no)
  ,n_complaints = n_distinct(complaint_id)
  ) %>%
  neat()

# how many missing shield_no?
# Shield No is missing from 458 officers
ds2 %>% filter(shield_no == 0L) %>%
  summarize(n_mos_with_missing_shield_no = n_distinct(unique_mos_id))%>%
  neat()


# Are shield numbers unique?
# No
ds2 %>% group_by(shield_no) %>%
  summarize(n_duplicate_shields = n_distinct(unique_mos_id)) %>%
  filter(n_duplicate_shields > 1) %>%
  arrange(desc(n_duplicate_shields)) %>%
  slice(1:20)%>%
  neat()

# 402 shields have at least one duplicate
ds2 %>% group_by(shield_no) %>%
  summarize(n_duplicate_shields = n_distinct(unique_mos_id)) %>%
  filter(n_duplicate_shields > 1 & n_duplicate_shields< 458) %>%
  summarize(
    n_duplicated_shields = sum(n_duplicate_shields)
  )%>%
  neat()


# 197 Shield numbers are registered to more than 1 officer
ds2 %>% group_by(shield_no) %>%
  summarize(n_duplicates = n_distinct(unique_mos_id)) %>%
  filter(n_duplicates > 1 & n_duplicates< 458) %>%
  group_by(n_duplicates) %>%
  summarize(n_shields = n())%>%
  neat()

# Each officer has no more than 1 shield number
ds2 %>% group_by(unique_mos_id) %>%
  summarize(n_duplicate_ids = n_distinct(shield_no)) %>%
  filter(n_duplicate_ids > 1) %>%
  arrange(desc(n_duplicate_ids))%>%
  neat()


# ---- officer-2 -------------------------


# Rank of the officer (as of July 2020)
ds2 %>% counts_and_percent("rank_now") %>% neat()
# Abbreviated rank offers greater detail
ds2 %>% counts_and_percent(c("rank_now","rank_abbrev_now")) %>% neat()



# Rank of the officer (at the time of the indident)
ds2 %>% counts_and_percent("rank_incident") %>% neat()
# Abbreviated rank offers greater detail
ds2 %>% counts_and_percent(c("rank_incident","rank_abbrev_incident")) %>% neat()

# How did the rank of officers change since the incident?
ds2 %>% counts_and_percent(c("rank_incident","rank_now")) %>% neat()

# ---- officer-3 -------------------------

# Ethnicity
ds2 %>% counts_and_percent(c("mos_ethnicity")) %>% neat()

# Gender
ds2 %>% counts_and_percent(c("mos_gender")) %>% neat()

# Age
ds2 %>% TabularManifest::histogram_continuous("mos_age_incident")


# ---- complainant-1 ------------------------------------------------------

# ---- complainant-2 ------------------------------------------------------
# What is the prevalence of ethnic backgrounds
ds2 %>% counts_and_percent(c("complainant_ethnicity5","complainant_ethnicity")) %>% neat()
ds2 %>% counts_and_percent(c("complainant_ethnicity5")) %>% neat()


ds2 %>% counts_and_percent(c("complainant_gender4","complainant_gender")) %>% neat()
ds2 %>% counts_and_percent(c("complainant_gender4")) %>% neat()


ds2 %>% TabularManifest::histogram_continuous("complainant_age_incident")

# However,
ds0 %>%
  group_by(complainant_age_incident) %>%
  summarize(n = n()) %>%
  arrange(complainant_age_incident)%>% neat_DT()
# There appears to be some data errors in complainant age
# all records with age < 10 are transformed into NA
ds2 %>%
  group_by(complainant_age_incident) %>%
  summarize(n = n()) %>%
  arrange(complainant_age_incident)%>% neat_DT()

# ---- complainant-3 ------------------------------------------------------
# ds2 %>% glimpse()


# ---- complaint-1 ------------------------------------------------------
ds2 %>% counts_and_percent("fado_type") %>% neat()
ds2 %>% counts_and_percent(c("fado_type","allegation")) %>% neat_DT()


# ---- complaint-2 ------------------------------------------------------
ds2 %>% counts_and_percent(c("disposition","penalty")) %>% neat()
ds2 %>% counts_and_percent(c("disposition")) %>% neat()


# ---- contact-1 ------------------------------------------------------
ds2 %>% counts_and_percent(c("contact_reason"))%>% arrange(desc(n)) %>% neat_DT()

ds2 %>% counts_and_percent(c("outcome_description"))%>% arrange(desc(n)) %>% neat_DT()


# ---- contact-2 ------------------------------------------------------

# ---- save-to-disk --------------------------------------------------------

ds2 %>% readr::write_rds("./data-unshared/derived/dto.rds")
ds2 %>% readr::write_rds("./data-public/derived/nypd-ccrb-cleaned.rds")
ds2 %>% readr::write_csv("./data-public/derived/nypd-ccrb-cleaned.csv")

# ----- publisher --------------------
path <- "./analysis/1-first-look/1-first-look.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)


