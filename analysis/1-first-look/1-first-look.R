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
    ,complainant_age_incident = ifelse(complainant_age_incident < 10,NA,complainant_age_incident)

  ) %>%
  mutate_at(
    c("complainant_ethnicity","mos_ethnicity"), factor
  ) %>%
   tidyr::separate(board_disposition,into = c("disposition", "penalty"), sep = " \\(", remove = FALSE) %>%
  mutate(
    penalty = str_remove(penalty, "\\)$")
  )

  # select(-c("year_received","month_received","year_closed","month_closed"))

ds1 %>% group_by(board_disposition) %>% count() %>% arrange(desc(n))
ds1 %>% group_by(disposition) %>% count()
ds1 %>% group_by(penalty) %>% count()

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
    ,disposition             #
    ,penalty                 # Penalty applied
  # Contact ( between MOS and the complanant)
    ,contact_reason
    ,outcome_description     # Contact outcome
  )
# ds2 %>% glimpse()
# setdiff(names(ds1),names(ds2))
#

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

# ---- officer-2 -------------------------
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



# Rank of the officer (as of July 2020)
ds2 %>% group_by(rank_now) %>% count() %>% neat()
ds2 %>% group_by(rank_now, rank_abbrev_now) %>% count()%>%neat()
# Abbreviated rank offers greater detail

# Rank of the officer (at the time of the indident)
ds2 %>% group_by(rank_incident) %>% count()%>% neat()
ds2 %>% group_by(rank_incident, rank_abbrev_incident) %>% count()%>% neat()
# Abbreviated rank offers greater detail

# Ethnicity
ds2 %>% group_by(mos_ethnicity) %>% count()%>% neat()

# Gender
ds2 %>% group_by(mos_gender) %>% count()%>% neat()

# Age
ds2 %>% TabularManifest::histogram_continuous("mos_age_incident")

# ---- officer-2 -------------------------

# ---- complainant-1 ------------------------------------------------------

# ---- complainant-2 ------------------------------------------------------
ds2 %>% group_by(complainant_ethnicity) %>% count()%>% neat()
ds2 %>% group_by(complainant_gender) %>% count()%>% neat()
ds2 %>% TabularManifest::histogram_continuous("complainant_age_incident")

# However,
ds0 %>%
  group_by(complainant_age_incident) %>%
  summarize(n = n()) %>%
  arrange(complainant_age_incident)%>% neat()
# There appears to be some data errors in complainant age
# all records with age < 10 are transformed into NA
ds2 %>%
  group_by(complainant_age_incident) %>%
  summarize(n = n()) %>%
  arrange(complainant_age_incident)%>% neat()

# ---- complainant-3 ------------------------------------------------------
ds2 %>% glimpse()


# ---- complaint-1 ------------------------------------------------------
ds2 %>% group_by(fado_type) %>% count()%>% arrange(desc(n))
ds2 %>%
  group_by(fado_type,allegation) %>%
  count() %>%
  arrange(fado_type,desc(n)) %>%
  print(n = nrow(.))
# ---- complaint-2 ------------------------------------------------------
ds2 %>% group_by(board_disposition) %>% count()%>% arrange(desc(n))

# ---- contact-1 ------------------------------------------------------
ds2 %>% group_by(contact_reason) %>% count() %>% arrange(desc(n))

ds2 %>% group_by(outcome_description) %>% count() %>% arrange(desc(n))

# ---- contact-2 ------------------------------------------------------

# ---- save-to-disk --------------------------------------------------------

ds2 %>% readr::write_rds("./data-unshared/derived/dto.rds")

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


