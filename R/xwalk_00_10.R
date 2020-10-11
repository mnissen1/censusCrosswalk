#' Crosswalk tracts from 2000 to 2010 boundaries
#'
#' This function takes ACS/Census data with 2000 tract boundaries, and loads in
#' weights from the LTBD
#' (https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx).
#' It assumes that all numeric values in the specified ACS/Census data are to be
#' crosswalked. If this is not the case, those variables should be filtered
#' prior to using the function.
#' It outputs the original data crosswalked to 2010 boundaries.
#'
#' @param census_data An R object containing ACS/Census data w/ 2000 boundaries
#' @param tractID variable name that contains the tract ID
#' @return Crosswalked ACS/Census data (2010 boundaries)
#' @export
xwalk_00_10 <- function(census_data, tractID){
  library(tidyverse)
  tractID <- enquo(tractID)

  # change tract ID name
  census_data <- census_data %>% rename(trtid00 = !!quo_name(tractID))

  # FIPS filter from Census/ACS data
  fips_filter <-
    paste(census_data %>% select(trtid00) %>% pull(), collapse = "|")

  # Filter crosswalk data by relevant FIPS (reduces interpolation time)
  crosswalk_filter <-
    ltbd_weights_00_10 %>%
    filter(str_detect(trtid00, fips_filter))

  #Interpolate 2000 boundary data to 2010 tracts
  census_data_xwalk <-
    census_data %>%
    # join to filtered crosswalk
    full_join(crosswalk_filter, by = "trtid00") %>%
    # change weight from character to numeric type
    mutate(weight = as.numeric(weight)) %>%
    # multiply all data by its weight
    mutate(
      across(
        # select all Census/ACS data
        where(is.numeric),
        # find value of data multiplied by weight
        ~ . * weight,
        # attach name in the following pattern
        .names = "{col}_00"
      )
    ) %>%
    # select for relevant variables (2010 tracts)
    select(trtid10, ends_with("_00")) %>%
    # group by 2010 tracts
    group_by(trtid10) %>%
    # sum up all variables
    summarise_all(sum) %>%
    # ungroup
    ungroup() %>%
    # select out unwanted weight vars
    select(-c(weight_00:changetype_00)) %>%
    # remove the "_00" suffix
    rename_with(~str_remove(.x, "_00$"), ends_with("_00"))

  return(census_data_xwalk)
}

