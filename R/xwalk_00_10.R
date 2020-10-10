#' Crosswalk tracts from 2000 to 2010 boundaries
#'
#' This function loads ACS/Census with 2000 tract boundaries, as well as weights
#' from the LTBD
#' (https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx).
#' It assumes that all numeric values in the specified ACS/Census data are to be
#' crosswalked, but this can be adjusted.
#' It outputs the original data crosswalked to 2010 boundaries, with the suffix
#' _00 to indicate 2000 boundary data.
#'
#' @param census_data An R object containing ACS/Census data
#' @param tractID variable name that contains the tract ID
#' @param vars ACS/Census variables to crosswalk (defaults to all numeric)
#' @return Crosswalked ACS/Census data (2010 boundaries)
#' @export
xwalk_00_10 <- function(census_data, tractID, vars = tidyselect::where(is.numeric)){
  library(tidyverse)

  tractID <- enquo(tractID)

  # change tract ID name
  census_data <- census_data %>% rename(trtid00 = tractID)

  # FIPS filter from Census/ACS data
  fips_filter <- paste(census_data %>% select(!! tractID), collapse = "|")

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
        all_of(vars),
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
    summarise_all(sum)

  return(census_data_xwalk)
}

