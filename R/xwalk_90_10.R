#' Crosswalk tracts from 1990 to 2010 boundaries
#'
#' This function takes ACS/Census data with 1990 tract boundaries, and loads in
#' weights from the LTBD
#' (https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx).
#' It assumes that you want all variables to be crosswalked, and includes a filter
#' to only choose numeric type variables. If you want to only crosswalk specific
#' variables, specify this in the vars argument (must be numeric).
#' It outputs the original data crosswalked to 2010 boundaries.
#'
#' @param census_data An R table containing ACS/Census data w/ 1990 boundaries
#' @param tractID variable name that contains the tract ID
#' @param vars Census/ACS variables to crosswalk. Defaults to all numeric
#' variables, but can be adjusted. Custom variables must be a single string
#' or a list of strings.
#' @return Crosswalked selected ACS/Census data (2010 boundaries, defaults to
#' all numeric data)
#' @export
xwalk_90_10 <- function(census_data, tractID, vars = names(census_data)){
  library(tidyverse)
  tractID <- enquo(tractID)

  # change tract ID name
  census_data <-
    census_data %>%
    rename(trtid90 = !!quo_name(tractID)) %>%
    mutate(
      trtid90 = str_pad(as.character(trtid90), 11, side = "left", pad = "0")
    )

  # filter tract ID from variables
  if (setequal(vars, names(census_data))) {
    vars <-
      census_data %>%
      select(-trtid90) %>%
      # make sure the data is type numeric
      select_if(is.numeric) %>%
      names()
  }

  # FIPS filter from Census/ACS data
  fips_filter <-
    paste(census_data %>% select(trtid90) %>% pull(), collapse = "|")

  # Filter crosswalk data by relevant FIPS (reduces interpolation time)
  crosswalk_filter <-
    ltbd_weights_90_10 %>%
    filter(str_detect(trtid90, fips_filter))

  #Interpolate 1990 boundary data to 2010 tracts
  census_data_xwalk <-
    census_data %>%
    # join to filtered crosswalk
    full_join(crosswalk_filter, by = "trtid90") %>%
    # change weight from character to numeric type
    mutate(weight = as.numeric(weight)) %>%
    # multiply all data by its weight
    mutate(
      across(
        # select selected Census/ACS data (defaults to everything)
        .cols = vars,
        # find value of data multiplied by weight
        ~ . * weight,
        # attach name in the following pattern
        .names = "{col}_90"
      )
    ) %>%
    # select for relevant variables (2010 tracts)
    select(trtid10, ends_with("_90")) %>%
    # group by 2010 tracts
    group_by(trtid10) %>%
    # sum up all variables
    summarise_all(sum) %>%
    # ungroup
    ungroup() %>%
    # remove the "_90" suffix
    rename_with(~str_remove(.x, "_90$"), ends_with("_90"))

  return(census_data_xwalk)
}

