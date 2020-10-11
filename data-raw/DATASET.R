## code to prepare `DATASET` dataset goes here

ltbd_weights_00_10 <- readr::read_csv("data-raw/crosswalk_2000_2010.csv")

ltbd_weights_90_10 <- readr::read_csv("data-raw/crosswalk_1990_2010.csv")

usethis::use_data(ltbd_weights_00_10, ltbd_weights_90_10, overwrite = TRUE, internal = TRUE)
