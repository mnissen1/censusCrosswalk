## code to prepare `DATASET` dataset goes here

ltbd_weights_00_10 <- readr::read_csv("crosswalk_2000_2010.csv")

usethis::use_data(ltbd_weights_00_10, overwrite = TRUE, internal = TRUE)
