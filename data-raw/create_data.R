# create tolbutamide data ####

tolbutamide <- data.frame(
    tolbutamide = c(rep(1, 8), rep(0, 5), rep(1, 98), rep(0, 115)),
  dead = c(rep(1, 8), rep(1, 5), rep(0, 98), rep(0, 115)),
  age = rep("<55y", 226),
  stringsAsFactors = FALSE
)

tolbutamide2 <- data.frame(
  tolbutamide = c(rep(1, 22), rep(0, 16), rep(1, 76), rep(0, 69)),
  dead = c(rep(1, 22), rep(1, 16), rep(0, 76), rep(0, 69)),
  age = rep("ge55y", 183),
  stringsAsFactors = FALSE
)

tolbutamide <- dplyr::bind_rows(tolbutamide, tolbutamide2)

# check  tolbutamide ####
sum(tolbutamide$dead) == 30 + 21
length(tolbutamide$dead) == 204 + 205
sum(tolbutamide$tolbutamide) == 204

# write tolbutamide data ####
devtools::use_data(tolbutamide)
rm(tolbutamide, tolbutamide2)
