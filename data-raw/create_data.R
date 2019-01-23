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

# ebola dataset for rates ####
#  Marks, M. Learning Clinical Epidemiology with R. [Project]. London School of Hygiene & Tropical Medicine, London, United Kingdom.

ebola <- read.csv(file = "https://datacompass.lshtm.ac.uk/608/1/mmc1.txt",
                  header = TRUE, sep = ",", stringsAsFactors = FALSE)
write.csv(ebola, "./data-raw/ebola.csv") # just so that there is always a copy

# dates and time
ebola$disease_onset <- as.Date(ebola$disease_onset, format = "%Y-%m-%d")
ebola$disease_ended <- as.Date(ebola$disease_ended, format = "%Y-%m-%d")
ebola$days_at_risk <- as.numeric(ebola$disease_ended - ebola$disease_onset)

# drop na ages, just for simplicity
ebola <- ebola[!is.na(ebola$age), ]
range(ebola$age)
ebola$age_group <- ifelse(ebola$age <= 3, "<=3",
                          ifelse(ebola$age > 3 & ebola$age < 21, "4-20",
                                 ifelse(ebola$age >= 21 & ebola$age < 41, "21-40",
                                        ifelse(ebola$age >= 41 & ebola$age < 61, "41-60", ">= 61"))))
ebola$age_group <- factor(ebola$age_group,
                          levels = c("<=3", "4-20", "21-40", "41-60", ">= 61" ))
table(ebola$age_group, useNA = "ifany")

# outcome
table(ebola$status, useNA = "ifany")
ebola$died <- ifelse(ebola$status == "died", 1, 0)
table(ebola$status, ebola$died, useNA = "ifany")
ebola <- ebola[,c("id", "age", "age_group", "sex", "disease_onset",
                  "disease_ended", "days_at_risk", "status", "died", "transmission")]
# just taking a look at the data
# ebola %>%
#   group_by(age_group) %>%
#   summarise_at(vars(died, days_at_risk), funs(sum(., na.rm = TRUE))) %>%
#   mutate(deaths_per_10_day = (died / days_at_risk) * 10)
#
# ebola %>%
#   group_by(sex, age_group) %>%
#   summarise_at(vars(died, days_at_risk), funs(sum(., na.rm = TRUE))) %>%
#   mutate(deaths_per_10_day = (died / days_at_risk) * 10)

# save
devtools::use_data(ebola)
rm(ebola)
