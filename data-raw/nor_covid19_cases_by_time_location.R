# create spltidy format data for cases
library(data.table)
library(magrittr)

# import data
# daily, nation
d <- data.table::fread("data-raw/covid19_cases.csv")

# remove pop and location for future use
d_pop_by_loc_time <- copy(unique(d[, .(year, pop, location_code, location_name)]))


# some transformation
d[, date :=as.Date(date)]
d[, date_of_publishing := NULL]
d[, year := NULL]
d[, week := NULL]
d[, yrwk := NULL]
d[, x := NULL]
# drop pop and location
d[, location_name:= NULL]
d[, pop := NULL]
d
# granularity_time: day
# granularity_geo: county, nation



# set to splfmt ----
spltidy::set_splfmt_rts_data_v1(d)
colnames(d)

# change variable names
setnames(
  d,
  c(
    "n",
    "pr100000"
  ),
  c(
    "covid19_cases_testdate_n",
    "covid19_cases_testdate_pr100000"
  )
)

d



# weekly aggregate
# on n, pr100000
# percentage per location per time should be summable

# granularity time: isoweek
week <- d[,.(
  covid19_cases_testdate_n = sum(covid19_cases_testdate_n),
  covid19_cases_testdate_pr100000 = sum(covid19_cases_testdate_pr100000),
  granularity_time = "isoweek"
),
keyby=.(
  location_code,
  border,
  age,
  sex,
  isoyearweek
)] %>%
  spltidy::create_unified_columns()

week

colnames(d)
colnames(week)

# put daily and weekly together
nor_covid19_cases_by_time_location <- rbind(d, week)

nor_covid19_cases_by_time_location[, location_code := stringr::str_replace(location_code, "county", "county_nor")]
nor_covid19_cases_by_time_location[location_code=="norge", location_code := "nation_nor"]

# save the data into data folder in .rda format
usethis::use_data(nor_covid19_cases_by_time_location, overwrite = TRUE)


# ?spltidy::nor_covid19_cases_by_time_location



