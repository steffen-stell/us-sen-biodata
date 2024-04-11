# Source: https://bioguide.congress.gov
# Date: 2024-03-04

library(tidyverse)
library(jsonlite)
library(lubridate)

json <- "data-in/BioguideProfiles" |>
  list.files(full.names = T) |>
  set_names() |>
  lapply(read_json)

# clean -------------------------------------------------------------------

senator <-
  json |>
  list_transpose() |>
  as_tibble() |>
  mutate_if(
    .predicate = \(x) is.list(x) & all(map_lgl(x, is.null) | map_lgl(x, \(y) length(y) == 1)),
    .funs = \(x) ifelse(map_lgl(x, is.null), NA, x) |> unlist()
    ) |>
  select(-relationship, -creativeWork, -researchRecord)

senatorCongress <-
  senator |>
  select(usCongressBioId, jobPositions) |>
  unnest_longer(jobPositions) |>
  unnest_wider(jobPositions) |>
  unnest_wider(job) |>
  rename(job_departureReason = departureReason,
         job_name = name,
         job_startDate = startDate,
         job_startCirca = startCirca,
         job_endDate = endDate,
         job_endCirca = endCirca,
         ) |>
  filter(job_name == "Senator") |>
  unnest_wider(congressAffiliation) |>
  unnest_wider(congress) |>
  mutate_at("represents", map_chr, \(x) x$regionCode) |>
  rename(
    congress_startDate = startDate,
    congress_endDate = endDate
  ) |>
  select(-job_name, -jobType, -name, -congressType,
         -deleted, -job_startCirca, -job_endCirca, # these are never true
         -departureReason # is either NA or equal to `job_departureReason`
         ) |>
  #filter(congressNumber >= 64) |>
  mutate(
    across(ends_with("Date"), as.Date),
  )

congress <- senatorCongress |>
  select(congressNumber, congress_startDate, congress_endDate) |>
  mutate(
    congress_startDate = if_else(congressNumber == 118, as.Date("2023-01-03"), congress_startDate),
    congress_endDate = case_when(
      congressNumber == 116 ~ as.Date("2021-01-03"),
      congressNumber == 118 ~ as.Date("2025-01-03"),
      T ~ congress_endDate)
  ) |>
  distinct() |>
  arrange(congressNumber)

senatorCongress <- senatorCongress |>
  ungroup() |>
  select(-starts_with("congress_")) |>
  arrange(usCongressBioId, congressNumber) |>
  # Manual imputation
  mutate(
    job_startDate = case_when(
      usCongressBioId == "T000254" & congressNumber == 83 ~ as.Date("1954-12-24"), # Strom Thurmond (SC)
      T ~ job_startDate
    ),
    job_endDate = case_when(
      usCongressBioId == "T000254" & congressNumber == 83 ~ as.Date("1956-04-04"), # Strom Thurmond (SC)
      usCongressBioId == "B000384" & congressNumber == 93 ~ as.Date("1974-12-20"), # Wallace BENNETT (UT)
      usCongressBioId == "A000342" & congressNumber == 79 ~ as.Date("1946-08-02"), # Warren R. Austin (VT)
      T ~ job_endDate
    ),
    job_departureReason = case_when(
      usCongressBioId == "T000254" & congressNumber == 83 ~ "Resigned", # Strom Thurmond (SC)
      usCongressBioId == "B000384" & congressNumber == 93 ~ "Resigned", # Wallace BENNETT (UT)
      usCongressBioId == "A000342" & congressNumber == 79 ~ "Resigned", # Warren R. Austion (VT)
      T ~ job_departureReason
    )
  )

senatorCongress |>
  filter(usCongressBioId == "B000384")

senatorCongress |>
  count(job_departureReason)

# -------------------------------------------------------------------------


servicePeriods <-
  senatorCongress |>
  inner_join(select(senator, usCongressBioId, givenName, familyName)) |>
  group_by(usCongressBioId) |>
  mutate(consecutive = cumsum(congressNumber-lag(congressNumber) != 1 | is.na(lag(congressNumber)) | represents != lag(represents) | is.na(lag(represents)) | !is.na(lag(job_endDate)))) |> # defining change of state as non-consecutive
  inner_join(congress) |>
  group_by(usCongressBioId, consecutive) |>
  summarize(
    represents = unique(represents),
    job_start = first(job_startDate),
    job_end = last(job_endDate),
    congress_start = first(congress_startDate),
    congress_end = last(congress_endDate),
    first_congress = first(congressNumber),
    last_congress = last(congressNumber)
    )

firsts <- servicePeriods |>
  group_by(first_congress) |>
  summarize(
    first_in_office = min(job_start, na.rm = T)
    )

lasts <- servicePeriods |>
  group_by(last_congress) |>
  summarize(
    last_in_office = min(job_end, na.rm = T)
    )

servicePeriods_imputed <- servicePeriods |>
  inner_join(firsts) |>
  inner_join(lasts) |>
  mutate(
    job_start_imputed = is.na(job_start),
    job_end_imputed = is.na(job_end),
    job_start = case_when(
      job_start_imputed & first_in_office <= congress_start ~ first_in_office,
      job_start_imputed & first_in_office > congress_start ~ congress_start,
      job_start_imputed & is.na(first_in_office) ~ congress_start,
      T ~ job_start
    ),
    job_end = case_when(
      job_end_imputed & last_in_office == Inf ~ congress_end,
      job_end_imputed & last_in_office <= congress_end ~ congress_end,
      job_end_imputed & last_in_office > congress_end ~ last_in_office,
      job_end_imputed & is.na(last_in_office) ~ congress_end,
      T ~ job_end
    )
  ) |>
  mutate(
    job_start = case_when(
      # usCongressBioId == "S000534" ~ as.Date("1926-12-07"), # Frank L. Smith
      #usCongressBioId == "C000154" ~ as.Date("1950-11-29"), # Frank Carlson
      # usCongressBioId == "B000010" ~ as.Date("1933-03-04"), # Nathan Bachman (TN)
      T ~ job_start
    ),
    job_end = case_when(
      #usCongressBioId == "M001092" ~ as.Date("1971-01-02"), # Lloyd Murphy (corrected after report)
      #usCongressBioId == "G000337" ~ as.Date("1946-11-06"), # Charles Gosset (corrected after report)
      usCongressBioId == "S000602" ~ as.Date("1970-11-16"), # Ralph Smith
      #usCongressBioId == "F000457" ~ as.Date("2018-01-02"), # Al Franken (corrected after report)
      usCongressBioId == "B001099" ~ as.Date("1954-12-02"), # Thomas Burke (OH)
      T ~ job_end
    )
  ) |>
  ungroup() |>
  inner_join(
    x = select(senator, usCongressBioId, familyName)
  )

# Points where more than 2 service periods overlap in a state
servicePeriods_imputed |>
  select(represents, job_start, job_end) |>
  pivot_longer(job_start:job_end, names_to = "type", values_to = "date") |>
  arrange(represents, date, type) |>
  group_by(represents) |>
  mutate(concurrent = cumsum((type == "job_start") - (type == "job_end"))) |>
  filter(
    #date > as.Date("1914-01-01"),
    concurrent > 2
    ) |>
  select(-type, state = represents) |> View()

# check that state
servicePeriods_imputed |>
  arrange(represents, job_start) |>
  filter(represents == "OR") |>
  inner_join(select(senator, usCongressBioId, familyName), y = _) |>
  arrange(job_start) |>
  View()


# Class -------------------------------------------------------------------
d_state_abbr <- read_csv(
  "data-in/state-abbreviations.csv",
  col_names = c("state_name", "state_code")
  )

# d_class <- read_csv("data-in/senate-classes.csv") |>
#   select(1:4) |>
#   pivot_longer(2:4, names_to = "class", values_to = "name") |>
#   filter(name != "—") |>
#   mutate(
#     party = str_extract(name, "(?<=\\()\\w+(?=\\))"),
#     name = str_remove(name, " \\(\\w+\\)"),
#     class = class |>
#       str_extract("\\d") |>
#       as.integer()
#   ) |>
#   inner_join(d_state_abbr, c("State" = "state_name"))

d_sen_class <- read_rds("data-in/d_sen_class.rds")

serviceWithClass <- servicePeriods_imputed |>
  filter(job_start <= ymd("2024-04-11"), job_end >= ymd("2024-04-11")) |>
  inner_join(
    y = select(d_sen_class, state, name_last, class),
    by = c("represents" = "state", "familyName" = "name_last")
  ) |>
  full_join(servicePeriods_imputed) |>
  select(-consecutive) |>
  relocate(class, .after = "represents")

#' @title Infer senator class from successors
#' inputs must be sorted by `job_end`. Only for single state, use grouping for all
#' @details Senator class can be inferred from adjacent tenures, as long as there are no double vacancies. This function infers only on basis of successors.
infer_sen_class <- \(class, job_start, job_end) {
  d <- tibble(class, job_start, job_end)

  occupied <- d |>
    filter(!is.na(class)) |>
    slice(1:2)

  for (i in seq_along(d$class)) {
    if (!is.na(d[i, "class"])) {
      occupied[occupied$class == d$class[i], "job_start"] <- d$job_start[i];
      next;
    }

    potential <- occupied |>
      filter(!d$job_end[i] > job_start)

    if (nrow(potential) == 0) {
      stop("More than 2 concurrent senators");
    } else if (nrow(potential) == 2) {
      warning("Double vacancy, class values will be NA prior to double vacancies");
      break;
    }

    d[i, "class"] <- potential[["class"]]
    occupied[occupied$class == potential[["class"]], "job_start"] <- d[i, "job_start"]
  }
  return(d$class)
}

class_imputed <- serviceWithClass |>
  arrange(represents, desc(job_end)) |>
  group_by(represents) |>
  filter(job_end > ymd("1914-01-01")) |>
  mutate(class = infer_sen_class(class, job_start, job_end))

class_imputed |>
  ungroup() |>
  count(is.na(class))

# To do: find class data for double vacancies

# -------------------------------------------------------------------------

d <- servicePeriods_imputed |>
  select(usCongressBioId, represents, job_start, job_end) |>
  mutate(
    .keep = "unused",
    period = job_start %--% (job_end - days(1))
    ) |>
  arrange(represents, period)

x <- d$period |>
  head()

int_overlaps(x[4], x[5])

servicePeriods_imputed |>
  select(usCongressBioId, represents, job_start, job_end) |>
  arrange(represents, job_start)



servicePeriods_imputed |>
  select(represents, job_start, job_end) |>
  mutate_at("job_end", `-`, 1) |>
  pivot_longer(job_start:job_end, names_to = "type", values_to = "date") |>
  arrange(represents, date, type) |>
  group_by(represents) |>
  mutate(concurrent = cumsum((type == "job_start") - (type == "job_end"))) |>
  filter(
    date > as.Date("1914-01-01"),
    concurrent == 0
  ) |>
  select(-type, state = represents) |>
  filter(date != ymd("2025-01-02"))



