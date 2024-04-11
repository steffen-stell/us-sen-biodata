# chronlist.pdf
# Source: https://www.senate.gov/artandhistory/history/resources/pdf/chronlist.pdf
library(tidyverse)
library(pdftools)
library(lubridate)


l_sen_chron <- "data-in/chronlist.pdf" |>
  pdf_data(T)

d_sen_abbr <- last(l_sen_chron)

d_sen_chron <- l_sen_chron |>
  head(-1) |> # Discard abbreviations page
  bind_rows(.id = "page") |>
  mutate_at("page", as.integer) |>
  # extract year
  group_by(page, y) |>
  mutate(
    start_year = ifelse(
      test = str_c(text, collapse = "") |> str_detect("^\\**\\d{4}\\**$"),
      yes = str_c(text, collapse = "") |> str_extract("\\d{4}"),
      no = NA
    ) |>
      as.integer()
  ) |>
  ungroup() |>
  fill(start_year) |>
  filter(
    !is.na(start_year), # Discard intro section
    font_name != "TimesNewRomanPS-BoldMT", # Discard headers
    text != "*" # Discard asterisks around year header which are no longer bold after 2011
    ) |>
  select(-font_name) |>
  # remove page numbers
  group_by(page) |>
  filter(y != max(y)) |>
  ungroup()

# Create fuzzy y variable for which elements within range of 3 units on y axis
# are considered to be on the same line
d_sen_chron <- d_sen_chron |>
  arrange(page, y, x) |>
  mutate(y_fuzzy = cumsum(abs(y - lag(y, default = 0) > 3))) |>
  group_by(page, y_fuzzy) |>
  mutate(y_fuzzy  = y |>
           mean() |>
           round() |>
           as.integer()
         ) |>
  ungroup() |>
  arrange(page, y_fuzzy, x)

d_footnotes <- d_sen_chron |>
  filter(!font_size %in% c(12, 7.92)) |>
  group_by(page, y_fuzzy) |>
  summarize(ft = str_c(text, collapse = " ")) |>
  ungroup() |>
  mutate(footnumber = ft |>
           str_detect("^\\d+ ") |>
           cumsum()
         ) |>
  group_by(footnumber) |>
  summarize(footnote = str_c(ft, collapse = " "))

d_senator <- d_sen_chron |>
  filter(font_size %in% c(12, 7.92)) |>
  mutate(
    type = case_when(
      x < 144 ~ "start_md",
      x >= 414 & !str_detect(text, "\\)") ~ "end_date",
      T ~ "senator"
    )
  ) |>
  group_by(page, y_fuzzy, type) |>
  summarize(value = str_c(text, collapse = " "), start_year = unique(start_year)) |>
  pivot_wider(names_from = type, values_from = value) |>
  ungroup() |>
  mutate(note = senator |>
           str_extract("^\\(.+") |>
           lead()
         ) |>
  filter(!str_detect(senator, "^\\(.+"))



# end_date fuzzy / broken for
#   1. Pinckney, Charles (DR-SC),
#   2. Cobb, Thomas W. (CRR/J-GA),
#   3. English, James E. (D-CT)
d_senator |>
  mutate(end_date2 = mdy(end_date)) |>
  filter(!is.na(end_date), is.na(end_date2))


d_senator <- d_senator |>
  mutate(end_date = mdy(end_date)) |>
  fill(start_md) |>
  mutate(
    start_date = str_c(start_md, ", ", start_year) |>
      mdy()
    ) |>
  separate(start_md, c("start_month", "start_day"), sep = " ") |>
  mutate(
    party = str_extract(senator, "(?<=\\()[^(]+(?=-[A-Z]{2}\\))"),
    state = str_extract(senator, "(?<=-)[A-Z]{2}(?=\\))"),
    footnumber = str_extract(senator, "\\d+(?!st)") |>
      as.integer(),
    senator = senator |>
      str_remove("\\d+(?!st)") |>
      str_remove("\\(.+\\)") |>
      str_squish(),
    senator = case_when(
      senator == "Buckingham William A." ~ "Buckingham, William A.",
      senator == "Wagner Robert F." ~ "Wagner, Robert F.",
      senator == "Laphonza Butler" ~ "Butler, Laphonza",
      T ~ senator
      )
    ) |>
  relocate(starts_with("start_"), starts_with("end_"), note, footnumber, .after = last_col()) |>
  left_join(d_footnotes) |>
  select(-footnumber, -y_fuzzy)

d_senator |>
  write_csv("data-out/chronlist.csv")

d_senator |>
  write_rds("data-out/chronlist.rds")


# footnote 7 on page 73 for Jay Rockefeller is broken
# font_size for footnotes is 7.92, in 2 cases 10.92


