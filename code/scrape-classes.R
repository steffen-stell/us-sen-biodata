library(rvest)

l_htm <- c(
  "https://www.senate.gov/senators/Class_I.htm",
  "https://www.senate.gov/senators/Class_II.htm",
  "https://www.senate.gov/senators/Class_III.htm"
) |>
  map(read_html)


d_sen_class <- l_htm |>
  map(html_table) |>
  map(bind_rows) |>
  bind_rows(.id = "class") |>
  filter(X1 != "Independents") |>
  mutate(
    name = str_extract(X1, "^[^(]+(?= \\()"),
    name_last = str_extract(name, "^[^,]+(?=,)"),
    name_first = str_remove(name, "^[^,]+, "),
    party = str_extract(X1, str_extract(X1, "(?<=\\()\\w+(?=-\\w{2}\\))")),
    state = str_extract(X1, str_extract(X1, "(?<=-)\\w{2}(?=\\))")),
    X1 = NULL
  )

d_sen_class |>
  write_csv("data-in/sen-class.csv")

d_sen_class |>
  write_rds("data-in/d_sen_class.rds")
