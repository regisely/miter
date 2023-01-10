library(dplyr)
library(readxl)
library(rbcb)
library(lubridate)
library(stringi)
library(purrr)
library(tidyr)

if (!file.exists("data-raw/icms_br.xlsx")) {
  download.file(
    "https://www.confaz.fazenda.gov.br/20220503_sigdef.xls", "data-raw/icms_br.xls",
    quiet = TRUE, method = "curl", extra = "-k -L"
  )
}

ufs <- read_excel("data-raw/icms_br.xls",
  sheet = "uf", range = cell_cols("B:C")
)

icms_raw <- read_excel("data-raw/icms_br.xls",
  sheet = "arrecadacao",
  range = cell_limits(c(2, 1), c(NA, 22))
)

bc_raw <- get_series(
  c(
    `Sao Paulo` = 25392,
    `Minas Gerais` = 25379,
    `Rio de Janeiro` = 25396,
    `Rio Grande do Sul` = 25401,
    `Parana` = 25408,
    `Bahia` = 25415,
    ipca = 433
  ),
  start_date = "2010-01-01",
  end_date = "2021-12-31"
)

icms_br <- icms_raw %>%
  mutate(date = ym(co_periodo)) %>%
  filter(date >= "2010-01-01" & date < "2022-01-01") %>%
  select(state = ESTADO, date, icms = `TOTAL DA ARRECADAÇÃO DO ICMS`)

top_6 <- icms_br %>%
  group_by(state) %>%
  summarise(icms = sum(icms)) %>%
  slice_max(icms, n = 6) %>%
  pull(state)

icms_br <- icms_br %>%
  filter(state %in% top_6) %>%
  mutate(state = stri_trans_general(state, id = "Latin-ASCII")) %>%
  left_join(
    bc_raw %>%
      reduce(inner_join, by = "date") %>%
      mutate(ipca = cumprod(1 + ipca / 100) * 100) %>%
      pivot_longer(-c("date", "ipca"), names_to = "state", values_to = "ibcr"),
    by = c("date", "state")
  ) %>%
  mutate(icms = icms / 1000000)

usethis::use_data(icms_br, overwrite = TRUE)
