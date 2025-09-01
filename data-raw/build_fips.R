# Create FIPS 2000
fips_2000 <- readxl::read_excel('fips_2000.xlsx', col_names = FALSE)
names(fips_2000) <- 'raw'
fips_2000 <- fips_2000 |>
  dplyr::mutate(
    raw = stringr::str_squish(raw),
    state = stringr::str_sub(raw, 1, 2),
    county = stringr::str_sub(raw, 3, 5)
  )
fips_2000 <- fips_2000 |>
  dplyr::mutate(name = stringr::str_replace_all(raw, pattern = '[:digit:]', ''))
fips_2000 <- fips_2000 |>
  dplyr::filter(county != '000')
fips_2000 <- fips_2000 |>
  dplyr::select(-raw)


# Create FIPS 2010
data(stata)
fips_2010 <- lapply(1:57, function(x) {
  fip <- stata$fips[x]
  labb <- tolower(stata$abb)[x]
  x <- NULL
  try({
    x <- readr::read_lines(glue::glue('https://www2.census.gov/geo/docs/reference/codes/files/st{fip}_{labb}_cou.txt'))
  })
  x
})
fips_2010 <- lapply(fips_2010, function(x) {
  do.call('rbind', stringr::str_split(x, ','))
})
fips_2010 <- lapply(fips_2010, function(x) {
  names(x) <- c('abb', 'fips', 'county', 'name', 'v')
  tibble::as_tibble(x)
})
fips_2010 <- do.call('rbind', fips_2010)
fips_2010 <- fips_2010 |> dplyr::select(state = V2, county = V3, name = V4)
fips_2010 <- fips_2010 |> dplyr::filter(state %in% unique(fips_2000$state))

# Create FIPS 2020
fips_2020 <- readxl::read_excel('all-geocodes-v2020.xlsx', skip = 4)
fips_2020 <- fips_2020 |>
  dplyr::filter(`Summary Level` == '050')
fips_2020 <- fips_2020 |> dplyr::filter(state %in% unique(fips_2000$state))
