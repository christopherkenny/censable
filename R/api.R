api_vars <- list(
  dec_2020 = list(
    all = c(
      pop      = 'P2_001N', pop_white = 'P2_005N', pop_black = 'P2_006N',
      pop_hisp = 'P2_002N', pop_aian  = 'P2_007N', pop_asian = 'P2_008N',
      pop_nhpi = 'P2_009N', pop_other = 'P2_010N', pop_two   = 'P2_011N',
      vap      = 'P4_001N', vap_white = 'P4_005N', vap_black = 'P4_006N',
      vap_hisp = 'P4_002N', vap_aian  = 'P4_007N', vap_asian = 'P4_008N',
      vap_nhpi = 'P4_009N', vap_other = 'P4_010N', vap_two   = 'P4_011N'
    ),
    all7 = c(
      pop7      = 'P1_001N', pop7_white = 'P1_003N', pop7_black = 'P1_004N',
      pop7_aian = 'P1_005N', pop7_asian = 'P1_006N',
      pop7_nhpi = 'P1_007N', pop7_other = 'P1_008N', pop7_two = 'P1_009N',
      vap7      = 'P3_001N', vap7_white = 'P3_003N', vap7_black = 'P3_004N',
      vap7_aian = 'P3_005N', vap7_asian = 'P3_006N',
      vap7_nhpi = 'P3_007N', vap7_other = 'P3_008N', vap7_two = 'P3_009N'
    )
  ),
  dec_2010 = list(
    all = c(
      pop      = 'P003001', pop_white = 'P005003', pop_black = 'P005004',
      pop_hisp = 'P004003', pop_aian  = 'P005005', pop_asian = 'P005006',
      pop_nhpi = 'P005007', pop_other = 'P005008', pop_two   = 'P005009',
      vap      = 'P010001', vap_white = 'P011005', vap_black = 'P011006',
      vap_hisp = 'P011002', vap_aian  = 'P011007', vap_asian = 'P011008',
      vap_nhpi = 'P011009', vap_other = 'P011010', vap_two   = 'P011011'
    ),
    all7 = c(
      pop7      = 'P003001', pop7_white = 'P003002', pop7_black = 'P003003',
      pop7_aian = 'P003004', pop7_asian = 'P003005',
      pop7_nhpi = 'P003006', pop7_other = 'P003007', pop7_two   = 'P005008',
      vap7      = 'P010001', vap7_white = 'P010003', vap7_black = 'P010004',
      vap7_aian = 'P010005', vap7_asian = 'P010006',
      vap7_nhpi = 'P010007', vap7_other = 'P010008', vap7_two   = 'P010009'
    )
  ),
  dec_2000 = list(
    pop = c(
      pop      = 'P004001', pop_white = 'P004005', pop_black = 'P004006',
      pop_hisp = 'P004002', pop_aian  = 'P004007', pop_asian = 'P004008',
      pop_nhpi = 'P004009', pop_other = 'P004010', pop_two   = 'P004011'
    ),
    vap = c(
      vap      = 'P006001', vap_white = 'P006005', vap_black = 'P006006',
      vap_hisp = 'P006002', vap_aian  = 'P006007', vap_asian = 'P006008',
      vap_nhpi = 'P006009', vap_other = 'P006010', vap_two   = 'P006011'
    ),
    pop7 = c(
      pop7      = 'P003001', pop7_white = 'P003003', pop7_black = 'P003004',
      pop7_aian = 'P003005', pop7_asian = 'P003006',
      pop7_nhpi = 'P003007', pop7_other = 'P003008', pop7_two   = 'P003009'
    ),
    vap7 = c(
      vap7      = 'P005001', vap7_white = 'P005003', vap7_black = 'P005004',
      vap7_aian = 'P005005', vap7_asian = 'P005006',
      vap7_nhpi = 'P005007', vap7_other = 'P005008', vap7_two   = 'P005009'
    )
  )
)

fetch_api_vars <- function(survey, year, groups) {
  match.arg(survey, c('dec'))

  lookup <- dplyr::case_when(
    survey == 'dec' ~ paste0(survey, '_', year),
    TRUE ~ NA_character_
  )
  categ <- dplyr::case_when(
    year == 2000 ~ groups,
    groups %in% c('vap', 'pop', 'all') ~ 'all',
    groups %in% c('vap7', 'pop7', 'all7') ~ 'all7',
    TRUE ~ NA_character_
  )

  vars <- unlist(unname(api_vars[[lookup]][categ]))

  if (groups == 'pop') {
    vars <- vars[stringr::str_detect(names(vars), 'pop')]
  } else if (groups == 'vap') {
    vars <- vars[stringr::str_detect(names(vars), 'vap')]
  } else if (groups == 'pop7') {
    vars <- vars[stringr::str_detect(names(vars), 'pop7')]
  } else if (groups == 'vap7') {
    vars <- vars[stringr::str_detect(names(vars), 'vap7')]
  }

  vars
}

fetch_api_vars_ap <- function(year, group, race) {
  api_vars_ap %>%
    dplyr::filter(
      .env$year == .data$year,
      .env$group == .data$group,
      .env$race == .data$race
    ) %>%
    dplyr::pull(.data$vars) %>%
    unlist()
}


get_census_api <- function(geography, year, state, county = NULL,
                           variables, tab = 'dec/pl', show_call = FALSE) {

  state <- match_fips(state)
  rg <- format_regions(geography, state, county, decade = year - (year %% 10))

  out <- censusapi::getCensus(
    name = tab,
    vintage = year,
    vars = c('GEO_ID', variables),
    region = rg$region,
    regionin = rg$regionin,
    key = get_census_key(),
    show_call = show_call
  )

  if (!is.null(names(variables)[1])) {
    names(out)[which(!is.na(match(names(out), variables)))] <- names(variables)[stats::na.omit(match(names(out), variables))]
  }
  out <- out %>%
    dplyr::relocate(GEOID = .data$GEO_ID) %>%
    dplyr::mutate(GEOID = stringr::str_sub(GEOID, start = 10))

  out
}

format_regions <- function(geography, state, county, decade) {

  if (is.null(county)) {
    ct <- '*'
  } else {
    ct <- match_county(state = state, counties = county, decade = decade)
  }

  if (!geography %in% c('state', 'county')) {
    rgin <- paste0('state:', state, '+county:', col_var(ct))
    rg <- paste0(geography, ':*')
  } else if (geography != state) {
    rgin <- paste0('state:', state)
    rg <- paste0(geography, ':', col_var(ct))
  } else {
    rgin <- NULL
    rg <- paste0('state:', col_var(state))
  }

  list(
    region = rg,
    regionin = rgin
  )
}

col_var <- function(...) {
  paste0(..., collapse = ',')
}

get_geometry <- function(geography, ...) {
  geography <- clean_geographies(geography)
  fn <- eval(parse(text = paste0('tinytiger::tt_', geography)))
  args <- list(...)[methods::formalArgs(fn)]
  if (geography == 'blocks' && 'county' %in% names(list(...)) && is.null(list(...)[['county']])) {
    args[['county']] <- NULL
  }
  do.call(fn, args) %>%
    dplyr::rename_with(.fn = function(x) stringr::str_sub(x, end = -3),
                       .cols = dplyr::any_of(c('GEOID20', 'GEOID10', 'GEOID00'))) %>%
    dplyr::select(.data$GEOID, .data$geometry)

}

clean_geographies <- function(x) {
  if (x %in% c('state', 'tract', 'block group', 'block',
               'county subdivision', 'place', 'voting district',
               'congressional district')) {
    x <- paste0(x, 's')
  } else if (x == 'county') {
    x <- 'counties'
  } else if (x == 'state legislative district (upper chamber)') {
    x <- 'state leg upper'
  } else if (x == 'state legislative district (lower chamber)') {
    x <- 'state leg lower'
  } else if (x == 'school district (unified)') {
    x <- 'unified school districts'
  } else if (x == 'school district (secondary)') {
    x <- 'secondary school districts'
  } else if (x == 'school district (elementary)') {
    x <- 'elementary school districts'
  }

  stringr::str_replace_all(x, ' ', '_')
}
