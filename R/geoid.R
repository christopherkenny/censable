#' Breakdown Census GEOID into Components
#'
#' @param .data dataframe, tibble, or sf tibble
#' @param GEOID Column in .data with Census GEOID
#' @param area_type String, default is 'spine' with type of GEOID. Options are
#' 'spine' for states, counties, tracts, block groups, and blocks. 'shd' for lower
#' state legislative districts, 'ssd' for upper state legislative districts, 'cd'
#' for congressional districts, or 'zcta' for zip code tabulation areas.
#'
#'
#' @return .data with added identifying columns based on area_type
#' @export
#' @concept geoid
#' @examples
#' data(mt_county)
#' mt_county <- mt_county %>% breakdown_geoid()
breakdown_geoid <- function(.data, GEOID = 'GEOID', area_type = 'spine') {
  if (missing(.data)) {
    stop('`.data` argument missing in `breakdown_geoid()`.')
  }

  GEOID <- rlang::ensym(GEOID)
  geoid_col <- rlang::eval_tidy(GEOID, .data)

  if (is.null(geoid_col[1])) {
    stop('`GEOID` argument is not a column in `.data`.')
  }

  if (stringr::str_length(geoid_col[1]) < 2) {
    stop('GEOID does not have a recognizable pattern.')
  } else {
    len <- stringr::str_length(geoid_col[1])
  }

  if (area_type == 'spine') {
    .data <- .data %>% dplyr::mutate(state = stringr::str_sub({{ GEOID }}, 1, 2))
    if (len >= 5) {
      .data <- .data %>% dplyr::mutate(county = stringr::str_sub({{ GEOID }}, 3, 5))
    }
    if (len >= 11) {
      .data <- .data %>% dplyr::mutate(tract = stringr::str_sub({{ GEOID }}, 6, 11))
    }
    if (len >= 12) {
      .data <- .data %>% dplyr::mutate(block_group = stringr::str_sub({{ GEOID }}, 12, 12))
    }
    if (len >= 15) {
      .data <- .data %>% dplyr::mutate(block = stringr::str_sub({{ GEOID }}, 12, 15))
    }
  } else if (area_type == 'shd') {
    .data <- .data %>% dplyr::mutate(state = stringr::str_sub({{ GEOID }}, 1, 2))
    .data <- .data %>% dplyr::mutate(shd = stringr::str_sub({{ GEOID }}, 3, 5))
  } else if (area_type == 'ssd') {
    .data <- .data %>% dplyr::mutate(state = stringr::str_sub({{ GEOID }}, 1, 2))
    .data <- .data %>% dplyr::mutate(ssd = stringr::str_sub({{ GEOID }}, 3, 5))
  } else if (area_type == 'cd') {
    .data <- .data %>% dplyr::mutate(state = stringr::str_sub({{ GEOID }}, 1, 2))
    .data <- .data %>% dplyr::mutate(cd = stringr::str_sub({{ GEOID }}, 3, 4))
  } else if (area_type == 'zcta') {
    .data <- .data %>% dplyr::mutate(zcta = {{ GEOID }})
  }

  .data
}



#' Create GEOID from Default Columns
#'
#' @param .data dataframe, tibble, or sf tibble
#' @param area_type Defaults to creating the smallest possible with 'spine'
#'  for states, counties, tracts, block groups, and blocks. You can also pass
#' one of the on spine geographies to create that specific level. Other options are 'shd' for lower
#' state legislative districts, 'ssd' for upper state legislative districts, 'cd'
#' for congressional districts, or 'zcta' for zip code tabulation areas.
#' @param state name of column with state component
#' @param county name of column with county component
#' @param tract name of column with tract component
#' @param block_group name of column with block group component
#' @param block name of column with block component
#' @param cd name of column with cd component
#' @param shd name of column with shd component
#' @param ssd name of column with ssd component
#' @param zcta name of column with zcta component
#'
#' @return .data with new column GEOID
#' @export
#' @concept geoid
#' @examples
#' data(mt_county)
#' mt_county <- mt_county %>% breakdown_geoid()
#' mt_county <- mt_county %>% dplyr::select(-dplyr::all_of('GEOID'))
#' mt_county <- mt_county %>% construct_geoid()
construct_geoid <- function(.data, area_type, state = 'state', county = 'county', tract = 'tract',
                            block_group = 'block group', block = 'block', cd = 'cd', shd = 'shd',
                            ssd = 'ssd', zcta = 'zcta') {

  if (missing(area_type) || area_type == 'spine') {
    if (all(c(block, tract, county, state) %in% names(.data))) {
      area_type <- 'block'
    } else if (all(c(block_group, tract, county, state) %in% names(.data))) {
      area_type <- 'block group'
    } else if (all(c(tract, county, state) %in% names(.data))) {
      area_type <- 'tract'
    } else if (all(c(county, state) %in% names(.data))) {
      area_type <- 'county'
    } else if (all(c(state) %in% names(.data))) {
      area_type <- 'state'
    } else {
      stop('No argument supplied to `area_type` but no on spine options fit.')
    }
  }

  state <- rlang::ensym(state)
  county <- rlang::ensym(county)
  tract <- rlang::ensym(tract)
  block_group <- rlang::ensym(block_group)
  block <- rlang::ensym(block)
  shd <- rlang::ensym(shd)
  ssd <- rlang::ensym(ssd)
  cd <- rlang::ensym(cd)
  zcta <- rlang::ensym(zcta)


  if (area_type == 'state') {
    .data <- .data %>%
      dplyr::mutate(GEOID = paste0({{ state }}))
  }

  if (area_type == 'county') {
    .data <- .data %>%
      dplyr::mutate(GEOID = paste0({{ state }}, {{ county }}))
  }

  if (area_type == 'tract') {
    .data <- .data %>%
      dplyr::mutate(GEOID = paste0({{ state }}, {{ county }}, {{tract}}))
  }

  if (area_type == 'block group') {
    .data <- .data %>%
      dplyr::mutate(GEOID = paste0({{ state }}, {{ county }}, {{tract}}, {{ block_group }}))
  }

  if (area_type == 'block') {
    .data <- .data %>%
      dplyr::mutate(GEOID = paste0({{ state }}, {{ county }}, {{tract}}, {{ block }}))
  }

  if (area_type == 'cd') {
    .data <- .data %>%
      dplyr::mutate(GEOID = paste0({{ state }}, {{ cd }}))
  }

  if (area_type == 'ssd') {
    .data <- .data %>%
      dplyr::mutate(GEOID = paste0({{ state }}, {{ ssd }}))
  }

  if (area_type == 'shd') {
    .data <- .data %>%
      dplyr::mutate(GEOID = paste0({{ state }}, {{ shd }}))
  }

  if (area_type == 'zcta') {
    .data <- .data %>%
      dplyr::mutate(GEOID = {{ zcta }})
  }

  .data
}

#' Create a GEOID from Columns
#'
#' @param .data dataframe, tibble, or sf tibble
#' @param ... columns of .data in the order you want to make the GEOID
#'
#' @return .data with new column GEOID
#' @export
#' @concept geoid
#' @examples
#' data(mt_county)
#' mt_county <- mt_county %>% custom_geoid(GEOID)
custom_geoid <- function(.data, ...) {
  .data %>% dplyr::mutate(GEOID = paste0(!!!rlang::enquos(...)))
}
