#' Discrete Time Hazard Table
#'
#' Formats cohort data into discrete time hazard data structures, to a specified time-point.
#' @param data input cohort (wide format)
#' @param exp_prefix exposure variable prefix
#' @param event_dates year of event variable name (s)
#' @param dlo_max year to extend cohort. Defaults to "none"
#' @keywords table
#' @import powerjoin
#' @import tidyr
#' @details
#' the data data.frame must contain the following variables:
#' @details
#' - id,
#' @details
#' - start_fu (start of follow-up, date)
#' @details
#' - dlo (date of last observation, date)
#' @details
#' - age_sfu (age at start of follow-up, numeric)
#' @details
#' - covariate_constant (any name, any format)
#'
#' @export
#' @examples
#' dth_table()
#' dth_table(table_example, exp_prefix = 'acm_hours', event_dates = c("meso"), dlo_max = 2022)




dth_table <- function(data, exp_prefix, event_dates, dlo_max = "none"){
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(event_dates), ~ ifelse(.x < start_fu | .x > dlo , NA,year(.x))),
           start_fu = ifelse(is.Date(start_fu),lubridate::year(start_fu), start_fu),
           dlo = ifelse(is.Date(dlo), lubridate::year(dlo), dlo),
           age_sfu = round(age_sfu),
           dplyr::across(dplyr::any_of(event_dates), ~ ifelse(is.na(.x), 0,.x)))


  constant_data <- data %>%
    dplyr::select(-dplyr::starts_with(exp_prefix)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(year = list(seq(start_fu, ifelse(dlo_max=="none",dlo,dlo_max))),
           age = list(seq(age_sfu, length.out =length(year))),
           year_fu = list(seq(0, length.out =length(year)))) %>%
    dplyr::unnest(cols = c(year, age, year_fu)) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(event_dates),
                  .fns = list(binary = ~dplyr::if_else(.x == year, 1, 0))),
                  dplyr::across(dplyr::any_of(paste0(event_dates, "_binary")),
                  .fns = ~dplyr::if_else(dlo >= year, .x, NA)))

  var_data <-   data %>%
    dplyr::select(id, dplyr::starts_with(exp_prefix)) %>%
    dplyr::pivot_longer(
      cols = dplyr::starts_with(exp_prefix),
      names_to = "year",
      names_prefix = exp_prefix,
      values_to = "annual_exposure") %>%
    dplyr::mutate(year = as.numeric(year))

  out_data <- constant_data %>%
    powerjoin::power_left_join(var_data, by = c("id", "year"), fill =0) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(cumulative_exposure = cumsum(dplyr::replace_na(annual_exposure,0))) %>%
    dplyr::relocate(year, age,year_fu, .after = id) %>%
    dplyr::relocate(dplyr::any_of(paste0(event_dates, "_binary")), .after = cumulative_exposure) %>%
    dplyr::relocate(dplyr::any_of(event_dates), .after = dplyr::last_col())

  return(out_data)
}
