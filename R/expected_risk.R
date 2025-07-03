#' Expected Risk of an Event of Interest
#'
#' Calculates the probability of surviving for each person i and time interval u.
#' @param data data.frame of predicted value of the estimated probability of the outcome for each person-period in the discrete time hazard table
#' @param hazards list of column names of the predicted hazard outcomes
#' @param id_in String column name of the identification variable. Defaults to "id"
#' @param prefix String of the predicted hazard outcomes column name prefix. Defaults to ""
#' @returns expected events: a data.frame of rt_x ... the number of predicted events corresponding to the specified hazards,
#' 'Estimated Person Years' total number of person years in the input data
#' @returns all data: a data.frame of cumulative probability of the outcome for each person-period in the discrete time hazard table
#' @export
#' @examples
#' expected_risks()
#' expected_risks(hazards_example, hazards = c("h1", "h2","h3","h4","h5","h6","h7"),
#' prefix = "h", id_in = "id")


# Function assumes the input data is organized by ascending year (2015, 2016, 2017 ...)

expected_risks <- function(data, hazards, id_in = "id", prefix = ""){
  du_helper <- function(h,d){h-d*(h-1)}
  data_hazards <- data %>%
    dplyr::group_by(!!dplyr::sym(id_in)) %>% ## group by worker id
    dplyr::mutate(h_pooled = rowSums(dplyr::across(dplyr::all_of(hazards))), # pooled hazards
           d_pooled = purrr::accumulate(h_pooled, du_helper), # overall d_u
           s_pooled = 1 - dplyr::lag(d_pooled, default = 0), # overall S_u probability of survival
           dplyr::across(dplyr::all_of(hazards),
                  .names = "D_u_{sub(prefix,'',.col)}",
                  function(x){cumsum(s_pooled*x)}), ## condition specific D_u, the probability an event occurs, given that you have survived to time t
           S_u = 1- rowSums(dplyr::across(dplyr::starts_with("D_u_"))), ## Overall survival function, the sum of the cumulative hazards (D_u) for the exhaustive set of competing events
           dplyr::across(dplyr::all_of(hazards),
                  .names = "rt_{sub(prefix,'',.col)}",
                  function(x){x *dplyr::lag(S_u, default = 1)})) ## Cumulative risk for each event of interest

  summary_hazards <- data_hazards %>%
    dplyr::ungroup() %>%
    dplyr::summarize( dplyr::across(dplyr::all_of(dplyr::starts_with(c("rt_","s_pooled"))), sum)) %>%
    dplyr::rename("Estimated Person Years" = s_pooled)

  return(list("expected events" = summary_hazards,
              "all data" = data_hazards))

}
