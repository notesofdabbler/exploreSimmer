
# taken from 
# https://github.com/r-simmer/simmer.plot/blob/master/R/plot.resources.R
#

get_utilization_df = function(x) {
  x <- x %>%
    dplyr::group_by(.data$resource, .data$replication) %>%
    dplyr::mutate(dt = .data$time - dplyr::lag(.data$time)) %>%
    dplyr::mutate(in_use = .data$dt * dplyr::lag(.data$server / .data$capacity)) %>%
    dplyr::summarise(utilization = sum(.data$in_use, na.rm = TRUE) / sum(.data$dt, na.rm=TRUE)) %>%
    dplyr::summarise(Q25 = stats::quantile(.data$utilization, .25),
                     Q50 = stats::quantile(.data$utilization, .5),
                     Q75 = stats::quantile(.data$utilization, .75))
  return(x)
}

get_avgQlength_df = function(x) {
  x <- x %>%
    dplyr::group_by(.data$resource, .data$replication) %>%
    dplyr::mutate(dt = .data$time - dplyr::lag(.data$time)) %>%
    dplyr::mutate(in_queue = .data$dt * dplyr::lag(.data$queue)) %>%
    dplyr::summarise(avg_Qlength = sum(.data$in_queue, na.rm = TRUE) / sum(.data$dt, na.rm=TRUE)) %>%
    dplyr::summarise(Q25 = stats::quantile(.data$avg_Qlength, .25),
                     Q50 = stats::quantile(.data$avg_Qlength, .5),
                     Q75 = stats::quantile(.data$avg_Qlength, .75))
  return(x)
}