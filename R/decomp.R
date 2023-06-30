#' Data decomposition
#'
#' This function performs simultaneous STL decomposition on each variable.
#'
#' @param miss_data matrix (ts object), missing data with n rows (observation) and k
#'        columns (variables).
#' @param init_method character, initial imputation method (univariate) that will
#'        be used. The univariate imputation methods are based on imputeTS package,
#'        and must be one of the "na_interpolation", "na_kalman", "na_locf",
#'        "na_ma", "na_mean", "na_replace", "na_seadec".
#' @param freq integer, the seasonal frequency of the data. If freq is set to NULL,
#'        the frequency will be estimated.
#' @param ... varargs, arguments of ts() function
#'
#' @return A list of two matrices, each with n rows and k columns.
#'          `no_season` matrix is the time series matrix with removed seasonal
#'         components, while `season` matrix is the time series matrix with
#'         seasonal component.
#' @export
#' @import stats
#'
#' @examples
#' # `decomp` can be used for completed data
#' decomp_comp = decomp(monthly_weather, freq = 12)
#'
#' # or incomplete data. The missing values will be estimated using the specified
#' # univariate imputation method.
#' miss_data = omit(monthly_weather, seed = 1, num_miss = 15)
#' colSums(is.na(miss_data))
#' decomp_incomp = decomp(monthly_weather, freq = 12, init_method = "na_ma")
#' colSums(is.na(decomp_incomp$no_season))
#' colSums(is.na(decomp_incomp$season))
#'
decomp <- function(miss_data, init_method = "na_interpolation", freq = NULL, ...){
  # Error Handling -------------------------------------------------------------
  if (ncol(miss_data) == 1){
    stop("Please input a multivariate time series matrix with n rows and k columns. k > 1")
  }

  if (is.null(freq)){
    stop("Please input the seasonal frequency of the data.")
  } else if (!is.null(freq) && freq < 1){
    stop("freq have to be greater than or equal to 1.")
  }

  # Decomposition ----------------------------------------------------------------
  y_init <- init_imp(miss_data, init_method = init_method)
  y_season <- matrix(nrow = nrow(y_init), ncol = ncol(y_init))
  y_no_season <- matrix(nrow = nrow(y_init), ncol = ncol(y_init))
  for (i in 1:ncol(y_init)){
    dec = stl(y_init[,i], s.window = freq, robust = TRUE)
    y_season[,i] <- dec$time.series[,"seasonal"]
    y_no_season[,i] <- dec$time.series[,"trend"] + dec$time.series[,"remainder"]
  }
  colnames(y_no_season) <- colnames(y_init)
  colnames(y_season) <- colnames(y_init)

  y_no_season <- ts(y_no_season, frequency = freq, ...)
  y_season <- ts(y_season, frequency = freq, ...)

  return(
    list(
      no_season = y_no_season,
      season = y_season
    )
  )
}
