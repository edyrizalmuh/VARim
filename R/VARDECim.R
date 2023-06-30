#' VARDEC-IM
#'
#' This function performs an imputation using Vector Autoregression Decomposition
#' Imputation Method (VARDEC-IM), a variation of VAR-IM which uses STL decomposition.
#' This function is more suitable for multivariate time series data with seasonality.
#'
#' @param miss_data matrix (ts object), missing data with n rows (observation)
#'        and k columns (variables).
#' @param freq integer, frequency of the seasonal component. How many lags does
#'        it ta for an entire cycle?
#' @param ic character, information criteria for model selection. The value has
#'        to be either "AIC", "HQ", or "SC".
#' @param min_norm numeric, threshold for the iteration of VAR modeling. The
#'        smaller the min_norm is, the longer the imputation takes to complete.
#' @param max_iter integer, number of maximum iteration if min_norm is not met.
#' @param max_lag integer, maximum lag of VAR to be considered.
#' @param p integer, lag of the VAR model if needed to be set manually.
#' @param init_method character, initial imputation method (univariate) that will
#'        be used. The univariate imputation methods are based on imputeTS package,
#'        and must be one of the "na_interpolation", "na_kalman", "na_locf",
#'        "na_ma", "na_mean", "na_replace", "na_seadec".
#' @param dummy matrix, exogen variables, varibales that are not included in the
#'        predictive model but can be used to improve the overall model. The
#'        matrix has to be in m x l dimension, where m is the number of
#'        observation and l is the number of the dummy variables.
#' @param outliers logical, if the logical == TRUE, dummy_outliers function will
#'        identify outlier on each variable.
#'
#' @return
#' A list with 4 objects:
#' 1. `imputed_data`: matrix/ts object of the final imputed data,
#' 1. `convergency`: matrix that details the norm/convergency of each iteration,
#' 1. `convergent_model`: VAR model of the convergent/last iteration, and
#' 1. `coefficient`: coefficient matrix of the last iteration
#'
#' @export
#' @import stats
#' @importFrom utils head
#' @importFrom vars VARselect
#' @importFrom tsDyn lineVar
#'
#' @examples
#' miss_data = omit(daily_weather, seed = 1, num_miss = 100)
#' colSums(is.na(miss_data))
#' # For data with 100 missing values, the most basic imputation that can be used are:
#' imp_data = VARDECim(miss_data, freq = 365)
#'
#' # Since VARDECim is intended to be used for seasonal data, seasonal frequency
#' # `freq` has to be known before imputation.
#'
#' # For better imputation results, a related dummy variable(s) may be used.
#' # The following examples are just illustration and clearly are not related to
#' # the data at all.
#'
#' # `dummy` properties can use a vector or a matrix with k columns (k variables).
#' num_row = nrow(daily_weather)
#' dummy_mat = rnorm(num_row,0,1)
#' imp_data_OneDummy = VARDECim(miss_data, freq = 365, dummy = dummy_mat)
#'
#' # For dummy with multiple variables:
#' dummy_mats = cbind(rnorm(num_row,0,1), rnorm(num_row,1,4))
#' imp_data_MultipleDummy = VARDECim(miss_data, freq = 365, dummy = dummy_mats)
#'
VARDECim <- function(miss_data, freq, ic = "AIC", min_norm = 0.05, max_iter = 10,
                     max_lag = 30, p = NULL, init_method = "na_interpolation",
                     dummy = NULL, outliers = FALSE){

  # Error Handling ----------------------------------------------------------

  if (!is.ts(miss_data)){
    stop("miss_data has to be a ts object.")
  }

  if (!ic %in% c("AIC", "HQ", "SC")){
    stop("ic has to be either AIC, HQ, or SC.")
  }

  if (!is.numeric(min_norm)) {
    stop("min_norm has to be numeric.")
  }

  if (max_iter < 1) {
    stop("max_iter has to be greater than or equal to 1.")
  }

  if (max_lag < 1){
    stop("max_lag has to be greater than or equal to 1.")
  }

  if (!is.numeric(p) & !is.null(p)) {
    stop("p has to be NULL or numeric.")
  }

  if (!init_method %in% c("na_interpolation", "na_kalman", "na_locf", "na_ma", "na_mean", "na_replace", "na_seadec")){
    stop("init_method has to be one of the na_interpolation, na_kalman, na_locf, na_ma, na_mean, na_replace, na_seadec.")
  }

  if (!typeof(dummy) %in% c("double", "NULL")){
    stop("dummy has to be numeric or NULL.")
  }

  # Output ---------------------------------------------------------------------
  res = list(
    imputed_data = matrix(),
    convergency = matrix(),
    convergent_model = list(),
    coefficient = matrix()
  )

  # Data Preparation -----------------------------------------------------------
  miss_index = which(is.na(miss_data), arr.ind = TRUE)
  y_init = init_imp(miss_data, init_method = init_method)
  dec = decomp(y_init, init_method = init_method, freq = freq)
  y_init = dec$no_season
  # season = dec$season

  lag_var =
    suppressWarnings(
      vars::VARselect(
        y = y_init,
        exogen = dummy,
        lag.max = max_lag,
      )$selection[paste0(ic,"(n)")]
    )
  all_stat = is_all_stationary(y_init)
  coef_norm = Inf
  coef_prev = 0
  iter = 0
  convergency_detail = matrix(ncol = 2, nrow = 0)
  colnames(convergency_detail) = c("iteration", "frobenius_norm")
  # Imputation -----------------------------------------------------------------
  if (all_stat == FALSE){
    warning(paste0("There are non-stationary variable(s).\n
          Use VARDim or VARDECim.\n
          VAR-IM failed, return imputation result using init_method (", init_method,")."))
  } else if (all_stat == TRUE){
    ## VAR ---------------------------------------------------------------------
    while ((coef_norm > min_norm) & (iter < max_iter)) {
      iter = iter + 1
      if (outliers == TRUE){
        do = dummy_outliers(y_init)
        if (iter == 1){
          dummy = cbind(dummy, do)
        } else {
          dummy[,do] = do
        }
      }

      model =
        invisible(
          tsDyn::lineVar(
            y_init,
            lag = lag_var,
            model = "VAR",
            I = "level",
            estim = "ML",
            exogen = dummy
          )
        )

      # stop condition: min_norm -----------------------------------------------
      koef = t(coef(model))
      if (identical(dim(koef), dim(coef_prev))){
        coef_norm = norm(koef - coef_prev, "F")
      } else {
        coef_norm = Inf
      }
      coef_prev <- koef
      convergency_detail <- rbind(convergency_detail, c(iter, coef_norm))

      # Update -----------------------------------------------------------------
      # use the trained model to predict the next missing values (sorted by row)
      miss_index_sort = miss_index[order(miss_index[,"row"],decreasing=FALSE),]
      temp_y_init = y_init
      for (mi in 1:nrow(miss_index_sort)){
        row = miss_index_sort[mi,"row"]
        col = miss_index_sort[mi,"col"]
        if (!is.null(dummy)){
          if (is.matrix(dummy)){
            temp_y_init[[row, col]] =
              predict(
                model,
                newdata = matrix(y_init[(row-model$lag):(row-1),], ncol = ncol(y_init)),
                n.ahead = 1,
                exoPred = t(dummy[row,])
              )[col]
          } else if (!is.matrix(dummy)){
            temp_y_init[[row, col]] =
              predict(
                model,
                newdata = matrix(y_init[(row-model$lag):(row-1),], ncol = ncol(y_init)),
                n.ahead = 1,
                exoPred = dummy[row]
              )[col]
          }
        } else if (is.null(dummy)){
          temp_y_init[[row, col]] =
            predict(
              model,
              newdata = matrix(y_init[(row-model$lag):(row-1),], ncol = ncol(y_init)),
              n.ahead = 1
            )[col]
        }
      }

      y_init = temp_y_init # update the imputed data using the new forecast results
    }
    y_init = y_init + dec$season

    # Update Output --------------------------------------------------------------
    res$imputed_data = y_init
    res$convergency = convergency_detail
    res$convergent_model = model
    res$coefficient = t(model$coefficients)
    return(res)
  }
}
