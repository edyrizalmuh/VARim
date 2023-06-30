#' Initial imputation
#'
#' VAR cannot be directly modeled on data with missing values, therefore, an
#' initial imputation is needed as the temporary data. These temporary data will
#' then be optimized using VAR-IM.
#'
#' @param miss_data matrix (ts object), missing data with n rows (observation) and k
#'        columns (variables).
#' @param init_method character, initial imputation method (univariate) that will
#'        be used. The univariate imputation methods are based on imputeTS package,
#'        and must be one of the "na_interpolation", "na_kalman", "na_locf",
#'        "na_ma", "na_mean", "na_replace", "na_seadec".
#'
#' @return A matrix with the same dimension as the `miss_data`, but with
#'        temporarily imputed values.
#' @export
#' @import stats
#' @importFrom imputeTS na_interpolation
#' @importFrom imputeTS na_kalman
#' @importFrom imputeTS na_locf
#' @importFrom imputeTS na_ma
#' @importFrom imputeTS na_mean
#' @importFrom imputeTS na_replace
#' @importFrom imputeTS na_seadec
#'
#' @examples
#' # simultaneously imputing missing values on multiple variables
#' miss_mult = omit(monthly_weather, seed = 1, num_miss = 10, first_var = FALSE)
#' colSums(is.na(miss_mult)) # number of missing values
#' imp_mult = init_imp(miss_mult)
#' colSums(is.na(imp_mult)) # number of missing values after initial imputation
#'
init_imp <- function(miss_data, init_method = "na_interpolation"){
    # Error Handling -----------------------------------------------------------
    if (!is.ts(miss_data)) {
      stop("miss_data has to be a ts object")
    }

    if (init_method == "na_interpolation"){
      method = imputeTS::na_interpolation
    } else if (init_method == "na_kalman"){
      method = imputeTS::na_kalman
    } else if (init_method == "na_locf"){
      method = imputeTS::na_locf
    } else if (init_method == "na_ma"){
      method = imputeTS::na_ma
    } else if (init_method == "na_mean"){
      method = imputeTS::na_mean
    } else if (init_method == "na_replace"){
      method = imputeTS::na_replace
    } else if (init_method == "na_seadec"){
      method = imputeTS::na_seadec
    } else {
      stop("init_method has to be one of the na_interpolation, na_kalman, na_locf, na_ma, na_mean, na_replace, na_seadec.")
    }

  # Imputasi awal ------------------------------------------------------------
  k = ncol(as.matrix(miss_data))
  y_init <- miss_data
  for (i in 1:k){
    y_init[,i] <- method(y_init[,i])
  }
  return(y_init)
}
