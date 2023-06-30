#' ADF test for all variables
#'
#' ADF test for all variables on matrix with k variables.
#'
#' @param init_data matrix (ts object), the initially imputed data with n rows
#'        (observation) and k columns (variables). init_data cannot have
#'        missing values.
#' @param crit_value numeric, critical value for ADF test.
#'
#' @return Logical, TRUE if all variable are stationary, FALSE if there is at
#'         least one variable that is non-stationary.
#'
#' @export
#' @importFrom tseries adf.test
#'
#' @examples
#' is_all_stationary(daily_weather)
#' try(is_all_stationary(monthly_weather))
#' try(is_all_stationary(monthly_weather, crit_value = 0.01))

is_all_stationary <- function(init_data, crit_value = 0.05){
  # Error Handling -------------------------------------------------------------
  if (sum(is.na(init_data)>0)){
    stop("init_data cannot have missing values. Perform initial imputation first.")
  }

  if (crit_value <= 0 & crit_value >= 1){
    stop("crit_value has to be within the interval of (0,1).")
  }

  # ADF Test -------------------------------------------------------------------
  stationary <- c()
  for (i in 1:ncol(init_data)){
    p.value <- suppressWarnings(tseries::adf.test(init_data[,i])$p.value)
    if (p.value >= crit_value){ # accept H0
      stationary[i] <- FALSE
    } else {
      stationary[i] <- TRUE
    }
  }

  if (sum(stationary) == ncol(init_data)){
    return(TRUE)
  } else {
    warning(paste0(colnames(init_data)[which(!stationary)], " are not stationary.\n"))
    return(FALSE)
  }
}
