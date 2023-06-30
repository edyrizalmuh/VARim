#' Identifying outliers
#'
#' This function identifies outliers on each variable using forecast::outliers.
#'
#' @param init_data matrix (ts object), the initially imputed data with n rows
#'        (observation) and k columns (variables). init_data cannot have
#'        missing values.
#' @param sig_level numeric, significant level for Chi-Square test.
#'
#' @return A matrix (ts object), containing indexes of the outliers from each
#'         variable. 1 means outlier while 0 means not outlier.
#' @export
#' @import forecast
#' @importFrom tibble tibble
#'
#' @examples
#' dum = dummy_outliers(monthly_weather)
#' sum(dum) # 16 observations are considered outliers in `monthly_weather` data
#'
dummy_outliers <- function(init_data, sig_level = 0.05){
  dummy_table =
    tibble::tibble(
      mahalanobis_dist =
        mahalanobis(
          init_data,
          center = colMeans(init_data, na.rm = TRUE),
          cov = cov(init_data, use = "complete.obs"),
          tol=1e-20
        ),
      pvalue = pchisq(mahalanobis_dist, df=ncol(init_data)-1, lower.tail=FALSE),
      dummy = ifelse(pvalue <= sig_level, 1, 0)
    )
  return(dummy_table$dummy)
}
