#' Randomly remove some data
#'
#' There are 6 removal schemes based on whether the data should be missing
#' consecutively, only on one variable or on multiple variable, as well as whether
#' an observation can have non-missing variable.
#'
#' @param completed_data matrix (ts object), complete data with n rows
#'        (observation) and k columns (variables).
#' @param seed integer, unique number to control the random removal.
#' @param num_miss integer, number of data that will be removed
#' @param consecutive logical, should the data be removed consecutively?
#' @param first_var logical, should the data be removed on first variable only?
#' @param completely_miss logical, should the data be removed simultaneously
#'        (all variables in the same observation are missing)?
#' @param max_lag integer, maximum lag for VAR-IM.
#'
#' @export
#' @return A matrix with same dimension as the `completed_data`, but with
#'        `num_miss` missing values.
#'
#' @examples
#' # The dafault setting will remove `num_miss` observations from the first variable.
#' miss1 = omit(monthly_weather, seed = 1, num_miss = 10)
#' colSums(is.na(miss1)) # number of missing values of each variable
#'
#' # to remove values on all variables (instead of the first variable only), use
#' # `first_var = FALSE`.
#' miss2 = omit(monthly_weather, seed = 1, num_miss = 10, first_var = FALSE)
#' colSums(is.na(miss2)) # number of missing values of each variable
#'
#' # If `first_var = FALSE` and `completely_miss = TRUE`, all variables of the
#' # randomly chosen observations will have missing values.
#' miss3 = omit(monthly_weather, seed = 1, num_miss = 10, first_var = FALSE, completely_miss = TRUE)
#' colSums(is.na(miss3)) # number of missing values of each variable
#' miss_index = which(is.na(miss3), arr.ind = TRUE)
#' miss_index[order(miss_index[,"row"],decreasing=FALSE),] # index of the missing values
#'
omit <- function(completed_data, seed, num_miss, max_lag = 20,
                 consecutive = TRUE, first_var = TRUE, completely_miss = FALSE){
  # Error Handling -----------------------------------------------------------
  if (!is.ts(completed_data)) {
    stop("completed_data has to be a ts object")
  }

  if (!is.numeric(seed) & !is.integer(seed)) {
    stop("seed has to be integer or numeric.")
  }

  if (!is.numeric(num_miss) & !is.integer(num_miss)) {
    stop("num_miss has to be integer.")
  }

  if (num_miss <= 0 | num_miss >= nrow(completed_data)) {
    stop(paste0("num_miss has to be the interval of [1,",nrow(completed_data),")."))
  }

  if (!is.logical(consecutive)){
    stop("consecutive has to be TRUE or FALSE.")
  }

  if (!is.logical(first_var)){
    stop("first_var has to be TRUE or FALSE.")
  }


  if (!is.logical(completely_miss)){
    stop("completely_miss has to be TRUE or FALSE.")
  }

  # Data removal ----------------------------------------------------------
  y_miss <- completed_data
  n <- nrow(as.matrix(completed_data))
  k <- ncol(as.matrix(completed_data))

  # A few of the first observations will be excluded from random removal since
  # VAR needs at least as many as p observations to be modeled, where p is the
  # lag of VAR model.
  first_index <- max_lag + 1
  if (consecutive == TRUE){
    if (first_var == TRUE){
      set.seed(seed)
      first_miss_row <- sample(first_index:(n-num_miss), 1)
      y_miss[first_miss_row:(first_miss_row+num_miss-1),1] <- NA
      # completely_miss is not matter if first_var == TRUE
    } else if (first_var == FALSE){
      if (completely_miss == TRUE){
        set.seed(seed)
        first_miss_row <- sample(first_index:(n-num_miss), 1)
        y_miss[first_miss_row:(first_miss_row+num_miss-1),1:k] <- NA
      } else if (completely_miss == FALSE){
        set.seed(seed)
        # randomly assign number of missing observations for each variable
        n_miss_col <- vector(mode = "numeric", length = k)
        n_miss <- num_miss
        for (i in 1:(k - 1)) {
          miss <- sample(1:n_miss, 1)
          n_miss_col[i] <- miss
          n_miss <- n_miss - miss
        }
        n_miss_col[k] <- n_miss

        first_miss_row <- vector(mode = "numeric", length = k)
        for (i in 1:k) {
          first_miss_row[i] <- sample(first_index:(n - n_miss_col[i]), 1)
        }

        # removal
        for (i in 1:k) {
          row_index <- first_miss_row[i]:(first_miss_row[i] + n_miss_col[i] - 1)
          y_miss[row_index, i] <- NA
        }
      }
    }
  } else if (consecutive == FALSE){
    if (first_var == TRUE){
      set.seed(seed)
      miss_index <- sample(first_index:n, num_miss, replace = FALSE)
      y_miss[miss_index, 1] <- NA
      # completely_miss is not matter if first_var == TRUE
    } else if (first_var == FALSE){
      if (completely_miss == TRUE){
        set.seed(seed)
        miss_index <- sample(first_index:n, num_miss, replace = FALSE)
        y_miss[miss_index, 1:k] <- NA
      } else if (completely_miss == FALSE){
        index_matrix <-
          cbind(
            rep(first_index:n, k),
            rep(rep(1:k, each = n - first_index + 1)),
            1:((n - first_index + 1) * k)
          )
        colnames(index_matrix) <- c("baris", "kolom", "index")

        set.seed(seed)
        # randomly choosing the row and column
        miss_index <- sample(index_matrix[, "index"], num_miss, replace = FALSE)
        miss_matrix <- index_matrix[index_matrix[, "index"] %in% miss_index, ]

        # removal
        for (i in 1:nrow(miss_matrix)) {
          row_index <- miss_matrix[i, "baris"]
          col_index <- miss_matrix[i, "kolom"]
          y_miss[row_index, col_index] <- NA
        }
      }
    }
  }

  # Output ---------------------------------------------------------------------
  return(y_miss)
}
