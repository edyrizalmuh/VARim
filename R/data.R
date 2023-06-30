#' Daily Weather Data
#'
#' Daily weather data collected by BMKG in Citeko Meteorological Station, Bogor
#' Regency, Indonesia. The data collected are from Januari 1991 to December 2002.
#'
#' @format matrix (ts object) with 4383 rows and 6 columns.
#' \describe{
#'   \item{min_temp}{Minimum temperature of the day (celcius)}
#'   \item{max_temp}{Maximum temperature of the day (celcius)}
#'   \item{avg_temp}{Average temperature of the day (celcius)}
#'   \item{avg_humidity}{Average humidity of the day (%)}
#'   \item{avg_rainfall}{Average rainfall of the day (mm)}
#'   \item{sunshine}{Duration of sunshine of the day (hour)}
#' }
#' @source <https://dataonline.bmkg.go.id/>
"daily_weather"

#' Monthly Weather Data
#'
#' Monthly weather data collected by BMKG in Citeko Meteorological Station, Bogor
#' Regency, Indonesia. The data collected are from Januari 1991 to December 2002.
#' This monthly data was calculated as the average of "daily_weather" data within
#' a month.
#'
#' @format matrix (ts object) with 144 rows and 6 columns.
#' \describe{
#'   \item{min_temp}{Minimum temperature of the day (celcius)}
#'   \item{max_temp}{Maximum temperature of the day (celcius)}
#'   \item{avg_temp}{Average temperature of the day (celcius)}
#'   \item{avg_humidity}{Average humidity of the day (%)}
#'   \item{avg_rainfall}{Average rainfall of the day (mm)}
#'   \item{sunshine}{Duration of sunshine of the day (hour)}
#' }
#' @source <https://dataonline.bmkg.go.id/>
"monthly_weather"
