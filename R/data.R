#' Example tree census data
#'
#' Individual tree stem measurements from two ForestPlots.net plots
#' (one in Ghana, one in Peru), covering multiple census years. This
#' dataset is provided as an example for use with
#' \code{\link{mergefp}} and the \code{SummaryAGWP} family of
#' functions.
#'
#' @format A data frame with 8099 rows and 68 variables. Key variables
#'   include:
#'   \describe{
#'     \item{PlotID}{Unique numeric plot identifier.}
#'     \item{PlotViewID}{Unique numeric plot-view identifier.}
#'     \item{Plot.Code}{Character plot code (e.g. \code{"JEN-11"}).}
#'     \item{TreeID}{Unique numeric stem identifier.}
#'     \item{Census.No}{Census number (integer).}
#'     \item{Census.Date}{Decimal year of census.}
#'     \item{Family}{Accepted plant family name.}
#'     \item{Genus}{Accepted genus name.}
#'     \item{Species}{Accepted species name.}
#'     \item{AllometricRegionID}{Allometric region identifier, used to
#'       look up height-diameter parameters in
#'       \code{\link{WeibullHeightParameters}}.}
#'     \item{D1, D2, D3, D4}{Diameter measurements at successive
#'       quality-control steps (mm). \code{D4} is the recommended
#'       diameter for biomass calculations.}
#'     \item{Extra.D4}{Basal (extra) diameter measurement (mm), used
#'       by equations in \code{\link{extra_d4_funs}}.}
#'     \item{POM}{Point of measurement (mm above ground).}
#'     \item{F1}{Recruitment flag: \code{"n"} indicates a new recruit.}
#'     \item{F4}{Point-of-measurement change flag: \code{"6"} indicates
#'       a POM change.}
#'     \item{Height}{Tree height (m), where measured.}
#'   }
#' @source ForestPlots.net Advanced Search.
#'   \url{https://www.forestplots.net}
#' @seealso \code{\link{md}}, \code{\link{wd}}, \code{\link{mergefp}}
"trees"

#' Example plot metadata
#'
#' Plot-level metadata for two ForestPlots.net plots (Ghana and Peru),
#' downloaded from the ForestPlots.net Query Library. This dataset is
#' provided as an example for use with \code{\link{mergefp}}.
#'
#' @format A data frame with 2 rows and 25 variables:
#'   \describe{
#'     \item{PlotCode}{Character plot code (e.g. \code{"JEN-11"}).}
#'     \item{PlotID}{Unique numeric plot identifier.}
#'     \item{PlotViewID}{Unique numeric plot-view identifier.}
#'     \item{Country}{Country name.}
#'     \item{Continent}{Continent name.}
#'     \item{Altitude}{Altitude above sea level (m).}
#'     \item{LatitudeDecimal}{Latitude in decimal degrees.}
#'     \item{LongitudeDecimal}{Longitude in decimal degrees.}
#'     \item{PlotArea}{Plot area (ha).}
#'     \item{ClusterName}{Name of the plot cluster.}
#'     \item{ClusterID}{Numeric cluster identifier.}
#'     \item{ForestMoistureName}{Forest moisture class name
#'       (e.g. \code{"Moist"}).}
#'     \item{ForestMoistureID}{Numeric forest moisture class
#'       identifier.}
#'     \item{ForestEdaphicName}{Forest edaphic type name
#'       (e.g. \code{"Terra Firma"}).}
#'     \item{ForestEdaphicID}{Numeric forest edaphic type
#'       identifier.}
#'     \item{ForestElevationName}{Forest elevation class name
#'       (e.g. \code{"Lowland"}).}
#'     \item{ForestElevationID}{Numeric forest elevation class
#'       identifier.}
#'     \item{BiogeographicalRegionName}{Biogeographical region name.}
#'     \item{BiogeographicalRegionID}{Numeric biogeographical region
#'       identifier.}
#'     \item{RegionName}{Broad region name
#'       (e.g. \code{"Amazonia, W"}).}
#'     \item{AllometricRegionID}{Allometric region identifier, used to
#'       look up height-diameter parameters in
#'       \code{\link{WeibullHeightParameters}}.}
#'   }
#' @source ForestPlots.net Query Library > Basic plot information >
#'   Plot Information for R Package.
#'   \url{https://www.forestplots.net}
#' @seealso \code{\link{trees}}, \code{\link{wd}}, \code{\link{mergefp}}
"md"

#' Example wood density data
#'
#' Wood density values for individual tree stems from two
#' ForestPlots.net plots (Ghana and Peru), downloaded from the
#' ForestPlots.net Query Library. This dataset is provided as an
#' example for use with \code{\link{mergefp}}.
#'
#' @format A data frame with 1309 rows and 5 variables:
#'   \describe{
#'     \item{PlotID}{Unique numeric plot identifier.}
#'     \item{PlotCode}{Character plot code (e.g. \code{"CAP-09"}).}
#'     \item{PlotViewID}{Unique numeric plot-view identifier.}
#'     \item{TreeID}{Unique numeric stem identifier.}
#'     \item{WD}{Wood density (g/cm\eqn{^3}).}
#'   }
#' @source ForestPlots.net Query Library > Wood Density > Wood Density
#'   of Individual Trees. \url{https://www.forestplots.net}
#' @seealso \code{\link{trees}}, \code{\link{md}}, \code{\link{mergefp}}
"wd"

#' Regional weibull height-diameter parameters
#'
#' Regional height-diameter parameters for Weibull models from Feldpausch et al. 2012.
#'
#' @format A data frame with 9 rows and 4 variables:
#' \describe{
#'	\item{AllometricRegionID}{ID for allometric regions from Feldpausch et al. 2012.}
#'	\item{a_par}{a parameter for Weibull model}
#'	\item{b_par}{b parameter for Weibull model}
#'	\item{c_par}{c parameter for Weibull model}
#' }
#' @source Feldpausch TR, Banin L, Phillips OL, Baker TR, Lewis SL et al. 2011. Height-diameter allometry of tropical forest trees. Biogeosciences 8 (5):1081-1106. doi:10.5194/bg-8-1081-2011
"WeibullHeightParameters"
