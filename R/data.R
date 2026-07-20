#' Example tree census data
#'
#' Individual tree stem measurements from two ForestPlots.net plots
#' (one in Ghana, one in Peru), covering multiple census years. This
#' dataset is provided as an example for use with
#' \code{\link{mergefp}} and the \code{SummaryAGWP} family of
#' functions.
#'
#' @format A data frame with 8099 rows and 68 variables:
#'   \describe{
#'     \item{Continent}{Continent name.}
#'     \item{Country}{Country name.}
#'     \item{PlotID}{Unique numeric plot identifier.}
#'     \item{Plot.Code}{Character plot code
#'       (e.g. \code{"JEN-11"}).}
#'     \item{PlotViewID}{Unique numeric plot-view identifier.}
#'     \item{Plot.Name}{Full descriptive name of the plot.}
#'     \item{Plot.View.Name}{Name of the plot view
#'       (e.g. \code{"Main Plot View"}).}
#'     \item{PI}{Principal investigator(s) associated with the
#'       plot, semicolon-separated.}
#'     \item{TreeID}{Unique numeric stem identifier.}
#'     \item{Tag.No}{Field tag number for the stem.}
#'     \item{Sub.Plot.T1}{Sub-plot identifier at the first
#'       hierarchical level.}
#'     \item{Sub.Plot.T2}{Sub-plot identifier at the second
#'       hierarchical level (often \code{NA}).}
#'     \item{Standardised.SubPlot.T1}{Standardised sub-plot
#'       identifier.}
#'     \item{X}{X-coordinate of the stem within the plot (m).}
#'     \item{Y}{Y-coordinate of the stem within the plot (m).}
#'     \item{Standardised.X}{Standardised X-coordinate within
#'       the plot (m).}
#'     \item{Standardised.Y}{Standardised Y-coordinate within
#'       the plot (m).}
#'     \item{AllometricRegionID}{Allometric region identifier,
#'       used to look up height-diameter parameters in
#'       \code{\link{WeibullHeightParameters}}.}
#'     \item{Family}{Accepted plant family name.}
#'     \item{Genus}{Accepted genus name.}
#'     \item{Species}{Accepted binomial species name.}
#'     \item{Sub.Species}{Sub-species name (empty string if
#'       none).}
#'     \item{Variety}{Variety name (usually \code{NA}).}
#'     \item{Original.Identification}{Original field
#'       identification as recorded by the collector.}
#'     \item{Recommended.Family}{Recommended accepted family
#'       name from the ForestPlots.net taxonomy.}
#'     \item{Recommended.Species}{Recommended accepted species
#'       name from the ForestPlots.net taxonomy.}
#'     \item{Recommended.Subspecies}{Recommended accepted
#'       sub-species name (empty string if none).}
#'     \item{Recommended.Variety}{Recommended variety name
#'       (usually \code{NA}).}
#'     \item{FamilyAPGID}{Numeric family identifier in the APG
#'       classification system.}
#'     \item{GenusID}{Numeric genus identifier.}
#'     \item{SpeciesID}{Numeric species identifier.}
#'     \item{SubSpeciesID}{Numeric sub-species identifier
#'       (usually \code{NA}).}
#'     \item{Census.No}{Census number (integer), starting at 1
#'       for the first census of each plot.}
#'     \item{Census.Date}{Decimal year of the census.}
#'     \item{Voucher.Code}{Herbarium voucher code(s) associated
#'       with the stem.}
#'     \item{Stem.Group.ID}{Group identifier linking stems of a
#'       multi-stemmed tree (usually \code{NA}).}
#'     \item{Main.Stem.Tag}{Tag number of the main stem in a
#'       multi-stemmed tree (usually \code{NA}).}
#'     \item{D0}{Raw diameter as entered in the field (mm).}
#'     \item{D1}{Diameter after the first quality-control step
#'       (mm).}
#'     \item{DPOMtMinus1}{Diameter measured at the point of
#'       measurement used in the previous census (mm).}
#'     \item{D2}{Diameter after the second quality-control step
#'       (mm).}
#'     \item{D3}{Diameter after the third quality-control step
#'       (mm).}
#'     \item{D4}{Diameter after the fourth quality-control step
#'       (mm); recommended diameter for biomass calculations.}
#'     \item{POM0}{Original point of measurement as entered in
#'       the field (mm above ground).}
#'     \item{POM}{Point of measurement used for this census (mm
#'       above ground).}
#'     \item{F1}{Stem status flag. \code{"a"}: alive survivor;
#'       \code{"n"}: new recruit (first record); \code{"p"}:
#'       prior recruit.}
#'     \item{F2}{Measurement quality flag.}
#'     \item{F3}{Diameter quality-control flag (integer code;
#'       0 = no issue).}
#'     \item{F4}{Point-of-measurement change flag (integer code;
#'       6 = POM changed since previous census).}
#'     \item{Extra.D0}{Extra (basal) raw diameter as entered in
#'       the field (mm); used for Cerrado-type vegetation.}
#'     \item{Extra.D}{Extra (basal) diameter (mm).}
#'     \item{Extra.DPOMtMinus1}{Extra diameter at the previous
#'       census point of measurement (mm).}
#'     \item{Extra.D2}{Extra diameter after the second
#'       quality-control step (mm).}
#'     \item{Extra.D3}{Extra diameter after the third
#'       quality-control step (mm).}
#'     \item{Extra.D4}{Extra (basal) diameter after the fourth
#'       quality-control step (mm); used by allometric equations
#'       in \code{\link{extra_d4_funs}}.}
#'     \item{Extra.POM0}{Original extra point of measurement
#'       (mm above ground).}
#'     \item{Extra.POM}{Extra point of measurement for this
#'       census (mm above ground).}
#'     \item{Extra.F3}{Diameter quality-control flag for the
#'       extra measurement.}
#'     \item{Extra.F4}{Point-of-measurement change flag for the
#'       extra measurement.}
#'     \item{CI}{Census interval flag (character).}
#'     \item{LI}{Light index recorded at the stem.}
#'     \item{CF}{Crown flag.}
#'     \item{CD1}{Crown diameter measurement 1.}
#'     \item{CD2}{Crown diameter measurement 2.}
#'     \item{Height}{Tree height (m), where measured.}
#'     \item{F5}{Height measurement quality flag.}
#'     \item{Height.Broken.At}{Height at which a broken or
#'       leaning tree was measured (m).}
#'     \item{Comments}{Free-text comments associated with the
#'       stem record.}
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
#'     \item{ForestEdaphicHeightName}{Forest edaphic type name
#'       used for height-diameter allometry
#'       (e.g. \code{"Terra Firma"}).}
#'     \item{ForestEdaphicHeightID}{Numeric forest edaphic type
#'       identifier used for height-diameter allometry.}
#'     \item{ForestElevationName}{Forest elevation class name
#'       (e.g. \code{"Lowland"}).}
#'     \item{ForestElevationID}{Numeric forest elevation class
#'       identifier.}
#'     \item{ForestElevationHeightName}{Forest elevation class
#'       name used for height-diameter allometry
#'       (e.g. \code{"Lowland"}).}
#'     \item{ForestElevationHeightID}{Numeric forest elevation
#'       class identifier used for height-diameter allometry.}
#'     \item{BiogeographicalRegionName}{Biogeographical region
#'       name (e.g. \code{"Western Amazon"}).}
#'     \item{BiogeographicalRegionID}{Numeric biogeographical
#'       region identifier.}
#'     \item{RegionName}{Broad region name
#'       (e.g. \code{"Amazonia, W"}).}
#'     \item{AllometricRegionID}{Allometric region identifier,
#'       used to look up height-diameter parameters in
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
