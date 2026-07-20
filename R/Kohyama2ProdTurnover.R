#' Calculate instantaneous biomass productivity and stem turnover rates
#'
#' Helper function that estimates biomass productivity, mortality, and
#' stem turnover rates for a population or species subgroup between two
#' censuses. Called internally by \code{\link{SummaryAGWP_Kohyama2}}.
#' Adapted from Kohyama et al. (2019); see
#' \url{https://github.com/kohyamat/p-B}.
#'
#' @param dbh1 Numeric vector. Tree diameter at first census (mm).
#' @param dbh2 Numeric vector. Tree diameter at second census (mm).
#' @param w1 Numeric vector. Individual tree biomass at first census
#'   (Mg).
#' @param w2 Numeric vector. Individual tree biomass at second census
#'   (Mg).
#' @param t Numeric. Census interval (years).
#' @param area Numeric. Plot area (ha).
#' @param Alive Integer vector. 1 if the stem was alive at the second
#'   census, 0 if dead.
#' @param Recruit Integer vector. 1 if the stem is a recruit (first
#'   appeared in the second census), 0 otherwise.
#'
#' @return A named numeric vector with nine elements:
#'   \describe{
#'     \item{B}{Period-mean stand biomass (Mg/ha).}
#'     \item{N}{Period-mean stem abundance (stems/ha).}
#'     \item{W_max}{99th percentile individual tree biomass for the
#'       initial population (Mg).}
#'     \item{p}{Instantaneous biomass growth rate (yr\eqn{^{-1}}).}
#'     \item{l}{Instantaneous biomass loss rate (yr\eqn{^{-1}}).}
#'     \item{r}{Instantaneous stem recruitment rate (yr\eqn{^{-1}}).}
#'     \item{m}{Instantaneous stem mortality rate (yr\eqn{^{-1}}).}
#'     \item{P}{Absolute woody productivity (\eqn{B \times p};
#'       Mg/ha/yr).}
#'     \item{P_simple}{Simple net productivity estimate
#'       (Mg/ha/yr).}
#'   }
#'
#' @references
#' Kohyama et al. 2019. Estimating net biomass production and loss from
#' repeated measurements of trees in forests and woodlands: Formulae,
#' biases and recommendations. \emph{Forest Ecology and Management}
#' 433: 729-740.
#'
#' @author T.S. Kohyama et al.
#' @seealso \code{\link{turnover_est}}, \code{\link{SummaryAGWP_Kohyama2}}
#' @export
productivity <- function(dbh1, dbh2, w1, w2, t, area, Alive,Recruit) {
  si <- ifelse(Alive==1 & Recruit==0,1,0) # survival
  di <- ifelse(Alive==0,1,0) # death
  ri <- Recruit # recruitment
  Ns0 <- sum(si, na.rm = TRUE)
  N0 <- Ns0 + sum(di, na.rm = TRUE)
  NT <- Ns0 + sum(ri, na.rm = TRUE)
  Bs0 <- sum(si * w1, na.rm = TRUE)
  BsT <- sum(si * w2, na.rm = TRUE)
  B0 <- Bs0 + sum(di * w1, na.rm = TRUE)
  BT <- BsT + sum(ri * w2, na.rm = TRUE)
  # period-mean biomass and abundance
  Nw <- ifelse(NT != N0, (NT - N0) / log(NT / N0), N0)
  N <- Nw / area # (per ha)
  Bw <- ifelse(BT != B0, (BT - B0) / log(BT / B0), B0)
  B <- Bw / area

  # Standardized maximum tree mass for initial population
  W_max <- as.numeric(quantile(w1[ri != 1], 0.99)) # Mg

  # turnover rates
  r <- try(turnover_est(si + ri, si, t),silent=TRUE)
  if(inherits(r,"try-error")){
    r<-NA
  }
  m <- try(turnover_est(si + di, si, t),silent=TRUE)
  if(inherits(m,"try-error")){
    m<-NA
  }
  p <- try(turnover_est(w2, si * w1, t),silent=TRUE)
  if(inherits(p,"try-error")){
    p<-NA
  }
  l <- try(turnover_est(w1, si * w1, t),silent=TRUE)
  if(inherits(l,"try-error")){
    l<-NA
  }

  # absolute productivity (Mg per ha per year)
  P <- p * B
  P_simple <- sum(((si + ri) * w2 - si * w1) / t)
  P_simple <- P_simple / area

  return(c(
    "B" = B,
    "N" = N,
    "W_max" = W_max,
    "p" = p,
    "l" = l,
    "r" = r,
    "m" = m,
    "P" = P,
    "P_simple" = P_simple
  ))
}

#' Estimate instantaneous turnover rate via Newton-Raphson iteration
#'
#' Solves for the instantaneous turnover rate \eqn{\rho} in the
#' exponential decay model \eqn{y \cdot e^{-\rho t} = z} using the
#' Newton-Raphson method. Called internally by
#' \code{\link{productivity}}.
#'
#' @param y Numeric vector. Initial values (e.g., initial biomass or
#'   stem counts).
#' @param z Numeric vector. Target values (e.g., surviving biomass or
#'   surviving stems at time \code{t}).
#' @param t Numeric. Time interval (years).
#'
#' @return A single numeric value: the estimated instantaneous turnover
#'   rate \eqn{\rho} (yr\eqn{^{-1}}).
#'
#' @references
#' Kohyama et al. 2019. Estimating net biomass production and loss from
#' repeated measurements of trees in forests and woodlands: Formulae,
#' biases and recommendations. \emph{Forest Ecology and Management}
#' 433: 729-740.
#'
#' @author T.S. Kohyama et al.
#' @seealso \code{\link{productivity}}
#' @export
turnover_est <- function(y, z, t) {
  f <- function(rho) {
    sum(y * exp(-rho * t) - z)
  }
  df <- function(rho) {
    sum(-t * y * exp(-rho * t))
  }
  # Newton-Rapton iteration
  rho <- 0.02
  precision <- 1.0e-12 # to stop iteration
  change <- precision + 1.0

  while (change > precision) {
    rho2 <- rho - f(rho) / df(rho)
    change <- abs(rho2 - rho)
    rho <- rho2
  }
  return(rho)
}
