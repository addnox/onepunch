#' Fit conditional severity distribution.
#'
#' @param loss A numeric vector
#' @param wt (Optional) a numeric vector with same length as \code{loss}
#' @return A named vector for parameters
#' @examples
#' \dontrun{
#' a <- c(2128626.826, 1001231.566, 2602453.407, 2652625.402, 2096552.901, 2481345.789, 1767929.774, 1132111.303, 1562121.048, 1529111.294, 1875538.967, 3671208.834, 1566929.993, 1900074.241, 1170474.947, 1958780.493, 1226562.609, 2364993.783, 4393903.605, 1711382.839, 3246468.782, 2749344.86, 2325670.505, 1094377.265, 1124804.947, 1098723.109, 1835881.542, 1602722.045, 1569878.823, 1024398.477, 2156012.186, 2644855.515, 1760780.099, 3764595.472, 1429465.965, 2008155.44, 1967520.282, 1954653.638, 6524999.764, 1839039.868, 2292137.751, 1631507.991, 1063858.728, 1061057.367, 1624566.74, 1200184.015, 2565640.453, 1230331.532, 1523257.634, 3314405.795, 2171018.269, 1551589.011, 1696161.672, 2262381.227)
#'
#' fit_severity(c(100, 200, 150, 300), Min = 100, Max = 500, distribution = "SimplePareto")
#' }
#' @export
fit_severity <- function(loss, distribution, Min, Max, wt = NULL) {
  x <- loss[loss >= Min & loss <= Max]

  if (!is.null(wt)) {
    wt <- wt[loss >= Min & loss <= Max]
    wt <- round(wt * 1e6, 0)
  }

  m_x <- mean(x)
  sd_x <- sd(x)
  cv_x <- sd_x / m_x
  sigma_x <- sqrt(log(1 + (cv_x ^ 2)))

  # a wrapper for fitdist
  ## by default, Melder-Mead method for multiple-parameter distr, and BFGS for single-parameter case
  mlefit <- function(...) {
    # some constants for mle start value calculation
    suppressWarnings(fitdistrplus::fitdist(
      data = x,
      method = "mle",
      fix.arg = list(xmin = Min, xmax = Max),
      control = list(maxit = 10000),
      weights = wt,
      ...))
  }

  # About starting values: alpha = beta = 1.5, theta = mean(x) / 1.5
  # Loss Model (Klugman) suggests another set of start values for most distribution
  # but the 1.5 rule is consistent with MR Fit

  res <- switch(distribution,
                SimplePareto = mlefit(distr = "SimplePareto", start = list(alpha = 1.5)),
                LogGamma = mlefit(distr = "LogGamma", start = list(alpha = 2, tau = 1)),
                LogNormal = mlefit(distr = "LogNormal", start = list(mu = log(m_x) + .5 * sigma_x ^2, sigma = sigma_x))
  )

  res_c <- data.table::data.table(
    distribution = distribution,
    parameter = names(res$estimate),
    estimate = res$estimate,
    sd = res$sd
  )

  return(res_c)
}

conditionalize <- function(distr_name, pkg_name = NULL) {
  fntype <- stringi::stri_sub(distr_name, 1, 1)  # p or d
  fnname <- stringi::stri_sub(distr_name, 2)
  if (is.null(pkg_name)) {
    namespace <- parent.frame()
  } else {
    namespace <- asNamespace(pkg_name)
  }

  pfn <- get(paste0("p", fnname), envir = namespace)  # e.g. pnorm
  dfn <- get(paste0("d", fnname), envir = namespace)
  qfn <- get(paste0("q", fnname), envir = namespace)
  rfn <- get(paste0("r", fnname), envir = namespace)

  if (fntype == "p") {

    fx <- function(q, xmin, xmax, ...) {
      if (length(q) == 0) return(numeric(0))
      res <- (pfn(q, ...) - pfn(xmin, ...))/ diff(pfn(c(xmin, xmax), ...))  # cannot use diff(pfn(c(xmin, q))), because q can be a vector

      res[q < xmin] <- 0
      res[q > xmax] <- 1

      return(res)
    }

  } else if (fntype == "d") {

    fx <- function(x, xmin, xmax, ...) {
      if (length(x) == 0) return(numeric(0))
      res <- dfn(x, ...) / diff(pfn(c(xmin, xmax), ...))

      res[x < xmin | x > xmax] <- 0

      return(res)
    }

  } else if (fntype == "q") {

    fx <- function(p, xmin, xmax, ...) {
      if (length(p) == 0) return(numeric(0))
      p_uncond <- p * diff(pfn(c(xmin, xmax), ...)) + pfn(xmin, ...)

      res <- qfn(p_uncond, ...)

      return(res)
    }
  } else if (fntype == "r") {
    fx <- function(n, xmin, xmax, ...) {
      p_prime <- runif(n) * diff(pfn(c(xmin, xmax), ...)) +  pfn(xmin, ...)
      res <- qfn(p_prime, ...)
      return(res)
    }

  } else {
    stop("Input function needs to be either pfoo, qfoo, dfoo or rfoo", call. = FALSE)
  }

  return(fx)
}


## Simple Pareto =========================
#' @export
dSimplePareto <- function(x, alpha, xmin, xmax) {
  fn_cond <- conditionalize("dpareto1", "actuar")
  res <- fn_cond(x, shape = alpha, min = xmin, xmin = xmin, xmax = xmax)

  return(res)
}

#' @export
pSimplePareto <- function(q, alpha, xmin, xmax) {
  fn_cond <- conditionalize("ppareto1", "actuar")
  res <- fn_cond(q, shape = alpha, min = xmin, xmin = xmin, xmax = xmax)

  return(res)

}

#' @export
qSimplePareto <- function(p, alpha, xmin, xmax) {
  fn_cond <- conditionalize("qpareto1", "actuar")
  res <- fn_cond(p, shape = alpha, min = xmin, xmin = xmin, xmax = xmax)

  return(res)
}

#' @export
rSimplePareto <- function(n, mu, sigma, xmin, xmax) {
  fn_cond <- conditionalize("rpareto1", "actuar")
  res <- fn_cond(n, shape = alpha, min = xmin, xmin = xmin, xmax = xmax)

  return(res)
}

## Lognormal ===============================
#' @export
dLogNormal <- function(x, mu, sigma, xmin, xmax) {
  fn_cond <- conditionalize("dlnorm")
  res <- fn_cond(x, meanlog = mu, sdlog = sigma, xmin = xmin, xmax = xmax)

  return(res)
}

#' @export
pLogNormal <- function(q, mu, sigma, xmin, xmax) {
  fn_cond <- conditionalize("plnorm")
  res <- fn_cond(q, meanlog = mu, sdlog = sigma, xmin = xmin, xmax = xmax)

  return(res)

}

#' @export
qLogNormal <- function(p, mu, sigma, xmin, xmax) {
  fn_cond <- conditionalize("qlnorm")
  res <- fn_cond(p, meanlog = mu, sdlog = sigma, xmin = xmin, xmax = xmax)

  return(res)
}

#' @export
rLogNormal <- function(n, mu, sigma, xmin, xmax) {
  fn_cond <- conditionalize("rlnorm")
  res <- fn_cond(n, meanlog = mu, sdlog = sigma, xmin = xmin, xmax = xmax)

  return(res)
}

## log-gamma ==================================

#' @export
dLogGamma <- function(x, alpha, tau, xmin, xmax) {
  fn_cond <- conditionalize("dlgamma", "actuar")
  res <- fn_cond(x, shapelog = tau, ratelog = alpha, xmin = xmin, xmax = xmax)

  return(res)
}

#' @export
pLogGamma <- function(q, alpha, tau, xmin, xmax) {
  fn_cond <- conditionalize("plgamma", "actuar")
  res <- fn_cond(q, shapelog = tau, ratelog = alpha, xmin = xmin, xmax = xmax)

  return(res)

}

#' @export
qLogGamma <- function(p, alpha, tau, xmin, xmax) {
  fn_cond <- conditionalize("qlgamma", "actuar")
  res <- fn_cond(p, shapelog = tau, ratelog = alpha, xmin = xmin, xmax = xmax)

  return(res)
}

#' @export
rLogGamma <- function(n, alpha, tau, xmin, xmax) {
  fn_cond <- conditionalize("rlgamma", "actuar")
  res <- fn_cond(n, shapelog = tau, ratelog = alpha, xmin = xmin, xmax = xmax)

  return(res)
}
