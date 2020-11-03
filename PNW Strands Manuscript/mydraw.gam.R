# The modified draw.gam function
mydraw.gam <- function (object, parametric = TRUE, select = NULL, scales = c("free", 
                                                                             "fixed"), align = "hv", axis = "lrtb", n = 100, unconditional = FALSE, 
                        overall_uncertainty = TRUE, dist = 0.1, ...) 
{
  scales <- match.arg(scales)
  S <- smooths(object)
  select <- gratia:::check_user_select_smooths(smooths = S, select = select)
  d <- gratia:::smooth_dim(object)
  take <- d <= 2L
  select <- select[take]
  S <- S[take]
  d <- d[take]
  is_re <- vapply(object[["smooth"]], gratia:::is_re_smooth, logical(1L))
  is_by <- vapply(object[["smooth"]], gratia:::is_by_smooth, logical(1L))
  if (any(is_by)) {
    S <- vapply(strsplit(S, ":"), `[[`, character(1L), 1L)
  }
  npara <- 0
  nsmooth <- length(S)
  if (isTRUE(parametric)) {
    terms <- parametric_terms(object)
    npara <- length(terms)
    p <- vector("list", length = npara)
  }
  g <- l <- vector("list", length = nsmooth)
  for (i in unique(S)) {
    eS <- evaluate_smooth(object, smooth = i, n = n, unconditional = unconditional, 
                          overall_uncertainty = overall_uncertainty, dist = dist)
    l[S == i] <- split(eS, eS[["smooth"]])
  }
  l <- l[select]
  d <- d[select]
  g <- g[select]
  if (length(g) == 0L) {
    message("Unable to draw any of the model terms.")
    return(invisible(g))
  }
  for (i in seq_along(l)) {
    g[[i]] <- draw(l[[i]])
  }
  if (isTRUE(parametric)) {
    for (i in seq_along(terms)) {
      p[[i]] <- evaluate_parametric_term(object, term = terms[i])
      g[[i + length(g)]] <- draw(p[[i]])
    }
  }
  if (isTRUE(identical(scales, "fixed"))) {
    wrapper <- function(x) {
      range(x[["est"]] + (2 * x[["se"]]), x[["est"]] - 
              (2 * x[["se"]]))
    }
    ylims <- range(unlist(lapply(l, wrapper)))
    if (isTRUE(parametric)) {
      ylims <- range(ylims, unlist(lapply(p, function(x) range(x[["upper"]], 
                                                               x[["lower"]]))))
    }
    gg <- seq_along(g)[c(d == 1L, rep(TRUE, npara))]
    for (i in gg) {
      g[[i]] <- g[[i]] + lims(y = ylims)
    }
  }
  g
}

# Example no. 1
dat <- gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
mod <- gam(y ~ s(x0),  data = dat, method = "REML")
p <- mydraw.gam(mod)
p[[1]] + ggtitle("My title")

# example no. 2
mod <- gam(y ~ s(x0) + x1, data = dat, method = "REML")
p <- mydraw.gam(mod)
# Plot graphs separately
p[[1]] + ggtitle("My title")
p[[2]] + ggtitle("My title")
# Arrange the two plots on the same figure
cowplot::plot_grid(plotlist = p)
