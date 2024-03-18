siena07ToConvergence <- function(alg, dat, eff, ans0 = NULL, max_runs = 30, set, convergence = 0.25, ...){
    numr <- 0
    ans <- siena07(alg, data = dat, effects = eff, prevAns = ans0, ...) # the first run
    repeat {
        save(ans, file = paste(sprintf("results/set%s/ans", set),numr,".RData",sep = "")) # to be safe
        numr <- numr + 1 # count number of repeated runs
        tm <- ans$tconv.max # convergence indicator
        cat(numr, tm,"\n") # report how far we are
        if (tm < convergence) {pbPost("note", title="Simulation complete: converged")
                        break} # success
        if (tm > 10) {pbPost("note", title="Simulation complete: No hope")
                        break} # divergence without much hope
        # of returning to good parameter values
        if (numr == max_runs) {break} # now it has lasted too long
        ans <- siena07(alg, data = dat, effects = eff, prevAns = ans, ...)
    }
    if (tm > convergence)
        {
        cat("Warning: convergence inadequate.\n")
        pbPost("note", title="Simulation complete: convergence inadequate.")
        }
    ans
}

# GeodesicDistribution calculates the distribution of non-directed
# geodesic distances; see ?sna::geodist
# The default for levls reflects the usual phenomenon
# that geodesic distances larger than 5
# do not differ appreciably with respect to interpretation.
# Note that the levels of the result are named;
# these names are used in the \code{plot} method.
GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}