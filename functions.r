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

