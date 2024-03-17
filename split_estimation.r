split_estimation <- function(set = c("1", "2"), filename){
    proc.time()
    library(methods)
    library(RPushbullet)
    library(RSiena)
    set <- as.character(set)
    load(sprintf("data/proc/siena_data%s.Rdata", set))
    load(sprintf("results/set%s/sufficient_even_better.RData", set))
    source("functions.r")
    
    data <- get(sprintf("siena_data%s" , set))
    # mini
    eff <- getEffects(data)
    eff <- includeEffects(eff, gwesp, name = sprintf("pta_depend%s" , set))
    

    # predict pta
        # inPop
        eff <- includeEffects(eff, inPopSqrt, name = sprintf("pta_depend%s" , set))
        # democracy
        if(set == "2"){eff <- includeEffects(eff, egoX, egoXaltX, interaction1 = sprintf("dem_depend%s" , set), name = sprintf("pta_depend%s" , set))}


    #predicting democracry
        # demoutdeg
        eff <- includeEffects(eff, outdeg, name = sprintf("dem_depend%s" , set), interaction1 = sprintf("pta_depend%s" , set))
        # avAlt
        eff <- includeEffects(eff, avAlt, name = sprintf("dem_depend%s" , set), interaction1 = sprintf("pta_depend%s" , set))
    

    #controll variables:
        # on PTA
            #dist
            eff <- includeEffects(eff, X, interaction1 = "dist_covar", name = sprintf("pta_depend%s" , set))
            # alliance
            eff <- includeEffects(eff, X, interaction1 = sprintf("ally_covar%s" , set), name = sprintf("pta_depend%s" , set))
            #trade
            eff <- includeEffects(eff, X, interaction1 = paste0("trade_log", set), name = sprintf("pta_depend%s" , set))
            # gdp
            if(set == "2") {eff <- includeEffects(eff, egoX, egoXaltX, interaction1 = paste0("gdp_log", set), name = sprintf("pta_depend%s" , set))}

        # on democracy
            eff <- includeEffects(eff, effFrom, interaction1 = paste0("gdp_log", set), name = sprintf("dem_depend%s" , set))
    
    alg <- sienaAlgorithmCreate(projname = "full_model", modelType = setNames(c(3),paste0("pta_depend", set)), nsub = 1, n3 = 3000, n2start = 250)
    
    assign(paste0("ans", set), siena07(alg, data = data, effects = eff, prevAns = get(paste0("ans", set)), nbrNodes = 7, useCluster = TRUE, returnDeps = TRUE))
    
    print(get(paste0("ans", set)))

    save(list = paste0("ans", set), file = sprintf("results/set%s/new.RData", set))
    
    pbPost("note", title= sprintf("Simulation %s complete", set))


}