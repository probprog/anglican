# m! source code map

    src/
      anglican/
        # Running inference and processing results
        core.clj       --- Running inference and auxiliary commands
        results.clj    --- Handling of inference results

        # Inference 
        ## General
        inference.clj  --- General inference functions
        state.clj      --- Particle state
        runtime.clj    --- Runtime support

        ## Algorithms

        ### Inferring distribution
        importance.clj --- Importance sampling
        [p]lmh.clj     --- [Parallel] Lightweight Metropolis-Hastings
        [p]almh.clj    --- [Parallel] Adaptive LMH
        smc.clj        --- Sequential Monte Carlo
        pimh.clj       --- Particle Independent Metropolis-Hastings
        pgibbs.clj     --- Particle Gibbs (Iterative Conditional SMC)
        pcascade.clj   --- Parallel Particle Cascade

        ### Estimating MAP
        siman.clj      --- Simulated annealing
        bamc.clj	   --- Bayesian ascent Monte Carlo

        ### Filtering
        pfilter.clj    --- Particle Filter

        # Code transformations
        xlat.clj       --- Anglican to Clojure
        trap.clj       --- Clojure to CPS
        emit.clj       --- Compiling CPS 

        # Java
        MTMersenneTwister.java --- thread-safe synchronized version
                          of cern.jet.random.engine.MersenneTwister 

      anglib/
        # Anglican library modules
        ## In particular: 
        crp.clj        --- Wrapped CRP for compatibility with the
                           original Anglican syntax; crp-based DPmem
        iris_data.clj  --- Ronald Fisher\'s Iris dataset
        beaver.clj     --- Beaver activity datasets
