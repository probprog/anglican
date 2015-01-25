# m! source code map


    src/
      embang/
        # Running inference and processing results
        core.clj       --- Running inference
        results.clj    --- Auxiliary REPL functions
        analysis.clj   --- Analysis of inference results (in progress)

        # Inference 
        ## General
        inference.clj  --- General inference functions
        state.clj      --- Particle state
        runtime.clj    --- Runtime support

        ## Algorithms
        importance.clj --- Importance sampling
        lmh.clj        --- Lightweight Metropolis-Hastings
        rdb.clj        --- Alias for lmh
        almh.clj       --- Adaptive Lightweight Metropolis-Hastings
        smc.clj        --- Particle Filter
        pgibbs.clj     --- Particle Gibbs
        siman.clj      --- MAP estimateion via simulated annealing
        map.clj        --- MAP estimation via search

        # Code transformations
        xlat.clj       --- Anglican to Clojure
        trap.clj       --- Clojure to CPS
        emit.clj       --- Compiling CPS 
