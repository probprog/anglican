# Anglican Source Code Map

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

        ### Posterior Estimation
        importance.clj --- Importance sampling (likelihood weighting)
        [p]lmh.clj     --- [Parallel] Lightweight Metropolis-Hastings
        [p]almh.clj    --- [Parallel] Adaptive LMH
        rmh.clj        --- Reuse Lightweight Metropolis-Hastings
        smc.clj        --- Sequential Monte Carlo
        pimh.clj       --- Particle Independent Metropolis-Hastings
        pgibbs.clj     --- Particle Gibbs (Iterative Conditional SMC)
        pcascade.clj   --- Particle cascade (asynchronous sequential Monte Carlo)
        pgas.clj       --- Particle Gibbs with ancestor sampling
        ipmcmc.clj     --- Interacting particle MCMC
        bbvb.clj       --- Black box variational Bayes

        ### MAP Estimation
        siman.clj      --- Simulated annealing
        bamc.clj	   --- Bayesian ascent Monte Carlo

        ### Filtering
        pfilter.clj    --- Particle Filter

        # Code transformations
        xlat.clj       --- Anglican to Clojure
        trap.clj       --- Clojure to CPS
        emit.clj       --- Compiling CPS 

      anglib/
        # Anglican library modules
        ## In particular: 
        crp.clj        --- Wrapped CRP for compatibility with the
                           original Anglican syntax; crp-based DPmem
        iris_data.clj  --- Ronald Fisher\'s Iris dataset
        beaver.clj     --- Beaver activity datasets
