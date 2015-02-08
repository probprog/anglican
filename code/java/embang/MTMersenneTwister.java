package embang;

import cern.jet.random.engine.MersenneTwister;

/**
 * Multithreaded MersenneTwister, required for parallel
 * versions of inference algorithms.
 */
public class MTMersenneTwister extends MersenneTwister {
	public MTMersenneTwister(java.util.Date d) {super(d);}
	public synchronized int nextInt() {return super.nextInt();}
}
