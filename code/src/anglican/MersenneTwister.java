package anglican;

/**
* Multithreaded MersenneTwister, required for parallel
* versions of inference algorithms.
*/
public class MersenneTwister 
	extends cern.jet.random.engine.MersenneTwister {
	public MersenneTwister(java.util.Date d) {super(d);}
	public synchronized int nextInt() {return super.nextInt();}
}
