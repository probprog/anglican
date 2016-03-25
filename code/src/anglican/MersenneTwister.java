package anglican;

/**
* Multithreaded MersenneTwister, required for parallel
* versions of inference algorithms.
*/
public class MersenneTwister 
	extends cern.jet.random.engine.MersenneTwister {

	/* mimicking the base class */
	private int mti = 0;
	private static final int N = 624;

	public MersenneTwister(java.util.Date d) {super(d);}

	public int nextInt() {
		if(mti >= N) {
			synchronized(this) {
				nextBlock();
				mti = 0;
			}
		}
		synchronized(this) {
			mti++;
		}
		return super.nextInt();
	}
}
