noteOnApacheRandomFns.md
Notes on some classes in org.apache.commons.math3.{random,distribution} .

----------------
WELL generators

Initialization of the Apache Commons Well generators in
org.apache.commons.math3.random happens in the superclass AbstractWell
rather than in the individual classes Well1024a, etc.

The Apache Commons Well generators in org.apache.commons.math3.random
do use the full long seed, I think; it's split into two ints in
AbstractWell.java.

In AbstractWell.java, in the method

	public void setSeed(final int[] seed)

the entire state int array v from the (usually smaller) seed.  

First the seed is copoied into the front of the array v.

Then the rest of the array is initialized in a loop, using what I am
assuming is a little PRNG, in line 165 in the 3.6.1 release.
So that's good.

There is no automatic flushing of the initial state, but it looks like
the algorithm differs from MT in that it recalculates on every step.
It doesn't just eat up the state until it needs to be replenished.
There's not test to see whether enough numbers have been used so that
it's time for the replenishment and twist.  (Need to check the article.)

----------------
Mersenne Twister

With initializing from an int or long, something similar is done in
MersenneTwister.java in

	public void setSeed(int seed)

in line 149.

But in 
	public void setSeed(int[] seed)

what happens is weird.  It looks like the passed array is ignored,
and setSeed(int seed) is called *with a constant*, and then the
array is initialized by a complex operation.

THERE IS NO AUTOMATIC FLUSHING OF THE INITIAL STATE.
So that's something the user must do.
