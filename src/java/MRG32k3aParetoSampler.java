/* Marshall Abrams' hacked version of the following to work with MRG32k3a: */

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
// package org.apache.commons.rng.sampling.distribution;

// import org.apache.commons.rng.UniformRandomProvider;

/**
 * Sampling from a <a href="https://en.wikipedia.org/wiki/Pareto_distribution">Pareto distribution</a>.
 *
 * <p>Sampling uses {@link MRG32k3a#nextDouble()}.</p>
 *
 * @since 1.0
 */
public class MRG32k3aParetoSampler {
    /** Scale. */
    private final double scale;
    /** 1 / Shape. */
    private final double oneOverShape;
    /** Underlying source of randomness. */
    private final MRG32k3a rng;

    /**
     * @param rng Generator of uniformly distributed random numbers.
     * @param scale Scale of the distribution.
     * @param shape Shape of the distribution.
     * @throws IllegalArgumentException if {@code scale <= 0} or {@code shape <= 0}
     */
    public MRG32k3aParetoSampler(MRG32k3a rng,
                                         double scale,
                                         double shape) {
        if (scale <= 0) {
            throw new IllegalArgumentException("scale is not strictly positive: " + scale);
        }
        if (shape <= 0) {
            throw new IllegalArgumentException("shape is not strictly positive: " + shape);
        }
        this.rng = rng;
        this.scale = scale;
        this.oneOverShape = 1 / shape;
    }

    /**
     * @param rng Generator of uniformly distributed random numbers.
     * @param source Source to copy.
     */
    private MRG32k3aParetoSampler(MRG32k3a rng,
                                          MRG32k3aParetoSampler source) {
        this.rng = rng;
        scale = source.scale;
        oneOverShape = source.oneOverShape;
    }

    /**
     * Returns a Pareto-distributed number from the generator embedded
     * in the object, using the supplied scale and shape parameters.
     */
    public double sample() {
        return scale / Math.pow(rng.nextDouble(), oneOverShape);
    }

    /* New method not in the Apache class I copied. */
    /**
     * @param u A double that should have been generated by some uniformly-distributed 
     * random number generator (not necessarily MRG32k3a).
     *
     * Returns a Pareto-distributed number based on the 
     * uniformly-distributed argument, using the supplied scale and shape parameters
     * during creation of the MRG32k3a instance.
     *
     * This method can be used with uniform random numbers generated by any PRNG.
     * (Perhaps it doesn't belong here--or perhaps the class is now misnamed.)
     *
     */
    public double sample(double u) {
        return scale / Math.pow(u, oneOverShape);
    }

    public String toString() {
        return "[Inverse method for Pareto distribution " + rng.toString() + "]";
    }

    public MRG32k3aParetoSampler withUniformRandomProvider(MRG32k3a rng) {
        return new MRG32k3aParetoSampler(rng, this);
    }

    /**
     * Creates a new Pareto distribution sampler.
     *
     * @param rng Generator of uniformly distributed random numbers.
     * @param scale Scale of the distribution.
     * @param shape Shape of the distribution.
     * @return the sampler
     * @throws IllegalArgumentException if {@code scale <= 0} or {@code shape <= 0}
     * @since 1.3
     */
    public static MRG32k3aParetoSampler of(MRG32k3a rng,
                                                  double scale,
                                                  double shape) {
        return new MRG32k3aParetoSampler(rng, scale, shape);
    }
}
