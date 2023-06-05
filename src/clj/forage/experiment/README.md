forage/experiments
===

This directory has the same general purpose as forage/matruns, but the
config and run scripts in experiment/ use the old MASON Continuous2D
environments in `forage.core.env-mason`.  Experiments in matruns/ use
the new `forage.core.env-matrix` (formerly `env-indexed`).  The same run
scripts can't be used with both environment libraries because the scale
is different.  A perceptual radius of 1 using env-mason might correspond
to a perceptual radius of 100 or 1000 in `env-matrix`.
