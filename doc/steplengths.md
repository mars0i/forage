steplengths.md
===
cf. Viswanathan et al., *Nature* 1999 (which uses $l$ instead of $x$,
and $r_v$ instead of $k$).

Assuming that $\mu > 1$ (and with careful abuse of notation concerning
$\infty$):

&nbsp;

$$\int_r^{\infty} x^{-\mu} \; dx =$$

$$\frac{x^{1-\mu}}{1-\mu} \biggr\rvert_r^{\infty} = \frac{1}{1-\mu}
(\infty^{1-\mu} - k^{1-\mu}) = \frac{1}{1-\mu} (0 - k^{1-\mu}) =$$

$$\frac{k^{1-\mu}}{\mu-1}$$


So to distribute step lengths $x$ as $x^{-\mu}$ with $k$ as the
minimum length,

&nbsp;

$$\mathsf{P}(x) = \frac{x^{-\mu}}{k^{1-\mu}/(\mu-1)} =
x^{-\mu}\frac{\mu-1}{k^{1-\mu}}$$

Compare this to the Pareto distribution:

CDF: $F_X(x) = 1 - \left(\frac{x_m}{x}\right)^{\alpha}$ for $x \leq x_m$.

i.e. $x_m$ is the minium value for $x$.

PDF: 

$$\mathsf{P}(x) = \frac{\alpha x_m^{\alpha}}{x^{\alpha + 1}} = \alpha x_m^{\alpha} x^{-(\alpha + 1)}$$

again for $x \geq x_m$.  Or using the notation of Apache Commons'
`ParetoDistribution`,

&nbsp;

$$\mathsf{P}(x) = \frac{\alpha k^{\alpha}}{x^{\alpha + 1}} = \alpha k^{\alpha} x^{-(\alpha + 1)}$$

This is equivalent to the power law density above if we let

&nbsp;

$$\mu = \alpha + 1, \;\; \alpha = \mu - 1$$

$$\frac{\mu-1}{k^{1-\mu}} = (\mu-1)k^{\mu-1} = \alpha k^{\alpha}$$

(So if I want to run simulations that parallel or are inspired by those in 
in Viswanathan et al. 1999, I can use the Apache Commons Pareto
distribution, decrementing the $\mu$ parameter before passing it to
`ParetoDistribution`.)

$\mu$ should be:

$$ 1 < \mu \leq 3 $$

where if $\mu = 3$, the stable distribution is Gaussian, and $\mu = 2$
is supposed to be the theoretical optimum for foraging..

i.e. since $\mu = \alpha + 1$, we need

$$1 < \alpha + 1 \leq 3$$

or

$$0 < \alpha \leq 2$$

where $\alpha = 1$, i.e. $\mu = 2$, is the theoretical optimum for
searches.


