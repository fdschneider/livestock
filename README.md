# livestock model package

This R package includes code for a paper on spatial indicators of land degradation authored by Florian Schneider and Sonia Kéfi. The repository of the study can be found at: https://github.com/cascade-wp6/resilience_rangelands

## model theory

The livestock model is a pair-approximation model of spatial vegetation dynamics.  This model approximates the dynamics that can be observed in cellular automata, but instead of defining individual cell states and transitions by neighborhood conditions, it defines transition probabilities of neighboring pairs of cells/localities in a grid/landscape.

The model assumes that each locality can either be empty of vegetation (0) or occupied (1). Pairs of localities can be either both empty (00), mixed (01) or both occupied (11). The pair-approximation model defines the population dynamics of those pairs in ordinary differential equations. 


## package structure

The package further includes a function `ini_rho()` to define the initial conditions of a system in dual state space of total cover and local cover. It takes two variables and returns an object of class `cover` which includes two list entries:  total vegetation cover on the landscape scale (`rho_1`)  and total cover of 11-pairs (`rho_11`). 

The package provides individual-level model definitions in the functions `colonization()` and `death()` which take an object of class `cover`. Those are defined as non-linear functions of total vegetation cover on the landscape scale (`rho_1`)  and average local cover `q_11`.

The dual state can be translated into average local vegetation cover with `q_11()`. 

The model specifications are provided within the model object `livestock`. This includes default parameters, and different simplifications of the model. The function `livestock$pair()` describes the differential equations for the pair-approximation and is called by default within the function `ode_run()` (making use of package deSolve).

A single model run is returned by calling

```
out <- run_ode(ini_rho(0.5), func = livestock$pair, parms = p)
par(mfrow = c(1,2); plot(out)
```

Parameters can be updated using the function `set_parms()`. 

```
out <- run_ode(ini_rho(0.5), func = livestock$pair, 
  parms = set_parms(livestock$parms, set = list(b = 0.2, f = 0.9, p = 0.99, v = 0.9))
  )
```

Several simulation/plotting functions exist to provide analytical graphs of the models. 

### get_equilibria()

### sim_trajectories()

### sim_bifurcations()

### plot_pairapproximation()

### plot_pairapproximation3D()

### plot_bifurcation()

### plot_bifurcation3D()




# LICENSE

MIT License

Copyright (c) 2016 Florian D. Schneider (fd.schneider@senckenberg.de)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
