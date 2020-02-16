[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/willpearse/MADtraits.svg?branch=master)](https://travis-ci.org/willpearse/MADtraits)
[![codecov](https://codecov.io/gh/willpearse/MADtraits/branch/master/graph/badge.svg)](https://codecov.io/gh/willpearse/MADtraits)
MADtraits - Make A Database of Traits
===============================================================
Will Pearse (will.pearse@usu.edu)

Part of the MAD world of packages that Make A Database from existing
data. *Use of MADtraits, and all MADworld packages, requires you to
cite the underlying trait data it downloads* - the function
`citations` will give you this citation information for whatever data
you are working with.

# Installation

```{R}
# install.packages("devtools") # (If devtools not installed)
library(devtools)
install_github("willpearse/MADtraits")
```

# Getting started

Pick a directory on your hard-drive that you can use as a 'cache' to
store data downloaded from individual papers/repositories using
MADtraits. Mine, for example, is `~/Code/MADtraits/cache`. This is
optional, but recommended, as otherwise it will take a very long time
to use MADtraits every time you use it. Once you've chosen that, run
the following:

```{R}
library(MADtraits)
data <- MADtraits("~/Code/MADtraits/cache")
```

This will take a while the first time, but as long as you always use
that same cache location, it will be almost instantaneous after
that.

Once you have that data, you can optionally 'clean' it harmonising
species' and trait names, and matching (as best possible) the units
across different measurements (e.g., converting all weights from kg to
g, picking units on the basis of the most commonly used one in the
data). Note that the nomenclature used in MADtraits isn't guaranteed
to be the one you prefer - read on to learn more about the internal
structure of MADtraits to do such cleaning for yourself.

```{R}
clean.data <- clean.MADtraits(data)
```

You can now subset your data according to particular species or
traits like this:

```{R}
clean.data[c("quercus_robur","quercus_ilex"), "height"]
```

# MADtraits structure

A MADtraits data object is really just `data.frame`s in a list: one
for continuous data, and the other for categorical data. Knowing this,
you can maniuplate the data however you want once you've downloaded it
using something like `aggreate` or `apply` to average across
species/traits.

```{R}
str(clean.data)
```

Note that the last column in each of the `data.frame`s is special:
it's `metadata`. This is set of `key:value` pairs, separated by `;`,
that allow you to extract additional information about each trait
observation (e.g., the `latitude` at which it was recorded).

# Contributing to MADtraits and its internals

Thank you for your interest in the package! We have a detailed set of
instructions for how the package works up available online
https://github.com/willpearse/MADtraits/wiki. Please follow those
instructions!
