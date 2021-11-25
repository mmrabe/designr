# designr

[![CRAN](http://www.r-pkg.org/badges/version/designr)](https://cran.r-project.org/package=designr)
[![downloads](http://cranlogs.r-pkg.org/badges/designr)](https://cran.r-project.org/package=designr)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4587102.svg)](https://doi.org/10.5281/zenodo.4587102)
[![Package
status](https://github.com/mmrabe/designr/actions/workflows/r.yml/badge.svg)](https://github.com/mmrabe/designr/actions/workflows/r.yml)

*designr* is an R package to create and simulate crossed factorial
designs.

## Installation

Install from CRAN within R using:

``` r
install.packages("designr")
```

Install the development version in R using `devtools`:

``` r
devtools::install_github("mmrabe/designr", build_vignettes = TRUE)
```

## Random and fixed factors

*designr* supports factorial designs with an arbitrary number of fixed
and random factors. Fixed factors are factors for which levels are known
and typically defined by the experimenter, e.g. an experimental
condition or a quasi-experimental variable such as a subject’s age
group. Conversely, the instances of random factors are usually not known
before data collection. Examples for random factors are subjects or
items in a typical psychological experiment, with the individual tested
subjects and used items being the instances of those random factors.

## Simple fixed-effects design

A fixed-effects design without repeated measurement is created as easily
as this:

``` r
design1 <- 
  fixed.factor("Age", levels=c("young", "old")) +
  fixed.factor("Material",  levels=c("word", "image"))
design1
```

    ## Factor design with 2 factor(s):
    ##  - Fixed factor `Age` with 2 level(s) (young, old) and 1 replication(s)
    ##  - Fixed factor `Material` with 2 level(s) (word, image) and 1 replication(s)
    ## 
    ## Design matrix with 4 planned observations:
    ## # A tibble: 4 × 2
    ##   Age   Material
    ##   <fct> <fct>   
    ## 1 young word    
    ## 2 old   word    
    ## 3 young image   
    ## 4 old   image

As can be seen, this experimental design requires 4 observations.

## Adding random factors

Assume we want to test different groups of subjects. Each subject will
only be `old` or `young` but be tested with stimuli of both categories
`word` and `image`. In a typical behavioral experiment, `Age` would now
be a between-subject/within-item factor and `Material` a
within-subject/between-item factor. In other words, `Material` is now
nested within the instances of `Subject`, whereas `Subject` is grouped
by `Age`.

``` r
design2 <- 
  fixed.factor("Age", levels=c("young", "old")) +
  fixed.factor("Material",  levels=c("word", "image")) +
  random.factor("Subject", groups = "Age")
design.codes(design2)
```

    ## # A tibble: 4 × 3
    ##   Subject  Age   Material
    ##   <fct>    <fct> <fct>   
    ## 1 Subject1 old   word    
    ## 2 Subject1 old   image   
    ## 3 Subject2 young word    
    ## 4 Subject2 young image

The minimal experimental design will still require 4 observations,
assigning one subject to each level of the between-subject factor `Age`.

## Nested designs

Note that `design1` is nested within `design2`. This means that instead
of defining `design2` like we did above, we can also derive it from the
existing `design1` by adding the random factor `Subject` like so:

``` r
design2 <- 
  design1 +
  random.factor("Subject", groups = "Age")
```

## Crossed random factors

Oftentimes, experiments will have more than one random factor, for
example `Subject` and `Item`. This is because items in behavioral
experiments are often prepared upfront and not randomly generated upon
presentation. In that case we would like to make sure that each item is
presented equally often across all subjects and within-item conditions.
Suppose that we are extending our example from above by a second random
factor `Item`. Contrary to `Subject`, `Item` is grouped by `Material`
because each item can only be a `word` or `image` but it may be
presented to both `old` and `young` subjects.

``` r
design3 <- 
  fixed.factor("Age", levels=c("young", "old")) +
  fixed.factor("Material",  levels=c("word", "image")) +
  random.factor("Subject", groups = "Age") +
  random.factor("Item", groups = "Material")
design.codes(design3)
```

    ## # A tibble: 4 × 4
    ##   Subject  Item  Age   Material
    ##   <fct>    <fct> <fct> <fct>   
    ## 1 Subject1 Item1 old   image   
    ## 2 Subject1 Item2 old   word    
    ## 3 Subject2 Item1 young image   
    ## 4 Subject2 Item2 young word

In this design, we plan to test 2 subjects, one `young` and one `old`,
and each of them will be presented two items, an `image` and a `word`.
The items will appear equally often in the levels of `Age` and subjects
will see an equal number of items in all levels of `Material`.

## Counterbalancing

Note that in the example above, each item really only appears once per
subject. However, suppose we introduce a third fixed factor, which
varies within subjects and within items, i.e. it is neither a subject
nor item level fixed property. This could be something like the contrast
on the screen or some other experimental manipulation that is
pseudo-randomly varied for each subject and each item.

The resulting design may look something like this:

``` r
design4 <- 
  fixed.factor("Age", levels=c("young", "old")) +
  fixed.factor("Material",  levels=c("word", "image")) +
  fixed.factor("Contrast",  levels=c("high", "low")) +
  random.factor("Subject", groups = "Age") +
  random.factor("Item", groups = "Material")
design.codes(design4)
```

    ## # A tibble: 8 × 5
    ##   Subject  Item  Age   Material Contrast
    ##   <fct>    <fct> <fct> <fct>    <fct>   
    ## 1 Subject1 Item1 old   image    high    
    ## 2 Subject1 Item1 old   image    low     
    ## 3 Subject1 Item2 old   word     high    
    ## 4 Subject1 Item2 old   word     low     
    ## 5 Subject2 Item1 young image    high    
    ## 6 Subject2 Item1 young image    low     
    ## 7 Subject2 Item2 young word     high    
    ## 8 Subject2 Item2 young word     low

In a fully crossed and balanced experimental design, each item would now
be presented twice per subject, once with `high` and once with `low`
contrast. This can be absolutely legitimate, depending on the research
question. In many behavioral experiments, however, the experimenter may
wish to prevent the same item from being presented twice because that
could introduce unwanted effects.

Essentially, what we want to do is to group each `Subject`×`Item`
pairing by `Contrast`, i.e. we want to ensure that each item assigned to
a subject is only assigned in either `high` or `low` contrast. We can
therefore add the interaction of `Subject` and `Item` as a random
factor, grouped by `Contrast`:

``` r
design5 <- 
  fixed.factor("Age", levels=c("young", "old")) +
  fixed.factor("Material",  levels=c("word", "image")) +
  fixed.factor("Contrast",  levels=c("high", "low")) +
  random.factor("Subject", groups = "Age") +
  random.factor("Item", groups = "Material") +
  random.factor(c("Subject","Item"), groups = "Contrast")
design.codes(design5)
```

    ## # A tibble: 16 × 5
    ##    Subject  Item  Age   Material Contrast
    ##    <fct>    <fct> <fct> <fct>    <fct>   
    ##  1 Subject1 Item1 old   image    high    
    ##  2 Subject1 Item2 old   image    low     
    ##  3 Subject1 Item3 old   word     high    
    ##  4 Subject1 Item4 old   word     low     
    ##  5 Subject2 Item1 old   image    low     
    ##  6 Subject2 Item2 old   image    high    
    ##  7 Subject2 Item3 old   word     low     
    ##  8 Subject2 Item4 old   word     high    
    ##  9 Subject3 Item1 young image    high    
    ## 10 Subject3 Item2 young image    low     
    ## 11 Subject3 Item3 young word     high    
    ## 12 Subject3 Item4 young word     low     
    ## 13 Subject4 Item1 young image    low     
    ## 14 Subject4 Item2 young image    high    
    ## 15 Subject4 Item3 young word     low     
    ## 16 Subject4 Item4 young word     high

The design now contains 16 planned observations for 4 subjects and 4
items. Each subject will be presented each item exactly once and an
equal number of items (1) in each combination of `Material`×`Contrast`.
Moreover, each item will be presented equally often in each combination
of `Age`×`Contrast`.

For a more detailed example, see the `design-to-dataframe` vignette (by
executing `vignette("design-to-dataframe")`) and the manual pages of the
package.
