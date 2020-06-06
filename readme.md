[![DOI](https://zenodo.org/badge/91143992.svg)](https://zenodo.org/badge/latestdoi/91143992)

# cerUB: Multivariate statistic protocols for integrating archaeometric data (geochemical, mineralogical, petrographic)

This package allows the user to apply four protocols of multivariate statistics for exploring archaeometric data, including geochemical and mineralogical compositions, and semi-quantitative petrographic characterizations. Protocols wrap several methods used in Geology and Ecology, relying on ade4 and vegan packages.

Tested in Windows 7 and Mac OS X 10.9.5.

Dependencies (packages you must install before installing cerUB):

* ade4
* vegan
* dbscan
* pcaPP
* robCompositions
* setRNG
* stringr

## How to cite

If you use this package, cite it like this:

> Angourakis, Andreas, & Martínez Ferreras, Verònica. (2017, September 23). cerUB - Protocols for exploring archaeometric data (R package). Zenodo. http://doi.org/10.5281/zenodo.975451

And cite our publication that describes the statistics used:

> Angourakis, Andreas, Verònica Martínez Ferreras, Alexis Torrano, and Josep M. Gurt Esparraguera. 2018. “Presenting Multivariate Statistical Protocols in R Using Roman Wine Amphorae Productions in Catalonia, Spain.” Journal of Archaeological Science 93 (May): 150–65. https://doi.org/10.1016/j.jas.2018.03.007.

## How to install

You can install the development version of this package from GitHub by running these lines in your console:

```
if (!require("devtools")) install.packages("devtools") # this will install devtools package, if not installed already
devtools::install_github("Andros-Spica/cerUB")  
```

## How to use

Please see the tutorial at <https://github.com/Andros-Spica/cerUB_tutorial>
