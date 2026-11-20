

# Alate Effects On Nonpersistent Infection Advancement (aeonia)


[![DOI](https://zenodo.org/badge/1444825708.svg)](https://zenodo.org/badge/latestdoi/1444825708)


This repo contains the R package `aeonia` that simulates aphid--parasitoid
dynamics and aphid-borne virus transmission.
It also contains scripts (many of which use the R package) that are used to
create figures in the manuscript "Natural enemies can mediate the impact of
plant-associated bacteria on insect-borne virus transmission".


# Organization

This repo contains the following top-level files/folders:

```
├── _plots
├── _scripts
├── aeonia.Rproj
├── CITATION.cff
├── data
├── data-raw
├── DESCRIPTION
├── inst
├── LICENSE.md
├── man
├── NAMESPACE
├── R
├── README.md
├── renv
├── renv.lock
└── src
```

Here are descriptions of each:

* `_plots`: Folder used to store figures created using R scripts.
    None are present in this repo, but they can be created using
    the provided scripts.
* `_scripts`: Folder containing R and shell scripts that run the simulations
    and create the plots used in the manuscript.
    They are ordered by use in the paper, and
    each file contains a description of what it contains.
    See "Reproducing the figures" below for more.
* `aeonia.Rproj`: RStudio project file for this project.
* `CITATION.cff`: Citation metadata (title, authors, version) for this
  repository; Zenodo uses it when archiving releases.
* `data`: Folder containing data used in the R package.
* `data-raw`: Folder containing a script (`gameofclones.R`) showing how I
  created the objects used in `data`.
* `DESCRIPTION`: Description file for the `aeonia` package.
* `inst`: folder containing necessary files to use the PCG family of random
  number generators in the C++ code for the `aeonia` package.
* `LICENSE.md`: License file for the `aeonia` package.
* `man`: Folder containing the documentation files for the `aeonia` package.
* `NAMESPACE`: File defining imports and export for the `aeonia` package.
* `R`: Folder containing the R files for the `aeonia` package.
* `README.md`: This file, the top-level README.
* `renv`: Contains necessary scripts to use R package `renv` to replicate
  the environment used to run these scripts.
  See "Replicating R environment" below for more.
* `renv.lock`: File containing metadata about installed packages used in this
  project so that they can be installed using the R package `renv`.
  See "Replicating R environment" below for more.
* `src`: Folder containing the C++ files for the `aeonia` package.






# Replicating R environment

I used R version 4.6.1 (platform: aarch64-apple-darwin23) for all my scripts.

This project uses the `renv` package, so if you want to use this, you must
first install it:

```r
install.packages("renv")
```

Then to install all the packages I used for these analyses, you can
simply run the following while having this project’s main directory as
your working directory:

``` r
renv::restore()
```

If you’d rather avoid `renv`, then you can install all the packages (in
the versions I used) this way:

``` r
if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
pkg_list <- jsonlite::read_json("renv.lock")[["Packages"]]
pkg_vers <- lapply(pkg_list, \(x) paste0(x[["Package"]], "@", x[["Version"]]))

install.packages(do.call(c, pkg_vers))
```


Package `renv` doesn't store `aeonia` itself, presumably because it's the same 
name as the project, so there are three ways to install `aeonia`.
The GitHub repository is named `aeonia2`, but the package it installs is
`aeonia` (loaded with `library(aeonia)`).

``` r
# If using `renv`:
renv::install("lucasnell/aeonia2@v0.1.0.5")

# If not using `renv`:
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("lucasnell/aeonia2@v0.1.0.5")

# If you've downloaded this repo, you can also install from source:
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
pkg <- devtools::build()
install.packages(pkg, repos = NULL, type = "source")
```



# Reproducing the figures

After setting up the R environment and installing `aeonia` (see above),
run the scripts in this order.
Unless stated otherwise, scripts are run from this project's main directory
(the one containing `aeonia.Rproj`).

1. Small landscapes (local):
   `_scripts/01-small/01-extremes.R`, then
   `_scripts/01-small/02b-extremes-manip.R`.
2. Large landscapes (SLURM cluster; we used Cornell BioHPC):
   copy `_scripts/02-large-hpc/00-large-hpc-preamble.R` to the cluster's
   home folder, then run `01-large-main.sh`, `02-large-interior.sh`,
   `03-large-r_fly.sh`, `04-large-densities.R`, and `05-large-emp-zeta.R`
   from `_scripts/02-large-hpc`, each in its own subfolder of that home folder.
   The commands to move each script to the cluster, run it, and copy its
   output back into `_scripts/interm-data` are in comments at the top of
   each script.
3. Landscapes for virus-attraction simulations (local):
   `source("_scripts/02-large-hpc/00-large-hpc-preamble.R")` to create
   `_scripts/interm-data/large-landscapes.rds`.
   It sets a fixed seed, so with the same R and `aeonia` versions it
   produces the same landscapes as on the cluster.
4. Large-landscape plots (local):
   `_scripts/03-large/01b-large-results.R`,
   `_scripts/03-large/02-large-densities-plots.R`,
   `_scripts/03-large/03-large-emp-zeta-plots.R`, and
   `_scripts/03-large/04-virus-attract.R`.

Each local script sets `.overwrite <- FALSE` near its top.
With this setting, a simulation is only run, and a plot only saved,
if its output file does not already exist.
Set `.overwrite <- TRUE` to re-run simulations and re-save plots regardless.
Output folders are created automatically.
Figures are written to `_plots` and intermediate simulation output to
`_scripts/interm-data`; each has a `README.md` describing its files.
