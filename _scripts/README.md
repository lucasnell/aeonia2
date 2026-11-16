
# `_scripts`

## Folder contents:

```
├── 00-preamble.R
├── 01-small
│   ├── 01-extremes.R
│   ├── 02a-heatmap-funs.R
│   └── 02b-extremes-manip.R
├── 02-large-hpc
│   ├── 00-large-hpc-preamble.R
│   ├── 01-large-main.sh
│   ├── 02-large-interior.sh
│   ├── 03-large-r_fly.sh
│   ├── 04-large-densities.R
│   └── 05-large-emp-zeta.R
├── 03-large
│   ├── 01a-large-plot-funs.R
│   ├── 01b-large-results.R
│   ├── 02-large-densities-plots.R
│   ├── 03-large-emp-zeta-plots.R
│   └── 04-virus-attract.R
├── interm-data
│   └── ...
└── README.md
```



## Folder descriptions:

- `01-small`: scripts for small landscape simulations
- `02-large-hpc`: scripts for large landscape simulations that had to be run
  on the high-performance cluster
- `03-large`: scripts for large landscape simulations
- `interm-data`: folder containing intermediate data from simulations;
  this folder has its own `README.md` file describing its contents.


## Running the scripts

- Scripts in `01-small` and `03-large` are run locally with the project's
  main directory (the one containing `aeonia.Rproj`) as the working directory.
  Scripts in `02-large-hpc` are run on a SLURM cluster (Cornell BioHPC);
  instructions for moving each to the cluster, running it, and copying its
  output back into `interm-data` are in comments at the top of each file.
  These commands assume an ssh host alias `biohpc` in `~/.ssh/config`
  with a `User` line giving your cluster username;
  `BIOHPC_USER=$(ssh -G biohpc | awk '/^user /{print $2}')` reads that
  username locally so it can be used in `scp` paths.
- Each local script sets `.overwrite <- FALSE` near its top.
  With `.overwrite <- FALSE`, simulations are only run, and plots only saved,
  when the output file does not already exist.
  Set `.overwrite <- TRUE` to re-run simulations and re-save plots regardless.
- Output folders (`_plots/main/extremes`, `_plots/supp`, and the
  `interm-data/large-*` folders) are created by `00-preamble.R` if missing.


## File descriptions:

- `00-preamble.R`: packages, intermediate-file paths, and helper objects
  used by all local scripts
- `01-small` files:
    - `01-extremes.R`: small landscape simulations of extremes (weak and strong
      parasitoid responsiveness to aphid densities) with respect to
      *Pseudomonas* effects on virus transmission
    - `02a-heatmap-funs.R`: functions used to generate heatmaps of virus 
      outcomes (or effects of *Pseudomonas* on those outcomes) versus two
      different parameters
    - `02b-extremes-manip.R`: extremes in small landscapes where we manipulate
      one or two parameters at a time
- `02-large-hpc` files:
    - `00-large-hpc-preamble.R`: preamble used for all large-hpc scripts;
      the biggest job of this script is to create the landscapes of
      *Pseudomonas* locations; when sourced locally, it creates
      `_scripts/interm-data/large-landscapes.rds`, which
      `03-large/04-virus-attract.R` requires
    - `01-large-main.sh`: primary large landscape simulations where we
      manipulate parasitoid responsiveness to aphid densities,
      loading probabilities, variability in initial aphid densities,
      alate attraction to virus-infected plants, and
      alate repellence from *Pseudomonas*-inhabited plants
    - `02-large-interior.sh`: same as `01-large-main.sh`, but initially
      virus-infected plants are located on the interior of the landscape
    - `03-large-r_fly.sh`: same as `01-large-main.sh`, but varying the
      alate flight radius (`r_fly`; 0.5×, 1×, and 2× its default)
      with loading probabilities of 0.5, no variability in initial aphid
      densities, no alate attraction to virus-infected plants, and
      no alate repellence from *Pseudomonas*-inhabited plants
    - `04-large-densities.R`: simulations for densities of insects and viruses
      across varying *Pseudomonas* occupancies and parasitoid responsiveness
    - `05-large-emp-zeta.R`: empirical zeta estimates in larger landscapes
- `03-large` files:
    - `01a-large-plot-funs.R`: miscellaneous large landscape helper (mostly 
      plotting) functions used in `01b-large-results.R`
    - `01b-large-results.R`: primary large landscape plots (requires output
      from `02-large-hpc/01-large-main.sh`, `02-large-interior.sh`, and
      `03-large-r_fly.sh`)
    - `02-large-densities-plots.R`: plots of densities through time in large 
      landscapes (requires output from `02-large-hpc/04-large-densities.R`)
    - `03-large-emp-zeta-plots.R`: plots for empirical estimates of zeta in 
      large landscapes (requires output from `02-large-hpc/05-large-emp-zeta.R`)
    - `04-virus-attract.R`: simulations and plots for how alate attraction to 
      virus-infected plants affects virus disease dynamics through time and 
      space
- `README.md`: this file
