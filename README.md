
> This repository stores the code and data used to perform the analysis presented in the manuscript *In-season predictions of daily harvest for lower Kuskokwim River subsistence salmon fisheries* by authors B. Staton, W. Bechtol, L. Coggins, and G. Decossas.
>
> **The manuscript is under review.**

[![ArticleDOI](https://img.shields.io/badge/Article-PLACEHOLDER%20IF%20ACCEPTED-blue?logo=doi&logoColor=f5f5f5)]()  
[![GitHub Repo Archive DOI](https://img.shields.io/badge/GitHub%20Repo%20Archive-PLACEHOLDER%20WHEN%20MINTED-blue?logo=github)]()

## Repository Structure

| Subdirectory              | Description                                                                                                                       |
|:--------------------------|:----------------------------------------------------------------------------------------------------------------------------------|
| `session-setup.R`         | Loads packages, sets directories, creates miscellaneous objects used throughout other scripts                                     |
| `make-figures.R`          | Creates all figures in the main text                                                                                              |
| `make-tables.R`           | Creates `.csv` files containing the summarized information ready for table formatting and inclusion in main text and Supplement A |
| `make-residual-figures.R` | Creates all residual diagnostic figures in Supplement B                                                                           |

## Dependencies

### ’KuskoHarv\*’ Package Family

Users will need to install the R package [‘KuskoHarvPred’](https://github.com/bstaton1/KuskoHarvPred).
From any R console, run:

``` r
install.packages("remotes")
remotes::install_github("bstaton1/KuskoHarvPred", ref = "v2023.3")
```

‘KuskoHarvPred’ contains infrastructure to perform the key analytical tasks presented in the manuscript:

- Regression analyses (e.g., `?fit_all_subsets()`)
- Model-averaging (e.g., `?predict_model_avg()`)
- Leave-one-out analyses (`?loo_pred()`, `?loo_pred_model_avg()`, `?whole_loo_analysis()`)
- Run the [interactive web application](https://bstaton.shinyapps.io/KuskoHarvPred-tool/) (`?run_predictive_tool()`)

The analyses take some time to run, so to streamline analyses, ‘KuskoHarvPred’ ships with several pre-compiled output objects, which are called repeatedly throughout the scripts in this repo:

- `KuskoHarvPred:::fit_data`: contains the data set used in regression modeling
- `KuskoHarvPred:::fit_lists`: contains all fitted regression models
- `KuskoHarvPred:::loo_output`: contains leave-one-out predictions and error summaries

Three other packages (and their dependencies) will simultaneously be installed: <img src="https://github.com/bstaton1/KuskoHarvPred/blob/v2023.3/inst/rstudio/KuskoHarvPred-tool/resources/all-logo-grouped.png?raw=true" align="right" height="250px"/>

- [‘KuskoHarvEst’](https://github.com/bstaton1/KuskoHarvEst) ([v1.3.0](https://github.com/bstaton1/KuskoHarvEst/releases/tag/v1.3.0)): contains the workflow to produce harvest estimates from interview and flight data.
- [‘KuskoHarvData’](https://github.com/bstaton1/KuskoHarvData) ([v2023.5](https://github.com/bstaton1/KuskoHarvData/releases/tag/v2023.5)): contains raw interview and aerial survey data, as well as compiled harvest and effort estimates, for each drift gillnet opener monitored by the lower Kuskokwim River in-season subsistence salmon harvest monitoring program.
- [‘KuskoHarvUtils’](https://github.com/bstaton1/KuskoHarvUtils) ([v0.2.0](https://github.com/bstaton1/KuskoHarvUtils/releases/tag/v0.2.0)): contains several helper functions used throughout the ‘KuskoHarv\*’ family of packages.

To ensure installation of the exact package versions used upon publication, install these packages as follows:

``` r
remotes::install_github("bstaton1/KuskoHarvUtils", ref = "v0.2.0")
remotes::install_github("bstaton1/KuskoHarvEst", ref = "v1.3.0")
remotes::install_github("bstaton1/KuskoHarvData", ref = "v2023.5")
```

### CRAN Packages

The table below shows all R packages (and their versions at time of publication; *typically* should not matter, included for completeness) that are specifically called (typically with `pkg::fn()`) or loaded (using `library(pkg)`).
Users will need to have these packages installed prior to executing any of the code in this repo.

<details>
<summary>
<b>Click to Expand/Hide Packages Table</b>
</summary>

| Package                                                            | Version | Description                                                                           |
|:-------------------------------------------------------------------|--------:|:--------------------------------------------------------------------------------------|
| [`DHARMa`](https://CRAN.R-project.org/package=DHARMa)              |   0.4.6 | Residual Diagnostics for Hierarchical (Multi-Level / Mixed) Regression Models         |
| [`htmltools`](https://CRAN.R-project.org/package=htmltools)        | 0.5.8.1 | Tools for HTML                                                                        |
| [`kableExtra`](https://CRAN.R-project.org/package=kableExtra)      |   1.4.0 | Construct Complex Table with ‘kable’ and Pipe Syntax                                  |
| [`knitr`](https://CRAN.R-project.org/package=knitr)                |    1.48 | A General-Purpose Package for Dynamic Report Generation in R                          |
| [`KuskoHarvPred`](https://www.github.com/bstaton1/KuskoHarvPred)   |  2023.3 | Harvest and Effort Predictions for Lower Kuskokwim River Subsistence Salmon Fisheries |
| [`KuskoHarvUtils`](https://www.github.com/bstaton1/KuskoHarvUtils) |   0.2.0 | Utility Functions to Support ‘KuskoHarv’ Family of Packages                           |
| [`lubridate`](https://CRAN.R-project.org/package=lubridate)        |   1.9.3 | Make Dealing with Dates a Little Easier                                               |
| [`MuMIn`](https://CRAN.R-project.org/package=MuMIn)                |  1.48.4 | Multi-Model Inference                                                                 |
| [`qgam`](https://CRAN.R-project.org/package=qgam)                  |   1.3.4 | Smooth Additive Quantile Regression Models                                            |
| [`reshape2`](https://CRAN.R-project.org/package=reshape2)          |   1.4.4 | Flexibly Reshape Data: A Reboot of the Reshape Package                                |
| [`rmarkdown`](https://CRAN.R-project.org/package=rmarkdown)        |    2.27 | Dynamic Documents for R                                                               |
| [`scales`](https://CRAN.R-project.org/package=scales)              |   1.3.0 | Scale Functions for Visualization                                                     |
| [`stringr`](https://CRAN.R-project.org/package=stringr)            |   1.5.1 | Simple, Consistent Wrappers for Common String Operations                              |
| [`this.path`](https://CRAN.R-project.org/package=this.path)        |   2.5.0 | Get Executing Script’s Path                                                           |

</details>

Running the code below will display the packages not already installed on your machine:

``` r
pkgs <- c("DHARMa", "htmltools", "kableExtra", "knitr", "KuskoHarvPred", "KuskoHarvUtils", "lubridate", "MuMIn", "qgam", "reshape2", "rmarkdown", "scales", "stringr", "this.path")
pkgs[!pkgs %in% rownames(installed.packages())]
```

## Reproducing Article Content

To reproduce all content presented in the manuscript, open [RStudio](https://posit.co/download/rstudio-desktop/) to the `KuskoHarvPred-ms-analysis.Rproj` location and execute:

``` r
fig_type = "pdf"                   # figure file extension; pdf or png
source("make-figures.R")           # exports figure files for manuscript
source("make-tables.R")            # exports .csv files for manuscript
source("make-residual-figures.R")  # exports residual diagnostic plots
```

After executing this code, the there will be two new subdirectories: `figures` and `tables` storing the content used in building the manuscript.

## Session Info

<details>
<summary>
<b>Click to Expand/Hide Session Info</b>
</summary>

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.4.1 (2024-06-14 ucrt)
    ##  os       Windows 11 x64 (build 22631)
    ##  system   x86_64, mingw32
    ##  ui       RTerm
    ##  language (EN)
    ##  collate  English_United States.utf8
    ##  ctype    English_United States.utf8
    ##  tz       America/Los_Angeles
    ##  date     2024-08-31
    ##  pandoc   3.1.11 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  ! package        * version  date (UTC) lib source
    ##    boot             1.3-30   2024-02-26 [2] CRAN (R 4.4.1)
    ##    cli              3.6.3    2024-06-21 [1] CRAN (R 4.4.1)
    ##    codetools        0.2-20   2024-03-31 [2] CRAN (R 4.4.1)
    ##    colorspace       2.1-1    2024-07-26 [1] CRAN (R 4.4.1)
    ##    DHARMa         * 0.4.6    2022-09-08 [1] CRAN (R 4.4.1)
    ##    digest           0.6.36   2024-06-23 [1] CRAN (R 4.4.1)
    ##    doParallel       1.0.17   2022-02-07 [1] CRAN (R 4.4.1)
    ##    evaluate         0.24.0   2024-06-10 [1] CRAN (R 4.4.1)
    ##    fastmap          1.2.0    2024-05-15 [1] CRAN (R 4.4.1)
    ##    foreach          1.5.2    2022-02-02 [1] CRAN (R 4.4.1)
    ##    generics         0.1.3    2022-07-05 [1] CRAN (R 4.4.1)
    ##    glue             1.7.0    2024-01-09 [1] CRAN (R 4.4.1)
    ##    htmltools      * 0.5.8.1  2024-04-04 [1] CRAN (R 4.4.1)
    ##    httpuv           1.6.15   2024-03-26 [1] CRAN (R 4.4.1)
    ##    iterators        1.0.14   2022-02-05 [1] CRAN (R 4.4.1)
    ##    kableExtra     * 1.4.0    2024-01-24 [1] CRAN (R 4.4.1)
    ##    knitr          * 1.48     2024-07-07 [1] CRAN (R 4.4.1)
    ##    KuskoHarvPred  * 2023.3   2024-08-31 [1] Github (bstaton1/KuskoHarvPred@cf202f9)
    ##    KuskoHarvUtils * 0.2.0    2024-08-14 [1] Github (bstaton1/KuskoHarvUtils@a8e6faa)
    ##    later            1.3.2    2023-12-06 [1] CRAN (R 4.4.1)
    ##    lattice          0.22-6   2024-03-20 [2] CRAN (R 4.4.1)
    ##    lifecycle        1.0.4    2023-11-07 [1] CRAN (R 4.4.1)
    ##    lme4             1.1-35.5 2024-07-03 [1] CRAN (R 4.4.1)
    ##    lubridate      * 1.9.3    2023-09-27 [1] CRAN (R 4.4.1)
    ##    magrittr         2.0.3    2022-03-30 [1] CRAN (R 4.4.1)
    ##    MASS             7.3-60.2 2024-04-26 [2] CRAN (R 4.4.1)
    ##    Matrix           1.7-0    2024-04-26 [2] CRAN (R 4.4.1)
    ##    mgcv           * 1.9-1    2023-12-21 [2] CRAN (R 4.4.1)
    ##    mime             0.12     2021-09-28 [1] CRAN (R 4.4.0)
    ##    minqa            1.2.7    2024-05-20 [1] CRAN (R 4.4.1)
    ##    MuMIn          * 1.48.4   2024-06-22 [1] CRAN (R 4.4.1)
    ##    munsell          0.5.1    2024-04-01 [1] CRAN (R 4.4.1)
    ##    nlme           * 3.1-164  2023-11-27 [2] CRAN (R 4.4.1)
    ##    nloptr           2.1.1    2024-06-25 [1] CRAN (R 4.4.1)
    ##    plyr             1.8.9    2023-10-02 [1] CRAN (R 4.4.1)
    ##    promises         1.3.0    2024-04-05 [1] CRAN (R 4.4.1)
    ##    qgam           * 1.3.4    2021-11-22 [1] CRAN (R 4.4.1)
    ##    R6               2.5.1    2021-08-19 [1] CRAN (R 4.4.1)
    ##    Rcpp             1.0.13   2024-07-17 [1] CRAN (R 4.4.1)
    ##    renv             1.0.7    2024-04-11 [1] CRAN (R 4.4.1)
    ##    reshape2       * 1.4.4    2020-04-09 [1] CRAN (R 4.4.1)
    ##    rlang            1.1.4    2024-06-04 [1] CRAN (R 4.4.1)
    ##    rmarkdown      * 2.27     2024-05-17 [1] CRAN (R 4.4.1)
    ##    rstudioapi       0.16.0   2024-03-24 [1] CRAN (R 4.4.1)
    ##    scales         * 1.3.0    2023-11-28 [1] CRAN (R 4.4.1)
    ##    sessioninfo      1.2.2    2021-12-06 [1] CRAN (R 4.4.1)
    ##    shiny            1.9.1    2024-08-01 [1] CRAN (R 4.4.1)
    ##    stringi          1.8.4    2024-05-06 [1] CRAN (R 4.4.0)
    ##    stringr        * 1.5.1    2023-11-14 [1] CRAN (R 4.4.1)
    ##    svglite          2.1.3    2023-12-08 [1] CRAN (R 4.4.1)
    ##    systemfonts      1.1.0    2024-05-15 [1] CRAN (R 4.4.1)
    ##  D this.path      * 2.5.0    2024-06-29 [1] CRAN (R 4.4.1)
    ##    timechange       0.3.0    2024-01-18 [1] CRAN (R 4.4.1)
    ##    vctrs            0.6.5    2023-12-01 [1] CRAN (R 4.4.1)
    ##    viridisLite      0.4.2    2023-05-02 [1] CRAN (R 4.4.1)
    ##    xfun             0.46     2024-07-18 [1] CRAN (R 4.4.1)
    ##    xml2             1.3.6    2023-12-04 [1] CRAN (R 4.4.1)
    ##    xtable           1.8-4    2019-04-21 [1] CRAN (R 4.4.1)
    ##    yaml             2.3.10   2024-07-26 [1] CRAN (R 4.4.1)
    ## 
    ##  [1] C:/Users/bstaton/AppData/Local/R/win-library/4.4
    ##  [2] C:/Program Files/R/R-4.4.1/library
    ## 
    ##  D ── DLL MD5 mismatch, broken installation.
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────

</details>
