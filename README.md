
<!-- README.md is generated from README.Rmd. Please edit that file -->

# General overview

This repository contains all data, scripts and functions used to
generate the Figure 1 of the paper “Three pathways to better recognize
the expertise of Global South researchers” accepted for publication in
npj Biodiversity journal.

In this study we brought secondary data from literature that evidence
colonial imprints in academic expertise, and propose three pathways to
reduce these biases.

# Repository structure

## Folders

- data: contains all data needed to perform the analysis.

  - `raw` contains all .txt files downloaded from Web of Science
    containing all records from 1000 top single author papers from
    different regions (defined accordingly World Bank - East Asia, Latin
    America, Middle and East Africa, South Asia, Sub-Sahara and
    USA-Canada)
  - `processed` contains all .txt in `raw` after being processed using
    Bibliometrix package. Subfolders `*_citing_articles` contains
    information on articles that cited the top 1000 papers in raw for
    each region.

- doc: .doc documents containing the manuscript

- output: contains all results, including the figures presented in the
  main text of this study. `res_boot_*.rds` are files containing results
  from rarefaction analysis described in
  `02_D_rarefaction_citation.Rmd`. Figure 1 and its components were
  generated using file `03_S_figure1-main-text.Rmd`

- R: contains all Rmardown files with all analyctical procedures to run
  the analysis using literature data present in this study.
  `01_C_getting_processing.Rmd` contains all the querries used in Web of
  Science and the processing workflow used to extract the information
  needed in downstream analysis using Bibliometrix package.
  `02_D_GeoMarkers.Rmd` and `02_D_rarefaction_citation.Rmd` contains,
  respectively the scripts to get the number of geographical markers and
  rarefied citation values for each country in each World Bank region
  used in this study. `03_S_figure1-main-text.Rmd` is used to produce
  the Figure in the article.

# Downloading the repository

The user can download this repo to a local folder in your computer or
clone it:

## downloading all files

``` r
download.file(url = "https://github.com/GabrielNakamura/Decolonizing_expertise/archive/main.zip", destfile = "Decolonizing_expertise.zip")
```

to unzip the .zip file in your computer type

``` r
unzip(zipfile = "Decolonizing_expertise.zip")
```

# Authors

Gabriel Nakamura, Bruno Eleres Soares, Valério D. Pillar, José Alexandre
F. Diniz-Filho and Leandro Duarte
