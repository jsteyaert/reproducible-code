# UU Reproducible Code Workshop
created by Jennie C. Steyaert on December 1st, 2022
j.c.steyaert@uu.nl

## Project organization
- PG = project-generated
- HW = human-writable
- RO = read only
```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── other_data     <- Other data needed to run, like standardized indices from outsdie partners
│   ├── SSI_HUC        <- Standardized Streamflow indices for all regions (PG)
│   └── HUC_FF         <- Fraction Filled Values by HUC2 region (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── scripts            <- Contains scripts to run figures 2-5

```
## Citations for Datasets Used:
1. Global Reservoirs and Dams Database from 
  Lehner, B., C. Reidy Liermann, C. Revenga, C. Vörösmarty, B. Fekete, P. Crouzet, P. Döll, M. Endejan, K. Frenken, J. Magome, C. Nilsson, J.C. Robertson,
  R. Rodel, N. Sindorf, and D. Wisser. 2011. High-resolution mapping of the world’s reservoirs and dams for sustainable river-flow management. Frontiers in
  Ecology and the Environment 9 (9): 494-502.
2. Standardized Precipitation Index from the National Center for Atmospheric Research
   https://climatedataguide.ucar.edu/climate-data/standardized-precipitation-index-spi
3. Standardized Streamflow Index Calculations from 
   Vicente-Serrano Sergio, M., López-Moreno Juan, I., Beguería, S., Lorenzo-Lacruz, J., Azorin-Molina, C., and Morán-Tejeda, E.: Accurate Computation of a
   Streamflow Drought Index, Journal of Hydrologic Engineering, 17, 318-332, 10.1061/(ASCE)HE.1943-5584.0000433, 2012.


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)
