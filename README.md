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
## 

## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)
