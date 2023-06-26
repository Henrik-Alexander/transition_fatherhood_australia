# Transition to fatherhood in Australia
This repository contains materials to reproduce the results from the study "Transition to fatherhood. Insights from the HILDA panel"



# Software & Hardware
The analysis were executed in [**R**](https://www.r-project.org/) version 4.2.1 (2022-06-23 ucrt). The computing unit was platform x86_64-w64-mingw32/x64 (64-bit).
The program was running under Windows Server x64 (build 17763)


# Directory Structure
The structure of the repository is as follows:

```
.
├-- .gitignore
├-- code
│   ├── 01-Data_preperation.R	<- Loads the general data
│   ├── 02-Fertility_data.R    <- Cleans and analases
├-- raw
│   ├── Stata-files
│	        └── biobirth.dta
│	        └── ppfad.dta
│	        └── hgen.dta
│	        └── pl_reduced.txt
│	        └── biocouplm.dta
├-- figures
├-- functions
│   ├── Packages.R		       <- Installs and loads the packages
│   ├── Graphics.R           <- Sets the graphic style
│   ├── Functions.R          <- Installs the functions
├-- results
├-- README.md
└── META.R		   <- Runs the entire project

```
