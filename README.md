# DeepDive-Input

This repository contains the scripts and data used to prepare the input files required for DeepDive analyses, as described in the manuscript:

**"A new analysis of the shark and ray fossil record reveals hidden diversity patterns"**  
*A. Gardiner1, G. H. Mathes1, R. Cooper2,3, K. Kocáková1, J.A. Villafaña4, D. Silvestro2,3,5*, C. Pimiento1,6*\*

¹ Department of Paleontology, University of Zurich, Zurich, 8006, Switzerland  
² Department of Biology, University of Fribourg, Fribourg, CH-1700, Switzerland  
³ Swiss Institute of Bioinformatics, Lausanne, 1015, Switzerland  
⁴ Departamento de Ecología, Facultad de Ciencias, Universidad Católica de la Santísima Concepción, Concepción 4090541, Chile  
⁶ Department of Biological and Environmental Sciences, University of Gothenburg, Gothenburg, 405 30, Sweden  
⁷ Department of Biosciences, Swansea University, Swansea, SA2 8PP, United Kingdom  

\*Contributed equally and are the corresponding authors: [daniele.silvestro@unifr.ch](mailto:daniele.silvestro@unifr.ch); [catalina.pimientohernandez@pim.uzh.ch](mailto:catalina.pimientohernandez@pim.uzh.ch)

---

## Overview

This repository includes scripts and data for generating the input files necessary for running DeepDive analyses. These inputs include:

- A `.csv` file containing formatted occurrence data for DeepDive.
- A `.ini` configuration file specifying model parameters and settings.

The repository ensures that data is properly cleaned, formatted, and validated to meet the requirements of DeepDive.


## Packages  
 
The following packages are required: 
*tidyverse*
*here*
*readxl*
*janitor*
*DeepDiveR*

All but the *DeepDiveR* package can be installed from CRAN using the **install.packages()** function. The *DeepDiveR* package can be installed by following the instructions [here](https://github.com/DeepDive-project/DeepDiveR).

## R Scripts

There are two R Scripts in this repository. The first one, *extant_species.R* simply calculates how many extant neoselachian species there are based on two different compilations. The second one, *sharks_runner.R*, prepares all input files for DeepDive and saves them in the data/ folder. 

## DeepDive

The input files were then used to run the DeepDive analysis on a HPC cluster. First, we installed the DeepDive python package in a virtual environment on the cluster following [these instruction](https://github.com/DeepDive-project/deepdive). We then activated the virtual environment and launched DeepDive for each init file separately using
```
python run_dd_config.py input_file.ini -cpu 70
```
where *input_file.ini* is replaced by the focal input, e.g. squaliformes_species.ini for the order squaliformes on species level. 
