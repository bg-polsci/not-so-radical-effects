# not-so-radical-effects
This repository contains the replication materials for the dissertation 'Not so Radical Effects: Estimating the Causal Impact of Radical Right Representation on Political Support', submitted in part-fulfilment of the BSc Philosophy, Politics and Economics at University College London.

### Repository contents

##### Data folder
The [Data](./Data/) folder contains the raw datasets used in the analysis:
- [GLES panel](./Data/GLES panel): contains waves 1 through 21 of the German Longitudinal Election Study, obtained from GESIS - Leibniz Institut für Sozialwissenschaften.
- [Constituency structural data](./Data/Constituency structural data): contains all covariates, which have been obtained from the Statistical Officer for the Länder ([land_surface.csv](./Data/Constituency structural data/land_surface.csv), [total_population.csv](./Data/Constituency structural data/total_population.csv), [total_foreigners.csv](./Data/Constituency structural data/total_foreigners.csv)), the Federal Employment Agency ([unemployment_2017_2018.xlsx](./Data/Constituency structural data/unemployment_2017_2018.xlsx), [unemployment_2020_2021.xlsx](./Data/Constituency structural data/unemployment_2020_2021.xlsx)), and the conversion table that is used to aggregate administrative-district-level covariates to the constituency level.
- [Constituency returns](./Data/Constituency returns): contains [afd_results.xlsx](./Data/Constituency returns/afd_results.xlsx), which shows the results of AfD by Land in the 2017 and 2021 federal elections; this data was manually collected from the Federal Returning Officer website.

**Please note: the data containing the constituency of residence of the GLES survey respondents can only be obtained directly from GESIS – Leibniz Institut für Sozialwissenschaften as it is sensitive data**. 

##### R code folder
The [R code](./R code/) folder contains the R files to reproduce the analysis in the paper:
- [Core analysis.R](./R code/Core%20analysis.R): code to reproduce the analysis described in the main body of the paper.
- [Appendix C.R](./R code/Appendix%20C.R): code to test for the parallel trends assumption.
- [Appendix D.R](./R code/Appendix%20D.R): code to run the attrition analysis.
- [Appendix E.R](./R code/Appendix%20E.R): code to run all design and specification robustness checks.
- [Figures.R](./R code/Figures.R): file to reproduce the figures in the main body of the paper.

Please note: the code must be run in this order to avoid errors.
