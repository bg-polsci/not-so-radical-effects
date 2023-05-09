# not-so-radical-effects
This repository contains the replication materials for the dissertation 'Not so Radical Effects: Estimating the Causal Impact of Radical Right Representation on Political Support', submitted in part-fulfilment of the BSc Philosophy, Politics and Economics at University College London.

The paper seeks to estimate the causal impact of radical right representation on political support - defined as a combination of political trust and satisfaction with democracy - among supporters of both mainstream and radical right parties. 


## Repository contents

### Data folder
The [Data](./Data/) folder contains the raw datasets used in the analysis:
- [GLES panel](./Data/GLES%20panel/): contains waves 1 through 21 of the German Longitudinal Election Study, obtained from GESIS - Leibniz Institut für Sozialwissenschaften.
- [Constituency structural data](./Data/Constituency%20structural%20data/): contains all covariates, which have been obtained from the Statistical Officer for the Länder ([land_surface.csv](./Data/Constituency%20structural%20data/land_surface.csv), [total_population.csv](./Data/Constituency%20structural%20data/total_population.csv), [total_foreigners.csv](./Data/Constituency%20structural%20data/total_foreigners.csv)), the Federal Employment Agency ([unemployment_2017_2018.xlsx](./Data/Constituency%20structural%20data/unemployment_2017_2018.xlsx), [unemployment_2020_2021.xlsx](./Data/Constituency%20structural%20data/unemployment_2020_2021.xlsx)), and the conversion table that is used to aggregate administrative-district-level covariates to the constituency level.
- [Constituency returns](./Data/Constituency%20returns/): contains [afd_results.xlsx](./Data/Constituency%20returns/afd_results.xlsx), which shows the results of AfD by Land in the 2017 and 2021 federal elections; this data was manually collected from the Federal Returning Officer website.

**Please note: the data containing the constituency of residence of the GLES survey respondents can only be obtained directly from GESIS – Leibniz Institute for the Social Sciences as it is sensitive data**. 

### Code folder
The [Code](./Code/) folder contains the R files to reproduce the analysis in the paper:
- [Core analysis.R](./Code/Core%20analysis.R): code to reproduce the analysis described in the main body of the paper.
- [Appendix B.R](./Code/Appendix%20B.R): code to compute descriptive statistics for the outcome variables.
- [Appendix C.R](./Code/Appendix%20C.R): code to test for the parallel trends assumption.
- [Appendix D.R](./Code/Appendix%20D.R): code to run the attrition analysis.
- [Appendix E.R](./Code/Appendix%20E.R): code to run all design and specification robustness checks.
- [Figures.R](./Code/Figures.R): file to reproduce the figures in the main body of the paper.

Please note: the code must be run in this order to avoid errors.
