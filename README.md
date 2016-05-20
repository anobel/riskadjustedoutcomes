# Beyond Classical Risk Adjustment
## Socioeconomic Status and Hospital Performance in Urologic Oncology

Anobel Y. Odisho MD MPH, Ruth Etzioni PhD, John L. Gore, MD MS

*Department of Urology, University of Washington; Fred Hutchinson Cancer Research Center*

### Abstract
**Importance**
Safety-net hospitals take care of a more patients from lower socioeconomic status (SES) than non-safety-net hospitals and may be disproportionately punished under readmission risk adjustment models that do not incorporate SES. Adequate risk adjustment is essential to ensure that reimbursement aligns with performance and is not influenced by patient selection. 

**Objective**
To develop a readmission risk adjustment framework that incorporates SES and to assess impact of SES on safety-net hospital readmission rate rankings.

**Design**
Quasi-experimental design using California Office of Statewide Health and Planning Data from 2007-2011. Unadjusted hospital rankings and predicted rankings under a base model, which simulated the Medicare Hospital Readmissions Reduction Program model (age, sex, comorbidity), were compared to predicted rankings under models incorporating SES and hospital factors. SES (multifactorial neighborhood score) at ZIP code level calculated from US Census data and hospital data was obtained from Medicare. 

**Setting**
All acute-care hospitals in California.

**Participants**
All patients undergoing radical cystectomy (RC) for bladder cancer (n = 3,771), partial nephrectomy (PN) (n = 5,556) and radical nephrectomy (RN) (n = 13,136) for kidney cancer.

**Main Outcomes and Measures**
Hospital rankings based on thirty-day all-cause acute-care readmission rate and differences between model predicted rankings. 

**Results**
The thirty-day readmission rate was 26.1% for RC, 8.3% in RN, and 9.5% in PN. For all procedures, the addition of SES, geographic, and hospital factors changed the overall hospital rankings significantly compared with the base model (p < 0.01), with the exception of SES in RC (p = 0.07) and SES and rural factors in PN (p = 0.12). For both RN and PN, the addition of SES and hospital factors significantly changed the mean ranking of SNHs. The mean ranking of SNHs increased when adding these factors, improving the ratio of observed relative to expected rankings (p < 0.01). For RC there was no significant change in rankings of SNHs with the addition of either SES, rural status, or hospital factors.

**Conclusions and Relevance**
Adding SES to existing Medicare readmission risk adjustment models leads to significant changes in hospital rankings, with a differential impact on safety-net hospitals.

### Data Sources
All data necessary to reproduce our results, except for private patient data, has been made available here both in raw and tidy formats. We do not have permission to make patient data publicly available, limiting our ability to make this work exactly reproducible. However, we have made every effort to make this work easy to replicate with your own patient data. All other data that was used is publicly available and the code to generate the tidy data from raw sources is available here. The data, in raw and tidy formats, is available in this GitHub repository in the *data* directory. In addition, at the time of manuscript submission, the data was deposited at to the Open Science Framework, frozen to prevent further editing, and assigned a Digital Object Identifier (DOI): [10.17605/OSF.IO/D643S](http://dx.doi.org/10.17605/OSF.IO/D643S).

### Description of Files
To re-create the entire tidy data set, the files should be executed in the following order.

**rao_sessioninfo.txt**: Detailed info regarding all software versions used at the time of final analysis.

**rao_processing_zip.R**: Create a ZIP to ZCTA crosswalk. This should be run *first8, as the crosswalk is used in many subsequent steps. This will create `data/tidy/zcta.csv` and `data/tidy/zctalatlon.csv`.

**rao_processing_oshpd.R**: This file imports raw OSHPD patient data (not available publicly), and tidies it in preparation for merging with additional datasets and final analysis. This will create `data/patient/tidy/pt.rds`.

**rao_processing_oshpdfinancial.R**: Download California Office of Statewide Health and Planning data to create a crosswalk from OSHPD ID to Medicare ID, used for merging OSHPD and Medicare data. This creates `data/tidy/oshpdxwalk.csv`.

**rao_processing_DSH.R**: Download and tidy Medicare data for hospital factors, Disproportionate Share Index, Case Mix Index. This creates `data/tidy/dsh.csv`.

**rao_processing_icd9cm.R**: create labels for ICD9 codes from Medicare data. This creates `data/tidy/icd9cm.csv`.

**rao_processing_medicarepos.R**: Download Medicare Provider of Service files and obtain number of FTE residents per facility. This creates `data/tidy/residents.csv`.

**rao_processing_drivingdistances.R**: This uses ggmap to calculate driving times and distances from patient to hospital zip centroids. This creates `data/tidy/distances.csv`.

**rao_processing_rural.R**: Assign RUCA codes at the zip code level for each patient. This creates `data/tidy/rural.csv`.

**rao_processing_spatial.R**: Merge data with TIGER/LINE shape files for creation of interactive maps. This creates `data/tidy/spatial.rda`.

**rao_processing_acs.R**: Using the `acs` package, data is downloaded directly from the US Census American Community Survey for California and Diez-Roux Neighborhood scores are calculated. This is then saved as `data/tidy/acs.csv` for analysis. This creates `data/tidy/acs.csv`.

**rao_processing_all.R**: This file merges tidy patient data with additional datasets to create the final analytic cohorts. This should be executed **LAST**. This creates `data/tidy/ptcombined.rds` `data/tidy/ptgu.rds`.


### Results
`floating.css` is necessary for the left-sided navigation menu used for all markdown reports.

**rao_describe_oshpd.Rmd**: Basic decriptive analyses of all variables from mulitple data sources and detailed output of all statistical models for each cohort. To create results for each cohort, name the desired cohort (`Cystectomy`, `PartialNx`, or `RadNx`) as a paramater in the `YAML`. 

**rao_tables.Rmd** Generates Tables 1 and 2 in the manuscript, which include all basic descriptive analyses by cohort and univariate tests of significance.

**rao_oecalculations.Rmd** Generates Tables 3 and 4 in the manuscript. This includes detailed model output for all comparisons in all three cohorts, along with tests of significance (Wilcoxon rank-sum and permutation testing).

