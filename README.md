# '_EpiReport_' package
EpiConcept / European Centre for Disease Prevention and Control (ECDC)

# Description

The EpiReport package allows the user to draft the __ECDC Annual Epidemiological 
Reports__ (see https://ecdc.europa.eu/en/annual-epidemiological-reports) 
in Microsoft Word format for a given disease `getAER()`.

Through standalone functions, the package is specifically designed to generate
each disease-specific output presented in these reports, using ECDC Atlas export data. 

The package includes:

* __Table__ with the distribution of cases by Member State 
over the last five years `getTableByMS()`
* __Seasonal plot__ with the distribution of cases at EU/EEA level, 
by month, over the past five years `getSeason()`
* __Trend plot__ with the trend and number of cases at EU/EEA level, by month, 
over the past five years `getTrend()`
* __Age and gender bar graph__ with the distribution of cases at EU/EEA level
`getAgeGender()`


Three types of datasets can be used:

* The default dataset included in the `EpiReport` package which includes 
Salmonellosis data for 2012-2016 exported from the ECDC Atlas (restricted access): `EpiReport::SALM2016`;
* A complete ECDC Atlas export from http://atlas.ecdc.europa.eu/public/index.aspx 
(csv format from the ECDC Atlas restricted access)
* Any dataset specified as described in the package vignette.

