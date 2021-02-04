# '_EpiReport_' package
European Centre for Disease Prevention and Control (ECDC)

# Description

The EpiReport package allows the user to draft an epidemiological report similar to the __ECDC Annual Epidemiological 
Reports__ (see https://ecdc.europa.eu/en/annual-epidemiological-reports) 
in Microsoft Word format for a given disease `getAER()`.

Through standalone functions, the package is specifically designed to generate
each disease-specific output presented in these reports. 

The package includes:

* __Table__ with the distribution of cases by Member State 
over the last five years `getTableByMS()`
* __Seasonal plot__ with the distribution of cases at EU/EEA level, 
by month, over the past five years `getSeason()`
* __Trend plot__ with the trend and number of cases at EU/EEA level, by month, 
over the past five years `getTrend()`
* __Age and gender bar graph__ with the distribution of cases at EU/EEA level
`getAgeGender()`


Two types of datasets can be used:

* The default dataset included in the `EpiReport` package which includes 
Denger data for 2015-2019: `EpiReport::DENGUE2019`;
* Any dataset specified as described in the package vignette.

