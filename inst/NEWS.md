EpiReport 1.0.1 (2021-02-04)
==================

### BUG FIXES

* Allowing NA values in functions
  + `plotBar()`
  + `plotBarGrouped()`
  + `plotSeasonality()`
  + `plotTS()`
  + `plotTSGrouped()`


EpiReport 1.0.0 (2020-12-18)
==================

### BUG FIXES

* Fixing the issue of mix between zero reporting and missing reports in the Trend and Season plots.


### NEW FEATURES

* New and more recent default dataset: DENGUE2019
* Allowing extra stratification for Table 1 on disease stage (i.e. HEPB and HEPC diseases)
* Allowing automatic landscape mode for Table 1 when extra stratification 
on acute vs. chronic disease is required
* Trend and Season plots are now followed by the list of countries reporting consistently
over the study period and included in the plot.
* Making the function `shapeECDCFlexTable()` more generic and adaptable to any kind of table.
Adding an option for having the last row in bold.
* `EpiReport::MSCode` reference table now have an extra column to allow the article "the" 
for some country name (e.g. The United Kingdom, The Netherlands)
* Adding the ECDC color palette with the new function `EcdcColors()`


### NEW FUNCTIONS

* In order to allow the user to produce additional outputs in the report 
and following the ECDC guidelines for presentation of surveillance data,
additional generic functions are now available:
  + `plotBar()`: plot bar graph
  + `plotBarGrouped()`: plot grouped bar graph
  + `plotBarH()`: plot horizontal bar graph
  + `plotBarGroupedH()`: plot grouped horizontal bar graph
  + `plotPie()`: plot pie chart
  + `plotTS()`: plot time series
  + `plotTSGrouped()`: plot grouped time series
  
* `EcdcColors()`
* `body_replace_gg_at_bkm()`


### BEHAVIORAL CHANGE

* Reordering of the outputs in the report.
* Renaming the default report name with a shorten name.
* Because of the updated version of the `officer` and `flextable` package,
existing outputs were not replaced when the report would be re-run.
Each output is now correctly replaced using new functions from these packages.
WARNING: you may have to update your bookmarks according to these new functions requirements.


### MINOR IMPROVEMENTS

* Bookmarks names have been simplified.
* Cleaning the Template accordingly and dropping bookmarks not used 
* Improving error messages (e.g. provide clear error message of the study period is incomplete).
* We now have two numbering: one for Figures and on for Tables.
* Numbering of maps in the report is now improved and take into account multiple maps.
* The country "Czech Republic" is now "Czechia" in `EpiReport::MSCode` reference table.
* Style of outputs caption is improved in the template provided in the package.

  
### DOCUMENTATION

* Vignettes have been updated with the list of bookmarks required in the Microsoft Word template.
 
  
### DEPRECATED

* The functions `plotAgeGender()` and `plotAge()` are still available in the 
package but they are discouraged. Instead, the user is encouraged to use the 
functions `plotBarGrouped()` and `plotBar()` respectively. 

<br>



EpiReport 0.1.1 (2020-05-11)
==================

### BUG FIXES

* adapting to new R and R packages versions

<br>



EpiReport 0.1.0 (2018-11-14)
==================
* First release of the EpiReport package on CRAN!
