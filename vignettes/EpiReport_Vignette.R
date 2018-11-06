## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE-------------------------------------------------------
pkgVersion <- packageDescription("EpiReport")$Version
pkgDate <- packageDescription("EpiReport")$Date
authorsString <- gsub("^ *|(?<= ) |\n| *$", "", 
                      packageDescription("EpiReport")$Authors, perl = TRUE)
authorList <- eval(parse(text = authorsString))
pkgAuthors <- paste(format(authorList, 
                           include = c("given", "family", "email", "comment"), 
                           braces = list(email = c("<", ">,<br />"), comment = c("", ""))), 
                    collapse = "<br /><br />")
pkgMaintainer <- packageDescription("EpiReport")$Maintainer
pkgLicense <- packageDescription("EpiReport")$License
pkgUrl <- packageDescription("EpiReport")$URL

## ---- echo=FALSE, results='asis'-----------------------------------------
my_dataset <- EpiReport::SALM2016
my_dataset <- dplyr::select(my_dataset, 
                            c("HealthTopicCode", "MeasurePopulation", "MeasureCode",
                              "TimeUnit", "TimeCode", "GeoCode", "XLabel",
                              "YLabel", "ZValue", "YValue", "N"))
knitr::kable(my_dataset[sample(1:nrow(EpiReport::SALM2016), 10), ], 
             row.names = FALSE,
             format = "html", table.attr = 'class="myTable"',
             caption = "__Tab.1 Example of Salmonellosis data 2012-2016__")

## ---- echo=FALSE, results='asis'-----------------------------------------
my_dataset <- EpiReport::AERparams
my_dataset <- dplyr::select(my_dataset, 
                            c("HealthTopic", "MeasurePopulation",
                              "TableUse", "AgeGenderUse",
                              "TSTrendGraphUse", "TSSeasonalityGraphUse",
                              "MapNumbersUse", "MapRatesUse", "MapASRUse"))
knitr::kable(my_dataset[sample(1:nrow(EpiReport::AERparams), 5), ], 
             row.names = FALSE,
             format = "html", table.attr = 'class="myTable"',
             caption = "__Tab.2 Example of the main columns of the parameter dataset__")

## ---- echo=FALSE, results='asis'-----------------------------------------
my_dataset <- EpiReport::MSCode
knitr::kable(my_dataset[sample(1:nrow(EpiReport::MSCode), 5), ], 
             row.names = FALSE,
             format = "html", table.attr = 'class="myTable"',
             caption = "__Tab.3 Example of geographical codes and associated labels__")

## ---- eval=FALSE---------------------------------------------------------
#  getAER()

## ---- eval=FALSE---------------------------------------------------------
#  output <- "C:/EpiReport/doc/"
#  getAER(outputPath = output)

## ---- warning=FALSE------------------------------------------------------
# --- Importing the dataset
PERT2016 <- read.table("data/PERT2016.csv", 
                       sep = ",", 
                       header = TRUE, 
                       stringsAsFactors = FALSE)

# --- Specifying the folder containing pertussis maps
pathMap <- paste(getwd(), "/maps", sep = "")


# --- (optional) Setting the local language in English for month label
Sys.setlocale("LC_TIME", "C")

# --- Producing the report
EpiReport::getAER(disease = "PERT", 
       year = 2016, 
       x = PERT2016, 
       pathPNG = pathMap)

## ---- eval = FALSE-------------------------------------------------------
#  getTemplate(output_path = "C:/EpiReport/template")

## ---- eval = FALSE-------------------------------------------------------
#  getAER(template = "C:/EpiReport/template/New_AER_Template.docx",
#         outputPath = "C:/EpiReport/doc/")

## ------------------------------------------------------------------------
EpiReport::getTableByMS()

## ------------------------------------------------------------------------
ZIKV2016 <- read.table("data/ZIKV2016.csv", 
                       sep = ",", 
                       header = TRUE, 
                       stringsAsFactors = FALSE)
EpiReport::getTableByMS(x = ZIKV2016, 
             disease = "ZIKV", 
             year = 2016)

## ---- fig.width = 7------------------------------------------------------
# --- Salmonellosis 2016 plot
EpiReport::getSeason()

## ---- fig.width = 7------------------------------------------------------
# --- Pertussis 2016 plot
EpiReport::getSeason(x = PERT2016,
                     disease = "PERT",
                     year = 2016)

## ---- fig.width = 7------------------------------------------------------
# --- Salmonellosis 2016 plot
EpiReport::getTrend()

## ---- fig.width = 7------------------------------------------------------
# --- Pertussis 2016 plot
EpiReport::getTrend(x = PERT2016,
                    disease = "PERT",
                    year = 2016)

## ---- fig.width = 7, fig.height=5, fig.retina=4, results='hide'----------
# --- Salmonellosis 2016 map
EpiReport::getMap()

## ---- eval = FALSE-------------------------------------------------------
#  # --- Pertussis 2016 map
#  EpiReport::getMap(disease = "PERT",
#                    year = 2016,
#                    pathPNG = "C:/EpiReport/maps/")

## ---- fig.width = 7------------------------------------------------------
# --- Salmonellosis 2016 bar graph
EpiReport::getAgeGender()

## ---- fig.width = 7------------------------------------------------------
# --- Zika 2016 bar graph
EpiReport::getAgeGender(x = ZIKV2016, 
                        disease = "ZIKV", 
                        year = 2016)

