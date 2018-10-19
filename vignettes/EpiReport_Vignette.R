## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE-------------------------------------------------------
pkgVersion <- packageDescription("EpiReport")$Version
pkgDate <- packageDescription("EpiReport")$Date
authorsString <- packageDescription("EpiReport")$Author
pkgMaintainer <- packageDescription("EpiReport")$Maintainer
pkgLicense <- packageDescription("EpiReport")$License
pkgUrl <- packageDescription("EpiReport")$URL

## ---- echo=FALSE, results='asis'-----------------------------------------
my_dataset <- EpiReport::SALM2016
knitr::kable(head(my_dataset), caption = "__Tab.1 Example of Pertussis data exported from the ECDC Atlas__")

