#' Capitalise the first letter
#'
#' Capitalise the first letter
#'
#' @param str character string to capitalise as a title
#' @return character string
#'
toCapTitle <- function(str) {
  paste(toupper(substring(str, 1,1)), substring(str, 2),
        sep="", collapse=" ")
}





#' AgeGender
#'
#' Function drawing ....
#'
#' @param disease character string for the health topic
#' @param year numeric
#' @param x dataset including required data for AE
#' @return A table
AgeGenderBarGph <- function(disease, year, x){

  if(missing(x)) {
    x <- EpiReport::SALM2016
  }

  parameters <- EpiReport::AERparams
  parameters <- dplyr::filter(parameters, parameters$HealthTopic == disease)
  x <- dplyr::filter(x, x$HealthTopic == disease)

  if (parameters$AgeGenderBarGraphUse =="Y"    #--AgeGender bar graph
     & parameters$AgeGenderRatesUse =="Y") {    #-- with Rates

    if(parameters$MeasurePopulation=="CONFIRMED"){    #-- Confirmed cases

      x <- dplyr::filter(x, x$Indicator == "Male to female ratio - AER Only (not visible in Public Atlas)"
                         & x$Population == "Confirmed cases"
                         & x$Time == year
                         & x$RegionCode== "EU_EEA31")

    }


#----------------------------------------------------------------------------------------------------------

    # if(parameters$MeasurePopulation=="ALL"){
    #   bargraph = subset(tessy , ((measurelabel == "Age and gender" | measurelabel == "Age and gender - AER Only (not visible in Public Atlas)") &
    #                                measurecode == "ALL.AGE_GENDER.RATE" &
    #                                timeunit == "Y" & timecode == YEAR & geocode == "EU_EEA31") )
    # }
    #
    # bargraph$xlabel = ifelse(bargraph$xlabel == "May-14", "5-14",bargraph$xlabel)
    # bargraph$xlabel = ifelse(bargraph$xlabel == "01-Apr", "1-4",bargraph$xlabel)
    # bargraph$xlabel = ifelse(bargraph$xlabel == "05-Sep", "5-9",bargraph$xlabel)
    # bargraph$xlabel = ifelse(bargraph$xlabel == "Oct-14", "10-14",bargraph$xlabel)
    #
    # #-----> Ordering the labels for gender variable
    # bargraph$ylabel = factor(bargraph$ylabel, c("Male","Female"))
    #
    #
    # ## Adjust to disaese specific age groups
    # if(DIS=="PNEU" | DIS=="LIST" | DIS=="HAEINF" ){
    #   ## Ordering the labels for age group variable
    #   bargraph$xlabel <- factor(bargraph$xlabel, levels = c("<1","1-4","5-14","15-24", "25-44", "45-64", "65+"))
    #
    #   ## Exclude NA
    #   bargraph <- bargraph[bargraph$xlabel %in% c("<1", "1-4","5-14","15-24", "25-44", "45-64", "65+"), ]
    #
    # }
    # else if (DIS=="MENI") {
    #
    #   ## Ordering the labels for age group variable
    #   bargraph$xlabel <- factor(bargraph$xlabel, levels = c("<1","1-4","5-14","15-24", "25-49", "50-64", "65+"))
    #
    #   ## Exclude NA
    #   bargraph <- bargraph[bargraph$xlabel %in% c("<1", "1-4","5-14","15-24", "25-49", "50-64", "65+"), ]
    #
    # }
    # else if (DIS=="BOTU") {
    #
    #   ## Ordering the labels for age group variable
    #   bargraph$xlabel <- factor(bargraph$xlabel, levels = c("<1","1-4","5-14", "15-24", "25-44", "45-64", "65+"))
    #
    #   ## Exclude NA
    #   bargraph <- bargraph[bargraph$xlabel %in% c("<1","1-4","5-14", "15-24", "25-44", "45-64", "65+"), ]
    #
    # }
    # else if (DIS=="GONO" | DIS =="CHLAM" | DIS =="SYPH") {
    #
    #   ## Ordering the labels for age group variable
    #   bargraph$xlabel <- factor(bargraph$xlabel, levels = c("0-14","15-24","25-34", "35-44", "45+"))
    #
    #   ## Exclude NA
    #   bargraph <- bargraph[bargraph$xlabel %in% c("0-14","15-24","25-34", "35-44", "45+"), ]
    #
    # }
    #
    # else if (DIS=="HEPB" | DIS =="HEPC") {
    #
    #   ## Ordering the labels for age group variable
    #   bargraph$xlabel <- factor(bargraph$xlabel, levels = c("0-4", "5-14", "15-19" ,"20-24" ,"25-34" , "35-44", "45-54", "55-64", "65+"))
    #
    #   ## Exclude NA
    #   bargraph <- bargraph[bargraph$xlabel %in% c("0-4", "5-14", "15-19" ,"20-24" ,"25-34" , "35-44", "45-54", "55-64", "65+"), ]
    #
    # }
    # else if (DIS=="MUMP" | DIS=="PERT") {
    #
    #   ## Ordering the labels for age group variable
    #   bargraph$xlabel <- factor(bargraph$xlabel, levels = c("<1","1-4","5-9","10-14", "15-19", "20-29", "30+"))
    #
    #   ## Exclude NA
    #   bargraph <- bargraph[bargraph$xlabel %in% c("<1","1-4","5-9","10-14", "15-19", "20-29", "30+"), ]
    #
    # }
    #
    # else {
    #   ## Ordering the labels for age group variable
    #   bargraph$xlabel <- factor(bargraph$xlabel, levels = c("0-4","5-14","15-24", "25-44", "45-64", "65+"))
    #
    #   ## Exclude NA
    #   bargraph <- bargraph[bargraph$xlabel %in% c("0-4","5-14","15-24", "25-44", "45-64", "65+"), ]
    #
    # }
    #
    #
    # # Setting the breaks and limits for the FIGURE 2 to be nice
    # FIG2BREAKS <- pretty(seq(0, max(bargraph$zvalue),
    #                          by = max(bargraph$zvalue)/5))
    #
    # #-----> Ploting age and gender bar graph
    #
    # p = ggplot(data=bargraph, aes(x=xlabel, y=zvalue, fill=ylabel)) +
    #   geom_bar(stat="identity", position=position_dodge()) +
    #   scale_fill_manual(values=c(COLOR1, COLOR2)) +
    #   scale_y_continuous(expand = c(0, 0), limits = c(0, max(FIG2BREAKS)), #
    #                      breaks = FIG2BREAKS) +
    #   labs(title="", x="Age (years)",y=paste(substring(parameters$AgeGender.bar.graph.label,1,1),
    #                                          tolower(substring(parameters$AgeGender.bar.graph.label,2)), sep="")) +         #Axis labels
    #   theme(axis.text=element_text(size=8, family = "Tahoma"), axis.title=element_text(size=9, family = "Tahoma")) +    #Axis text style
    #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  #Empty backgroud
    #   theme(axis.line = element_line(colour = COLORAXIS), axis.line.x = element_blank()) +
    #   theme(legend.position = "right", legend.title =element_blank(), legend.text =element_text(size=8, family="Tahoma"),
    #         legend.key.width = unit(0.8, "cm"), legend.key.size = unit(0.4, "cm"))
    #
    # #-----> Replacing the BARGPH_AGEGENDER_BOOKMARK with ggplot plot and a caption
    # my_text_prop <- textProperties(color='#000000',
    #                                font.size = 9, font.weight = 'bold', font.family = 'Tahoma' )
    #
    # # Create the caption as piece of text (pot)
    # if(parameters$MeasurePopulation=="CONFIRMED") {
    #   mypot <- pot(paste("Figure ", FIG_NO, ". Distribution of confirmed ",parameters$Label,
    #                      " cases per 100 000 population, by age and gender, ",
    #                      "EU/EEA, ", YEAR, sep = ""),
    #                format = my_text_prop)
    # }else{
    #   mypot <- pot(paste("Figure ", FIG_NO, ". Distribution of ",parameters$Label,
    #                      " cases per 100 000 population, by age and gender, ",
    #                      "EU/EEA, ", YEAR, sep = ""),
    #                format = my_text_prop)
    #
    # }
    #
    # # Add the caption
    # doc = addParagraph(doc, value = mypot, bookmark = "BARGPH_AGEGENDER_CAPTION"  )
    # #Add the plot
    # doc = addPlot( doc , fun = function() print(p) , bookmark = "BARGPH_AGEGENDER_BOOKMARK", width = 6, height = 4 )
    # # Add to the figure numbering
    # FIG_NO <- FIG_NO+1

#----------------------------------------------------------------------------------------------------------

    }


}








#' Time series graph
#'
#' Function drawing the first table of the AER: Number of cases by Member States over the last 5 years
#' @param x dataset including required data for AER
#' @return A table
#' @export
tseasonalityByMS <- function(x){


  ## ----
  ## Setting default arguments if missing
  ## ----

  if(missing(x)){ x <- EpiReport::SALM2016 }

  ## ----
  ## Plot output
  ## ----

  .p <- suppressWarnings( ggplot2::ggplot(x, ggplot2::aes(x$GeoCode, x$XValue))) +
    suppressWarnings(ggplot2::geom_bar(stat="identity"))

  .p


}








