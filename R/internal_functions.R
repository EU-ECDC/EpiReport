#' Get AER Word template
#'
#' Function to get the standard Word template for ECDC Annual Epidemiological Report
#' 
#' @param output_path The path where to create the Word output. 
#' Defaut location will be the current working directory (getwd())
#' @usage getTemplate(output_path)
#' @return A word document
#' @examples 
#' getTemplate(output_path = "C:/R/AER")
#' @export
#' 
getTemplate <- function(output_path){
  
  ## ----
  ## Setting default arguments if missing
  ## ----
  
  if(missing(output_path)){ 
    output_path <- getwd() 
  }
  
  ## ----
  ## Initialising the Word object
  ## ----
  
  doc <- officer::read_docx(path = file.path(system.file(package = "EpiReport"), "template/Empty_AER_template.docx" ))
  
  ## ----
  ## Generating the word output
  ## ----
  
  print(doc, target = paste(output_path, "/Empty_AER_template.docx", sep=""))
  
}




#' Get the table 
#'
#' Function returning the table
#' 
#' @param x dataset including required data for AER
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param index figure number
#' @param doc Word document (see \code{officer} package)
#' @return ?
#' @export
getTableByMS <- function(x, disease = "Salmonellosis", year = 2016, 
                   reportParameters, index = 1, doc){
  
  ## ----
  ## Setting default arguments if missing
  ## ----
  
  if(missing(x)) { x <- EpiReport::AERdata }
  if(missing(disease)) { disease <- "Salmonellosis" }    # disease <- "SALM"
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(index)) { index <- 1 }
  
  ## ----
  ## Filtering
  ## ----
  
  x <- dplyr::filter(x, x$HealthTopic == disease)
  if( nrow(x) ==0 ) {
    stop(paste('The dataset does not include the selected disease "', disease, '".'))
  }
  disease <- "SALM"
  reportParameters <- dplyr::filter(reportParameters, reportParameters$HealthTopic == disease)
  if( nrow(reportParameters) ==0 ) {
    stop(paste('The disease "', disease, '" is not described in the parameter table.
               The report cannot be produced.'))
  }
  
  
  ## ----
  ## Table
  ## ----
  t <- x[1:15, 1:6]
  ft <- flextable::flextable(t)
  ft <- flextable::theme_zebra(ft)
  ft <- flextable::autofit(ft)
  
  if(missing(doc)) {    
    
    ## ------ If no Word document, then just preview the map
    return(ft)
    
  } else {
    officer::cursor_bookmark(doc, id = "TABLE1_BOOKMARK")
    doc <- flextable::body_add_flextable(doc, value = ft)
    return(doc)
  }
  
}



#' Get the disease map 
#'
#' Function returning the map
#' 
#' @param x dataset including required data for AER
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param index figure number
#' @param pathPNG character string, path to the folder containing the maps in PNG
#' @param doc Word document
#' @return ?
#' @export
getMap <- function(x, disease = "Salmonellosis", year = 2016, 
                   reportParameters, index = 1, pathPNG, doc){
  
  ## ----
  ## Setting default arguments if missing
  ## ----
  
  if(missing(x)) { x <- EpiReport::AERdata }
  if(missing(disease)) { disease <- "Salmonellosis" }    # disease <- "SALM"
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(index)) { index <- 1 }
  if(missing(pathPNG)) { pathPNG <- system.file("maps", package = "EpiReport") }
  
  ## ----
  ## Filtering
  ## ----
  
  x <- dplyr::filter(x, x$HealthTopic == disease)
  if( nrow(x) ==0 ) {
    stop(paste('The dataset does not include the selected disease "', disease, '".'))
    }
  disease <- "SALM"
  reportParameters <- dplyr::filter(reportParameters, reportParameters$HealthTopic == disease)
  if( nrow(reportParameters) ==0 ) {
    stop(paste('The disease "', disease, '" is not described in the parameter table.
               The report cannot be produced.'))
    }
  
  
  ## ----
  ## Number of cases map
  ## ----
  
  if(reportParameters$Map.numbers.use == "Y") {
    
    ## ----- Caption definition
    # ....
    
    ## ----- Map
    namePNG <- paste(pathPNG, "SALM_CONFIRMED.COUNT.png", sep = "/")
    
    if(missing(doc)) {    # If no Word document, then just preview the map
      img <- png::readPNG(namePNG)
      grid::grid.raster(img)
    } else {    # If word document provided, add the maps in the doc
      officer::cursor_bookmark(doc, id = "MAP_NB_BOOKMARK")
      doc <- officer::body_add_img(doc, namePNG, width = 7.018, height = 4.956)
      return(doc)
    }
    
  }
  
  
  
  ## ----
  ## Rates map
  ## ----
  
  if(reportParameters$Map.rates.use == "Y") {
    
    ## ------ Maps
    namePNG <- paste(pathPNG, "SALM_CONFIRMED.RATE.png", sep = "/")
    
    if(missing(doc)) {    
      
      ## ------ If no Word document, then just preview the map
      img <- png::readPNG(namePNG)
      grid::grid.raster(img)
      
    } else {    
      
      ## ------ If word document provided, add the maps in the doc
      officer::cursor_bookmark(doc, id = "MAP_RATE_BOOKMARK")
      doc <- officer::body_add_img(doc, namePNG, width = 7.018, height = 4.956)
      
      ## ------ Caption definition
      pop <- ifelse(reportParameters$MeasurePopulation == "CONFIRMED", "confirmed ", "")
      caption <- paste("Figure ", index, ". Distribution of ", pop, reportParameters$Label,  
                       " cases per 100 000 population by country, ", 
                       "EU/EEA, ", year, sep = "")
      officer::cursor_bookmark(doc, id = "MAP_RATE_CAPTION")
      doc <- officer::body_add_break(doc)
      doc <- officer::body_add_par(doc, value = caption)
      
      #-----> Adding the link
      # link <- pot('Link to the graph',
      #             hyperlink = parameters$Map.rates.URL,
      #             format=textProperties(color = MAINCOLOR , underline = TRUE ))
      # doc <- addParagraph(doc, link, bookmark = "MAP_RATE_BOOKMARK_LINK")
      
      return(doc)
    }
  }
  
  
  ## ----
  ## Age Specific Rate map
  ## ----
  
  if(reportParameters$Map.ASR.use == "Y") {
    
    ## ----- Caption definition
    # ....
    
    ## ----- Map
    namePNG <- paste(pathPNG, "SALM_CONFIRMED.AGESTANDARDISED.RATE.png", sep = "/")
    
    if(missing(doc)) {    # If no Word document, then just preview the map
      img <- png::readPNG(namePNG)
      grid::grid.raster(img)
    } else {    # If word document provided, add the maps in the doc
      officer::cursor_bookmark(doc, id = "MAP_ASR_BOOKMARK")
      doc <- officer::body_add_img(doc, namePNG, width = 7.018, height = 4.956)
      return(doc)
    }
    
  }

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
    x <- EpiReport::AERdata
  }
  
  parameters <- EpiReport::AERparams
  parameters <- dplyr::filter(parameters, parameters$HealthTopic == disease)
  x <- dplyr::filter(x, x$HealthTopic == disease)
  
  if (parameters$AgeGender.bar.graph.used=="Y"    #--AgeGender bar graph 
     & parameters$AgeGender.Rates.use=="Y") {    #-- with Rates
    
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
  
  if(missing(x)){ x <- EpiReport::AERdata }
  
  ## ----
  ## Plot output
  ## ----
  
  .p <- suppressWarnings( ggplot2::ggplot(x, ggplot2::aes(x$RegionCode, x$NumValue))) +
    suppressWarnings(ggplot2::geom_bar(stat="identity"))
  
  .p
  
  
}




#' Produce the AER report
#'
#' Function to generate the AER Word report
#' 
#' @param template The word document in which to add the table. 
#' Default value is the empty template included in the package getTemplate().
#' @param outputPath The path for the word output.
#' Default value is the current working directory getwd().
#' @param x dataset including required data for AER
#' @param disease character string, disease name
#' @param year numeric, year to produce the report for
#' @param reportParameters dataset of parameters for the report
#' @param pathPNG character string, path to the folder containing the maps in PNG
#' 
#' @usage getAER(template, outputPath, x, disease, year, reportParameters, pathPNG)
#' 
#' @return A word document
#' @export
getAER <- function(template, outputPath = getwd(), 
                   x, disease = "Salmonellosis", year = 2016, 
                   reportParameters, pathPNG){
  
  ## ----
  ## Setting default arguments if missing
  ## ----
  
  if(missing(template)){ 
    template <- file.path(system.file(package = "EpiReport"), "template/Empty_AER_template.docx" ) 
    }
  if(missing(outputPath)){ 
    outputPath <- getwd() 
    }
  if(missing(x)) { x <- EpiReport::AERdata }
  if(missing(disease)) { disease <- "Salmonellosis" }    # disease <- "SALM"
  if(missing(year)) { year <- 2016 }
  if(missing(reportParameters)) { reportParameters <- EpiReport::AERparams }
  if(missing(pathPNG)) { pathPNG <- system.file("maps", package = "EpiReport") }
  
  
  ## ----
  ## Initialising the Word object
  ## ----

  doc <- officer::read_docx(path = template)
  options( "ReporteRs-fontsize" = 9,
           "ReporteRs-default-font" = "Tahoma")
  dataParProp <- officer::fp_par(
    padding.top = 1, padding.bottom = 1, padding.left = 0, padding.right = 0,
    text.align = "center")
  
  ## ----
  ## Adding the table
  ## ----
  
  doc <- EpiReport::getTableByMS(x = x, 
                                 disease = disease, 
                                 year = year, 
                                 reportParameters = reportParameters, 
                                 index = 1, 
                                 doc = doc)
  
  ## ----
  ## Bar graph
  ## ----
  
  
  
  ## ----
  ## Map
  ## ----
  
  doc <- EpiReport::getMap(x = x, 
                           disease = disease, 
                           year = year, 
                           reportParameters = reportParameters, 
                           index = 1, 
                           pathPNG = pathPNG, 
                           doc = doc)
  
  
  ## ----
  ## TS plot
  ## ----
  p <- suppressWarnings(EpiReport::tseasonalityByMS())
  officer::cursor_bookmark(doc, id = "TS_SEASON_BOOKMARK")
  doc <- officer::body_add_gg( doc, 
                               value = p, 
                               width = 7.2, 
                               height = 3)
  

  

  ## ----
  ## Generating the word output
  ## ----
  
  print( doc, target = paste(outputPath, "/AER_report_", disease, year, ".docx", sep="") )
  
}