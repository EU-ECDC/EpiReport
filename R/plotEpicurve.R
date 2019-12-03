#' plotEpicurve
#'
#' This function draws an epicurve, either one square per each case or bars per time unit. Requires case-based data, currently requires either a column
#' with 1's or Yes's for cases. Draws by dates, or by continuous x-scale (weeks, months, years). 
#' Note that accepts also a line list with 1/0 or Yes/No for case/non-case. Use squares = FALSE if large number of cases or long time interval.
#' @param data Your data frame/line list.
#' @param xvar Variable with the date, week, month or year of onset (requires long format data), 
#' given without quotes.
#' @param yvar Variable with cases/non-cases as 1/0 or Yes/No (also allows data with only 1's), given without quotes.
#' @param group If applicable, a grouping variable, given without quotes. Defaults to one group 
#' (cases), in which leaves the legend unprinted. In case of groupings, works up to 7 groups with 
#' ECDC colours.
#' @param squares Whether to draw one square for each case or only one bar per time unit. Defaults to one square per case, use "FALSE" in case of large number of cases or long time intervals.
#' @param ymax y-axis upper limit, defaults to max number of cases per time unit. Tick marks are always pretty.
#' @param col_scale Colour scale for the squares/bars, defaults to qualitative colour scale; select one of the following: "qualitative",
#' "green", "blue", "red".
#' @param border Square/bar border colour, defaults to "white". "NA" removes the borders.
#' @param xtitle x-axis title.
#' @param legend_title Legend title
#' @param x_axis_ticks Tick marks for x-axis. Use ONLY if x-axis uses dates or months; select one of the following: "days", "2 days", 
#' "weeks", "2 weeks", "months", "years". Defaults to "days" with days and to numbers with numeric weeks or months. Selecting "weeks" prints only 
#' mondays for x-axis ticks. In case of "months" labelled automatically with abbreviated 
#' month names.
#' @param x_axis_limits x-axis limits, for dates use character dates as c("2017-01-01", "2017-12-31"), otherwise a numeric vector lenght of two.
#' @keywords epicurve
#' @author Tommi Karki
#' @export
#' @examples
#' # Create dummy data
#' mydat <- data.frame(ID = c(seq(1,10,1)),
#' Gender = c(rep(c("F", "M"),5)),
#' AgeGroup = c(rep(c("0-18", "18-65", "65+"),3), "65+"),
#' Case = c(1,1,1,0,0,1,1,1,1,1),
#' Cases_char = c(rep("Yes",3), "No", "No", rep("Yes", 5)),
#' DateOfOnset = as.Date(c("2017-06-11", "2017-06-11", 
#'                        "2017-06-11", NA, NA, "2017-06-10", 
#'                        "2017-06-14", "2017-06-14",
#'                        "2017-06-19", "2017-06-19")),
#' Month = c(sample(c(3:6),5, replace = TRUE), sample(c(1:12),5)),
#' Week = c(sample(c(10:12),5, replace = TRUE), sample(c(1:53),5)))
#' 
#' # Plot
#' plotEpicurve(data = mydat, xvar = DateOfOnset, yvar = Case, ymax = 5)
#' 
#' # Plot by two level grouping (e.g. Gender)
#' plotEpicurve(mydat, xvar = DateOfOnset, yvar = Case, ymax=5, group = Gender)
#' 
#' # Plot by multilevel grouping (e.g AgeGroup)
#' plotEpicurve(mydat, xvar = DateOfOnset, yvar = Case, ymax=5, group = AgeGroup,
#' xtitle = "Date of onset")
#' 
#' # Plot by months using x-axis options
#' plotEpicurve(mydat, xvar = Month, yvar = Case, ymax = 5, x_axis_ticks = "months", 
#' xtitle = "Month of onset")
#' 
#' # Plot with character Yes/No cases variable
#' plotEpicurve(mydat, xvar = Month, yvar = Cases_char, ymax = 5, x_axis_ticks = "months", 
#' xtitle = "Date of onset of disease x")
#' 
#' # Create bigger dummy data
#' biggerdat <- data.frame(ID = c(seq(1,1000,1)),
#' Case = 1, Month=round(rnorm(1000, mean=6, sd=1.5)),
#' Gender = rep(c("F", "M"),500))
#' 
#' # Plot by months using squares = FALSE and border = NA
#' plotEpicurve(biggerdat, xvar = Month, yvar = Case, squares = FALSE, 
#' xtitle = "Month of onset", x_axis_ticks = "months", border = NA)
#' 
#' Plot by months using squares = FALSE and by grouping
#' plotEpicurve(biggerdat, xvar = Month, yvar = Case,  group = Gender,
#' squares = FALSE, xtitle = "Month of onset", x_axis_ticks = "months")
plotEpicurve <- function(data, 
                      xvar,
                      yvar,
                      group = NULL,
                      squares = TRUE,
                      ymax = NULL,
                      xtitle = "Time",
                      legend_title = group,
                      col_scale = "qualitative",
                      border = "white",
                      x_axis_ticks = c("days", "2 days", "weeks", "2 weeks", "months", "years"),
                      x_axis_limits = NULL) {
  # Not ready yet, make the function accept y/n, yes/no, YES/NO, Yes/No etc...using tolower and grepl, e.g. grepl("y", tolower(xvar))
  xvar <- deparse(substitute(xvar))
  yvar <- deparse(substitute(yvar))
  group <- deparse(substitute(group))
  if(any(grepl("y", tolower(data[[yvar]])))){
    data[[yvar]] <- ifelse(grepl("y", tolower(data[[yvar]])), 1, 0)
    data[[yvar]] <- as.integer(data[[yvar]])
  }else if(any(!data[[yvar]]%in%c(0,1))){
    print("Variable yvar needs to contain 1/0 or Yes/No, 1's for cases to be plotted on the epicurve!")
    stop()}
  if(squares == TRUE){
    x_axis_ticks <- match.arg(x_axis_ticks)
    # col_scale <- match.arg(col_scale)
    if(!is.null(ymax)){
      if(ymax < 5){
        FIGBREAKS <- seq(0, ymax, by = 1)
      }else{
        FIGBREAKS <- pretty(seq(0, ymax, by = ymax/5))}
    }else{
      if(max(tapply(data[[yvar]], data[[xvar]], "sum") < 5)){
        FIGBREAKS <- seq(0, max(tapply(data[[yvar]], data[[xvar]], "sum")),
                         by = 1)
      }else{
        FIGBREAKS <- pretty(seq(0, max(tapply(data[[yvar]], data[[xvar]], "sum")),
                                by = max(tapply(data[[yvar]], data[[xvar]], "sum")/5)))
      }
    }
    
    if(x_axis_ticks == "months"){
      xlabs <- substring(month.name,1,3)
    }
    
    if(!is.null(x_axis_limits) & is(data[[xvar]], "Date")){
      x_axis_limits <- as.Date(x_axis_limits)}
    
    p1 <- ggplot(data, 
                 aes_string(x = xvar, y = yvar, fill = group)) + 
      geom_col(col=border, width = 1) +
      theme_classic() +
      coord_equal() +
      scale_y_continuous(expand = c(0,0), limits = c(0,max(FIGBREAKS)),
                         breaks = FIGBREAKS)
    if(is(data[[xvar]], "Date")){
      p1 <- p1 + scale_x_date(date_breaks = x_axis_ticks,
                              limits = x_axis_limits)
    }else if(x_axis_ticks == "months"){
      p1 <- p1 + scale_x_continuous(limits = x_axis_limits,
                                    breaks = seq(1,12,1),
                                    labels = xlabs)
    }else{
      p1 <- p1 + scale_x_continuous(breaks = seq(1, max(data[[xvar]], by = 1)))}
    p1 <- p1 +
      xlab(xtitle) +
      ylab("Number of cases") +
      theme(axis.text.y = element_text(size = 8,  colour = "black"),
            axis.text.x = element_text(size = 8,  angle = 36,
                                       hjust = 1, colour = "black"), axis.title = element_text( size = 11, 
                                                                                                colour = "black"),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
            legend.key.width = unit(0.8, "cm"), legend.key.size = unit(0.4, "cm"))
    if(group == "NULL") {
      p1 <- p1 + geom_col(col=border, fill=SurvColors(col_scale), width = 1) +
        guides(fill=FALSE)
    }else if(length(unique(data[[group]])) == 2){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=2)) +
        guides(fill = guide_legend(title = legend_title))
    }else if(length(unique(data[[group]])) == 3){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=3)) +
        guides(fill = guide_legend(title = legend_title))
    }else if(length(unique(data[[group]])) == 4){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=4)) +
        guides(fill = guide_legend(title = legend_title))
    }else if(length(unique(data[[group]])) == 5){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=5)) +
        guides(fill = guide_legend(title = legend_title))
    }else if(length(unique(data[[group]])) == 6){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=6)) +
        guides(fill = guide_legend(title = legend_title))
    }else{
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=7)) +
        guides(fill = guide_legend(title = legend_title))
    }
  }else{
    # Contains still some fixes to do, group-argument does not work, months/weeks not
    # perfectly, y-axis ticks could be better
    if(group=="NULL"){
      dat <- as.data.frame(as.table(tapply(data[[yvar]], 
                                           data[[xvar]], 
                                           "sum")))
      names(dat) <- c(xvar, yvar)
    }else{
      dat <-as.data.frame(as.table(tapply(data[[yvar]], 
                                          list(data[[xvar]], data[[group]]), 
                                          "sum")))
      names(dat) <- c(xvar, group, yvar)
    }
    if(class(data[[xvar]])=="Date"){
      dat[[xvar]] <- as.Date(as.character(dat[[xvar]]))
    }else{
      dat[[xvar]] <- as.integer(as.character(dat[[xvar]]))
    }
    x_axis_ticks <- match.arg(x_axis_ticks)
    if(group == yvar){
      FIGBREAKS <- pretty(seq(0, max(dat[[yvar]]),
                              by = max(dat[[yvar]])/5))
    }else{
      FIGBREAKS <- pretty(seq(0, max(tapply(dat[[yvar]], dat[[xvar]], "sum", na.rm=TRUE)),
                              by = max(tapply(dat[[yvar]], dat[[xvar]], "sum",  na.rm=TRUE))/5))
    }
    
    # If x_axis_ticks is "months", the unit is months thus labelling 
    # could be abbreviated month names.
    
    if(x_axis_ticks == "months"){
      xlabs <- substring(month.name,1,3)
    }
    
    if(!is.null(x_axis_limits) & is(dat[[xvar]], "Date")){
      x_axis_limits <- as.Date(x_axis_limits)}
    p1 <- ggplot(dat, 
                 aes_string(x = xvar, y = yvar, fill = group)) + 
      geom_bar(col=border, stat = "identity", width = 1) +
      theme_classic() +
      scale_y_continuous(expand = c(0,0), limits = c(0,max(FIGBREAKS)),
                         breaks = FIGBREAKS)
    if(is(dat[[xvar]], "Date")){
      p1 <- p1 + scale_x_date(date_breaks = x_axis_ticks,
                              limits = x_axis_limits)
    }else if(x_axis_ticks == "months"){
      p1 <- p1 + scale_x_continuous(limits = x_axis_limits,
                                    breaks = seq(1,12,1),
                                    labels = xlabs)
    }else{
      p1 <- p1 + scale_x_continuous(breaks = seq(1, max(dat[[xvar]], by = 1)))}
    p1 <- p1 + xlab(label = xtitle) +
      ylab("Number of cases") +
      theme(axis.text.x = element_text(size = 8,  colour = "black"),
            axis.text.y = element_text(size = 8,  colour = "black"),
            axis.title = element_text( size = 9, colour = "black"),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
            legend.key.width = unit(0.8, "cm"), legend.key.size = unit(0.4, "cm"))
    if(group == "NULL") {
      p1 <- p1 + geom_bar(col=border, fill=SurvColors(col_scale), 
                          stat = "identity", width = 1) +
        guides(fill=FALSE)
    }else if(length(unique(data[[group]])) == 2){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=2)) +
        guides(fill = guide_legend(title = legend_title))
    }else if(length(unique(data[[group]])) == 3){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=3)) +
        guides(fill = guide_legend(title = legend_title))
    }else if(length(unique(data[[group]])) == 4){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=4)) +
        guides(fill = guide_legend(title = legend_title))
    }else if(length(unique(data[[group]])) == 5){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=5)) +
        guides(fill = guide_legend(title = legend_title))
    }else if(length(unique(data[[group]])) == 6){
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=6)) +
        guides(fill = guide_legend(title = legend_title))
    }else{
      p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=7)) +
        guides(fill = guide_legend(title = legend_title))
    }
  }  
  return(p1) 
}