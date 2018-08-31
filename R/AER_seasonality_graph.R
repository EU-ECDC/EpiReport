
#' @export
AER_seasonality_graph <- function(data, xvar = "timecode",
                                  yvar = "n",
                                  yearvar  = "timeyear",
                                  monthvar = "timemonth",
                                  year = 2015){
  #-----> Compute mean, min and max
  sums <- data[data[[yearvar]]!=year,]
  aggeuyear <- data[data[[yearvar]]==year,]
  dt <- data.table(sums)
  dt <-dt[,list(Mean4Years = mean(get(yvar), na.rm = TRUE),Max4Years=max(get(yvar)), 
                        Min4Years=min(get(yvar))),by=get(monthvar)]
  
  FIGTSBREAKS <- pretty(seq(0, max(data[[yvar]]),
  by = max(data[[yvar]], dt[["Max4Years"]])/5))
 
p1 <- ggplot(aggeuyear, aes_string(x = xvar)) + 
  geom_ribbon(aes(ymin=dt$Min4Years, ymax=dt$Max4Years,
              fill = paste("Min-max (",min(data[[yearvar]]),"-",year-1, ")",sep="")),
              alpha=0.5) +
  geom_line(aes_string(y=yvar, colour = '"Year"'), size=1.1) +
  geom_line(aes(y=dt$Mean4Years, color = "Mean"), linetype="longdash", size=0.6) +
  scale_x_date(date_labels = "%b", date_breaks= "1 month", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks=FIGTSBREAKS, limits = c(0,max(FIGTSBREAKS))) +
  xlab("Month") +
  ylab("Number of cases") +
  scale_colour_manual("",
                      values=c("Year"= "#69AE23", "Mean"="#767171"),
                      labels=c("year"=as.character(year), "Mean"=paste("Mean (",min(data[[yearvar]]),"-",year-1, ")",sep="") ) ) +
  scale_fill_manual("",values="grey80") +
  theme(axis.text=element_text(size=8, family = "Tahoma"), axis.title=element_text(size=9)) +    #Axis text style
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +  #Empty backgroud
  theme(axis.line = element_line(colour = "#767171") ) +
  theme(legend.position = "right", legend.title =element_blank(), legend.text =element_text(size=8, family = "Tahoma"),
        legend.key=element_blank(), legend.key.width = unit(0.8, "cm"))
return(p1)
  }