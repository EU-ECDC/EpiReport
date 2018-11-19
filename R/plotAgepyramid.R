#' plotAgepyramid
#'
#' This function draws an age pyramid; improvements TBD (axis labels, selecting colours).
#' @param data Your data frame
#' @param xvar Variable with the population for each group, given in quotes.
#' @param yvar Variable with the agegroups for the y-axis, given in quotes.
#' @param group The grouping variable, usually gender, given in quotes.
#' @keywords agepyramid
#' @author Tommi Karki
#' @export
#' @examples
#' # Create dummy data
#' mydat <- data.frame(Age = factor(rep(x = 1:7, times = 1),
#' labels = c("0-5", "6-10", "11-15",
#'           "16-24", "25-45", "46-64", ">65")),
#' Gender = rep(x = c("Female", "Male"), each = 7),
#' Population = rep(c(30,34,55,80,120,115,75),2))

#' # Plot
#' plotAgepyramid(data = mydat, xvar = "Population", yvar = "Age", group = "Gender")

plotAgepyramid <- function(data, xvar, yvar, group) {
data[[yvar]] <- as.factor(data[[yvar]])
data[[xvar]] <- ifelse(data[[group]] == "Male", data[[xvar]]*(-1), data[[xvar]])
if(max(data[[xvar]]) >= abs(min(data[[xvar]]))){
  limits <- c(max(data[[xvar]])*(-1), max(data[[xvar]]))
}else{
  limits <- c(min(data[[xvar]]), min(data[[xvar]])*(-1))
}
p1 <- ggplot(data = data,
       mapping = aes_string(x = yvar, fill = group,
                     y = xvar)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = limits) +
  labs(y = xvar) +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values = SurvColors(col_scale = "qualitative", n=2)) +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.key.width = unit(0.8, "cm"),
        legend.key.size = unit(0.4, "cm"))

return(p1)
}
