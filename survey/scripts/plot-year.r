# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("magrittr")
# install.packages("forcats")
# install.packages('ggthemes', dependencies = TRUE)

library(ggplot2)
# library(magrittr)
# library(forcats)
library(dplyr)
library(purrr)
library("ggthemes")
library("scales")

# Ordered by area in the spreadsheet:
area <- c("development", "development", "development", "development", "development", "other", "other", "other", "other", "other", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing", "testing")
year <- c(2015, 2016, 2015, 2017, 2018, 2017, 2016, 2018, 2017, 2013, 2009, 2010, 2011, 2012, 2013, 2013, 2013, 2014, 2014, 2014, 2014, 2015, 2016, 2016, 2016, 2016, 2017, 2017, 2018, 2018, 2018, 2018, 2014, 2018)
venue <- c("ASE", "UIST", "UIST", "UIST", "ASE", "STVR", "ICSE", "TSE", "EMSE", "ICSE", "UIST", "ICSM", "STVR", "ICST", "ICSE", "ICSM", "ICST", "ASE", "SBST", "TSE", "ISSTA", "ICST", "ASE", "ASE", "ICST", "UIST", "ASE", "IUI", "ICST", "JSS", "STVR", "TOIT", "Other", "EMSE")
#citations <- c()

accum <- function(dframe, colname) {
	len <- length(row(dframe[1]))
	indecies <- 1:(len-1)
	for(i in indecies) {
		dframe[i+1, colname] = dframe[i+1, colname] + dframe[i, colname]
	}
	return(dframe)
}

data <- data.frame(year, area)

# data <- data %>%
# 	arrange(year, area, desc(year)) %>%
# 	group_by(year, area) %>%
# 	summarize(count = n()) %>% accum("count")

areas <- split(data, data$area)
plot <- ggplot()
seriesList <- c()
anames <- names(areas)
final <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(final) <- c("year", "count", "area")
for (a in 1:length(areas)) {
	seriesAccum <- get(anames[a], areas) %>% group_by(year) %>% summarize(count = n()) %>% accum("count")
	# seriesAbs <- get(anames[a], areas) %>% group_by(year) %>% summarize(count = n())
	# print(plot <- plot + geom_col(data=seriesAccum, aes(year, count)))
	# invisible(readline(prompt="Press [enter] to continue"))
	seriesAccum['area'] <- anames[a]
	# str(seriesAccum)
	# seriesList[a] = seriesAccum

	final <- rbind(final, seriesAccum)
	
	# if (length(final) == 0) {
	# 	final <- seriesAccum
	# } else {
	# 	merge(final, seriesAccum, by="year")	
	# }
}


final %>% ggplot() + geom_col(aes(year, count, fill=area)) +
	labs(x="Publication year", y="Cumulative publication count", title="Publications by year and stage of SE") +
	theme_economist() + scale_colour_economist() + scale_y_continuous(position = "right") + 
	# theme_hc() + scale_colour_hc() +
	theme(legend.position = c(0.2, 0.75), legend.title = element_blank())

# grouped <- group_by(data, year)
# summarize(grouped, count = n())
# grouped <- group_by(data, year, area) %>% summarize(count = n())

# SEarea <- c("Testing", "Development", "Other")
# count <- c(24, 5, 5)
# data <- data.frame(SEarea, count)
# 
# ggplot(data) + geom_col(aes(x=SEarea,y=count))

