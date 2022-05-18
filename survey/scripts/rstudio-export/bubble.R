library(readr)
library(ggplot2)
CSV_dir <- "~/github/tse2020/scripts/rstudio-export/CSVs"
RQ1 <- read_csv(paste(CSV_dir, "/RQ1.csv", sep=""))
RQ2 <- read_csv(paste(CSV_dir, "/RQ2.csv", sep=""))
RQ3 <- read_csv(paste(CSV_dir, "/RQ3.csv", sep=""))
Methods <- read_csv(paste(CSV_dir, "/Methods.csv", sep=""))
TasksTestsBroken <- read_csv(paste(CSV_dir, "/TasksTestsBroken.csv", sep=""))
Tasks <- read_csv(paste(CSV_dir, "/Tasks.csv", sep=""))
Areas <- read_csv(paste(CSV_dir, "/Areas.csv", sep=""))
RQ1$Area[RQ1$Area=="Maintenance and Comprehension"] = "Maintenance/\nComprehension"
TasksTestsBroken$Tasks[TasksTestsBroken$Tasks=="Maintenance and Comprehension"] = "Maintenance/\nComprehension"
TasksTestsBroken$Stage[TasksTestsBroken$Stage=="Maintenance and Comprehension"] = "Maintenance/\nComprehension"
TasksTestsBroken$Tasks[TasksTestsBroken$Tasks=="Requirement Engineering"] = "Requirements"
TasksTestsBroken$Stage[TasksTestsBroken$Stage=="Requirement Engineering"] = "Requirements"

createBubbleChartMethodTask <- function() {
  data <- merge(Methods, Tasks, by = c("Citation"));
  data <- data[,c("Method", "SE Task")];
  data <- as.data.frame(table(data));
  data <- data[data$Freq != 0,];
  #View(data)
  ggplot(data, aes(x = Method, y = SE.Task, label = Freq)) +
    geom_point(aes(size = Freq), alpha = .4) +
    geom_text(size = 4) +
    scale_size(range = c(1, 15)) +
    theme_bw() +
    ylab("Software Engineering Task") +
    xlab("Visual Method") +
    theme(legend.position="none")
}

createTechniquesChart <- function() {
  data <- merge(Methods, TasksTestsBroken, by = c("Citation"));
  data <- data[,c("Method", "Tasks", "Stage")];
  data <- as.data.frame(table(data));
  data <- data[data$Freq != 0,];
  #View(data)
  ggplot(data, aes(x = factor(Method, levels = c("Differential methods", "Transformational methods", "Search methods"), 
                                      labels = c("Differential", "Transformational", "Search")), 
                   y = factor(Tasks, levels = c("Requirements",
                                                "Design",
                                                "Development",
                                                "Unit Testing",
                                                "Test Case Generation",
                                                "Root Cause Analysis",
                                                "Regression Testing",
                                                "Acceptance Testing",
                                                "Usability Testing",
                                                "Maintenance/\nComprehension")), 
                   label = Freq)) +
    geom_point(aes(size=Freq, fill=factor(Stage)), color="black", pch=21, alpha = .8) +
    geom_text(aes(color=Stage=="Design"), size=4) +
    scale_size(range = c(6, 20)) +
    scale_colour_manual(values=c("black", "white"), guide=FALSE) +
    scale_fill_grey(start=0, end=1) +
    theme_bw() +
    ylab("Software Engineering Area/Task") +
    xlab("Visual Technique") +
    theme(legend.title=element_blank(),
          legend.position="top",
          legend.spacing.x = unit(0, "cm"),
          legend.text = element_text(margin = margin(r = 10, unit = "pt")),
          legend.margin=margin(l = -3.2, unit='cm')
    ) +
    guides(size=FALSE, fill = guide_legend(override.aes = list(size=6)))
}

# Done
createRationaleChart <- function() {
	
	data <- merge(RQ1, RQ2, by = c("Citation"));
	data <- data[,c("Rationale", "Area")];
	data <- as.data.frame(table(data));
	data <- data[data$Freq != 0,];
	#View(data)
	ggplot(data, aes(x = Rationale, y = factor(Area, levels = c("Requirements", "Design", "Development", "Testing", "Maintenance/\nComprehension")), label = Freq)) +
		geom_point(aes(size=Freq, fill=factor(Area)), color="black", pch=21, alpha = .8) +
		geom_text(aes(color=Area=="Design"), size=4) +
		scale_size(range = c(4, 18)) +
		scale_colour_manual(values=c("black", "white"), guide=FALSE) +
		scale_fill_grey(start=0, end=1) +
		theme_bw() +
		ylab("Software Engineering Area") +
		theme(legend.title=element_blank(),
			 legend.position="none") +
		guides(size=FALSE)
}

# Done
createArtifactsChart <- function() {
  data <- merge(RQ3, TasksTestsBroken, by = c("Citation"));
  data <- data[,c("Artifact", "Tasks", "Stage")];
  data <- as.data.frame(table(data));
  data <- data[data$Freq != 0,];
  #View(data)
  ggplot(data, aes(x = factor(Artifact, levels = c("Full-Interface Visual Artifacts", "Component Visual Artifacts", "Video Artifacts", "Natural Input Artifacts"), 
                                        labels = c("Full-interface", "Localized", "Temporal", "Natural Input")), 
                   y = factor(Tasks, levels = c("Requirements",
                                                "Design",
                                                "Development",
                                                "Unit Testing",
                                                "Test Case Generation",
                                                "Root Cause Analysis",
                                                "Regression Testing",
                                                "Acceptance Testing",
                                                "Usability Testing",
                                                "Maintenance/\nComprehension")), 
                   label = Freq)) +
    geom_point(aes(size=Freq, fill=factor(Stage)), color="black", pch=21, alpha = 1) +
    geom_text(aes(color=Stage=="Design"), size=4) +
    scale_size(range = c(6, 20)) +
    scale_colour_manual(values=c("black", "white"), guide=FALSE) +
    scale_fill_grey(start=0, end=1) +
    theme_bw() +
    ylab("Software Engineering Area/Task") +
    xlab("Visual Artifact") +
    theme(legend.title=element_blank(),
          legend.position="top",
          legend.spacing.x = unit(0, "cm"),
          legend.text = element_text(margin = margin(r = 10, unit = "pt")),
          legend.margin=margin(l = -3.2, unit='cm')) +
    guides(size=FALSE, fill = guide_legend(override.aes = list(size=5)))
}

# Done
createTrendsChartCount <- function() {
t<-unique(Areas[,  c("Citation","Year")])
  t1<-table(t[,"Year"])
  data = as.data.frame(t1,stringsAsFactors = FALSE);
  colnames(data)<-c("Year","Freq")
#  levels(data$Year) <- c(min(levels(data$Year)):max(levels(data$Year)))
  data[nrow(data)+1, ] <- c(2003,0)
  data[nrow(data)+1, ] <- c(2004,0)
  data[nrow(data)+1, ] <- c(2005,0)
  data[nrow(data)+1, ] <- c(2006,0)
  data[nrow(data)+1, ] <- c(2008,0)
  data[nrow(data)+1, ] <- c(2009,0)
  ggplot(data) + 
    geom_col(aes(x = Year, y = Freq), alpha = .65, size=0.2) +
    labs(x="Publication year", y="Publication count") +
    #scale_fill_manual(values=c("black", "blue", "red", "green", "purple")) +
    scale_y_continuous(breaks=0:15, expand = c(0, 0), limits = c(0,15)) +
#    expand_limits(x = 0, y = 0)
#    scale_x_continuous(expand = c(0, 0)) +
 #   scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(legend.title=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1))
          # legend.position="top",
          # legend.margin=margin(l = -1.5, b = -0.2, unit='cm')) +
          # guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))
}

# Done
createAreaTrendsCumulative <- function() {
  
	data = as.data.frame(table(Areas[, c("SE Area","Year")]));
	data.copy = data.frame(data)
	for (y in unique(as.character(data.copy$Year))) {
	  for (area in unique(data.copy$SE.Area)) {
	    d <- data.copy[as.numeric(as.character(data$Year)) <= as.numeric(y) & data.copy$SE.Area == area, ]
	    data$Freq[as.character(data$Year) == y & data$SE.Area == area] <- sum(d$Freq)
	  }
	}
	ggplot(data) + 
		geom_line(aes(x = factor(Year), y = Freq, group=SE.Area, linetype=SE.Area), alpha = 0.99, size=0.65) +
		labs(x="Publication year", y="Publication count") +
		#scale_color_manual(values=c("black", "blue", "red", "green", "purple")) +
	  #scale_linetype_manual(values=c("dotted", "dotdash", "solid", "dashed", "longdash")) +
		scale_y_continuous(breaks=seq(0, 30, by=5)) +
		theme_bw() +
		theme(
		  legend.title=element_blank(),
			#legend.position="top",
			legend.position = c(.33, .75),
			legend.key.width = unit(1.2, "cm"),
			legend.direction = "vertical",
			legend.box.background = element_rect(colour = "black"),
			legend.text=element_text(size=8),
			legend.margin=margin(l=+0.2, t = -0.1, b = +0.1, unit='cm')
		)
}

# Done
createVenuesChart <- function() {
  data = unique(Areas[, c("Citation", "Venue")])[,  c("Venue")]
  data = as.data.frame(table(data));
  colnames(data) <- c("Venue", "Freq")
  ggplot(data) + 
    geom_col(aes(x = factor(Venue, levels = unique(data$Venue)[order(-data$Freq)]), y = Freq), alpha = .65,  size=0.2) +
    labs(x="Venue", y="Publication count") +
#    scale_y_continuous(breaks=0:10) +
    scale_y_continuous(breaks=1:10, expand = c(0, 0), limits = c(0,7.5)) +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position="top",
          panel.grid.minor=element_blank(),
          legend.margin=margin(l = -1.5, b = -0.2, unit='cm'),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))
}

Areas$`SE Area`[Areas$`SE Area`=="Maintenance and Comprehension"] = "Maintenance/\nComprehension"
Tasks$`SE Area`[Tasks$`SE Area`=="Maintenance and Comprehension"] = "Maintenance/\nComprehension"
Tasks$`SE Area`[Tasks$`SE Area`=="Requirement Engineering"] = "Requirements"

Tasks$`SE Task` = gsub("\\\\n", "\n", Tasks$`SE Task`,fixed = T)

# Done
createTasksAreasPlot <- function() {
  tasksAreas = unique(Tasks[, c("SE Task", "SE Area")])
  taskCounts = as.data.frame(table(Tasks[, c("SE Task")]))
  colnames(taskCounts) <- c("SE Task", "Freq")
  data = merge(tasksAreas, taskCounts, by = c("SE Task"))
  
  ggplot(data, 
         aes(x = factor(`SE Task`, levels = unique(data$`SE Task`)[order(data$`SE Area`, data$Freq)]),
             y = Freq + 5, fill = `SE Area`)) + 
    geom_col(alpha = .80, width = 1, color="gray30") +
    geom_text(aes(label = Freq), size = 2.5, nudge_y = 1) + 
    labs(x="", y="") +
    theme_bw() +
    #scale_fill_manual(values=c("black", "blue", "red", "green", "purple")) +
    scale_fill_grey(start=0, end=1) +
    scale_x_discrete(position="top") +
  	theme(legend.title=element_blank(),
          legend.position="top",
          legend.margin=margin(b = -0.5, unit='cm'),
  	      legend.spacing.x = unit(.07, "cm"),
  	      legend.text = element_text(margin = margin(r = 10, unit = "pt")),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
          ) +
     
    coord_polar()
}