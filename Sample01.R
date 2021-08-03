# Ricca Callis
# EN 605.662 Data Visualization
# Project 4 - Data Exploration and Design
# 3 Required Data Sets:
#   Data Set 1: https://www.kaggle.com/spscientist/students-performance-in-exams 
#   Data Set 2: https://www.kaggle.com/kimjihoo/coronavirusdataset?select=Case.csv 
#   Data Set 3: https://www.kaggle.com/karangadiya/fifa19 

# Project Instructions:

# I. Purpose:
#   Recently, a number of libraries have been released to help organizations 
#   develop new visualizations and illustration tools. Some of the popular libraries include D3, 
#   Chart.js, Plot.ly, Highcharts, Bokeh, ggplot, matplotlib, ProtoVIS, R Shiny, NVD3, etc… 
#   As data scientists, it is important for us to have a basic understanding of those libraries 
#   and their capabilities. The purpose of this assignment is to get familiar with open source 
#   libraries by developing three sample visualizations using any libraries for JavaScript, R, or Python.

# II. Task
#   1. Analyze 3 data sets. The data should have 3 or more variables and 100 or more rows.
#   2. Develop 3 different visualizations by leveraging the library of your choice to illustrate
#      the datasets under consideration.
#   3. File structure: students should structure their project the following way:
#       o your_lastname_project04/
#           	Paper: your_lastname_project04.pdf
#                 • Introduction: What the project is about
#                 • Dataset: explain and provide links to the sources
#                 • Approach: explain which libraries you selected
#                 • Visualization #1: Explain, provide screenshot, and justify
#                 • Visualization #2: Explain, provide screenshot, and justify
#                 • Visualization #3: Explain, provide screenshot, and justify
#                 • Conclusion
#                 • References
#           	src/
#                 • Sample01/
#                     o Index1.html or Sample1.py or Sample1.R, etc…
#                     o Data1.csv
#                     o Screenshot_sample01.jpg
#                 • Sample02/
#                     o Index2.html or Sample2.py or Sample2.R, etc…
#                     o Data2.csv
#                     o Screenshot_sample02.jpg
#                 • Sample03/
#                     o Index3.html or Sample3.py or Sample3.R, etc…
#                     o Data3.csv
#                     o Screenshot_sample03.jpg
#          Requeriments.txt: List dependencies (e.g. R, Shiny R, Python, matplotlib, D3, etc…). 
# IV. What to submit
#     • A .zip file with the file structure shown above
#     • A paper describing the projects, the datasets that were chosen, the thee visualizations or
#       dashboards that were developed (including screenshot), and explanation of what was
#       updated from any sample code that was used.
#     • Submit document through Blackboard. Please use the following file format: your_lastname_project04.zip

# Load standard libraries
library(readr)
library(ggplot2)
library(readxl)
library (MPV)
library (olsrr)
library("ggpubr")
library("Hmisc")
library(caret)
library (lattice)
library(leaps)
library(MASS)
library(alr3)
library(readxl)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library("plotly")
library(corrplot)
library(maps)
library(alr3)
library(rms)
library(ppcor)
library(ggthemes)
library(data.table)
library(ISLR)
library(tibble)
library(aod)
library(tidyverse)
library(psych)
library(pastecs)
library(summarytools)
library(magrittr)
library(scales)
library(sf)
library(lubridate)
library(ggraph)
library(igraph)
library(dplyr)
library(reshape)
library(tidygraph)
library(ggthemes)
library(ggExtra)
library(cowplot)
library(maps)
library(highcharter)
install.packages("imputeTS")
library(imputeTS)
library(corrplot)
# Set  working directory
#setwd("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 4")

# DATA SET 1
# Data on student performance
# https://www.kaggle.com/spscientist/students-performance-in-exams 
# Read csv file
StudentsPerformance <- read_csv("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 4/StudentsPerformance.csv")

# Columns:
# Gender (chr): female, male
# Race/ethnicity (chr): group A, group B, group C, group D
# Parental level of education (chr): bachelor's degree, some college, masters degree, associates degree
# Lunch (chr): standard, reduced
# Test preparation course (chr): none, completed
# Math score (num; double)
# Reading score (num; double)
# Rriting score (num; double)

# Look at the first six rows
head(StudentsPerformance)
# Look at all data & attach it
View(StudentsPerformance)
attach(StudentsPerformance)
str(StudentsPerformance)
# 1000 observations, 8 variables

# Check for missing data/null values
apply(is.na(StudentsPerformance[,]),2,sum)
# No missing values found

# Descriptive Statistics
# Summary Statistics
summary(StudentsPerformance) # N, class, min, 1Q, Median, Mean, 3Q, Max each variable
describe(StudentsPerformance) # N, mean, sd, median, min, max, range, skew, kurtosis
stat.desc(StudentsPerformance) # null, min, max, rang, sum, median, mean, SE mean, CI mean, var, std, coef var
# Mean Math Score: 66.09
# Mean Reading Score: 69.17
# Mean Writing Score: 68.05

# Test Score Boxplots
# Grouped Boxplot
# Good way to compare continuous variables for different categories & check distribution shape & look for outliers
# Boxplots provide info about: Median, Mean, 1Q, 3Q, Min, Max
boxplot(StudentsPerformance[c(6,7,8)], col=c(2,3,5),
        xlab="Student Test Performance",
        ylab="Count",
        main="Grouped Boxplot")

# Bar charts in plotly
testScoreBarChartPlotly <-plot_ly(
  x=c("math score", "reading score", "writing score"),
  y=c(66.09, 69.17, 68.05),
  name="Average Student Performance",
  type="bar"
)
# Display the visualization
testScoreBarChartPlotly

# Beautify the bar chart
# Bar Chart with Hover Text
x<-c("math score", "reading score", "writing score")
y=c(66.09, 69.17, 68.05)
text <-c('Mean Math Score: 66.09', 'Mean Reading Score: 69.17', 'Mean Writing Score: 68.05')
data<-data.frame(x,y,text)
testScoreBarChartHoverTextPlotly <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
                                            marker = list(color = 'rgb(158,202,225)',
                                                          line = list(color = 'rgb(8,48,107)',
                                                                      width = 1.5)))
# Add title & axis labels
testScoreBarChartHoverTextPlotly <- testScoreBarChartHoverTextPlotly %>% layout(title = "Average Student Test Performance",
                                                                                xaxis = list(title = ""),
                                                                                yaxis = list(title = ""))

# Display the visualization
testScoreBarChartHoverTextPlotly

# We can also add direct labels
x<-c("math score", "reading score", "writing score")
y=c(66.09, 69.17, 68.05)
text <-c('Mean Math Score: 66.09', 'Mean Reading Score: 69.17', 'Mean Writing Score: 68.05')
data<-data.frame(x,y,text)
testScoreBarChartHoverDirectLabelsPlotly <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
                                                    text = y, textposition = 'auto',
                                                    marker = list(color = 'rgb(158,202,225)',
                                                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
# Add title & axis labels
testScoreBarChartHoverDirectLabelsPlotly <- testScoreBarChartHoverDirectLabelsPlotly %>% layout(title = "Average Student Test Performance",
                                                                                                xaxis = list(title = ""),
                                                                                                yaxis = list(title = ""))
# Display the visualization
testScoreBarChartHoverDirectLabelsPlotly

# Math Score Scatterplot
gender_math_scatter <-StudentsPerformance %>% 
  select(gender, `math score`) %>% filter(!is.na(`math score`)) %>% 
  ggplot(aes(x=gender, y = `math score`, color = gender)) + 
  geom_line ()+ geom_jitter(width = 0.2, alpha = 0.2) +  
  ggtitle('Gender Differences in Math Scores')
gender_math_scatter

# Math Score Boxplot
gender_math_graph <- StudentsPerformance %>% 
  select(gender, `math score`) %>% filter(!is.na(`math score`)) %>% 
  ggplot(aes(x=gender, y = `math score`, color = gender)) + 
  geom_boxplot()+ geom_jitter(width = 0.2, alpha = 0.2) +  
  ggtitle('Gender Differences in Math Scores')
gender_math_graph

# Reading Score Boxplot
gender_reading_graph <- StudentsPerformance %>% 
  select(gender, `reading score`) %>% filter(!is.na(`reading score`)) %>% 
  ggplot(aes(x=gender, y = `reading score`, color = gender)) + 
  geom_boxplot()+ geom_jitter(width = 0.2, alpha = 0.2) +  
  ggtitle('Gender Differences in Reading Scores')
gender_reading_graph

# Writing Score Boxplot
gender_writing_graph <- StudentsPerformance %>% 
  select(gender, `writing score`) %>% filter(!is.na(`writing score`)) %>% 
  ggplot(aes(x=gender, y = `writing score`, color = gender)) + 
  geom_boxplot()+ geom_jitter(width = 0.2, alpha = 0.2) +  
  ggtitle('Gender Differences in Writing Scores')
gender_writing_graph

# Histograms are useful to show the distribution of continuous data
# We can display two genders on the same graph and compare their distributions
# Histogram: Gender Math Score
gender_math_histogram <- StudentsPerformance %>% filter(!is.na(`math score`)) %>% 
  ggplot(aes(`math score`, fill = gender)) + 
  geom_histogram(binwidth = 5) +
  ggtitle('Distribution of Gender Differences in Student Math Scores')
gender_math_histogram

# Histogram: Gender Reading Score
gender_reading_histogram <- StudentsPerformance %>% filter(!is.na(`reading score`)) %>% 
  ggplot(aes(`reading score`, fill = gender)) + 
  geom_histogram(binwidth = 5) +
  ggtitle('Distribution of Gender Differences in Student Reading Scores')
gender_reading_histogram

# Histogram: Gender Writing Score
gender_writing_histogram <- StudentsPerformance %>% filter(!is.na(`writing score`)) %>% 
  ggplot(aes(`writing score`, fill = gender)) + 
  geom_histogram(binwidth = 5) +
  ggtitle('Distribution of Gender Differences in Student Writing Scores')
gender_writing_histogram

# Stacked Vertical Line Graph (gray background)
# Student Performance on Math and Writing Scores by Gender
ggplot(data = StudentsPerformance, aes(x = gender)) +
  geom_line(aes(y = `math score`, color = 'Math Score')) +
  geom_line(aes(y = `writing score`, color = 'Writing Score')) +
  xlab('Gender') + ylab('Test Score') +
  ggtitle('Student Performance on Math and Writing Scores by Gender')

# Stacked Vertical Line Graph (white background)
# Student Performance on Math and Writing Scores by Gender
# Changing the theme of a plot is as easy as adding a: "+ theme_??()"
# Example: theme_bw()
ggplot(data = StudentsPerformance, aes(x = gender)) +
  geom_line(aes(y = `math score`, color = 'Math Score')) +
  geom_line(aes(y = `writing score`, color = 'Writing Score')) +
  xlab('Gender') + ylab('Test Score') +
  ggtitle('Student Performance on Math and Writing Scores by Gender') +
  theme_bw()

# Horizontal Boxplot
# Lunch Status on Student Performance in Reading
boxplot(`reading score`~lunch,data=StudentsPerformance,
        horizontal=TRUE,
        names=c("Standard","Free"),
        col=c("steelblue","wheat"),
        xlab="Student Performance in Reading",
        main="Lunch Status on Student Performance in Reading")

# Vertical Boxplot
# Lunch Status on Student Performance in Reading
boxplot(`reading score`~lunch,data=StudentsPerformance,
        horizontal=FALSE,
        names=c("Standard","Free"),
        col=c("steelblue","wheat"),
        xlab="Lunch",
        ylab="Student Reading Score",
        main="Lunch Status on Student Performance in Reading")

# Vertical q plot
# Test Preparation Effect on Student Math Score
qplot(`test preparation course`, `math score`, data = StudentsPerformance, fill = I("darkblue"))

# Vertical Bar chart/Histogram Bins
counts <- table(StudentsPerformance$`writing score`)
barplot(counts, main="Writing Score Distribution",
        xlab="Writing Score", ylab="Count")

# Stacked Scatterplot
# Effect of Parent Education and Race/Ethnicity on Student Math Scores
math_parent_edu <- StudentsPerformance %>% 
  ggplot(aes(x= `parental level of education`, y = `math score`, color = `race/ethnicity`)) + 
  geom_jitter( width = 0.3,alpha = 0.5) + 
  ggtitle('Effect of Parent Education & Race/Ethnicity on Math Scores')
math_parent_edu
# Use theme() function to make x-axis readable
math_parent_edu <- math_parent_edu + theme(axis.text.x = element_text(angle = 90))
math_parent_edu

# Add a filter
raceFilter <- highlight_key(math_parent_edu)
gg <- ggplot(raceFilter) + 
  geom_point(aes(x= `parental level of education`, y = `math score`, color = `race/ethnicity`)) + 
  geom_jitter( width = 0.3,alpha = 0.5) + 
  ggtitle('Effect of Parent Education & Race/Ethnicity on Math Scores') +
  theme(axis.text.x = element_text(angle = 90))
filter<-bscols(
  filter_select("id", "Select a Race", raceFilter, ~`race/ethnicity`),
  ggplotly(gg)
)
bscols(filter)

# Effect of Parent Education and Race/Ethnicity on Student Reading Scores
# Use theme() function to make x-axis readable
reading_parent_edu <- StudentsPerformance %>% 
  ggplot(aes(x= `parental level of education`, y = `reading score`, color = `race/ethnicity`)) + 
  geom_jitter( width = 0.3,alpha = 0.5) + 
  ggtitle('Effect of Parent Education & Race/Ethnicity on Reading Scores') +
  theme(axis.text.x = element_text(angle = 90))
reading_parent_edu

# Effect of Parent Education and Race/Ethnicity on Student Writing Scores
# Use theme() function to make x-axis readable
writing_parent_edu <- StudentsPerformance %>% 
  ggplot(aes(x= `parental level of education`, y = `writing score`, color = `race/ethnicity`)) + 
  geom_jitter( width = 0.3,alpha = 0.5) + 
  ggtitle('Effect of Parent Education & Race/Ethnicity on Writing Scores') +
  theme(axis.text.x = element_text(angle = 90))
writing_parent_edu

# Use theme() function to make x-axis readable
math_parent_edu <- math_parent_edu + theme(axis.text.x = element_text(angle = 90))
math_parent_edu

# Grades Give According to Score in Each scubject
StudentsPerformance$mathGrade <- ifelse(StudentsPerformance$`math score`>=0 & StudentsPerformance$`math score` <= 32,'F',
                                        ifelse(StudentsPerformance$`math score`>=33 & StudentsPerformance$`math score`<=50,'D',
                                               ifelse(StudentsPerformance$`math score`>=51 & StudentsPerformance$`math score`<=70,'C',
                                                      ifelse(StudentsPerformance$`math score`>=71 & StudentsPerformance$`math score` <= 90,'B',
                                                             ifelse(StudentsPerformance$`math score`>=91 & StudentsPerformance$`math score`<=100,'A','Na')))))
StudentsPerformance$readingGrade <- ifelse(StudentsPerformance$`reading score`>=0 & StudentsPerformance$`reading score` <= 32,'F',
                                           ifelse(StudentsPerformance$`reading score`>=33 & StudentsPerformance$`reading score`<=50,'D',
                                                  ifelse(StudentsPerformance$`reading score`>=51 & StudentsPerformance$`reading score`<=70,'C',
                                                         ifelse(StudentsPerformance$`reading score`>=71 & StudentsPerformance$`reading score` <= 90,'B',
                                                                ifelse(StudentsPerformance$`reading score`>=91 & StudentsPerformance$`reading score`<=100,'A','Na')))))
StudentsPerformance$writingGrade <- ifelse(StudentsPerformance$`writing score`>=0 & StudentsPerformance$`writing score` <= 32,'F',
                                           ifelse(StudentsPerformance$`writing score`>=33 & StudentsPerformance$`writing score`<=50,'D',
                                                  ifelse(StudentsPerformance$`writing score`>=51 & StudentsPerformance$`writing score`<=70,'C',
                                                         ifelse(StudentsPerformance$`writing score`>=71 & StudentsPerformance$`writing score` <= 90,'B',
                                                                ifelse(StudentsPerformance$`writing score`>=91 & StudentsPerformance$`writing score`<=100,'A','Na')))))

# Recheck header: Make sure new variables were added as columns
head(StudentsPerformance)

# Summary Table for each performance score
SummaryScoreTable=StudentsPerformance %>% 
  group_by(`math score`,`reading score`,`writing score`) %>% 
  summarise('Total.Pct(%)' = (sum(`math score`,`reading score`,`writing score`)/300)*100)
head(SummaryScoreTable)
# Table Results
#       `math score` `reading score` `writing score` `Total.Pct(%)`
#1            0              17              10            9  
#2            8              24              23           18.3
#3           18              32              28           26  
#4           19              38              32           29.7
#5           22              39              33           31.3
#6           23              44              36           34.3

# Gender differences in Math Grade Distributions
Math_Grade = StudentsPerformance %>% group_by(gender,mathGrade) %>% summarise(mathGrade_count = sum(length(mathGrade)))
head(Math_Grade)
#  Table Results
# Row Gender    MathGrade     MathGrade_count
#  1  female    A                   21
#  2  female    B                  149
#  3  female    C                  251
#  4  female    D                   82
#  5  female    F                   15
#  6  male      A                   29

# Vertical BarChart
# Gender by Math Grade
math_grade_gender_plot <-ggplot(data = Math_Grade) +
  geom_bar(mapping = aes(x = mathGrade,mathGrade_count,fill = gender), stat = 'Identity',position = 'dodge') + 
  ggtitle('Effect of Gender on Distribution of Math Grades')
math_grade_gender_plot

# Select filter
genderHighlight <- highlight_key(Math_Grade)
gg2<-ggplot(genderHighlight) + 
  geom_bar(mapping = aes(x = mathGrade,mathGrade_count,fill = gender), stat = 'Identity',position = 'dodge') + 
  ggtitle('Effect of Gender on Distribution of Math Grades')
select <- bscols(
  filter_select("id", "Select a Gender", genderHighlight, ~gender),
  ggplotly(gg2)
)
bscols(select)

# Side by Side Vertical BarCharts
# Gender by Distribution of Grades
# Create dataset of male and female test scores and grades
view(StudentsPerformance)
Data = StudentsPerformance[c(1,6,7,8,9,10,11)]
head(Data)
# Gender by Math Grades
ggplot(data = Data)+
  geom_bar(mapping = aes(x = gender,`math score`,fill = mathGrade),stat = 'Identity',position = 'dodge') +
  ggtitle('Effect of Gender on Distribution of Math Grades')

# Gender by Reading Grades
ggplot(data = Data)+
  geom_bar(mapping = aes(x = gender,`reading score`,fill = readingGrade),stat = 'Identity',position = 'dodge') +
  ggtitle('Effect of Gender on Distribution of Reading Grades')

# Gender by Writing Grades
ggplot(data = Data)+
  geom_bar(mapping = aes(x = gender,`writing score`,fill = writingGrade),stat = 'Identity',position = 'dodge') +
  ggtitle('Effect of Gender on Distribution of Writing Grades')

# Gender Differences in Reading Grades
Reading_Grade = StudentsPerformance %>% group_by(gender,readingGrade) %>% summarise(readingGrade_count = sum(length(readingGrade)))
head(Reading_Grade)
# Table Results
# Row   Gender  readingGrade  readingGrade_count
# 1     female      A                 52
# 2     female      B                 250
# 3     female      C                 184
# 4     female      D                 27
# 5     female      F                 5
# 6     male        A                 10

# Vertical BarChart
# Gender by Reading Grade
ggplot(data = Reading_Grade) +
  geom_bar(mapping = aes(x = readingGrade,readingGrade_count,fill = gender), stat = 'Identity',position = 'dodge') + 
  ggtitle('Effect of Gender on Distribution of Reading Grades')

# Gender Differences in Writing Grades
Writing_Grade = StudentsPerformance %>% group_by(gender,writingGrade) %>% summarise(writingGrade_count = sum(length(writingGrade)))
head(Writing_Grade)
# Table Results
# Row   Gender  writingGrade  writingGrade_count
# 1     female      A                 58
# 2     female      B                 243
# 3     female      C                 182
# 4     female      D                 28
# 5     female      F                 7
# 6     male        A                 10

# Vertical BarChart
# Gender by Writing Grade
ggplot(data = Writing_Grade) +
  geom_bar(mapping = aes(x = writingGrade,writingGrade_count,fill = gender), stat = 'Identity',position = 'dodge') + 
  ggtitle('Effect of Gender on Distribution of Writing Grades')

# Group-By Scatterplot with tooltip
groupByScatterParentEduPlotly <- plot_ly(
  type = 'scatter',
  x = StudentsPerformance$`race/ethnicity`,
  y = StudentsPerformance$`math score`,
  text = paste("Student: ", rownames(StudentsPerformance),
               "<br>Race/Ethnicity: ", StudentsPerformance$`race/ethnicity`,
               "<br>Math Score: ", StudentsPerformance$`math score`,
               "<br>Parent's Level of Education: ", StudentsPerformance$`parental level of education`),
  hoverinfo = 'text',
  mode = 'markers',
  transforms = list(
    list(
      type = 'groupby',
      groups = StudentsPerformance$`parental level of education`,
      styles = list(
        list(target = "bachelor's degree", value = list(marker =list(color = 'blue'))),
        list(target = "some college", value = list(marker =list(color = 'red'))),
        list(target = "associate's degree", value = list(marker =list(color = 'black'))),
        list(target = "high school", value=list(marker=list(color='yellow'))))
    )
  )
)

# Add title & axis labels
groupByScatterParentEduPlotly <- groupByScatterParentEduPlotly %>% layout(title = "Effect of Parental Education & Race on Student Math Scores",
                                                                          xaxis = list(title = "Race/Ethnicity"),
                                                                          yaxis = list(title = "Math Scores"))
# Display the visualization
groupByScatterParentEduPlotly

# Filter
parentEduFilter <- highlight_key(StudentsPerformance)
gg3 <-ggplot(parentEduFilter) + geom_point(aes(`race/ethnicity`,`math score`,fill =`parental level of education`))
onlysee <- bscols(
  filter_select("id","Select Parental Education", parentEduFilter, ~`parental level of education`),
  ggplotly(gg3)
)
bscols(onlysee)

#Swap GroupBy Parameter
groupByRaceScatterPlotly <- plot_ly(
  type = 'scatter',
  x = StudentsPerformance$`parental level of education`,
  y = StudentsPerformance$`math score`,
  text = paste("Student: ", rownames(StudentsPerformance),
               "<br>Parental Level of Education: ", StudentsPerformance$`parental level of education`,
               "<br>Math Score: ", StudentsPerformance$`math score`,
               "<br>Student Race/Ethnicity: ", StudentsPerformance$`race/ethnicity`),
  hoverinfo = 'text',
  mode = 'markers',
  transforms = list(
    list(
      type = 'groupby',
      groups = StudentsPerformance$`race/ethnicity`,
      styles = list(
        list(target = "group A", value = list(marker =list(color = 'blue'))),
        list(target = "group B", value = list(marker =list(color = 'red'))),
        list(target = "group C", value = list(marker =list(color = 'black'))),
        list(target = "group D", value = list(marker =list(color ='yellow')))
      )
    )
  )
)

# Add title & axis labels
groupByRaceScatterPlotly <- groupByRaceScatterPlotly %>% layout(title = "Effect of Parental Education & Race on Student Math Scores",
                                                                xaxis = list(title = "Parental Level of Education"),
                                                                yaxis = list(title = "Math Scores"))
# Display the visualization
groupByRaceScatterPlotly


#Plotly heatmap for correlations
corr2 <- cor(dplyr::select_if(StudentsPerformance, is.numeric))
plot_ly(colors='RdBu') %>%
  add_heatmap(x=rownames(corr2), y=colnames(corr2), z=corr2)%>%
  colorbar(limits=c(-1,1)) %>% layout(title = "Student Test Score Correlation Heatmap")


# Correlation matrix
corr2a <- round(cor(StudentsPerformance$`math score`, StudentsPerformance$`reading score`, StudentsPerformance$`writing score`), 1)

# Plot
ggcorrplot(corr2a, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Student Performance", 
           ggtheme=theme_bw)