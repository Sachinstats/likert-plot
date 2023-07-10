library(readxl)
library(haven)
library(finalfit)
library(dplyr)
library(ggplot2)
library(likert)
library(patchwork)
library(knitr)
library(kableExtra)
library(tidyverse)
library(tidytext)
data <- read_sav("C:/Users/Sachin/Downloads/final absenteeism data cleaned.sav")
#glimpse(data)
library(labelled)
data <-  data %>% unlabelled()
data11 <- data[,c(14:18)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data11 <-  as.data.frame(data11)
names(data11) = c("The remuneration paid  for the work I do. ","The possibility to make as much money as my friends","How my pay compares with that for parallel jobs in other companies","My salary and the amount of work I do","How my salary compares with that same status of other workers")

result_wps <- likert(data11, )
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "pay", y = "Percent")

plot_wps
#######################################################################
data22 <- data[,c(19:24)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data22 <-  as.data.frame(data22)
glimpse(data22)
names(data22) = c("The courage of co-operation among my co-workers","The possibility to build up close companionship with my co-workers.","The openness of my co-workers",". The approach of my co-workers are easy to make friends With","The manner of my co-workers get along with each other.")

result_wps <- likert(data11)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "co-worker", y = "Percent")

plot_wps
###############################################################################

data33 <- data[,c(25:29)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data33 <-  as.data.frame(data33)
glimpse(data33)
names(data33) = c(". Employees job security.","The way my job gives for a secure future.","How steady employees job is"," The method of my job provides for steady and stable employment"," The system of  layoffs and transfers are avoid in my job.")
result_wps <- likert(data33)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
###############################################################################

data44<- data[,c(30:34)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data44 <-  as.data.frame(data44)
glimpse(data44)
names(data44) = c(" The technical “know-how” of my supervisor","The competence of my supervisor in making decisions","The way my boss delegates work to others","The way my boss provides help on hard problems","The way my boss trains his/her employees")
result_wps <- likert(data44)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Supervision / Superior – Subordinate Relationship", y = "Percent")

plot_wps
#######################################################################################ata44<- data[,c(30:34)]
data55<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data55 <-  as.data.frame(data55)
glimpse(data55)
names(data55) = c("The working conditions (lighting, ventilation,heating, etc.) on this job","The physical surroundings where I work","The pleasantness of the working conditions","The physical conditions of the job","The working conditions")
result_wps <- likert(data55)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Environment Conditions", y = "Percent")

plot_wps
##########################################################################################################################################################

data66<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data66 <-  as.data.frame(data66)
glimpse(data66)
names(data66) = c("I enjoy the ‘social’ aspect of my work","I am satisfied with my surrounding environment","I enjoy interacting with my colleague","Your job has great impact on the people outside the organisation","The results of your work are likely to affect the lives of other people")
result_wps <- likert(data66)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Work It Self ", y = "Percent")

plot_wps
#################################################################################

data77<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data77 <-  as.data.frame(data77)
glimpse(data77)
names(data77) = c("Your job is very important and significant in the broader scheme of things","The opportunity to do different things from time to time variety in my work","The variety  my work","The routine  my work")
result_wps <- likert(data77)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
##############################################################################################

data88<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data88 <-  as.data.frame(data88)
glimpse(data88)
names(data88) = c("The chance to do something different every day","The chance to do many things on the job","I have a good sense of what makes my job meaningful","I have discovered work that has a satisfying purpose")
result_wps <- likert(data88)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Task Significance", y = "Percent")

plot_wps
######################################################################################

data99<- data[,c(44:50)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data99 <-  as.data.frame(data99)
glimpse(data99)
names(data99) = c("Being able to see the results of the work I do.","Being able to take pride in a job done","Being able to do something worthwhile","The way I am noticed when I do a good job",". The way I get full credit for the work I do.","The recognition I get for the work I do","The chance to be responsible for planning my work")
result_wps <- likert(data99)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Recognise ", y = "Percent")

plot_wps
######################################################################################3

data10<- data[,c(51:56)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data10 <-  as.data.frame(data10)
glimpse(data10)
names(data10) = c(". The chance to make decisions on my work.","Overall, I am pleased with my work","My job in this organisation has met my expectations","Overall, I am satisfied in my current practice","My current work situation is not a major source of
frustration in my life","In general, I like my job.")
result_wps <- likert(data10)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Responsibility", y = "Percent")

plot_wps


#################################################################################
##############################GENDER############################################  
################################################################################


data111 <- data[,c(14:18)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data111 <-  as.data.frame(data111)
names(data111) = c("The remuneration paid  for the work I do. ","The possibility to make as much money as my friends","How my pay compares with that for parallel jobs in other companies","My salary and the amount of work I do","How my salary compares with that same status of other workers")

result_wps <- likert(data111,grouping = data$Gender)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "pay", y = "Percent")

plot_wps

####################################################################################
data222 <- data[,c(19:24)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data222 <-  as.data.frame(data222)
glimpse(data222)
names(data222) = c("The courage of co-operation among my co-workers","The possibility to build up close companionship with my co-workers.","The openness of my co-workers",". The approach of my co-workers are easy to make friends With","The manner of my co-workers get along with each other.")

result_wps <- likert(data11,grouping = data$Gender)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "co-worker", y = "Percent")

plot_wps
########################################################################################
data333 <- data[,c(25:29)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data333 <-  as.data.frame(data333,grouping = data$Gender)
glimpse(data33)
names(data333) = c(". Employees job security.","The way my job gives for a secure future.","How steady employees job is"," The method of my job provides for steady and stable employment"," The system of  layoffs and transfers are avoid in my job.")
result_wps <- likert(data333)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
#########################################################################################
data444<- data[,c(30:34)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data444 <-  as.data.frame(data444)
glimpse(data444)
names(data444) = c(" The technical “know-how” of my supervisor","The competence of my supervisor in making decisions","The way my boss delegates work to others","The way my boss provides help on hard problems","The way my boss trains his/her employees")
result_wps <- likert(data44,grouping = data$Gender)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Supervision / Superior – Subordinate Relationship", y = "Percent")

plot_wps
#########################################################################################
data555<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data55 <-  as.data.frame(data555)
glimpse(data555)
names(data555) = c("The working conditions (lighting, ventilation,heating, etc.) on this job","The physical surroundings where I work","The pleasantness of the working conditions","The physical conditions of the job","The working conditions")
result_wps <- likert(data55,grouping = data$Gender)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Environment Conditions", y = "Percent")

plot_wps
########################################################################################
data666<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data666 <-  as.data.frame(data666)
glimpse(data666)
names(data666) = c("I enjoy the ‘social’ aspect of my work","I am satisfied with my surrounding environment","I enjoy interacting with my colleague","Your job has great impact on the people outside the organisation","The results of your work are likely to affect the lives of other people")
result_wps <- likert(data666,,grouping = data$Gender)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Work It Self ", y = "Percent")

plot_wps
############################################################################################
data777<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data777 <-  as.data.frame(data777)
glimpse(data777)
names(data777) = c("Your job is very important and significant in the broader scheme of things","The opportunity to do different things from time to time variety in my work","The variety  my work","The routine  my work")
result_wps <- likert(data777,grouping = data$Gender)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
###################################################################################
data88<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data88 <-  as.data.frame(data88)
glimpse(data88)
names(data88) = c("The chance to do something different every day","The chance to do many things on the job","I have a good sense of what makes my job meaningful","I have discovered work that has a satisfying purpose")
result_wps <- likert(data88,grouping = data$Gender)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Task Significance", y = "Percent")

plot_wps
#################################################################################
data999<- data[,c(44:50)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data999 <-  as.data.frame(data999)
glimpse(data999)
names(data999) = c("Being able to see the results of the work I do.","Being able to take pride in a job done","Being able to do something worthwhile","The way I am noticed when I do a good job",". The way I get full credit for the work I do.","The recognition I get for the work I do","The chance to be responsible for planning my work")
result_wps <- likert(data999,grouping = data$Gender)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Recognise ", y = "Percent")

plot_wps
############################################################################

data100<- data[,c(51:56)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)
data100 <-  as.data.frame(data100)
glimpse(data100)
names(data100) = c(". The chance to make decisions on my work.","Overall, I am pleased with my work","My job in this organisation has met my expectations","Overall, I am satisfied in my current practice","My current work situation is not a major source of
frustration in my life","In general, I like my job.")

result_wps <- likert(data11,grouping = data$Gender)

                                          
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Responsibility", y = "Percent")

plot_wps
####################################################################################

###################################################################################
#################################Marital status####################################
data111 <- data[,c(14:18)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data111 <-  as.data.frame(data111)
names(data111) = c("The remuneration paid  for the work I do. ","The possibility to make as much money as my friends","How my pay compares with that for parallel jobs in other companies","My salary and the amount of work I do","How my salary compares with that same status of other workers")

result_wps <- likert(data111,grouping = data$Marital_status)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "pay", y = "Percent")

plot_wps
#################################################################################
data222 <- data[,c(19:24)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data222 <-  as.data.frame(data222)
glimpse(data222)
names(data222) = c("The courage of co-operation among my co-workers","The possibility to build up close companionship with my co-workers.","The openness of my co-workers",". The approach of my co-workers are easy to make friends With","The manner of my co-workers get along with each other.")

result_wps <- likert(data11,grouping = data$Marital_status)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "co-worker", y = "Percent")

plot_wps
###################################################################################

data333 <- data[,c(25:29)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data333 <-  as.data.frame(data333,grouping = data$Marital_status)
glimpse(data33)
names(data333) = c(". Employees job security.","The way my job gives for a secure future.","How steady employees job is"," The method of my job provides for steady and stable employment"," The system of  layoffs and transfers are avoid in my job.")
result_wps <- likert(data333)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
######################################################################################
data444<- data[,c(30:34)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data444 <-  as.data.frame(data444)
glimpse(data444)
names(data444) = c(" The technical “know-how” of my supervisor","The competence of my supervisor in making decisions","The way my boss delegates work to others","The way my boss provides help on hard problems","The way my boss trains his/her employees")
result_wps <- likert(data44,grouping = data$Marital_status)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Supervision / Superior – Subordinate Relationship", y = "Percent")

plot_wps
##################################################################################
data555<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data55 <-  as.data.frame(data555)
glimpse(data555)
names(data555) = c("The working conditions (lighting, ventilation,heating, etc.) on this job","The physical surroundings where I work","The pleasantness of the working conditions","The physical conditions of the job","The working conditions")
result_wps <- likert(data55,grouping = data$Marital_status)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Environment Conditions", y = "Percent")

plot_wps
################################################################################
data666<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data666 <-  as.data.frame(data666)
glimpse(data666)
names(data666) = c("I enjoy the ‘social’ aspect of my work","I am satisfied with my surrounding environment","I enjoy interacting with my colleague","Your job has great impact on the people outside the organisation","The results of your work are likely to affect the lives of other people")
result_wps <- likert(data666,,grouping = data$Marital_status)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Work It Self ", y = "Percent")

plot_wps
############################################################################################
data777<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data777 <-  as.data.frame(data777)
glimpse(data777)
names(data777) = c("Your job is very important and significant in the broader scheme of things","The opportunity to do different things from time to time variety in my work","The variety  my work","The routine  my work")
result_wps <- likert(data777,grouping = data$Marital_status)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
###################################################################################
data88<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data88 <-  as.data.frame(data88)
glimpse(data88)
names(data88) = c("The chance to do something different every day","The chance to do many things on the job","I have a good sense of what makes my job meaningful","I have discovered work that has a satisfying purpose")
result_wps <- likert(data88,grouping = data$Marital_status)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Task Significance", y = "Percent")

plot_wps
#################################################################################
data999<- data[,c(44:50)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data999 <-  as.data.frame(data999)
glimpse(data999)
names(data999) = c("Being able to see the results of the work I do.","Being able to take pride in a job done","Being able to do something worthwhile","The way I am noticed when I do a good job",". The way I get full credit for the work I do.","The recognition I get for the work I do","The chance to be responsible for planning my work")
result_wps <- likert(data999,grouping = data$Marital_status)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Recognise ", y = "Percent")

plot_wps
############################################################################

data100<- data[,c(51:56)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)
data100 <-  as.data.frame(data100)
glimpse(data100)
names(data100) = c(". The chance to make decisions on my work.","Overall, I am pleased with my work","My job in this organisation has met my expectations","Overall, I am satisfied in my current practice","My current work situation is not a major source of
frustration in my life","In general, I like my job.")

result_wps <- likert(data11,grouping = data$Marital_status)


plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Responsibility", y = "Percent")

plot_wps
####################################################################################################################################
####################################################AGE############################################################
####################################################################################################################
data111 <- data[,c(14:18)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data111 <-  as.data.frame(data111)
names(data111) = c("The remuneration paid  for the work I do. ","The possibility to make as much money as my friends","How my pay compares with that for parallel jobs in other companies","My salary and the amount of work I do","How my salary compares with that same status of other workers")

result_wps <- likert(data111,grouping = data$Age)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "pay", y = "Percent")

plot_wps
#################################################################################
data222 <- data[,c(19:24)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data222 <-  as.data.frame(data222)
glimpse(data222)
names(data222) = c("The courage of co-operation among my co-workers","The possibility to build up close companionship with my co-workers.","The openness of my co-workers",". The approach of my co-workers are easy to make friends With","The manner of my co-workers get along with each other.")

result_wps <- likert(data11,grouping = data$Age)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "co-worker", y = "Percent")

plot_wps
###################################################################################

data333 <- data[,c(25:29)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data333 <-  as.data.frame(data333,grouping = data$Age)
glimpse(data33)
names(data333) = c(". Employees job security.","The way my job gives for a secure future.","How steady employees job is"," The method of my job provides for steady and stable employment"," The system of  layoffs and transfers are avoid in my job.")
result_wps <- likert(data333)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
######################################################################################
data444<- data[,c(30:34)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data444 <-  as.data.frame(data444)
glimpse(data444)
names(data444) = c(" The technical “know-how” of my supervisor","The competence of my supervisor in making decisions","The way my boss delegates work to others","The way my boss provides help on hard problems","The way my boss trains his/her employees")
result_wps <- likert(data44,grouping = data$Age)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Supervision / Superior – Subordinate Relationship", y = "Percent")

plot_wps
##################################################################################
data555<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data55 <-  as.data.frame(data555)
glimpse(data555)
names(data555) = c("The working conditions (lighting, ventilation,heating, etc.) on this job","The physical surroundings where I work","The pleasantness of the working conditions","The physical conditions of the job","The working conditions")
result_wps <- likert(data55,grouping = data$Age)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Environment Conditions", y = "Percent")

plot_wps
################################################################################
data666<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data666 <-  as.data.frame(data666)
glimpse(data666)
names(data666) = c("I enjoy the ‘social’ aspect of my work","I am satisfied with my surrounding environment","I enjoy interacting with my colleague","Your job has great impact on the people outside the organisation","The results of your work are likely to affect the lives of other people")
result_wps <- likert(data666,,grouping = data$Age)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Work It Self ", y = "Percent")

plot_wps
############################################################################################
data777<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data777 <-  as.data.frame(data777)
glimpse(data777)
names(data777) = c("Your job is very important and significant in the broader scheme of things","The opportunity to do different things from time to time variety in my work","The variety  my work","The routine  my work")
result_wps <- likert(data777,grouping = data$Age)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
###################################################################################
data88<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data88 <-  as.data.frame(data88)
glimpse(data88)
names(data88) = c("The chance to do something different every day","The chance to do many things on the job","I have a good sense of what makes my job meaningful","I have discovered work that has a satisfying purpose")
result_wps <- likert(data88,grouping = data$Age)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Task Significance", y = "Percent")

plot_wps
#################################################################################
data999<- data[,c(44:50)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data999 <-  as.data.frame(data999)
glimpse(data999)
names(data999) = c("Being able to see the results of the work I do.","Being able to take pride in a job done","Being able to do something worthwhile","The way I am noticed when I do a good job",". The way I get full credit for the work I do.","The recognition I get for the work I do","The chance to be responsible for planning my work")
result_wps <- likert(data999,grouping = data$Age)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Recognise ", y = "Percent")

plot_wps
############################################################################

data100<- data[,c(51:56)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)
data100 <-  as.data.frame(data100)
glimpse(data100)
names(data100) = c(". The chance to make decisions on my work.","Overall, I am pleased with my work","My job in this organisation has met my expectations","Overall, I am satisfied in my current practice","My current work situation is not a major source of
frustration in my life","In general, I like my job.")

result_wps <- likert(data11,grouping = data$Age)


plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Responsibility", y = "Percent")

plot_wps
################################################################################################
#################################Working areas################################################################
##############################################################################################
data111 <- data[,c(14:18)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data111 <-  as.data.frame(data111)
names(data111) = c("The remuneration paid  for the work I do. ","The possibility to make as much money as my friends","How my pay compares with that for parallel jobs in other companies","My salary and the amount of work I do","How my salary compares with that same status of other workers")
glimpse(data111)
result_wps <- likert(data111,grouping = data$Working_area)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "pay", y = "Percent")

plot_wps
#################################################################################
data222 <- data[,c(19:24)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data222 <-  as.data.frame(data222)
glimpse(data222)
names(data222) = c("The courage of co-operation among my co-workers","The possibility to build up close companionship with my co-workers.","The openness of my co-workers",". The approach of my co-workers are easy to make friends With","The manner of my co-workers get along with each other.")

result_wps <- likert(data11,grouping = data$Working_area)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "co-worker", y = "Percent")

plot_wps
###################################################################################

data333 <- data[,c(25:29)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data333 <-  as.data.frame(data333,grouping = data$Working_area)
glimpse(data33)
names(data333) = c(". Employees job security.","The way my job gives for a secure future.","How steady employees job is"," The method of my job provides for steady and stable employment"," The system of  layoffs and transfers are avoid in my job.")
result_wps <- likert(data333)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
######################################################################################
data444<- data[,c(30:34)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data444 <-  as.data.frame(data444)
glimpse(data444)
names(data444) = c(" The technical “know-how” of my supervisor","The competence of my supervisor in making decisions","The way my boss delegates work to others","The way my boss provides help on hard problems","The way my boss trains his/her employees")
result_wps <- likert(data44,grouping = data$Working_area)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Supervision / Superior – Subordinate Relationship", y = "Percent")

plot_wps
##################################################################################
data555<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data55 <-  as.data.frame(data555)
glimpse(data555)
names(data555) = c("The working conditions (lighting, ventilation,heating, etc.) on this job","The physical surroundings where I work","The pleasantness of the working conditions","The physical conditions of the job","The working conditions")
result_wps <- likert(data55,grouping = data$Working_area)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Environment Conditions", y = "Percent")

plot_wps
################################################################################
data666<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data666 <-  as.data.frame(data666)
glimpse(data666)
names(data666) = c("I enjoy the ‘social’ aspect of my work","I am satisfied with my surrounding environment","I enjoy interacting with my colleague","Your job has great impact on the people outside the organisation","The results of your work are likely to affect the lives of other people")
result_wps <- likert(data666,,grouping = data$Working_area)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Work It Self ", y = "Percent")

plot_wps
############################################################################################
data777<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data777 <-  as.data.frame(data777)
glimpse(data777)
names(data777) = c("Your job is very important and significant in the broader scheme of things","The opportunity to do different things from time to time variety in my work","The variety  my work","The routine  my work")
result_wps <- likert(data777,grouping = data$Working_area)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
###################################################################################
data88<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data88 <-  as.data.frame(data88)
glimpse(data88)
names(data88) = c("The chance to do something different every day","The chance to do many things on the job","I have a good sense of what makes my job meaningful","I have discovered work that has a satisfying purpose")
result_wps <- likert(data88,grouping = data$Working_area)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Task Significance", y = "Percent")

plot_wps
#################################################################################
data999<- data[,c(44:50)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data999 <-  as.data.frame(data999)
glimpse(data999)
names(data999) = c("Being able to see the results of the work I do.","Being able to take pride in a job done","Being able to do something worthwhile","The way I am noticed when I do a good job",". The way I get full credit for the work I do.","The recognition I get for the work I do","The chance to be responsible for planning my work")
result_wps <- likert(data999,grouping = data$Working_area)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Recognise ", y = "Percent")

plot_wps
############################################################################

data100<- data[,c(51:56)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)
data100 <-  as.data.frame(data100)
glimpse(data100)
names(data100) = c(". The chance to make decisions on my work.","Overall, I am pleased with my work","My job in this organisation has met my expectations","Overall, I am satisfied in my current practice","My current work situation is not a major source of
frustration in my life","In general, I like my job.")

result_wps <- likert(data11,grouping = data$Working_area)


plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Responsibility", y = "Percent")

plot_wps
####################################################################################################################
####################################Length of employment########################################
##################################################################################################################3
data111 <- data[,c(14:18)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data111 <-  as.data.frame(data111)
names(data111) = c("The remuneration paid  for the work I do. ","The possibility to make as much money as my friends","How my pay compares with that for parallel jobs in other companies","My salary and the amount of work I do","How my salary compares with that same status of other workers")

result_wps <- likert(data111,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "pay", y = "Percent")

plot_wps
#################################################################################
data222 <- data[,c(19:24)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data222 <-  as.data.frame(data222)
glimpse(data222)
names(data222) = c("The courage of co-operation among my co-workers","The possibility to build up close companionship with my co-workers.","The openness of my co-workers",". The approach of my co-workers are easy to make friends With","The manner of my co-workers get along with each other.")

result_wps <- likert(data11,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "co-worker", y = "Percent")

plot_wps
###################################################################################

data333 <- data[,c(25:29)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data333 <-  as.data.frame(data333,grouping = data$Length_of_Employment)
glimpse(data33)
names(data333) = c(". Employees job security.","The way my job gives for a secure future.","How steady employees job is"," The method of my job provides for steady and stable employment"," The system of  layoffs and transfers are avoid in my job.")
result_wps <- likert(data333,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
######################################################################################
data444<- data[,c(30:34)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data444 <-  as.data.frame(data444)
glimpse(data444)
names(data444) = c(" The technical “know-how” of my supervisor","The competence of my supervisor in making decisions","The way my boss delegates work to others","The way my boss provides help on hard problems","The way my boss trains his/her employees")
result_wps <- likert(data44,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Supervision / Superior – Subordinate Relationship", y = "Percent")

plot_wps
##################################################################################
data555<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data55 <-  as.data.frame(data555)
glimpse(data555)
names(data555) = c("The working conditions (lighting, ventilation,heating, etc.) on this job","The physical surroundings where I work","The pleasantness of the working conditions","The physical conditions of the job","The working conditions")
result_wps <- likert(data55,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Environment Conditions", y = "Percent")

plot_wps
################################################################################
data666<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data666 <-  as.data.frame(data666)
glimpse(data666)
names(data666) = c("I enjoy the ‘social’ aspect of my work","I am satisfied with my surrounding environment","I enjoy interacting with my colleague","Your job has great impact on the people outside the organisation","The results of your work are likely to affect the lives of other people")
result_wps <- likert(data666,,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Work It Self ", y = "Percent")

plot_wps
############################################################################################
data777<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data777 <-  as.data.frame(data777)
glimpse(data777)
names(data777) = c("Your job is very important and significant in the broader scheme of things","The opportunity to do different things from time to time variety in my work","The variety  my work","The routine  my work")
result_wps <- likert(data777,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
###################################################################################
data88<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data88 <-  as.data.frame(data88)
glimpse(data88)
names(data88) = c("The chance to do something different every day","The chance to do many things on the job","I have a good sense of what makes my job meaningful","I have discovered work that has a satisfying purpose")
result_wps <- likert(data88,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Task Significance", y = "Percent")

plot_wps
#################################################################################
data999<- data[,c(44:50)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data999 <-  as.data.frame(data999)
glimpse(data999)
names(data999) = c("Being able to see the results of the work I do.","Being able to take pride in a job done","Being able to do something worthwhile","The way I am noticed when I do a good job",". The way I get full credit for the work I do.","The recognition I get for the work I do","The chance to be responsible for planning my work")
result_wps <- likert(data999,grouping = data$Length_of_Employment)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Recognise ", y = "Percent")

plot_wps
############################################################################

data100<- data[,c(51:56)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)
data100 <-  as.data.frame(data100)
glimpse(data100)
names(data100) = c(". The chance to make decisions on my work.","Overall, I am pleased with my work","My job in this organisation has met my expectations","Overall, I am satisfied in my current practice","My current work situation is not a major source of
frustration in my life","In general, I like my job.")

result_wps <- likert(data100,grouping = data$Length_of_Employment)


plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Responsibility", y = "Percent")

plot_wps
##############################################################################################################################
############################################Educational level#################################################
###################################################################################################
data111 <- data[,c(14:18)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data111 <-  as.data.frame(data111)
names(data111) = c("The remuneration paid  for the work I do. ","The possibility to make as much money as my friends","How my pay compares with that for parallel jobs in other companies","My salary and the amount of work I do","How my salary compares with that same status of other workers")

result_wps <- likert(data111,grouping = data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "pay", y = "Percent")

plot_wps
#################################################################################
data222 <- data[,c(19:24)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data222 <-  as.data.frame(data222)
glimpse(data222)
names(data222) = c("The courage of co-operation among my co-workers","The possibility to build up close companionship with my co-workers.","The openness of my co-workers",". The approach of my co-workers are easy to make friends With","The manner of my co-workers get along with each other.")

result_wps <- likert(data222,grouping = data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "co-worker", y = "Percent")

plot_wps
###################################################################################

data333 <- data[,c(25:29)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data333 <-  as.data.frame(data333,grouping = data$Educational_qualification)
glimpse(data33)
names(data333) = c(". Employees job security.","The way my job gives for a secure future.","How steady employees job is"," The method of my job provides for steady and stable employment"," The system of  layoffs and transfers are avoid in my job.")
result_wps <- likert(data333,grouping=data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
######################################################################################
data444<- data[,c(30:34)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data444 <-  as.data.frame(data444)
glimpse(data444)
names(data444) = c(" The technical “know-how” of my supervisor","The competence of my supervisor in making decisions","The way my boss delegates work to others","The way my boss provides help on hard problems","The way my boss trains his/her employees")
result_wps <- likert(data444,grouping = data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Supervision / Superior – Subordinate Relationship", y = "Percent")

plot_wps
##################################################################################
data555<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data555 <-  as.data.frame(data555)
glimpse(data555)
names(data555) = c("The working conditions (lighting, ventilation,heating, etc.) on this job","The physical surroundings where I work","The pleasantness of the working conditions","The physical conditions of the job","The working conditions")
result_wps <- likert(data555,grouping = data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Environment Conditions", y = "Percent")

plot_wps
################################################################################
data666<- data[,c(35:39)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data666 <-  as.data.frame(data666)
glimpse(data666)
names(data666) = c("I enjoy the ‘social’ aspect of my work","I am satisfied with my surrounding environment","I enjoy interacting with my colleague","Your job has great impact on the people outside the organisation","The results of your work are likely to affect the lives of other people")
result_wps <- likert(data666,grouping = data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Work It Self ", y = "Percent")

plot_wps
############################################################################################
data777<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data777 <-  as.data.frame(data777)
glimpse(data777)
names(data777) = c("Your job is very important and significant in the broader scheme of things","The opportunity to do different things from time to time variety in my work","The variety  my work","The routine  my work")
result_wps <- likert(data777,grouping = data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Job Security ", y = "Percent")

plot_wps
###################################################################################
data88<- data[,c(40:43)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data88 <-  as.data.frame(data88)
glimpse(data88)
names(data88) = c("The chance to do something different every day","The chance to do many things on the job","I have a good sense of what makes my job meaningful","I have discovered work that has a satisfying purpose")
result_wps <- likert(data88,grouping = data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Task Significance", y = "Percent")

plot_wps
#################################################################################
data999<- data[,c(44:50)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)

data999 <-  as.data.frame(data999)
glimpse(data999)
names(data999) = c("Being able to see the results of the work I do.","Being able to take pride in a job done","Being able to do something worthwhile","The way I am noticed when I do a good job",". The way I get full credit for the work I do.","The recognition I get for the work I do","The chance to be responsible for planning my work")
result_wps <- likert(data999,grouping = data$Educational_qualification)
plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Recognise ", y = "Percent")

plot_wps
############################################################################

data100<- data[,c(51:56)]
#data11[sapply(data33, is.character)] <- lapply(data11[sapply(data11, is.character)], as.factor)
#data11[sapply(data33, is.factor)] <- lapply(data11[sapply(data11, is.factor)], as.ordered)
data100 <-  as.data.frame(data100)
glimpse(data100)
names(data100) = c(". The chance to make decisions on my work.","Overall, I am pleased with my work","My job in this organisation has met my expectations","Overall, I am satisfied in my current practice","My current work situation is not a major source of
frustration in my life","In general, I like my job.")

result_wps <- likert(data11,grouping = data$Educational_qualification)


plot_wps <- plot(result_wps, centered=TRUE, type = "bar", legend = "") + labs(title = "Responsibility", y = "Percent")

plot_wps
