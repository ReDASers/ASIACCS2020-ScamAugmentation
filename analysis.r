setwd("C:\\Lesson\\Phd\\ReMiND\\Other's works\\Scaffolding New\\New Design\\Results")
table <- read.csv("test_results_new.csv")
source('utils.r')

personality_correlation(table)

draw_bar_plot_reasoning(table)

group_sizes <- general_stat(table)

draw_bar_plot(table)

regression_analysis(table)

#######################################################################################################
###########################################PC vs Mobile################################################
#######################################################################################################
condition1 = (table$platform=='windows'|table$platform=='mac')
condition2 = (table$platform=='android'|table$platform=='iphone')
a = significant_test_with_condition(table, condition1, condition2)
t.test(table$time[condition2], table$time[condition1])

#######################################################################################################
##################################NLG vs Human generated emails########################################
#######################################################################################################
#NLG
ret <- group_stat(table, "Branch B")
print_stat(ret, "NLG")

#Human
ret <- group_stat(table, "Branch D")
print_stat(ret, "Human")

a = significant_test(table, "Branch B", "Branch D")
a = significant_test_per_question(table, "Branch B", "Branch D")

# wilcox.test(table$F1[table$New.Percent.Branch=="Branch B"], table$F1[table$New.Percent.Branch=="Branch D"])
# wilcox.test(table$DR[table$New.Percent.Branch=="Branch B"], table$DR[table$New.Percent.Branch=="Branch D"])
# wilcox.test(table$Fake.DR[table$New.Percent.Branch=="Branch B"], table$Fake.DR[table$New.Percent.Branch=="Branch D"])
# wilcox.test(table$Legit.DR[table$New.Percent.Branch=="Branch B"], table$Legit.DR[table$New.Percent.Branch=="Branch D"])
#######################################################################################################
##################################Gmail vs Linkedin########################################
#######################################################################################################
#Gmail
ret <- group_stat(table, "Branch F")
print_stat(ret, "Gmail")

#Linkedin
ret <- group_stat(table, "Branch H")
print_stat(ret, "Linkedin")

a = significant_test(table, "Branch F", "Branch H")
a = significant_test_per_question(table, "Branch F", "Branch H")
# LCR_Gmail = c(1-table$Fake.DR[table$New.Percent.Branch=="Branch F"], table$Legit.DR[table$New.Percent.Branch=="Branch F"])
# LCR_LinkedIn = c(1-table$Fake.DR[table$New.Percent.Branch=="Branch H"], table$Legit.DR[table$New.Percent.Branch=="Branch H"])
# wilcox.test(LCR_Gmail, LCR_LinkedIn, paired=FALSE)
# t.test(table$F1[table$New.Percent.Branch=="Branch F"], table$F1[table$New.Percent.Branch=="Branch H"])
# wilcox.test(table$DR[table$New.Percent.Branch=="Branch F"], table$DR[table$New.Percent.Branch=="Branch H"])
# wilcox.test(table$Fake.DR[table$New.Percent.Branch=="Branch F"], table$Fake.DR[table$New.Percent.Branch=="Branch H"])
# wilcox.test(table$Legit.DR[table$New.Percent.Branch=="Branch F"], table$Legit.DR[table$New.Percent.Branch=="Branch H"])


#######################################################################################################
###############################sender, receiver info comparison########################################
#######################################################################################################
#Sender
ret <- group_stat(table, "Branch G")
print_stat(ret, "Sender")

#Receiver
ret <- group_stat(table, "Branch E")
print_stat(ret, "Receiver")

#None
ret <- group_stat(table, "Branch A")
print_stat(ret, "None")

#Both
ret <- group_stat(table, "Branch C")
print_stat(ret, "Both")

a = significant_test_four_branches(table, "Branch G", "Branch E",  "Branch A", "Branch C")

library(dplyr)
library(FSA)
library(lattice)
f1scores = c(table$F1[table$New.Percent.Branch=="Branch A"], table$F1[table$New.Percent.Branch=="Branch C"],
                    table$F1[table$New.Percent.Branch=="Branch E"], table$F1[table$New.Percent.Branch=="Branch G"])

detection_rates = c(table$DR[table$New.Percent.Branch=="Branch A"], table$DR[table$New.Percent.Branch=="Branch C"],
                    table$DR[table$New.Percent.Branch=="Branch E"], table$DR[table$New.Percent.Branch=="Branch G"])

fraudulent_detection_rates = c(table$Fake.DR[table$New.Percent.Branch=="Branch A"], table$Fake.DR[table$New.Percent.Branch=="Branch C"],
                                table$Fake.DR[table$New.Percent.Branch=="Branch E"], table$Fake.DR[table$New.Percent.Branch=="Branch G"])

branches = c(table$New.Percent.Branch[table$New.Percent.Branch=="Branch A"], table$New.Percent.Branch[table$New.Percent.Branch=="Branch C"],
         table$New.Percent.Branch[table$New.Percent.Branch=="Branch E"], table$New.Percent.Branch[table$New.Percent.Branch=="Branch G"])

f1_df = data.frame(f1scores, branches)
df = data.frame(detection_rates, branches)
fraudulent_df = data.frame(fraudulent_detection_rates, branches) 
#kruskal.test(detection_rates ~ branches, data = df)

Data = mutate(df, branches = factor(branches, levels=unique(branches), labels=c('Branch A', 'Branch C', 'Branch E', 'Branch G')))
Summarize(detection_rates ~ branches, data = Data)

fraudulent_Data = mutate(fraudulent_df, branches = factor(branches, levels=unique(branches), labels=c('Branch A', 'Branch C', 'Branch E', 'Branch G')))
Summarize(fraudulent_detection_rates ~ branches, data = fraudulent_Data)

histogram(~ detection_rates | branches, data=Data, layout=c(1,4))
#Kruskal Wallis
kruskal.test(detection_rates ~ branches, data = Data)
kruskal.test(fraudulent_detection_rates ~ branches, data = fraudulent_Data)

boxplot(detection_rates ~ branches, data = Data, ylab="detction rate", xlab="branches")
boxplot(fraudulent_detection_rates ~ branches, data = fraudulent_Data, ylab="detction rate", xlab="branches")

#Data$branches = factor(Data$branches, levels=c("none", "both", "receiver", "sender"))
PT = dunnTest(detection_rates ~ branches, data=Data,method="bh")
PT = dunnTest(fraudulent_detection_rates ~ branches, data=fraudulent_Data,method="bh")
