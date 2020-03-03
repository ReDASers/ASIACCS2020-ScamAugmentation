library(aod)
library(plyr)
library(rms)
#library(reshape2)

confidence_signiticant <- function(table){
  rights = na.omit(table$confidence_right)
  wrongs = na.omit(table$confidence_wrong)
  wilcox.test(rights, wrongs)
  return(0)
  library(FSA)
  group_by(table, New.Percent.Branch) %>%
    summarise(
      count = n(),
      mean = mean(WDR, na.rm = TRUE),
      sd = sd(WDR, na.rm = TRUE)
    )
  temp <- droplevels(table[table$New.Percent.Branch=='Branch A' | 
                  table$New.Percent.Branch=='Branch C' |
                  table$New.Percent.Branch=='Branch G' |
                  table$New.Percent.Branch=='Branch E',])
  res.aov <- aov(WDR ~ New.Percent.Branch, 
                 data = temp)
  summary(res.aov)
  
  dunnTest(WDR ~ New.Percent.Branch, 
           data = temp, method="holm")
}

calculate_ratio <- function(total_count, DR_dataframe, condition){
  conditioned_count = length(DR_dataframe$DR[condition])
  conditioned_ratio = conditioned_count/total_count
  return(conditioned_ratio)
}

general_stat <- function(table){
  sizes <- c()
  for (branch in c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')){
    branch_size = nrow(table[table$New.Percent.Branch==sprintf("Branch %s", branch),])
    print(sprintf("|Branch %s|=%d", branch, branch_size))
    sizes <- c(sizes, branch_size)
  }
  return(sizes)
}

regression_analysis <- function(table){
  fit <- lm(DR~Age.+Gender+Your.occupation.+Highest.degree.achieved.+Approximate...of.emails.you.receive.in.each.day.,
             data=table) 
  summary(fit)
  anova(fit)
  confint(fit)
  wald.test(b=coef(fit), Sigma=vcov(fit), Terms=13:14)
  lrm(DR~Age.+Gender+Your.occupation.+Highest.degree.achieved.+Approximate...of.emails.you.receive.in.each.day.,data=table) 
}

anova_time_reasoning <- function(table){
  df = data.frame(Block = c("Q1", "Q2", "Q3", "Q4"), 
                  time = c(table$Q1_time, table$Q2_time, 
                            table$Q3_time, table$Q4_time), 
                  length = c(table$Q1_res_len, table$Q2_res_len, 
                              table$Q3_res_len, table$Q4_res_len))
  fit <- aov(time~Block, data = table)
}

draw_bar_plot_reasoning <- function(table){
  df = data.frame(Block = c("Q1", "Q2", "Q3", "Q4"), 
                  time = c(mean(table$Q1_time), mean(table$Q2_time), 
                             mean(table$Q3_time), mean(table$Q4_time)), 
                  length = c(mean(table$Q1_res_len), mean(table$Q2_res_len), 
                           mean(table$Q3_res_len), mean(table$Q4_res_len)))
  value_matrix = matrix(, nrow = 2, ncol = 4)
  value_matrix[1,] = df$length
  value_matrix[2,] = df$time
  barplot(value_matrix, names.arg = df$Block, beside = TRUE, space = c(0.1,0.6),
          col = c("blue", "red"), legend.text = c("Time", "Length"), 
          ylim=c(0.0, 140), cex.axis=1.5, cex.names=1.3, 
          main="Detection Rate of Legit. and Phish by Various Groups", 
          cex.main=1.5, ylab = "time(s) and lenght(chars)", cex.lab = 1.3,
          args.legend = list(x = "top", bty = "o", inset=c(0, -0.05), 
                             cex=1, horiz=TRUE, y.intersp=0.1))
  
  fit <- aov(length~Block, data = df)
  fit <- aov(time~Block, data = df)
  
  temp = data.frame(time=na.omit(table$Q1_ordered_time), group='1')
  for (i in 2:32){
    column = sprintf("Q%d_ordered_time", i)
    question = sprintf("%d", i) 
    temp = rbind(temp, 
                 data.frame(time=na.omit(table[,column]), 
                            group=question))
  }
  ggplot(data = temp,
         aes(x=group,y=time))+
    geom_boxplot(outlier.shape=NA)+
    coord_cartesian(ylim=c(0, 280))+
    labs(title='Distribution of time spent on each question', 
         x = "Questions", y = "Time (s)")+
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size=17))
  
  fit<-aov(time~group, data=temp)
  results <- TukeyHSD(fit, 'group')
}

confidence_plots <- function(table){
  library(HH)
  for (branch in c("Branch A", "Branch B", "Branch C", "Branch D", 
                   "Branch E", "Branch F", "Branch G", "Branch H")){
    confidences <- table[table$New.Percent.Branch==branch,][,c('Q1_confidence','Q2_confidence','Q3_confidence','Q4_confidence')]
    #library(dplyr)
    #confidences <- select(table, 'New.Percent.Branch', 'Q1_confidence','Q2_confidence','Q3_confidence','Q4_confidence')
    names(confidences) <- c("Q1", "Q2", "Q3", "Q4")
    #confidences <- data.frame(lapply(confidences, factor, ordered=TRUE, 
    #                                 levels=1:5, 
    #                                 labels=c("Not confident","Slightly confident",
    #                                          "Somewhat confident","Fairly confident","Completely confident")))
    for (i in c("Q1", "Q2", "Q3", "Q4")){
      aggregated <- setNames(aggregate(confidences[i], 
                                        by =confidences[i], 
                                        FUN=length),
                              c('Response', i))
      if (i != "Q1"){
        total_aggregated = merge(total_aggregated,aggregated)
      } else {
        total_aggregated = aggregated
      }
    }
    total_aggregated = setNames(data.frame(t(total_aggregated[,-1])), 
                                total_aggregated[,1])
    total_aggregated$Question <- rownames(total_aggregated)
    if (branch == "Branch B"){
      b = "NLG"
    } else if (branch == "Branch D"){
      b = "Human"
    } else if (branch == "Branch F"){
      b = "Gmail"
    } else if (branch == "Branch H"){
      b = "Linkedin"
    } else if (branch == "Branch G"){
      b = "Sender"
    } else if (branch == "Branch E"){
      b = "Receiver"
    } else if (branch == "Branch A"){
      b = "None"
    } else if (branch == "Branch C"){
      b = "Both"
    }
    Group <-c(b, b, b, b)
    total_aggregated_branch <- cbind(total_aggregated, Group)
    if (branch != "Branch A"){
      total_aggregated_all <- rbind(total_aggregated_all , total_aggregated_branch)
    } else {
      total_aggregated_all = total_aggregated_branch
    }
  }
  
  likertColor(2)
  likert(Question~.|Group, data=total_aggregated_all, as.percent=TRUE,
         positive.order=FALSE, layout=c(2,4),colorFunction="sequential_hcl",
         strip.left=strip.custom(bg="gray97"),
         strip=FALSE, ylab=NULL,
         main = list("Participants' Confidence Per Group and Question",
                     x=unit(.5, "npc")), 
         sub= list("Confidence Level",
                   x=unit(.5, "npc")), 
         par.strip.text=list(cex=2, lines=1),
         resize.height.tuning=2,
         left.text.cex =10,left.text.font =2,
         between=list(x=3, y=0),
         right.text.cex =1, right.text.font=2)
}

personality_correlation <- function(table){
  library(corrplot)
  table$ACL=rowMeans(table[c('Q1_confidence','Q2_confidence','Q3_confidence','Q4_confidence')])
  columns = c('agreeableness','extraversion', 'conscientiousness', 
              'openness', 'neuroticism', 'CRT', 'Fake.DR', 'Legit.DR', 'DR', 'ACL')
  correlations <- cor(table[,columns])
  res1 <- cor.mtest(table[,columns])
  colnames(correlations) <- c("Agre.", "Extra.", "Cons.", "Open.", "Neur.", "CRT", "FDR", "LDR", "DR", "ACL")
  rownames(correlations) <- c("Agre.", "Extra.", "Cons.", "Open.", "Neur.", "CRT", "FDR", "LDR", "DR", "ACL")
  corrplot(correlations, method="square", type="upper", tl.col = "black", addCoef.col = "black", 
           tl.cex=1.8, cl.cex = 1.5, title="Correlation between personality traits and detection rate", mar=c(0,0,1,0),
           diag = FALSE, tl.srt=90, p.mat = res1$p, sig.level = .05, number.cex =1.5, pch.cex = 4)
  for (trait in c('conscientiousness', 'extraversion', 'openness', 'agreeableness', 'neuroticism')){
    for (performance in c('Fake.DR', 'Legit.DR', 'F1', 'DR')){
      test<-cor.test(table[table$New.Percent.Branch=='Branch A',][,trait], table[table$New.Percent.Branch=='Branch A',][,performance])
      if (test$p.value < 0.05){
        print(sprintf("Correlation between %s and %s=%f (%f)", trait, performance, test$p.value, test$estimate))
      }
    }
  }
}

draw_bar_plot <- function(table){
  means=ddply(table, .(table$New.Percent.Branch), summarise, 
              Legit.DR=mean(Legit.DR), Fake.DR=mean(Fake.DR))
  barplot(rbind(means$Legit.DR, means$Fake.DR)[, c(8, 6, 4, 2, 7, 5, 3, 1)], beside=T, 
          #col=c("#768BE6","#E16A86"),
          col=c("cornflowerblue","pink"),
          names.arg=c('LinkedIn', 'Gmail', 'Human', 'NLG', 'Sender', 'Receiver', 'Send+Rec.', 'None'),
          ylim=c(0.0,1.0), cex.axis=1.5, cex.names=1.3, 
          #main="Detection Rate of Legit. and Job Scam by Various Groups", 
          cex.main=1.5)
  legend("top", c("Legit DR","Phish DR"), pch=15, 
          col=c("cornflowerblue","pink"), cex=1.2,
          bty="o", horiz=TRUE, y.intersp=0.1, text.font=2)
}

group_stat <- function(table, branch){
  ##group-Fake
  group.all.count = length(table$Fake.DR[table$New.Percent.Branch==branch])
  group.fake.DR = data.frame(table$Fake.DR[table$New.Percent.Branch==branch])
  colnames(group.fake.DR) <- c("DR")
  group.Fake.all.legit.ratio = calculate_ratio(group.all.count, group.fake.DR, group.fake.DR$DR==0)
  group.Fake.atleastone.legit.ratio = calculate_ratio(group.all.count, group.fake.DR, group.fake.DR$DR<1)
  group.Fake.none.legit.ratio = calculate_ratio(group.all.count, group.fake.DR, group.fake.DR$DR==1)
  
  #group-Legit
  group.all.count = length(table$Legit.DR[table$New.Percent.Branch==branch])
  group.legit.DR = data.frame(table$Legit.DR[table$New.Percent.Branch==branch])
  colnames(group.legit.DR) <- c("DR")
  group.legit.all.legit.ratio = calculate_ratio(group.all.count, group.legit.DR, group.legit.DR$DR==1)
  group.legit.atleastone.legit.ratio = calculate_ratio(group.all.count, group.legit.DR, group.legit.DR$DR>0)
  group.legit.none.legit.ratio = calculate_ratio(group.all.count, group.legit.DR, group.legit.DR$DR==0)
  
  return(list(GFALR=group.Fake.all.legit.ratio,
              GFOLR=group.Fake.atleastone.legit.ratio,
              GFNLR=group.Fake.none.legit.ratio,
              GLALR=group.legit.all.legit.ratio,
              GLOLR=group.legit.atleastone.legit.ratio,
              GLNLR=group.legit.none.legit.ratio))
}

print_stat <- function(stats, name){
  group.Fake.all.legit.ratio=stats$GFALR
  group.Fake.atleastone.legit.ratio=stats$GFOLR
  group.Fake.none.legit.ratio=stats$GFNLR
  group.legit.all.legit.ratio=stats$GLALR
  group.legit.atleastone.legit.ratio=stats$GLOLR
  group.legit.none.legit.ratio=stats$GLNLR
  print(sprintf("%s-Fake: Ratio of participants who labeled both questions as legitimate:  %f", 
                name, group.Fake.all.legit.ratio))
  print(sprintf("%s-Fake: Ratio of participants who labeled at least one question as legitimate:  %f", 
                name, group.Fake.atleastone.legit.ratio))
  print(sprintf("%s-Fake: Ratio of participants who labeled none of the questions as legitimate:  %f",
                name, group.Fake.none.legit.ratio))
  print(sprintf("%s-Legit: Ratio of participants who labeled both questions as legitimate:  %f", 
                name, group.legit.all.legit.ratio))
  print(sprintf("%s-Legit: Ratio of participants who labeled at least one question as legitimate:  %f",
                name, group.legit.atleastone.legit.ratio))
  print(sprintf("%s-Legit: Ratio of participants who labeled none of the questions as legitimate:  %f",
                name, group.legit.none.legit.ratio))
}

group_count_fake_with_condition <- function(table, condition){
  group.all.count = length(table$Fake.DR[condition])
  group.fake.DR = data.frame(table$Fake.DR[condition])
  colnames(group.fake.DR) <- c("DR")
  conditioned_count = length(group.fake.DR$DR[group.fake.DR$DR<1])
  return (list(group.all.count=group.all.count, conditioned_count=conditioned_count))
}

group_count_fake <- function(table, branch){
  group.all.count = length(table$Fake.DR[table$New.Percent.Branch==branch])
  group.fake.DR = data.frame(table$Fake.DR[table$New.Percent.Branch==branch])
  colnames(group.fake.DR) <- c("DR")
  conditioned_count = length(group.fake.DR$DR[group.fake.DR$DR<1])
  return (list(group.all.count=group.all.count, conditioned_count=conditioned_count))
}

group_count_legit_with_condition <- function(table, condition){
  group.all.count = length(table$Legit.DR[condition])
  group.legit.DR = data.frame(table$Legit.DR[condition])
  colnames(group.legit.DR) <- c("DR")
  conditioned_count = length(group.legit.DR$DR[group.legit.DR$DR>0])
  return (list(group.all.count=group.all.count, conditioned_count=conditioned_count))
}

group_count_legit <- function(table, branch){
  group.all.count = length(table$Legit.DR[table$New.Percent.Branch==branch])
  group.legit.DR = data.frame(table$Legit.DR[table$New.Percent.Branch==branch])
  colnames(group.legit.DR) <- c("DR")
  conditioned_count = length(group.legit.DR$DR[group.legit.DR$DR>0])
  return (list(group.all.count=group.all.count, conditioned_count=conditioned_count))
}

significant_test_with_condition <- function(table, first_condition, second_condition){
  group_first= group_count_fake_with_condition(table, first_condition)
  group_second= group_count_fake_with_condition(table, second_condition)
  
  prop_test_fake = prop.test(c(group_second$conditioned_count, group_first$conditioned_count), 
                             c(group_second$group.all.count, group_first$group.all.count),
                             correct = FALSE)
  
  group_first = group_count_legit_with_condition(table, first_condition)
  group_second = group_count_legit_with_condition(table, second_condition)
  
  prop_test_legit = prop.test(c(group_second$conditioned_count, group_first$conditioned_count), 
                              c(group_second$group.all.count, group_first$group.all.count),
                              correct = FALSE)
  
  return(list(prop_test_fake=prop_test_fake, 
           prop_test_legit=prop_test_legit))
}

significant_test <- function(table, first_branch, second_branch){
  group_first= group_count_fake(table, first_branch)
  group_second= group_count_fake(table, second_branch)
  
  prop_test_fake = prop.test(c(group_second$conditioned_count, group_first$conditioned_count), 
                             c(group_second$group.all.count, group_first$group.all.count),
                             correct = FALSE)
  
  group_first = group_count_legit(table, first_branch)
  group_second = group_count_legit(table, second_branch)
  
  prop_test_legit = prop.test(c(group_second$conditioned_count, group_first$conditioned_count), 
                             c(group_second$group.all.count, group_first$group.all.count),
                             correct = FALSE)
  
  return(c(prop_test_fake=prop_test_fake, 
           prop_test_legit=prop_test_legit))
}

significant_test_four_branches <- function(table, 
                                           first_branch, second_branch,
                                           third_branch, fourth_branch){
  #fourth: both
  #third: None
  #second: Receiver
  #first: Sender
  group_first= group_count_fake(table, first_branch)
  group_second= group_count_fake(table, second_branch)
  group_third= group_count_fake(table, third_branch)
  group_fourth= group_count_fake(table, fourth_branch)
  
  prop_test_fake = prop.test(c(group_fourth$conditioned_count, group_third$conditioned_count, 
                               group_second$conditioned_count, group_first$conditioned_count),
                             c(group_fourth$group.all.count, group_third$group.all.count,
                               group_second$group.all.count, group_first$group.all.count),
                             correct = FALSE)
  
  pairwise.prop.test(c(group_fourth$conditioned_count, group_second$conditioned_count, 
                       group_second$conditioned_count, group_first$conditioned_count),
                     c(group_fourth$group.all.count, group_third$group.all.count,
                       group_second$group.all.count, group_first$group.all.count))
  
  group_first = group_count_legit(table, first_branch)
  group_second = group_count_legit(table, second_branch)
  group_third = group_count_legit(table, third_branch)
  group_fourth = group_count_legit(table, fourth_branch)
  
  prop_test_legit = prop.test(c(group_fourth$conditioned_count, group_second$conditioned_count, 
                                group_second$conditioned_count, group_first$conditioned_count),
                              c(group_fourth$group.all.count, group_third$group.all.count,
                                group_second$group.all.count, group_first$group.all.count),
                              correct = FALSE)
  
  pairwise.prop.test(c(group_fourth$conditioned_count, group_second$conditioned_count, 
                       group_second$conditioned_count, group_first$conditioned_count),
                     c(group_fourth$group.all.count, group_third$group.all.count,
                       group_second$group.all.count, group_first$group.all.count))
  
  return(c(prop_test_fake=prop_test_fake, 
           prop_test_legit=prop_test_legit))
}

significant_test_per_question <- function(table, first_branch, second_branch){
  group.all.count_first = 2*length(table$Fake.DR[table$New.Percent.Branch==first_branch])
  group.fake.DR = data.frame(table$Fake.DR[table$New.Percent.Branch==first_branch])
  colnames(group.fake.DR) <- c("DR")
  conditioned_count_first = 0
  conditioned_count_first = conditioned_count_first + length(group.fake.DR$DR[group.fake.DR$DR==0.5])
  conditioned_count_first = conditioned_count_first + 2*length(group.fake.DR$DR[group.fake.DR$DR==1])
  
  group.all.count_second = 2*length(table$Fake.DR[table$New.Percent.Branch==second_branch])
  group.fake.DR = data.frame(table$Fake.DR[table$New.Percent.Branch==second_branch])
  colnames(group.fake.DR) <- c("DR")
  conditioned_count_second = 0
  conditioned_count_second = conditioned_count_second + length(group.fake.DR$DR[group.fake.DR$DR==0.5])
  conditioned_count_second = conditioned_count_second + 2*length(group.fake.DR$DR[group.fake.DR$DR==1])
  
  prop_test_fake = prop.test(c(conditioned_count_second,conditioned_count_first), 
                             c(group.all.count_second,group.all.count_first),
                             correct = FALSE)
  
  group.all.count_first = 2*length(table$Legit.DR[table$New.Percent.Branch==first_branch])
  group.legit.DR = data.frame(table$Legit.DR[table$New.Percent.Branch==first_branch])
  colnames(group.legit.DR) <- c("DR")
  conditioned_count_first = 0
  conditioned_count_first = conditioned_count_first + length(group.legit.DR$DR[group.legit.DR$DR==0.5])
  conditioned_count_first = conditioned_count_first + 2*length(group.legit.DR$DR[group.legit.DR$DR==1])
  
  group.all.count_second = 2*length(table$Legit.DR[table$New.Percent.Branch==second_branch])
  group.legit.DR = data.frame(table$Legit.DR[table$New.Percent.Branch==second_branch])
  colnames(group.legit.DR) <- c("DR")
  conditioned_count_second = 0
  conditioned_count_second = conditioned_count_second + length(group.legit.DR$DR[group.legit.DR$DR==0.5])
  conditioned_count_second = conditioned_count_second + 2*length(group.legit.DR$DR[group.legit.DR$DR==1])
  
  prop_test_legit = prop.test(c(conditioned_count_second,conditioned_count_first), 
                              c(group.all.count_second,group.all.count_first),
                              correct = FALSE)
  
  return(c(prop_test_fake=prop_test_fake, 
           prop_test_legit=prop_test_legit))
}

diagram <- function(temp){
  library(ggplot2)
  ggplot(temp, aes(x = Highest.degree.achieved., y = F1)) +
    geom_boxplot(size = .75) +
    #geom_jitter(alpha = .5) +
    facet_grid(Your.occupation. ~ Gender
               , margins = TRUE) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  op <- par(mar = c(7,4.5,2,1) + 0.1)
  boxplot(F1~reasoning_group, data=table, xaxt = "n", xlab="Number of clues mentioned", 
          drop=TRUE, line=3, cex.lab=2, cex.axis=2,cex.main=2)
  #axis(2, at=1:3, labels=c("<=1\n(inattentive)", ">1&<=3\n(moderate attentive)", ">3\n(attentive)"),
  #     side = 1, tick = FALSE, cex.axis=2)
  means <- tapply(table$F1,table$reasoning_group,mean)
  points(means,col="red",pch=8, cex=1)
  
  
  plot <- ggplot(data = temp, aes(y = F1, x = reasoning_group)) + 
    geom_boxplot(fatten = NULL) +
    stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 0.75, size = 1, linetype = "solid")
  print(plot)
}

ordered_logistic_regression  <- function(table){
  table$merged_plafrom[table$platform == "windows"] = "PC"
  table$merged_plafrom[table$platform == "mac"] = "PC"
  table$merged_plafrom[table$platform == "iphone"] = "mobile"
  table$merged_plafrom[table$platform == "android"] = "mobile"
  
  m1 <- polr(as.factor(DR) ~Age.+merged_plafrom, data = table, Hess=TRUE)
  ctable <- coef(summary(m1))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind(ctable, "p value" = p)
  
  m2 <- polr(as.factor(DR) ~Age.+merged_plafrom+How.many.years.have.you.been.using.email.+Approximate...of.emails.you.receive.in.each.day.+How.frequently.do.you.check.social.networks..e.g...Facebook..Twitter..Google..Blogs..LinkedIn.., data = table, Hess=TRUE)
  m3 <- polr(as.factor(DR) ~Age.+
               merged_plafrom+How.many.years.have.you.been.using.email.+
               Approximate...of.emails.you.receive.in.each.day.+
               How.frequently.do.you.check.social.networks..e.g...Facebook..Twitter..Google..Blogs..LinkedIn..+
               Do.you.use.any.spam.filter..stand.alone.or.part.of.Antivirus..to.filter.emails.in.your.personal.account.+
               Have.you.ever.used.browsers..bookmarks.+
               Have.you.ever.installed.computer.software.on.your.computer.+
               Have.you.ever.used.internet.calling.software..e.g...Skype..Hangout.etc...+
               Have.you.ever.used.online.systems.for.banking.or.shopping.+
               Have.you.seen.any.specific.information.in.the.last.year.that.helps.you.detect.email.scams., data = table, Hess=TRUE)
  technology_background= table[c("Do.you.use.any.spam.filter..stand.alone.or.part.of.Antivirus..to.filter.emails.in.your.personal.account.", 
                                 "Have.you.ever.used.browsers..bookmarks.", 
                                 "Have.you.ever.installed.computer.software.on.your.computer.", 
                                 "Have.you.ever.used.internet.calling.software..e.g...Skype..Hangout.etc...", 
                                 "Have.you.ever.used.online.systems.for.banking.or.shopping.",
                                 "Have.you.seen.any.specific.information.in.the.last.year.that.helps.you.detect.email.scams.")]
  table$technology_background =rowSums(technology_background=="Yes") 
  
  m2 <- polr(as.factor(DR) ~Age.+merged_plafrom+How.many.years.have.you.been.using.email.+Approximate...of.emails.you.receive.in.each.day.+How.frequently.do.you.check.social.networks..e.g...Facebook..Twitter..Google..Blogs..LinkedIn..+technology_background, data = table, Hess=TRUE)
  anova(m1, m2)
  
}
