# c53 project
library(tidyverse)
eics = read_csv('EICS.csv')

# research question 1
# we want to test whether mean employment insurance for those who joined
# is different from those who did not

# filter out the 2 columns that we want and filter out non response
# benamnt labeld from 996-999 are non response
# union_g labled as 1 means the person is a member
# union_g labeled as 2 means the person is not a member

eics[,c('CASEID', 'union_g', 'benamnt')] %>% 
  filter(union_g %in% c(1,2)) %>%
  filter(!benamnt %in% c(996,997,998,999)) -> eics_1
eics_1$union_g[eics_1$union_g==1] = 'Member'
eics_1$union_g[eics_1$union_g==2] = 'Not_Member'

# number of members in a union
eics_1 %>% group_by(union_g) %>%
  summarise(n=n())

# var test, test for equal variance
with(eics_1, var.test(benamnt~union_g))

# t test for comparing means, with equal variance
with(eics_1, t.test(benamnt~union_g,
                    alternative='greater',
                    var.equal=T))

# pval is really small, there is evidence to suggest that
# mean is greater

with(eics_1, t.test(benamnt~union_g, conf.level=0.95, var.equal=T))
# get 95% CI

# boxplot
eics_1 %>% 
  ggplot(aes(x=union_g, y=benamnt)) +
  geom_boxplot(fill='red') +
  xlab('Membership Status') +
  ylab('Benefit Amount') +
  ggtitle('Benefit Amount vs. Membership Status')

# histograms 30 bins
eics_1 %>% 
  ggplot(aes(x=benamnt)) +
  geom_histogram(bins=30) +
  facet_wrap(~union_g) + 
  xlab('Membership Status') +
  ylab('Benefit Amount')


# research question 2

# in the immigr_g variable, immigrants are labeled as 2
# and Canadians by birth are labled as 1
# in the benefit variable, received are labeled as 1-3
# did not receive is labeled as 4 and no response is labeled as 9

eics[, c('immigr_g', 'benefit')] %>%
  filter(immigr_g %in% c(1,2),
         benefit %in% c(1,2,3,4)) -> eics_2
eics_2$benefit[eics_2$benefit<=3] = 'received'
eics_2$benefit[eics_2$benefit==4] = 'not_received'
eics_2$immigr_g[eics_2$immigr_g==1] = 'canadian_by_birth'
eics_2$immigr_g[eics_2$immigr_g==2] = 'Immigrant'

contingency.table.eics = table(eics_2$immigr_g, eics_2$benefit)
addmargins(contingency.table.eics) # contingency table with margins
chisq.test(contingency.table.eics, correct=F) # chi squared without yates
chisq.test(contingency.table.eics, correct=F)$stdres # adjust std res
chisq.test(contingency.table.eics, correct=F)$expected # expected counts

# barplot
eics_2 %>% ggplot(aes(x=immigr_g, fill=benefit)) +
  geom_bar(position='dodge') + xlab('Immigrant Status') +
  ggtitle('Number of Benefit Received per Immigrant Status')

eics_2 %>% group_by(immigr_g) %>%
  count(benefit) %>%
  mutate(prop=n/sum(n)) -> eics_prop

# proportion barplot
eics_prop %>% ggplot(aes(x=benefit, y=prop)) +
  geom_bar(stat='identity') + xlab('Immigrant Status') +
  facet_wrap(~immigr_g)

# adjusted standardized residuals exceed 3
# canadian by birth tend to receive more benefits
# than immigrants

#  reject H0, pval = 0.00124
# There is an association between being an immigrant
# and receiving employment insurance benefits