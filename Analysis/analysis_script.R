library(tidyverse)
library(ggpubr)


# PMF results for ML100k, ML1M, Jester, ML10M
# I do not have BPMF results for Jester (running now, as of 6/4)
D1 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_ml100k.csv")
D2 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_ml1m.csv")
D3 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_jester.csv")
D4 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_ml10m.csv")

# Minor data cleaning
D = bind_rows(D1, D2, D3, D4)
D[which(D$datasets == "jester"),"datasets"] = "Jester"
D = D %>% mutate(datasets = as.factor(datasets), lf = as.factor(lf)) #iter = as.factor(iter))
D$datasets = factor(D$datasets, levels = c("ML100k", "ML1M", "Jester", "ML10M"))


# Panel of PMF results
D %>% mutate(D = lf, Iterations = max_epoch_pmf) %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() + facet_wrap(~datasets, nrow = 1) + 
  scale_x_continuous(breaks = c(unique(D$max_epoch_pmf))) + 
  xlab("Iterations") + theme(plot.title = element_text(size = 12, face = "bold"), 
                             plot.subtitle = element_text(size = 11, face = "bold"),
                             axis.text=element_text(size=11),
                             axis.title=element_text(size=11,face="bold"))

# Panel of BPMF results
D %>% mutate(D = lf, Iterations = iter) %>% 
  ggplot(aes(y = bpmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() + facet_wrap(~datasets, nrow = 1) + 
  scale_x_continuous(breaks = c(unique(D$iter))) + 
  xlab("Iterations") + theme(plot.title = element_text(size = 12, face = "bold"), 
                             plot.subtitle = element_text(size = 11, face = "bold"),
                             axis.text=element_text(size=11),
                             axis.title=element_text(size=11,face="bold"))


# Side-by-side ML100k PMF & BPMF
A = D %>% mutate(D = lf, Iterations = max_epoch_pmf) %>% 
  filter(datasets == "ML100k") %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() +# + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D$max_epoch_pmf))) + 
  xlab("Iterations") + ggtitle("ML100k") +
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))

B = D %>% mutate(D = as.factor(lf)) %>% 
  filter(datasets == "ML100k") %>%
  ggplot(aes(y = bpmf_rmse, x = iter, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iteration Scheme') + 
  ylab('RMSE') + theme_bw() + ggtitle("ML100k") + 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))# + facet_wrap(~datasets)

ggarrange(A, B, nrow = 1, common.legend = T, legend = "bottom")


# Side-by-side ML1M PMF & BPMF
A = D %>% mutate(D = as.factor(lf), Iterations = max_epoch_pmf) %>% 
  filter(datasets == "ML1M") %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() +# + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D$max_epoch_pmf))) + 
  xlab("Iterations") + ggtitle("ML1M")+ 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))

B = D %>% mutate(D = as.factor(lf)) %>% 
  filter(datasets == "ML1M") %>%
  ggplot(aes(y = bpmf_rmse, x = iter, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iteration Scheme') + 
  ylab('RMSE') + theme_bw() + ggtitle("ML1M") + 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))# + facet_wrap(~datasets)

ggarrange(A, B, nrow = 1, common.legend = T, legend = "bottom")


# Side-by-side ML10M PMF & BPMF
A = D %>% mutate(D = as.factor(lf), Iterations = max_epoch_pmf) %>% 
  filter(datasets == "ML10M") %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() +# + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D$max_epoch_pmf))) + 
  xlab("Iterations") + ggtitle("ML10M")+ 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))

B = D %>% mutate(D = as.factor(lf)) %>% 
  filter(datasets == "ML10M") %>%
  ggplot(aes(y = bpmf_rmse, x = iter, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iteration Scheme') + 
  ylab('RMSE') + theme_bw() + ggtitle("ML10M") + 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))# + facet_wrap(~datasets)

ggarrange(A, B, nrow = 1, common.legend = T, legend = "bottom")



summary(lm(pmf_rmse ~ datasets + lf + max_epoch_pmf - 1, data = D))

summary(lm(pmf_rmse ~ datasets + lf*max_epoch_pmf - 1, data = D))


#summary(lm(bpmf_rmse ~ datasets + lf + iter - 1, data = D_bpmf)) # can use this later



