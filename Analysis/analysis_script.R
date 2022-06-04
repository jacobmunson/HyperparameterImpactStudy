library(tidyverse)
library(ggpubr)


# PMF results for ML100k, ML1M, Jester, ML10M
# I do not have BPMF results for Jester (running now, as of 6/4)
D1 <- read_csv("C:/Users/Jacob/Documents/Experimental Design (STAT541)/541_project2_pmf_bpmf_run_results.csv")
D2 <- read_csv("C:/Users/Jacob/Documents/Experimental Design (STAT541)/541_project2_pmf_bpmf_run_results_jester.csv")
D3 <- read_csv("C:/Users/Jacob/Documents/Experimental Design (STAT541)/541_project2_pmf_bpmf_run_results_ml10m.csv")

D_pmf = bind_rows(D1, D2, D3)
D_pmf[which(D_pmf$datasets == "jester"),"datasets"] = "Jester"
D_pmf = D_pmf %>% mutate(datasets = as.factor(datasets), lf = as.factor(lf)) #iter = as.factor(iter))


D_pmf$datasets = factor(D_pmf$datasets, levels = c("ML100k", "ML1M", "Jester", "ML10M"))



D_pmf %>% mutate(D = lf, Iterations = max_epoch_pmf) %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D_pmf$max_epoch_pmf))) + 
  xlab("Iterations") + theme(plot.title = element_text(size = 12, face = "bold"), 
                             plot.subtitle = element_text(size = 11, face = "bold"),
                             axis.text=element_text(size=11),
                             axis.title=element_text(size=11,face="bold"))


A = D_pmf %>% mutate(D = lf, Iterations = max_epoch_pmf) %>% 
  filter(datasets == "ML100k") %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() +# + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D_pmf$max_epoch_pmf))) + 
  xlab("Iterations") + ggtitle("ML100k") +
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))


B = D_pmf %>% mutate(D = as.factor(lf)) %>% 
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



A = D_pmf %>% mutate(D = as.factor(lf), Iterations = max_epoch_pmf) %>% 
  filter(datasets == "ML1M") %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() +# + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D_pmf$max_epoch_pmf))) + 
  xlab("Iterations") + ggtitle("ML1M")+ 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))


B = D_pmf %>% mutate(D = as.factor(lf)) %>% 
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




A = D_pmf %>% mutate(D = as.factor(lf), Iterations = max_epoch_pmf) %>% 
  filter(datasets == "ML10M") %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() +# + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D_pmf$max_epoch_pmf))) + 
  xlab("Iterations") + ggtitle("ML10M")+ 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))


B = D_pmf %>% mutate(D = as.factor(lf)) %>% 
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



summary(lm(pmf_rmse ~ datasets + lf + max_epoch_pmf - 1, data = D_pmf))

summary(lm(pmf_rmse ~ datasets + lf*max_epoch_pmf - 1, data = D_pmf))


#summary(lm(bpmf_rmse ~ datasets + lf + iter - 1, data = D_bpmf)) # can use this later



