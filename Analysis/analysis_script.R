library(tidyverse)
library(ggpubr)


## PMF results for ML100k, ML1M, Jester, ML10M
# I do not have BPMF results for Jester (running now, as of 6/4)
D1 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_ml100k.csv")
D2 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_ml1m.csv")
D3 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_jester.csv")
D4 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_ml10m.csv")
D5 <- read_csv("C:/Users/Jacob/Documents/GitHub/HyperparameterImpactStudy/Data/pmf_bpmf_results_bookcrossing.csv")


## Some dataset summary statistics
# Dataset       | users  | items  | ratings  | density
# ______________________________________________________________________________
# ML100k (D1)   | 610    | 9724   | 100836   | 0.01699968 = 100836/(610*9724)
# ML1M (D2)     | 6040   | 3706   | 1000209  | 0.04468363 = 1000209/(6040*3706)
# Book Crossing | 105283 | 340553 | 1149780  | 0.00003206799 = 1149780/(105283*340553)
# Jester (D3)   | 73421  | 100    | 4136360  | 0.5633756 = 4136360/(73421*100) 
# ML10M (D4)    | 69878  | 10677  | 10000054 | 0.01340333 = 10000054/(69878*10677)
# ML20M         | 138493 | 26744  | 20000263 | 0.005399848 = 20000263/(138493*26744)

#D1$num_users = 610; D1$num_items = 9724; D1$num_ratings = 100836; D1$density = 0.01699968    # may need this information eventually - keeping hard-coded here
#D2$num_users = 6040; D2$num_items = 3706; D2$num_ratings = 1000209; D2$density = 0.04468363
#D3$num_users = 73421; D3$num_items = 100; D3$num_ratings = 4136360; D3$density = 0.5633756
#D4$num_users = 69878; D4$num_items = 10677; D4$num_ratings = 10000054; D4$density = 0.01340333





# Minor data cleaning
D = bind_rows(D1, D2, D3, D4, D5)
D[which(D$datasets == "jester"),"datasets"] = "Jester"
D[which(D$datasets == "book-crossing"),"datasets"] = "Book Crossing"

D = D %>% mutate(datasets = as.factor(datasets), lf = as.factor(lf)) #iter = as.factor(iter))
D[which(D$datasets == "Jester" & D$lf == 99),"lf"] = as.factor(100)

# Options to arrange the dataset for panel graphics
D$datasets = factor(D$datasets, levels = c("ML100k", "ML1M", "Book Crossing", "Jester", "ML10M")) # arranged by dataset size
D$datasets = factor(D$datasets, levels = c("ML100k", "ML1M", "ML10M", "Jester", "Book Crossing")) # arranged by number of users
D$datasets = factor(D$datasets, levels = c("Jester", "ML1M", "ML100k", "ML10M", "Book Crossing")) # arranged by number of items
D$datasets = factor(D$datasets, levels = c("Book Crossing", "ML10M", "ML100k", "ML1M", "Jester")) # arranged by density

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
A1 = D %>% mutate(D = lf, Iterations = max_epoch_pmf) %>% 
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

B1 = D %>% mutate(D = as.factor(lf)) %>% 
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

ggarrange(A1, B1, nrow = 1, common.legend = T, legend = "bottom")


# Side-by-side ML1M PMF & BPMF
A2 = D %>% mutate(D = as.factor(lf), Iterations = max_epoch_pmf) %>% 
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

B2 = D %>% mutate(D = as.factor(lf)) %>% 
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

ggarrange(A2, B2, nrow = 1, common.legend = T, legend = "bottom")


# Side-by-side ML10M PMF & BPMF
A3 = D %>% mutate(D = as.factor(lf), Iterations = max_epoch_pmf) %>% 
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

B3 = D %>% mutate(D = as.factor(lf)) %>% 
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

ggarrange(A3, B3, nrow = 1, common.legend = T, legend = "bottom")


# Side-by-side Jester PMF & BPMF
A4 = D %>% mutate(D = as.factor(lf), Iterations = max_epoch_pmf) %>% 
  filter(datasets == "Jester") %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() +# + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D$max_epoch_pmf))) + 
  xlab("Iterations") + ggtitle("Jester")+ 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))

B4 = D %>% mutate(D = as.factor(lf)) %>% 
  filter(datasets == "Jester") %>%
  ggplot(aes(y = bpmf_rmse, x = iter, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iteration Scheme') + 
  ylab('RMSE') + theme_bw() + ggtitle("Jester") + 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))# + facet_wrap(~datasets)

ggarrange(A4, B4, nrow = 1, common.legend = T, legend = "bottom")



# Side-by-side Book Crossing PMF & BPMF
A5 = D %>% mutate(D = as.factor(lf), Iterations = max_epoch_pmf) %>% 
  filter(datasets == "Book Crossing") %>% 
  ggplot(aes(y = pmf_rmse, x = Iterations, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iterations') + 
  ylab('RMSE') + theme_bw() +# + facet_wrap(~datasets) + 
  scale_x_continuous(breaks = c(unique(D$max_epoch_pmf))) + 
  xlab("Iterations") + ggtitle("Book Crossing")+ 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))

B5 = D %>% mutate(D = as.factor(lf)) %>% 
  filter(datasets == "Book Crossing") %>%
  ggplot(aes(y = bpmf_rmse, x = iter, color = D)) +
  geom_point(size = 2) + 
  geom_smooth(method = 'lm', formula = 'y~x') + 
  theme_bw() + xlab('Iteration Scheme') + 
  ylab('RMSE') + theme_bw() + ggtitle("Book Crossing") + 
  theme(plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 11, face = "bold"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11,face="bold"))# + facet_wrap(~datasets)

ggarrange(A5, B5, nrow = 1, common.legend = T, legend = "bottom")


ggarrange(A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, 
          nrow = 2, ncol = 5, common.legend = T, legend = "bottom")


summary(lm(pmf_rmse ~ datasets + lf + max_epoch_pmf - 1, data = D))

summary(lm(pmf_rmse ~ datasets + lf*max_epoch_pmf - 1, data = D))

summary(lm(bpmf_rmse ~ datasets + lf*iter - 1, data = D))


D$density

#summary(lm(bpmf_rmse ~ datasets + lf + iter - 1, data = D_bpmf)) # can use this later



