##screwing around with data to recreate figures
setwd("./Downloads/")
require(tidyverse)
exp_data <- read_csv("./GSE1730_forR.csv")

#recreating figure 1b
exp_data_selected <- exp_data %>%
  select(LOCUSTAG, GENE, EXP_70_TestInt, EXP_70_CntlInt, EXP_71_TestInt, EXP_71_CntlInt, EXP_72_TestInt,
        EXP_72_CntlInt, EXP_73_TestInt, EXP_73_CntlInt, EXP_74_TestInt, EXP_74_CntlInt, EXP_75_TestInt,
        EXP_75_CntlInt, EXP_76_TestInt, EXP_76_CntlInt, EXP_77_TestInt, EXP_77_CntlInt, EXP_78_TestInt,
        EXP_78_CntlInt, EXP_79_TestInt, EXP_79_CntlInt, EXP_80_TestInt, EXP_80_CntlInt) %>%
  mutate(mean_exp = (EXP_70_TestInt + EXP_70_CntlInt + EXP_71_TestInt + EXP_71_CntlInt + EXP_72_TestInt +
         EXP_72_CntlInt + EXP_73_TestInt + EXP_73_CntlInt + EXP_74_TestInt + EXP_74_CntlInt + EXP_75_TestInt +
         EXP_75_CntlInt + EXP_76_TestInt + EXP_76_CntlInt + EXP_77_TestInt + EXP_77_CntlInt + EXP_78_TestInt +
         EXP_78_CntlInt + EXP_79_TestInt + EXP_79_CntlInt + EXP_80_TestInt + EXP_80_CntlInt)/22) %>%
  mutate(stdev_exp = sqrt(((EXP_70_TestInt-mean_exp)^2 + (EXP_70_CntlInt-mean_exp)^2 + (EXP_71_TestInt-mean_exp)^2 + (EXP_71_CntlInt-mean_exp)^2 + (EXP_72_TestInt-mean_exp)^2 +
                        (EXP_72_CntlInt-mean_exp)^2 + (EXP_73_TestInt-mean_exp^2) + (EXP_73_CntlInt-mean_exp)^2 + (EXP_74_TestInt-mean_exp)^2 + (EXP_74_CntlInt-mean_exp)^2 + (EXP_75_TestInt-mean_exp)^2 +
                        (EXP_75_CntlInt-mean_exp)^2 + (EXP_76_TestInt-mean_exp^2) + (EXP_76_CntlInt-mean_exp)^2 + (EXP_77_TestInt-mean_exp)^2 + (EXP_77_CntlInt-mean_exp)^2 + (EXP_78_TestInt-mean_exp)^2 +
                        (EXP_78_CntlInt-mean_exp)^2 + (EXP_79_TestInt-mean_exp)^2 + (EXP_79_CntlInt-mean_exp)^2 + (EXP_80_TestInt-mean_exp)^2 + (EXP_80_CntlInt-mean_exp)^2)/22)) %>%
  mutate(RSD = stdev_exp/mean_exp)

RSDs <- na.omit(exp_data_selected$RSD)
meanRSD <- mean(RSDs)
pct_lessthan_one <- round(length(which(RSDs < 1))/length(RSDs), digits = 3)

exp_data_selected %>%
  ggplot() +
  geom_histogram(aes(x = RSD)) + 
  geom_vline(aes(xintercept = 1, color = "red")) +
  geom_text(x = .4, y = 500, label = paste0(pct_lessthan_one*100, "%", sep = ""), color = "red")









