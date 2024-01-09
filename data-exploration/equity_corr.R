# Correlation analysis for equity quintiles at the region level

library(magrittr)
library(psrccensus)
library(psrcelmer)
library(dplyr)
library(srvyr)
library(data.table)
library(tidyverse)

# By Census Tract -------
#pull equity shares from Elmer
eq_tract_shares <- get_table(schema="equity", tbl_name="v_tract_shares")

table(eq_tract_shares$data_year)

#filter for 2020
eq_tract_shares_20 <- eq_tract_shares %>% 
  dplyr::filter(data_year==2020)  

#reorder columns to match order wanted for output
eq_tract_shares_20 <- eq_tract_shares_20[, c("geoid", "poc_quintile", "income_quintile", "disability_quintile", 
                                              "lep_quintile", "youth_quintile", "older_quintile")] 

#change quintiles to numbers
eq_tract_num_20 <- eq_tract_shares_20 %>%
  mutate(poc_quintile = recode(poc_quintile, "High" = 5, "Medium High" = 4, "Medium" = 3, "Low Medium" = 2, "Low" = 1)) %>%
  mutate(income_quintile = recode(income_quintile, "High" = 5, "Medium High" = 4, "Medium" = 3, "Low Medium" = 2, "Low" = 1)) %>%
  mutate(disability_quintile = recode(disability_quintile, "High" = 5, "Medium High" = 4, "Medium" = 3, "Low Medium" = 2, "Low" = 1)) %>%
  mutate(lep_quintile = recode(lep_quintile, "High" = 5, "Medium High" = 4, "Medium" = 3, "Low Medium" = 2, "Low" = 1)) %>%
  mutate(youth_quintile = recode(youth_quintile, "High" = 5, "Medium High" = 4, "Medium" = 3, "Low Medium" = 2, "Low" = 1)) %>%
  mutate(older_quintile = recode(older_quintile, "High" = 5, "Medium High" = 4, "Medium" = 3, "Low Medium" = 2, "Low" = 1))
  
cor_data <- eq_tract_num_20

cor_data <- cor_data %>%
  dplyr::select(-geoid)

# correlation matrix
cor(cor_data,use = "complete.obs")

library(Hmisc)

# create matrix of correlation coefficients and p-values
rcorr(as.matrix(cor_data))

# visualize correlation matrix
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")

corrplot(cor(cor_data), type="lower")

# other visualization option in corrplot package but difficult to reorder
# rquery.cormat(cor_data) 

# Another package that can be used for correlation analysis
# library(GGally)
# 
# # correlation matrix
# ggpairs(cor_data, columns = 1:6,
#         upper = list(continuous = wrap("cor", size = 3)))
# 
# # visualize correlation matrix 
# ggcorr(cor_data)
