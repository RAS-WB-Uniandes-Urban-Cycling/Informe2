require(pacman)
pacman::p_load(reticulate)
pacman::p_load(dplyr)
use_python("/Users/germancarvajal/anaconda3/envs/R_env/bin/python")

# Synthetic minority class over-sampling technic SMOTE
# for continuous and categorical features.
# using imbalance-learn implementation through python
# asumes the first column contains the target and
# the distance for the knn is calculated over all features
# 
# Data is a dataframe with numerical values and factors
# 
# sampling strategy is the proportion of minority observations
# over the majority observations after the over-sampling
# 
# random_state is the seed

SMOTENC_py<- function(Data,sampling_strategy = 1,random_state = NA){
  
  # Drops missing values from the data frame
  Data <- Data %>% drop_na()
  
  # Identifies categorical features and stores the indices
  cat_feat <- which(unlist(lapply(Data,is.factor)))
  
  # Converts the random_state parameter to an integer value
  random_state <- ifelse(is.na(random_state), NA, as.integer(random_state))
  
  # Imports imbalance-learn python module
  imblearn <- reticulate::import("imblearn")
  
  # Separates the target "y" from the features "X" and converts all to numeric types
  y <- Data[,1] %>% as.matrix(.)
  X <- Data[,-1] %>% 
    mutate_all(.funs=funs(as.numeric)) %>% 
    as.matrix(.)
  
  attr(X, "dimnames")<-NULL
  
  # Creates an SMOTENC object for the data 
  sm <- imblearn$over_sampling$SMOTENC(random_state = random_state,
                                       categorical_features = as.integer(cat_feat-2),
                                       sampling_strategy = sampling_strategy)
  
  # Creates the new dataframe with the synthetic observations
  sm_res <- sm$fit_resample(X = X, y = y)
  
  new_Data <- cbind(sm_res[[2]],sm_res[[1]]) %>% 
    as.data.frame(.) %>% 
    set_names(names(Data))
  
  # Includes the correct factor levels for categorical variables
  for(i in cat_feat) {
    new_Data[,i] <- levels(Data[,i])[new_Data[,i]]
    new_Data[,i] <- factor(new_Data[,i],levels = levels(Data[,i]))
  }
  
  return(new_Data)
  
}
