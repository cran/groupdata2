## ----include=FALSE-------------------------------------------------------

knitr::opts_chunk$set(collapse = T, comment = "#>", fig.align='center')
options(tibble.print_min = 4L, tibble.print_max = 4L)


## ----warning=FALSE,message=FALSE-----------------------------------------
# Attach some packages
library(groupdata2)
library(dplyr)
library(ggplot2)
library(knitr) # kable()
library(lmerTest) #lmer()
library(broom) #tidy()
library(hydroGOF) # rmse()


# Create dataframe
df <- data.frame("participant" = factor(rep(c('1','2', '3', '4', '5', '6'), 3)),
                "age" = rep(c(20,23,27,21,32,31), 3),
                "diagnosis" = rep(c('a', 'b', 'a', 'b', 'b', 'a'), 3),
                "score" = c(10,24,15,35,24,14,24,40,30,50,54,25,45,67,40,78,62,30))

# Order by participant
df <- df[order(df$participant),] 

# Remove index
rownames(df) <- NULL

# Add session info
df$session <- as.integer(rep(c('1','2', '3'), 6))

# Show the dataframe
kable(df, align = 'c')


## ------------------------------------------------------------------------
lm(score~diagnosis, df) %>%
  summary() %>%
  tidy()

## ------------------------------------------------------------------------
m0 <- 'score~1+(1|participant)'
m1 <- 'score~diagnosis+(1|participant)'
m2 <- 'score~diagnosis+age+(1|participant)'
m3 <- 'score~diagnosis+session+(1|participant)'
m4 <- 'score~diagnosis*session+(1|participant)'
m5 <- 'score~diagnosis*session+age+(1|participant)'


## ------------------------------------------------------------------------
df_folded <- fold(df, k=5)

# Order by .folds
df_folded <- df_folded[order(df_folded$.folds),]

kable(df_folded, align='c')


## ------------------------------------------------------------------------
df_folded <- fold(df, k=5, cat_col = 'diagnosis')

# Order by .folds
df_folded <- df_folded[order(df_folded$.folds),]

kable(df_folded, align='c')


## ------------------------------------------------------------------------
df_folded %>% 
  group_by(.folds) %>% 
  count(diagnosis) %>% 
  kable(align='c')


## ------------------------------------------------------------------------
# Set seed so that we get the exact same folds every time we run our script
set.seed(1)

# Use fold with cat_col and id_col
df_folded <- fold(df, k=3, cat_col = 'diagnosis', id_col = 'participant')

# Order by .folds
df_folded <- df_folded[order(df_folded$.folds),]

kable(df_folded, align='c')


## ------------------------------------------------------------------------
df_folded %>% 
  group_by(.folds) %>% 
  count(diagnosis, participant) %>% 
  kable(align='c')


## ------------------------------------------------------------------------
crossvalidate <- function(df, k, model, dependent, random){
  
  # Initialize empty list for recording performances
  performances <- c()
  
  # One iteration per fold
  for (fold in c(1:k)){
    
    # Create training set for this iteration
    # Subset all the datapoints where .folds does not match the current fold
    training_set <- df[df$.folds != fold,]
    
    # Create test set for this iteration
    # Subset all the datapoints where .folds matches the current fold
    test_set <- df[df$.folds == fold,]
    
    ## Train model

    # If there is a random effect,
    # use lmer to train model
    # else use lm

    if (isTRUE(random)){

      # Train linear mixed effects model on training set
      model <-  lmer(model, training_set, REML=FALSE)

    } else {

      # Train linear model on training set
      model <-  lm(model, training_set)

    }

    ## Test model

    # Predict the dependent variable in the test_set with the trained model
    predicted <- predict(model, test_set, allow.new.levels=TRUE)

    # Get the Root Mean Square Error between the predicted and the observed
    RMSE <- rmse(predicted, test_set[[dependent]])

    # Add the RMSE to the performance list
    performances[fold] <- RMSE


  }

  # Return the mean of the recorded RMSEs
  return(c('RMSE' = mean(performances)))

}


## ------------------------------------------------------------------------
m0
crossvalidate(df_folded, k=3, model=m0, dependent='score', random=TRUE)

m1
crossvalidate(df_folded, k=3, model=m1, dependent='score', random=TRUE)

m2
crossvalidate(df_folded, k=3, model=m2, dependent='score', random=TRUE)

m3
crossvalidate(df_folded, k=3, model=m3, dependent='score', random=TRUE)

m4
crossvalidate(df_folded, k=3, model=m4, dependent='score', random=TRUE)

m5
crossvalidate(df_folded, k=3, model=m5, dependent='score', random=TRUE)


## ------------------------------------------------------------------------
lmer(m4, df_folded) %>%
  summary()



## ------------------------------------------------------------------------
timeSeriesFrame = data.frame('residents' = austres)

# Show structure of dataframe
str(timeSeriesFrame) 

# Show head of data
timeSeriesFrame %>% head(12) %>% kable(col.names = NULL)


## ----echo=FALSE, message=FALSE-------------------------------------------
# Plot of time series

ggplot(timeSeriesFrame, aes(seq_along(residents), residents)) +
  geom_point() +
  labs(x = 'Measurement', y = 'Residents')



## ------------------------------------------------------------------------
ts = timeSeriesFrame %>%
  
  # Group data
  group(n = 12, method = 'greedy') %>%
  
  # Find means of each group
  dplyr::summarise(mean = mean(residents))

# Show new data
ts %>% kable() 

## ----echo=FALSE, message=FALSE-------------------------------------------
# Plot of time series

ggplot(ts, aes(.groups, mean)) +
  geom_point() +
  labs(x = 'Groups', y = 'Mean n of residents')



## ------------------------------------------------------------------------
ts = timeSeriesFrame %>%
  
  # Group data
  group(n = 12, method = 'greedy') %>%
  
  # Find range of each group
  dplyr::summarise(range = diff(range(residents)))

# Show new data
ts %>% kable() 


## ------------------------------------------------------------------------
main_group_size = 12

# Loop through a list ranging from 1-30
for (step_size in c(1:30)){
  
  # If the remainder is 0
  if(main_group_size %staircase% step_size == 0){
    
    # Print the step size
    print(step_size)
    
  }
  
}


## ------------------------------------------------------------------------
ts <- timeSeriesFrame %>%
  
  # Group data
  group(n = 12, method='greedy') %>%
  
  # Create subgroups
  do(group(., n = 2, method='staircase', col_name = '.subgroups'))

# Show head of new data
ts %>% head(24) %>% kable() 


## ------------------------------------------------------------------------
# Show tail of new data
ts %>% tail(17) %>% kable()


## ------------------------------------------------------------------------
ts_means <- ts %>%
  
  # Convert .subgroups to an integer and then to a factor
  mutate(.subgroups = as.integer(.subgroups),
         .subgroups = as.factor(.subgroups)) %>%
  
  # Group by first .groups, then .subgroups
  group_by(.groups, .subgroups) %>%
  
  # Find the mean and range of each subgroup
  dplyr::summarise(mean = mean(residents),
                   range = diff(range(residents)))

# Show head of new data
ts_means %>% head(9) %>% kable() 

## ----echo=FALSE, message=FALSE-------------------------------------------
# Plot of time series

ggplot(ts_means, aes(seq_along(mean), mean)) +
  geom_point() +
  labs(x = 'Subgroups', y = 'Mean n of residents')



