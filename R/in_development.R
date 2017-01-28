# This is for developing new functions that can then be placed in their own script .R



# Create balanced folds

# load_dataset <- function(){
#   data <- XLConnect::readWorksheet(XLConnect::loadWorkbook("../ClinicalData.xlsx"),sheet=1)
#
#   data$ID <- as.factor(data$ID)
#
#   return(data)
#
# }

#fold(load_dataset(), 2, cat_col = 'Diagnosis', method='staircase') %>% group_by(Diagnosis) %>% count(.groups)

