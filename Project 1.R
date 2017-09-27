#--------Step 1 : Collect the Breast cancer data -----#



wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

wbcd <- wbcd[,-1] #Remove the patient ID.

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

#-----Step 2 : Clean the data -----#

# To normalize wrt Z-scale
wbcd_z <- as.data.frame(scale(wbcd[,2:31]))

# Normalize using a function.
normalize <- function(x){
  
    return( (x - min(x))/ (max(x) - min(x)))
}

# Applying the normalizing function to all the numerical columns.
wbcd_n <- as.data.frame(lapply(wbcd[, 2:31], normalize))

#------- Step 3 : Train the data -------#

wbcd_Train <- wbcd_n[1:469,]
 
wbcd_Test <- wbcd_n[470:569,]

wbcd_Train_labels <- wbcd[1:469,1]

wbcd_Test_Labels <- wbcd[470:569,1]

library(class)

# wbcd_test_pred stores the predicted value using the K-Nearest Neighbor method.
wbcd_test_pred <- knn(train = wbcd_Train, test = wbcd_Test, cl = wbcd_Train_labels, k =21 )


#-------- Step 4: Visalizing the results-------#
library(gmodels)

CrossTable( x = wbcd_Test_Labels, y = wbcd_test_pred, prop.chisq = FALSE)


