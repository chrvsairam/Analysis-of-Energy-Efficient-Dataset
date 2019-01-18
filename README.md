# Analysis-of-Energy-Efficient-Dataset in R
The Project is about analyzing the performance of different models like Multiple Linear Regression, k-Fold, LOOCV, Regression Trees, Random Forests, Bagging and Boosting for Energy Efficient Dataset.
Dataset URL:https://archive.ics.uci.edu/ml/datasets/Energy+efficiency#


Dataset:The Dataset consists of 1296 observations with 10 variables of which 8 are predictor variables and 2 are response variables. 
        The Data Set is taken from UCI Machine learning Repository where it contains information about factors that are responsible for
        energy efficiency of the Houses. The following 8 features(X1,X2,X3,X4,X5,X6,X7,X8) are useful to predict the both of the 
        responses(Y1,Y2).All these attributes are real values.


Observations:By observing Mean Squared Errors from the implemented models. Linear regression performs badly when compared to tree based 
             models because linear regression has an MSE of 10.13 for Y1 and 11.26 for Y2 whereas for regression tree it is 9.37 and 9.71 
             which is the least performer of all the tree based models.The order for best performed models to this dataset
             is Boosting > Bagging > Random Forest > fitting regression tress > multiple linear model.
