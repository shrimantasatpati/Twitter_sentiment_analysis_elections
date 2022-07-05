#Data Pre-procesing Step  
# importing libraries
!pip install numpy
!pip install matplotlib
!pip install pandas
!pip install sklearn
import numpy as nm  
import matplotlib.pyplot as mtp  
import pandas as pd
import sklearn
from sklearn import metrics
  
#importing datasets  
data_set= pd.read_csv("C:/Users/super/OneDrive/Desktop/N_Modi.DF.csv")
data_set
data_set.shape

#Extracting Independent and dependent Variable  
x= data_set.iloc[:, 14].values
x
x=x.reshape(-1,1)
y= data_set.iloc[:, 15].values
y=y.reshape(-1,1)

# Splitting the dataset into training and test set.  
from sklearn.model_selection import train_test_split  
x_train, x_test, y_train, y_test= train_test_split(x, y, test_size= 0.25, random_state=0)
x_train
x_test
y_train

#feature Scaling  
from sklearn.preprocessing import StandardScaler    
st_x= StandardScaler()    
x_train= st_x.fit_transform(x_train)    
x_test= st_x.transform(x_test)

#Fitting Logistic Regression to the training set  
from sklearn.linear_model import LogisticRegression  
classifier= LogisticRegression(solver="liblinear", random_state=0)


classifier.fit(x_train, nm.ravel(y_train,order='C'))
LogisticRegression(C=1.0, class_weight=None, dual=False, fit_intercept=True,  
                   intercept_scaling=1, l1_ratio=None, max_iter=100,  
                   multi_class='warn', n_jobs=None, penalty='l2',  
                   random_state=0, solver="liblinear", tol=0.0001, verbose=0,  
                   warm_start=False)

classifier.classes_
classifier.coef_
classifier.intercept_



#classifier.predict_proba(x)
#classifier.predict(x)
#classifier.score(x,y)


#Predicting the test set result  
y_pred= classifier.predict(x_test)
classifier.score(x_train, nm.ravel(y_train,order='C'))
classifier.score(x_test, y_test)
#Creating the Confusion matrix  
from sklearn.metrics import confusion_matrix  
cm= confusion_matrix(y_test,y_pred)
cm
print(metrics.confusion_matrix(y_test, y_pred))
# Printing the precision and recall, among other metrics
print(metrics.classification_report(y_test, y_pred))

cm = confusion_matrix(y_test, y_pred)

fig, ax = mtp.subplots(figsize=(8, 8))
ax.imshow(cm)
ax.grid(False)
ax.xaxis.set(ticks=(0, 1), ticklabels=('Predicted 0s', 'Predicted 1s'))
ax.yaxis.set(ticks=(0, 1), ticklabels=('Actual 0s', 'Actual 1s'))
ax.set_ylim(1.5, -0.5)
for i in range(2):
    for j in range(2):
        ax.text(j, i, cm[i, j], ha='center', va='center', color='red')
mtp.show()
