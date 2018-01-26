# BAX-ML
This repository is created for the BAX-452 Machine Learning course. It contains the files for each assignment arranged in separate folders.

### Prep_Assignment Folder -
1. Contains the assignment based on the wine quality dataset downloaded from Kaggle. The Kaggle exercise was classification, but I have used logistic regression modeling for this assignment.
2. Link to the data - https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009/data
3. The data contains entries for different wines with the chemical composition as columns and the quality marked in numbers
4. Higher the quality value, better the wine quality (Generally, >6.5 is considered good qualtiy wine)
5. The following code builds a logistic model to predit the quality of a wine given the chemical composition

### Titanic Survival Analysis Folder -
The aim of the project is to predict survival on the Titanic based on the dataset of the onboard passengers of the historical journey embarked on April 15, 1912. The survival prediction output is submitted as "Titanic_Prediction.csv" with 2 columns - PassengerID and Survival prediction (1/0) and 418 rows. In brief, the process was to clean the training dataset, perform EDA (along with visualization), finalize the model variables, run few ML techniques (Logistic Regression, Decision Tree and Random Forest) to train the model and finally make predictions on the unseen Test dataset. Based on the Accuracy and features importance, I have chosen Random Forest to make the final predictions.

### Diamond Price Prediction Folder -
This classic dataset "Diamonds" contains the prices and other attributes of almost 54,000 diamonds. It's a great dataset for beginners learning to work with data analysis and visualization. Our aim is to fulfill the above requirements and predict the price of a diamond with a given set of properties. We use linear regression modeling technique from the sci-kit package.
