# Deep-Learning-for-Learning-Analytics-Project
Part of MSc Thesis for Business Intelligence and Smart Services Master Programme.

For my thesis I want to compare deep learning models (LSTM) with traditional machine learning models for predicting student performance. I hypothesize that LSTMs will be able to predict student performance more accurately than traditional machine learning models.

If you want to run the necessary code, first download the OULAD dataset here: https://analyse.kmi.open.ac.uk/open_dataset Kuzilek J., Hlosta M., Zdrahal Z. Open University Learning Analytics dataset Sci. Data 4:170171 doi: 10.1038/sdata.2017.171 (2017).

I structured my repository in the following way:

- Data Transformation and Analysis (R): Here you can find the R files necessary to (1) transform the original data into a suitable format for exploration and analysis, (2) explore the data in more detail in a descriptive sense. These insights are also documented and can be found in the results folder. (3) Develop basic machine learning models (logistic regression, SVM, naive Bayes & random forests) to predict student performance.

- LSTM (Python): Here you can find the Jupyter notebooks necessary to build the LSTM models used to predict student performance. These LSTM models are build with the Keras package.

- Results: Here you can find the (preliminary) results of the data exploration and analysis I documented.