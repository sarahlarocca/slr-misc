# Import libraries

import numpy as np
import os
import pandas as pd
import pickle
import sys
import xgboost as xgb
 
from hyperopt import fmin, tpe, hp, STATUS_OK
from sklearn.ensemble import GradientBoostingClassifier as gbc
from sklearn import cross_validation as cv

def load_data():
    
    # Read CSV to data frame
    data = pd.read_csv('filename.csv', sep=',')
    data.columns = [x.lower() for x in data.columns]
    data['target_column'] = data['target_column'] - 1
    
    # Factorize categorical variables
    for col in data.columns:
        if data[col].dtype == 'O':
            data[col] = pd.factorize(data[col])[0]

    # Create numpy arrays for modeling
    feature_names = data.columns.drop(['id_column', 'target_column'])
    X = data[feature_names].values
    y = data['target_column'].values
    
    return X, y

X, y = load_data()

# Create indices for crossfolds

nfolds = 10
skf = cv.StratifiedKFold(y,
                        n_folds=nfolds,
                        shuffle=True,
                        random_state=17)

skf

def objective(params):
    params['max_depth'] = int(params['max_depth'])
    params['n_estimators'] = int(params['n_estimators'])
    print 'Training model with parameters: '
    print params
    
    # Fit model to data
    bst = gbc(**params)
    score = cv.cross_val_score(bst, X, y,
                               cv=skf,
                               scoring='log_loss').mean()

    print 'Score {0}\n\n'.format(score)
    return {'loss': abs(score), 'status': STATUS_OK}

# Define the hyperparameter space
space = {'loss' : 'deviance',
         'learning_rate' : hp.quniform('learning_rate', 0.02, 0.5, 0.02),
         'n_estimators' : hp.quniform('n_estimators', 10, 1000, 5),
         'max_depth' : hp.quniform('max_depth', 1, 15, 1),
         'subsample' : hp.quniform('subsample', 0.5, 1, 0.05),
         'verbose' : 1
}

best = fmin(objective, space=space, algo=tpe.suggest, max_evals=250)
print 'Optimal parameters for dtrain are: ', best
