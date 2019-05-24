# install these prior to running code
'''pip install boto3
pip install psycopg2
pip install d6tflow
pip install -Iv luigi == 2.8.3'''

import pandas as pd
import numpy as np
import d6tflow as dt
import luigi as li
import sklearn as sk
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import make_scorer
import matplotlib as plt

test_file_name = 'test.csv'
file_name = 'train.csv'

# by read in data
class TaskGetData(dt.tasks.TaskCSVPandas):

    def run(self):
        df_train = pd.read_csv(file_name)
        self.save(df_train)

class TaskPreprocess(dt.tasks.TaskCachePandas):
    do_preprocess = li.BoolParameter(default = True)

    def requires(self):
        return TaskGetData()

    def run(self):
        df_train = self.input().load()
        if self.do_preprocess:
            df_train=df_train.fillna(df_train.mean())
            df_train.iloc[:, :-1] = sk.preprocessing.scale(df_train.iloc[:,:-1])
        self.save(df_train)

class TaskTrain(dt.tasks.TaskPickle):
    do_preprocess = li.BoolParameter(default = True)

    def requires(self):
        return TaskPreprocess(do_preprocess = self.do_preprocess)

    def run(self):
        df_train = self.input().load()
        class_weights = {0:1, 1:2}
        model = RandomForestClassifier(n_estimators = 400, random_state = 0, max_depth=20, class_weight=class_weights, min_samples_split=5,min_samples_leaf=4)
        model.fit(df_train.iloc[:, :-1], df_train['LuxL'])
        self.save(model)

dt.preview(TaskTrain())
dt.run(TaskTrain())

model = TaskTrain().output().load()

# run model on test data
def load_test_data(file_name):
    test_df = pd.read_csv(test_file_name)
    return(test_df)

def get_test_preds(test_data):
    test_df = test
    test_df = test_df.fillna(test_df.mean())
    test_df.iloc[:,:-1] = sk.preprocessing.scale(test_df.iloc[:,:-1])
    preds = model.predict(test_df.iloc[:, :-1])
    return(preds)

test = load_test_data(test_file_name)
predictions = get_test_preds(test)

print('Model ran on Test Data')
print('Accuracy Score \n', sk.metrics.accuracy_score(predictions, test.iloc[:, -1]))
print('AUC Score \n', sk.metrics.roc_auc_score(predictions, test.iloc[:, -1]))
print('Confusion Matrix \n', sk.metrics.confusion_matrix(predictions, test.iloc[:, -1]))
print('F1-score \n', sk.metrics.f1_score(predictions, test.iloc[:, -1]))
print('Confusion Matrix \n', sk.metrics.classification_report(predictions, test.iloc[:, -1]))

def plot_importances():
    model = TaskTrain().output().load()
    df_train = TaskPreprocess().output().load()
    df_importance = pd.Series(model.feature_importances_, index=df_train.iloc[:,:-1].columns)
    import matplotlib.pyplot as plt
    df_importance.sort_values(ascending=False).plot.bar()
    plt.xlabel('feature')
    plt.ylabel('importance')
    plt.title('')
    plt.tight_layout()
    plt.savefig('plot.png')

plot_importances()