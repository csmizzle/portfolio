import pandas as pd
import numpy as np
import sklearn as sk
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import train_test_split
from sklearn.cluster import KMeans
from math import sin, cos, sqrt, atan2, radians

file_name = 'DC_Properties.csv'

# read in data
def load_data(f_name):
    df = pd.read_csv(f_name)
    return(df)

# recude mem usage
def reduce_mem_usage(df, verbose=True):
    numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
    start_mem = df.memory_usage().sum() / 1024**2
    for col in df.columns:
        col_type = df[col].dtypes
        if col_type in numerics:
            c_min = df[col].min()
            c_max = df[col].max()
            if str(col_type)[:3] == 'int':
                if c_min > np.iinfo(np.int8).min and c_max < np.iinfo(np.int8).max:
                    df[col] = df[col].astype(np.int8)
                elif c_min > np.iinfo(np.int16).min and c_max < np.iinfo(np.int16).max:
                    df[col] = df[col].astype(np.int16)
                elif c_min > np.iinfo(np.int32).min and c_max < np.iinfo(np.int32).max:
                    df[col] = df[col].astype(np.int32)
                elif c_min > np.iinfo(np.int64).min and c_max < np.iinfo(np.int64).max:
                    df[col] = df[col].astype(np.int64)
            else:
                if c_min > np.finfo(np.float16).min and c_max < np.finfo(np.float16).max:
                    df[col] = df[col].astype(np.float16)
                elif c_min > np.finfo(np.float32).min and c_max < np.finfo(np.float32).max:
                    df[col] = df[col].astype(np.float32)
                else:
                    df[col] = df[col].astype(np.float64)
    end_mem = df.memory_usage().sum() / 1024**2
    if verbose: print('Mem. usage decreased to {:5.2f} Mb ({:.1f}% reduction)'.format(end_mem, 100 * (start_mem - end_mem) / start_mem))
    return (df)

# encode categorical variables with
def preprocess(df):
    df = df.drop(['CMPLX_NUM','LIVING_GBA','SALEDATE','GIS_LAST_MOD_DTTM','ASSESSMENT_SUBNBHD','YR_RMDL'], axis=1)
    df = df.dropna(subset=['PRICE','STYLE','FULLADDRESS','QUADRANT','SQUARE'])
    df['SQUARE'] = df['SQUARE'].str.extract('(\d+)', expand=False)
    df.columns = df.columns.str.replace('[^a-zA-Z]', '')
    df.columns = df.columns.str.replace(' ', '')
    # fill nas
    cat_cols = list(df.select_dtypes(include=['object']).columns)
    df[cat_cols] = df[cat_cols].fillna('nodata')
    print('Converting', str(cat_cols), 'to numerical cols')
    df[cat_cols] = df[cat_cols].apply(LabelEncoder().fit_transform)
    # cluster labels
    print('Clustering.... \n' )
    kmeans = KMeans(n_clusters = 4, init = 'k-means++', random_state = 42)
    df['cluster'] = kmeans.fit_predict(df.iloc[:, [36, 37]].values)
    # distance from whit house
    # establish target variable
    divide = 1000000
    df["LuxL"] = (df["PRICE"] >= divide)
    df['LuxL'] = df['LuxL'].astype(int)
    df = df.drop('PRICE', axis = 1)
    print('There are', len(df), 'rows in the cleaned data.')
    return(df)

def dist_from_white_house(df):
    lat_longs = df[['LATITUDE', 'LONGITUDE']]
    lat_longs['white_lat'] = 38.8977
    lat_longs['white_lon'] = 77.0365

    distances = []

    # approximate radius of earth in km
    for a, b, c, d in lat_longs.itertuples(index=False):
            R = 6373.0
            lat1 = a
            lon1 = b
            lat2 = c
            lon2 = d
            dlon = lon2 - lon1
            dlat = lat2 - lat1
            a1 = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
            c1 = 2 * atan2(sqrt(a1), sqrt(1 - a1))
            distance = R * c1

            distances.append(distance)

    return(distances)

# split data
def split_write_csv(df):
    data = df
    train, test = train_test_split(data, test_size = .2, random_state = 42)
    data.to_csv('data_full.csv', index=False)
    train.to_csv('train.csv', index=False)
    test.to_csv('test.csv', index=False)
    return(print('Data has been clean, split, and saved. Check wd for files.'))

# load
df = load_data(file_name)
df = reduce_mem_usage(df)
dist = dist_from_white_house(df)
df['dist_WH'] = dist

# place y varibale at the end of the df , helps with later code
cols_at_end = ['LuxL']
df = df[[c for c in df if c not in cols_at_end]
        + [c for c in cols_at_end if c in df]]

# split and output
df_encoded = preprocess(df)
split_write_csv(df_encoded)
print('Model training... \n')