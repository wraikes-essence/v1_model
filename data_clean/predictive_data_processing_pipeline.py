# ## Description: Predictive Model, All Impressions
# Each record is a unique user and campaign combination. Use only the first survey response per user & campaign.  And remove all impressions afterwards.  Convert the remaining impressions into separate columns with one cut per column, and the value signifying the frequency.
# 
# - Site/Format/Device
# - Baseline
# - Month
# - Creative Size

import pandas as pd, numpy as np, os 
from datetime import timedelta, datetime
from functools import reduce
import matplotlib.pyplot as plt


def main():
    #read in config file
    #df = pull in df from BQ

    df = df_new_features(df)
    dfs = supporting_dfs(df)
    final = df_merge(dfs)

    #save down files


def start_date_in_previous_month(dt, count):
    '''
    Get start date for the baseline.
    '''

    _days = 7 * count
    tmp = dt.replace(day=1) - timedelta(days=1)
    return tmp.replace(day=1) - timedelta(days=_days)


def end_date_in_previous_month(dt, count):
    '''
    Get end date for the baseline.
    '''

    _days = 7 * count
    return dt.replace(day=1) - timedelta(days=1) + timedelta(days=_days)


def get_baseline(df, row, label, count):
    '''
    Calculate baseline if sample > 200, else expand the dates until 
    receive a sufficient sample.
    '''

    start = start_date_in_previous_month(row['date'], count)
    end = end_date_in_previous_month(row['date'], count)
    sample = get_sampleSize(df, start, end)
        
    if (sample >= 200):
        baseline = get_baseline_helper(df, start, end, label)
        return baseline
    
    else:
        count += 1
        return get_baseline(df, row, label, count+1)


def get_sampleSize(df, start, end):
    '''
    Get sample size for baseline.
    '''

    size = df[
        (df.grp == 'CON') &
        (df.date >= start) &
        (df.date <= end)
    ].shape[0]
    
    return size


def get_baseline_helper(df, start, end, label):
    '''
    Calculate baseline based on filters.
    '''
    avg = df.loc[
        (df.grp == 'CON') &
        (df.date >= start) &
        (df.date <= end)
    ][label].mean()
     
    return avg


def freq_counts(df, label):
    '''
    Create new dataframe that creates new variables for each "label" with frequency as the value.
    '''
    
    return df.groupby(['user_id', 'campaign_id', label]).size().unstack().reset_index().fillna(0)


def df_new_features(df):
    '''
    Create new features for dataframe:
        Date, month, cut id, creative size & baseline.
    '''
    df['date'] = pd.to_datetime(df.date)
    df['month'] = df.date.dt.month    
    df['cuts'] = df.device_name.str.cat([df.channel_name, df.medium_name, df.prst_site], sep='_')
    df['vid_size'] = df.dcm_ad.str.extract(r'(:[\d\d]{1,2}|[\d\d]{1,2}s)')[0].str.replace('s|:', '')
    df['creative_size'] = np.where(df.creative_pixel_size == '0x0', 
                                   df.vid_size, 
                                   df.creative_pixel_size)
    df['baseline'] = df.apply(lambda x: get_baseline(df, x, 'answer_desired_Aided_awareness', 0), axis=1)


def supporting_dfs(df):
    '''
    Create grouped_by supporting dataframes to construct the final dataframe.
    '''

    df_answer = df.groupby(['user_id', 'campaign_id'])['answer_desired_Aided_awareness'].max().reset_index()
    df_grp = df.groupby(['user_id', 'campaign_id'])['grp'].first().reset_index()
    df_freq = df.groupby(['user_id', 'campaign_id'])['frequency'].first().reset_index()
    df_base = df.groupby(['user_id', 'campaign_id'])['baseline'].mean().reset_index()

    df_creative_size = freq_counts(df, 'creative_size')
    df_cuts = freq_counts(df, 'cuts')
    df_month = freq_counts(df, 'month')

    return [df_answer, df_grp, df_freq, df_base, df_creative_size, df_cuts, df_month]


def df_merge(dfs):
    '''
    Merge supporting dataframes.
    '''

    df_final = reduce(lambda left, right: pd.merge(left, right, how = 'inner', on=['user_id', 'campaign_id']), dfs)
    df_final = pd.get_dummies(df_final, columns=['grp'])
    
    return df_final


if __name__ == '__main__':
    main()



