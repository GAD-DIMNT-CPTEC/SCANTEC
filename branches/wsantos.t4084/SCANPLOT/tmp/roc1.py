#import glob
import csv
import pandas as pd
#import numpy as np
import matplotlib.pyplot as plt
from time_exec import time_statistics

from roc_config import * 

def figure_config_legend(var_1):
        plt.legend([var_1], loc=0)
        return

def figure_config(ts,var_1,title,time,statistic,previsao):

        figure_plot_config(ts,title,time,statistic,previsao)

        figure_config_legend(var_1)
        
        plt.plot()
        return

@time_statistics
def plot_ROC_1_var(var_1,title,time,statistic,previsao):
    
    a = open_file(statistic,time)    
    
    list_ = []

    for file_ in a:
        df = pd.read_csv(file_)
        list_.append(df)
        ts = df.loc[0:107,[var_1]]
        print(file_)

    figure_config(ts,var_1,title,time,statistic,previsao)   
    figure_plot_save(title,time,statistic) 
        
    return


