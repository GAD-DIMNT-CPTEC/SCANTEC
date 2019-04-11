import glob
import csv
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from time_exec import time_statistics

###   ROC2 . py 

@time_statistics
def plot_ROC(var_1, var_2,var_3, title,time,statistic,previsao):
    path =r'dadosscantec/' # use your path
    allFiles = glob.glob(path + "/"+str(statistic)+"EXP"+ str(time) + "*.csv")

    list_ = []

    for file_ in allFiles:
        df = pd.read_csv(file_)
        list_.append(df)
        ts = df.loc[0:107,[var_1, var_2,var_3]]
        print(file_)
        plt.plot(ts)
        plt.title(title+'_'+str(statistic)+str(time)+'_Prev_'+str(previsao)+'_horas')
        plt.xlabel("Tempo")
        #plt.ylabel("Y-axis")
        plt.legend([var_1, var_2,var_3], loc=0)
        plt.tight_layout()
        plt.plot()
        plt.savefig('img/'+ str(title)+str(statistic)+str(time)+'.png')
        return
