import glob
import matplotlib.pyplot as plt


#Xdef open_file(statistic,time):
#        path =r'dadosscantec/' # use your path
#        allFiles = glob.glob(path + "/"+str(statistic)+"EXP"+ str(time) + "*.csv")
#        print("OK")
#        
#        return (allFiles)

def open_file(statistic,time):
        path =r'dadosscantec/exp1/diario/GL/' # use your path
        allFiles = glob.glob(path + "/"+str(statistic)+"EXP01"+ + "*.csv")
        print("OK")
        
        return (allFiles)



def figure_plot_save(title,time,statistic):
        plt.savefig('img/'+ str(title)+str(statistic)+str(time)+'.png')
        return


def figure_plot_config(ts,title,time,statistic,previsao):
        plt.plot(ts)
        plt.title(title+'_'+str(statistic)+str(time)+'_Prev_'+str(previsao)+'_horas')
        plt.ylabel(str(statistic))
        plt.xlabel("Tempo")
        return