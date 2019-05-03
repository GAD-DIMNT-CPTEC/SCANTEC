"""
Objetivo: converter as tabelas do SCANTEC para o formato CSV.

Uso: python3 convert_to_csv.py

Observações: o usuário deverá alterar o valor do parâmetro "base_path".
"""

import ntpath
import glob
import pandas as pd
import os
from fjso import select

base_path = '/home/carlos/Documents/INPE2019/GDAD/SMG/teste_scamtec_v100/repo_carlos/wsantos.t4084/SCANPLOT/dadosscantec/aval_SMG/'

a = select(6,3,12,5,6,5,2,4,2)

# Frequencia: mensal, diario
for j in range(0,2):
    # Varia experimentos
    for x in range(0,2):   
        # Varia regiao
        for b in range(0,5):   
            path = base_path  + str(a[9][j]) + '/00Z/' + str(a[7][x]) + '.' + str(a[6][b])
            print("path:",path)
            allFiles = glob.glob(path + "/*.scam")
            print("allFiles:",allFiles)   
 
            list_ = []
    
            for file_ in allFiles:
                name_file=ntpath.basename(file_)
                print(name_file)
                df = pd.read_csv(file_,delimiter='\s+')
                df.to_csv(path + '/' + name_file[0:-5] + '.csv', float_format='%g', index=False)
                print("*******")
                print("Periodo:"+ str(a[9][j]))
                print("REGIAO:"+ str(a[6][b]))
                print("EXPERIMENTO:"+ str(a[7][x]))
                print("*******")
                print(df)
