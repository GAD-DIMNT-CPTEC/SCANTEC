import glob
import pandas as pd
import os
from fjso import select


#Transformar os trechos abaixo em funções                     - Nok
#corrigir script para o modelo atual                          - Nok
#corrigir diretorio que buscará os dados                      - Nok
#corrigir etapa que separa as previsões em arquivos distintos - Nok
#corrigr -> criar diretorio 'new_file'                        - Nok
#corrigir nome do file de saida                               - ok

#Para experimentos convencionais do scantec
#Falta:
#Criar a função
#separar em funções para reuso
#importar o arquivo no devido local
#

######path ='.'
######allFiles = glob.glob(path + "/*.scam")
######
######list_ = []
######
######for file_ in allFiles:
######    name_file=os.path.splitext(file_)[0]
######    print(name_file)
######    df = pd.read_csv(file_,delimiter='\s+')
######    df.to_csv('new_file/'+name_file + '.csv', float_format='%g', index=False)
######    print(df)
######
######
#######Para experimentos do ensemble do scantec

#path ='/home/wanderson/Documentos/novos_experimentos/SCAMTEC_trunk/jan/saida_scantec_01N/saida_scantec_01N_RMSE'

###
#Criar um 'for' para variar:
# - exp[1 e 2]
# - diario e mensal
# - regioes

a =select(6,3,12,5,6,5,5,4,2)

#frequencia: mensal, diario
for j in range(0,2):
    #varia exp1 exp2
    for x in range(0,2):   
        #varia regiao
        for b in range(0,5):   
            ##path ='/home/wanderson/Documentos/teste_scamtec_v100/SCANTEC/dadosscantec/exp1/diario/GL'
            path ='/home/wanderson/Documentos/teste_scamtec_v100/SCANTEC/dadosscantec/'+str(a[7][x]) +'/'+ str(a[9][j])+'/'+str(a[6][b])
            allFiles = glob.glob(path + "/*.scam")
    
            #print(str(a[6][b]))
            #print(str(a[7][x]))
            #print(str(a[9][j]))
            
    
            list_ = []
    
            for file_ in allFiles:
                name_file=os.path.splitext(file_)[0][82:136]
                print(name_file)
                df = pd.read_csv(file_,delimiter='\s+')
                #df.to_csv('dadosscantec/01N_RMSE/'+name_file + '.csv', float_format='%g', index=False)
                #df.to_csv('dadosscantec/exp1/diario/GL/'+name_file + '.csv', float_format='%g', index=False)
                df.to_csv('dadosscantec/'+str(a[7][x]) +'/'+ str(a[9][j])+'/'+str(a[6][b])+'/'+name_file + '.csv', float_format='%g', index=False)
                print("*******")
                print("Periodo:"+ str(a[9][j]))
                print("REGIAO:"+ str(a[6][b]))
                print("EXPERIMENTO:"+ str(a[7][x]))
                print("*******")
                print(df)
    
    
    