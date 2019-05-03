import json
from pprint import pprint
import itertools


with open('json/scanplot.json', encoding='utf-8') as f:
    data = json.load(f)

def select(teste1,teste3,teste4,teste5,teste6,teste7,teste8,teste9,teste10):

  result1 = []
  result2 = []
  result3 = []
  result4 = []
  result5 = []
  result6 = []
  result7 = []
  result8 = []
  result9 = []
  result10 = []
  

  for a in range(0,teste1): 
    var1 = data['var_scantec'][0]['position'][a]
    result1.append(var1)

    var2 = data['var_scantec'][1]['previsao'][a]
    result2.append(var2)

  for c in range(0,teste3):    
    var3 = data['var_scantec'][2]['estatistica'][c]
    result3.append(var3)
  
  for c in range(0,teste4):    
    var4 = data['var_scantec'][3]['title'][c]
    result4.append(var4)
  
  for e in range(0,teste5):    
    var5 = data['var_scantec'][4]['level'][e]
    result5.append(var5)

  for f in range(0,teste6):    
    var6 = data['var_scantec'][5]['positionSCANTEC'][f]
    result6.append(var6)  

  for g in range(0,teste7):    
    var7 = data['var_scantec'][6]['regiao'][g]
    result7.append(var7)

  for h in range(0,teste8):    
    var8 = data['var_scantec'][7]['experimentos'][h]
    result8.append(var8)  
  
  for i in range(0,teste9):    
    var9 = data['var_scantec'][8]['horario'][i]
    result9.append(var9)

  for j in range(0,teste10):    
    var10 = data['var_scantec'][9]['frequencia'][j]
    result10.append(var10)    

  return (result1,result2,result3,result4,result5,result6,result7,result8,result9,result10)






#
#
##https://developer.rhino3d.com/guides/rhinopython/python-xml-json/
##https://codingnetworker.com/2015/10/python-dictionaries-json-crash-course/
#
