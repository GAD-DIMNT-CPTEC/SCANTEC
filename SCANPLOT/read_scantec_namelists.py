#! /usr/bin/env python3

import re

# Lê o arquivo scantec.vars e transforma a lista de variáveis e níveis e um dicionário
filename = 'scantec.vars'
 
VarsLevs = {}

# Com o método "with open", o arquivo é fechado automaticamente ao final
with open(filename,'r') as scantec_vars:
  for line in scantec_vars.readlines():
    rline = line.lstrip()
    if not (rline.startswith('#') or rline.startswith('::') or rline.startswith('variables:')):
      varlevdesc = rline.strip().split(' ', 1)
      VarsLevs[varlevdesc[0]] = varlevdesc[1].strip('\"')

print('VarsLevs = ',VarsLevs)    

# Lê do arquivo scantec.conf e transforma as informações principais em um dicionário
filename = 'scantec.conf'

# A função a seguir lê a linha com a informação requerida e cria uma lista com os elementos separados  de acordo com o separador ':'
Confs = {}

def key_value(linew):
  nlist = re.split(': ',linew)
  key = nlist[0]
  value = nlist[1].split()[0]
  Confs[key] = value
  return Confs 

# A função a seguir lê a lista de experimentos e cria um dicionário
Exps = {}

def key_value_exps(lexps):
  for i in range(2, len(lexps)): # 2: desconsidera as linhas "Experiments:" e "#ModelId Name Diretory File_Name_with_mask"
    slexps = lexps[i].split()
    Exps[slexps[1]] = [slexps[0], slexps[2]]
    Confs['Experiments'] = Exps
  return Confs

# Com o método "with open", o arquivo é fechado automaticamente ao final
with open(filename,'r') as scantec_conf:
  for line in scantec_conf:
    if line.startswith('Starting Time'):
      key_value(line)
    elif line.startswith('Ending Time'):
      key_value(line)
    elif line.startswith('Analisys Time Step'):
      key_value(line)
    elif line.startswith('Forecast Time Step'):
      key_value(line)
    elif line.startswith('History Time'):
      key_value(line)
    elif line.startswith('scantec tables'):
      key_value(line)
    elif line.startswith('run domain number'):
      key_value(line)
    elif line.startswith('run domain lower left lat'):
      key_value(line)
    elif line.startswith('run domain lower left lon'):
      key_value(line)
    elif line.startswith('run domain upper right lat'):
      key_value(line)
    elif line.startswith('run domain upper right lon'):
      key_value(line)
    elif line.startswith('run domain resolution dx'):
      key_value(line)
    elif line.startswith('run domain resolution dy'):
      key_value(line)
    elif line.startswith('Reference Model Name'):
      key_value(line)
    elif line.startswith('Experiments'):
      exps = []
      while not(line.startswith('::')):
        exps.append(line)
        line = next(scantec_conf)
      key_value_exps(exps)
    elif line.startswith('Reference file'):
      key_value(line)
    elif line.startswith('Climatology Model Name'):
      key_value(line)
    elif line.startswith('Climatology file'):
      key_value(line)
    elif line.startswith('Output directory'):
      key_value(line)

print('Confs = ',Confs)
