#! /usr/bin/env python3
#-*- coding: utf-8 -*-

from datetime import timedelta, date

# Ideia da função retirada desse site
#Como utiliza-lo:
# retirado desse site:
# https://www.w3resource.com/python-exercises/date-time-exercise/python-date-time-exercise-50.php

# Exemplo de como utilizar a função
#start_dt = date(2015, 5, 1)
##end_dt = date(2015, 5, 5)
#for dt in daterange(start_dt, end_dt):
######print(dt.strftime("%Y-%m-%d"))

def daterange(date1, date2):
    for n in range(int ((date2 - date1).days)+1):
        yield date1 + timedelta(n)


# Classe criada para passar os parametros de data

# Exemplo de como utilizar a classe
#p = definirData(1,4,5,2015)
#print(p.__str__)


class definirData:
        def __init__(self,diaInicial,diaFinal,mes,ano):
                self.diaI = diaInicial  
                self.diaF = diaFinal
                self.mes  = mes
                self.ano  = ano
        
        def getDiaInicial(self):
                return self.diaI
        
        def getDiaFinal(self):
                return self.diaF
        
        def getmes(self):
                return self.mes
        
        def getano(self):
                return self.ano
        
        def getStart(self):
                return date(self.ano,self.mes,self.diaI)
        
        def getEnd(self):
                return date(self.ano,self.mes,self.diaF)
        



