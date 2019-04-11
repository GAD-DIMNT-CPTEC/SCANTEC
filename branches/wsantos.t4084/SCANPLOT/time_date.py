from datetime import timedelta, date

def daterange(date1, date2):
    for n in range(int ((date2 - date1).days)+1):
        yield date1 + timedelta(n)

# Como utiliza-lo:
# retirado desse site:
# https://www.w3resource.com/python-exercises/date-time-exercise/python-date-time-exercise-50.php

#start_dt = date(2015, 5, 1)
#end_dt = date(2015, 5, 5)
#for dt in daterange(start_dt, end_dt):
#    #print(dt.strftime("%Y-%m-%d"))
#    #print(dt.strftime("%Y%m%d"))
#    anomesdia = dt.strftime("%Y%m%d")
#    dia = dt.strftime("%d")
#    mes = dt.strftime("%m")
#    ano = dt.strftime("%Y")
#    ####print("dia: "+str(dia))
#    ####print("mes: "+str(mes))
#    ####print("ano: "+str(ano))
#    
#    #print(dt.strftime("%Y-%m-%d"))
#    print("ano-mes-dia "+str(anomesdia))
