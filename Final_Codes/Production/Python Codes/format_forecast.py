# -*- coding: utf-8 -*-
"""
Created on Wed Mar 20 15:06:32 2019

@author: agupta466
"""

import pandas as pd
import numpy as np
import fnmatch
import sys
from pathlib import Path
import os

path = Path(__file__).parents[2]
#sys.path.append(str(p)+"/Production/Input Files")
#sys.path.append(str(p)+"/Production/Output Files")
sys.path.append(str(path)+"/Export/Python Codes")

import Forecasting_Export as export
from Forecasting_Export import Replace,Replace_comma,get_date

encoding='ISO-8859-15'

def prod_processing():
    all_files=os.listdir(str(path)+'/Production/Output Files/')
    date=fnmatch.filter(all_files, "*_Produção.csv*")
    date=str(date)[2:10]
    filename=date+u'_Produção.csv'
    prod=pd.read_csv(str(path)+'/Production/Output Files/'+filename,encoding='utf-8-sig',engine='python')
    for index,row in prod.iloc[:,:].iterrows():
        prod.loc[index,'Estado']=prod.loc[index,'Estado'].upper()
    
    for field in prod.iloc[:,6:9]:
        prod[field] = prod[field].map(Replace)
        
    for field in prod.iloc[:,6:9]:
        prod[field] = prod[field].map(Replace_comma)
    
    prod.iloc[:,6:9] = prod.iloc[:,6:9].apply(pd.to_numeric, errors = 'coerce')
    prod.to_csv(str(path)+'/Production/Output Files/'+date+u'_Produção.csv',index=False,encoding=encoding)
# =============================================================================
#     return(prod)
# =============================================================================
    
def main():
    prod_processing()
# =============================================================================
#     production=prod_processing()
#     date=get_date()
#     production.to_csv(str(path)+'/Production/Output Files/'+date+u'_Produção.csv',index=False,encoding=encoding)
# =============================================================================
# =============================================================================
#     all_files=os.listdir(str(path)+'/Production/Output Files/')
#     date=fnmatch.filter(all_files, "*_Produção.csv*")
#     date=str(date)[2:10]
#     filename=date+u'_Produção.csv'
#     prod=pd.read_csv(str(path)+'/Production/Output Files/'+filename,encoding=encoding,engine='python')
#     for index,row in prod.iloc[:,:].iterrows():
#         prod.loc[index,'Estado']=prod.loc[index,'Estado'].upper()
#     prod1=prod[(prod['Ano']<2019)&(prod['Tipo Ajustado']=='Real/Calculada')]
#     prod2=prod[(prod['Ano']==2019)&(prod['Tipo Ajustado']=='Prevista')]
#     
#     total=pd.concat([prod1.reset_index(drop=True),prod2.reset_index(drop=True)],axis=0)
#     total.to_csv(str(path)+'/Production/Output Files/'+date+u'_Produção.csv',index=False,encoding=encoding)
#     
#     all_files=os.listdir(str(path)+'/Production/Output Files/')
#     date=fnmatch.filter(all_files, "*_Produção.xlsx*")
#     date=str(date)[2:10]
#     filename=date+u'_Produção.xlsx'
#     prod=pd.read_excel(str(path)+'/Production/Output Files/'+filename,encoding='utf-8-sig')
#     for index,row in prod.iloc[:,:].iterrows():
#         prod.loc[index,'Estado']=prod.loc[index,'Estado'].upper()
#     prod1=prod[(prod['Ano']<2019)&(prod['Tipo Ajustado']=='Real/Calculada')]
#     prod2=prod[(prod['Ano']==2019)&(prod['Tipo Ajustado']=='Prevista')]
#     
#     total=pd.concat([prod1.reset_index(drop=True),prod2.reset_index(drop=True)],axis=0)
#     total.to_excel(str(path)+'/Production/Output Files/'+date+u'_Produção.xlsx',index=False,encoding=encoding)
# =============================================================================
    
if __name__ == '__main__':
    main()