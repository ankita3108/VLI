
import pandas as pd
import numpy as np
import os

production_data = pd.read_csv('../Input Files/Produção_all.csv')
production_data.head(3)
production_data = production_data.round(3)

production_data.columns = ['%Confiança', 'Ano', 'Estado', 'Microrregião', 'Mês', 'Produto','Produção', 'Produção Max', 'Produção Mín', 'Tipo', 'Tipo Ajustado',
       'UF']


## new format for corn
production_data_2019_corn = production_data[(production_data["Tipo Ajustado"] == "Real/Calculada") &(production_data["Tipo"] == "Real") & (production_data["Produto"] != "Soja")].reset_index(drop=True)
production_data_2019_soja = production_data[(production_data["Tipo Ajustado"] == "Real/Calculada") &(production_data["Tipo"] == "Real") & (production_data["Produto"] == "Soja")].reset_index(drop=True)


## new format for corn
production_data_2019_rem_corn = production_data[(production_data["Tipo Ajustado"] == "Prevista") &(production_data["Tipo"] == "Prevista") & (production_data["Produto"] != "Soja") & (production_data["%Confiança"] == 70)].reset_index(drop=True)
production_data_2019_rem_soya = production_data[(production_data["Tipo Ajustado"] == "Prevista") &(production_data["Tipo"] == "Prevista") & (production_data["Produto"] == "Soja") & (production_data["%Confiança"] == 70)].reset_index(drop=True)


## for corn
production_data_2019_new_corn = production_data_2019_corn[['Ano','Mês','UF','Produto','Produção']]
production_data_2019_new1_corn = production_data_2019_rem_corn[['Ano','Mês','UF','Produto','Produção']]


## for soya
production_data_2019_new_soya = production_data_2019_soja[['Ano','Mês','UF','Produto','Produção']]
production_data_2019_new1_soya = production_data_2019_rem_soya[['Ano','Mês','UF','Produto','Produção']]


production_data_to_use_corn =production_data_2019_new_corn.append(production_data_2019_new1_corn,ignore_index = True)
production_data_to_use_soya =production_data_2019_new_soya.append(production_data_2019_new1_soya,ignore_index = True)


## Production_data for Soya
new_fraction = pd.DataFrame()
unique_states = production_data_to_use_soya["UF"].unique()
for i in unique_states:
    
    value = round(production_data_to_use_soya[production_data_to_use_soya["UF"]==i].groupby(['Ano','Mês','UF'],as_index = False).sum()["Produção"],3)
    month = production_data_to_use_soya[production_data_to_use_soya["UF"]==i].groupby(['Ano','Mês','UF'],as_index = False).sum()["Mês"]
    year =production_data_to_use_soya[production_data_to_use_soya["UF"]==i].groupby(['Ano','Mês','UF'],as_index = False).sum()["Ano"]
    
    new_fraction["Year"] = year
    new_fraction["Month"] = month
    new_fraction[i] = value

## covarites for soya
cov_soya =  pd.read_csv('../Input Files/Covarite_data.csv')

## Corn_covarites
cov_corn =  pd.read_csv('../Input Files/covarites_corn.csv')

import os
path_output ='../Data Directory'
os.chdir(path_output)
os.mkdir('production_lags_soya')
os.mkdir('production_lags_corn')

data = new_fraction.copy()
## for soya

columns = ["Year","Month","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12","lag13","lag6_sum_prod",
           "lag9_sum_prod","lag12_sum_prod"]
for i in range(2,len(data.columns)):
    
    index=range(0,96)
    new_data = pd.DataFrame(index=index,columns=columns)
    new_data = new_data.fillna(0)
    new_data["Year"] = data["Year"]
    new_data["Month"] = data["Month"]
    
    
    new_data["lag6_sum_prod"] = round(data[data.columns[i]].rolling(min_periods = 1, window = 6).sum(),3)
    new_data["lag6_sum_prod"] = new_data["lag6_sum_prod"].shift(1)
    
    new_data["lag9_sum_prod"] = round(data[data.columns[i]].rolling(min_periods = 1, window = 9).sum(),3)
    new_data["lag9_sum_prod"] = new_data["lag9_sum_prod"].shift(1)
    
    new_data["lag12_sum_prod"] = round(data[data.columns[i]].rolling(min_periods = 1, window = 12).sum(),3)
    new_data["lag12_sum_prod"] = new_data["lag12_sum_prod"].shift(1)
    
    new_data['lag1'] = data[data.columns[i]].shift(1)
    new_data['lag2'] = data[data.columns[i]].shift(2)
    new_data['lag3'] = data[data.columns[i]].shift(3)
    new_data['lag4'] = data[data.columns[i]].shift(4)
    new_data['lag5'] = data[data.columns[i]].shift(5)
    new_data['lag6'] = data[data.columns[i]].shift(6)
    new_data['lag7'] = data[data.columns[i]].shift(7)
    new_data['lag8'] = data[data.columns[i]].shift(8)
    new_data['lag9'] = data[data.columns[i]].shift(9)
    new_data['lag10'] = data[data.columns[i]].shift(10)
    new_data['lag11'] = data[data.columns[i]].shift(11)
    new_data['lag12'] = data[data.columns[i]].shift(12)
    new_data['lag13'] = data[data.columns[i]].shift(13)

        
    new_data['CBOT_lag1'] = cov_soya["CBOT"].shift(1)
    new_data['CBOT_lag2'] = cov_soya["CBOT"].shift(2)
    new_data['CBOT_lag3'] = cov_soya["CBOT"].shift(3)
    
    
    new_data = new_data.replace(np.nan, 0.00)
    
    result = pd.concat([new_data, cov_soya], axis=1)    

    
    result.to_csv('production_lags_soya'+'/'+"lags"+data.columns[i]+".csv",index = False)


## Production_data for corn
del production_data_to_use_soya
new_fraction = pd.DataFrame()
unique_states = production_data_to_use_corn["UF"].unique()
for i in unique_states:
    
    value = round(production_data_to_use_corn[production_data_to_use_corn["UF"]==i].groupby(['Ano','Mês','UF'],as_index = False).sum()["Produção"],3)
    month = production_data_to_use_corn[production_data_to_use_corn["UF"]==i].groupby(['Ano','Mês','UF'],as_index = False).sum()["Mês"]
    year =production_data_to_use_corn[production_data_to_use_corn["UF"]==i].groupby(['Ano','Mês','UF'],as_index = False).sum()["Ano"]
    
    new_fraction["Year"] = year
    new_fraction["Month"] = month
    new_fraction[i] = value

## for corn
data = new_fraction.copy()
columns = ["Year","Month","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12","lag13","lag6_sum_prod",
           "lag9_sum_prod","lag12_sum_prod"]
for i in range(2,len(data.columns)):
    
    index=range(0,96)
    new_data = pd.DataFrame(index=index,columns=columns)
    new_data = new_data.fillna(0)
    new_data["Year"] = data["Year"]
    new_data["Month"] = data["Month"]
    
    
    new_data["lag6_sum_prod"] = round(data[data.columns[i]].rolling(min_periods = 1, window = 6).sum(),3)
    new_data["lag6_sum_prod"] = new_data["lag6_sum_prod"].shift(1)
    
    new_data["lag9_sum_prod"] = round(data[data.columns[i]].rolling(min_periods = 1, window = 9).sum(),3)
    new_data["lag9_sum_prod"] = new_data["lag9_sum_prod"].shift(1)
    
    new_data["lag12_sum_prod"] = round(data[data.columns[i]].rolling(min_periods = 1, window = 12).sum(),3)
    new_data["lag12_sum_prod"] = new_data["lag12_sum_prod"].shift(1)
    
    new_data['lag1'] = data[data.columns[i]].shift(1)
    new_data['lag2'] = data[data.columns[i]].shift(2)
    new_data['lag3'] = data[data.columns[i]].shift(3)
    new_data['lag4'] = data[data.columns[i]].shift(4)
    new_data['lag5'] = data[data.columns[i]].shift(5)
    new_data['lag6'] = data[data.columns[i]].shift(6)
    new_data['lag7'] = data[data.columns[i]].shift(7)
    new_data['lag8'] = data[data.columns[i]].shift(8)
    new_data['lag9'] = data[data.columns[i]].shift(9)
    new_data['lag10'] = data[data.columns[i]].shift(10)
    new_data['lag11'] = data[data.columns[i]].shift(11)
    new_data['lag12'] = data[data.columns[i]].shift(12)
    new_data['lag13'] = data[data.columns[i]].shift(13)

    new_data = new_data.replace(np.nan, 0.00)
    
    result = pd.concat([new_data, cov_corn], axis=1)    

    
    result.to_csv('production_lags_corn'+'/'+"lags"+data.columns[i]+".csv",index = False)
