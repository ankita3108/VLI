import pandas as pd
import numpy as np
import os

state_name = pd.read_csv('../Input Files/states_names.csv',encoding='latin-1')

xls_soja = pd.ExcelFile('../Input Files/COMEX_2012_2019_20190208_SOJA.xlsx')
actual_soja = pd.read_excel(xls_soja, 'Resultado')

xls_corn = pd.ExcelFile('../Input Files/COMEX_2012_2019_20190208_MILHO.xlsx')
actual_corn = pd.read_excel(xls_corn, 'Resultado')

soja_final = actual_soja.groupby(['Ano','Mês','UF do Produto']).sum().reset_index()

corn_final = actual_corn.groupby(['Ano','Mês','UF do Produto']).sum().reset_index()

uniques_states_soja = soja_final["UF do Produto"].unique()
uniques_states_corn = corn_final["UF do Produto"].unique()

year = []
month = []

for i in range(2012,2019):
    for j in range(1,13):
        
        month.append(j)
        year.append(i)

month.append(1)
year.append(2019)

soya_data_to_use = pd.DataFrame()
soya_data_to_use["Ano"] = year
soya_data_to_use["Mês"] = month

soya_data_to_use_final = pd.DataFrame()

list_ = []

for i in uniques_states_soja:
    
    soja_final_use = soja_final[soja_final["UF do Produto"]==i].reset_index(drop=True)
    del soja_final_use["Valor FOB (US$)"]
    del soja_final_use["UF do Produto"]
    del soja_final_use["Código NCM"]
    df = soja_final_use.rename(columns = {'Quilograma Líquido':i})

    df_new = pd.merge(soya_data_to_use,df, on=['Ano','Mês'], how='outer').reset_index(drop=True)
    
    df_new = df_new.loc[:].div(1000000, axis=0)
    
    soya_data_to_use_final[i] = df_new[i]
    
soya_data_to_use_final.insert(0, "Ano", year)
soya_data_to_use_final.insert(0, "Mês", month)
soya_data_to_use_final.fillna(0, inplace=True)

corn_data_to_use = pd.DataFrame()
corn_data_to_use["Ano"] = year
corn_data_to_use["Mês"] = month

corn_data_to_use_final = pd.DataFrame()


for i in uniques_states_corn:
    
    corn_final_use = corn_final[corn_final["UF do Produto"]==i].reset_index(drop=True)
    del corn_final_use["Valor FOB (US$)"]
    del corn_final_use["UF do Produto"]
    del corn_final_use["Código NCM"]
    df = corn_final_use.rename(columns = {'Quilograma Líquido':i})

    df_new = pd.merge(corn_data_to_use,df, on=['Ano','Mês'], how='outer').reset_index(drop=True)
    
    df_new = df_new.loc[:].div(1000000, axis=0)
    
    corn_data_to_use_final[i] = df_new[i]
    
corn_data_to_use_final.insert(0, "Ano", year)
corn_data_to_use_final.insert(0, "Mês", month)
corn_data_to_use_final.fillna(0, inplace=True)

## CORN DATA TRANSFORMATION
export_corn = corn_data_to_use_final.copy()
export_corn_col = export_corn.columns[2:]

my_list = state_name["Full_Name"].values
new_columns = []
new_columns.append("Mês")
new_columns.append("Ano")
for i in export_corn_col:
    if i.title() in my_list:
        
        value = state_name[state_name["Full_Name"]== i.title()].reset_index(drop=True)["State"][0]        
        new_columns.append(value)
        
    else:
        new_columns.append(i.title())
        
export_corn.columns = new_columns
export_corn = export_corn.round(3)
export_corn["Unallocated"] = export_corn["Mercadoria Nacionalizada"] + export_corn["Não Declarada"] + export_corn["Zona Não Declarada"]
del export_corn["Mercadoria Nacionalizada"]
del export_corn["Não Declarada"] 
del export_corn["Zona Não Declarada"]

export_corn_states = export_corn.copy()
del export_corn_states["Ano"]
del export_corn_states["Mês"]
del export_corn_states["Unallocated"]


MUL_FACTOR = []
for i in export_corn["Unallocated"]:
    MUL_FACTOR.append(float(i))
df_new = export_corn_states.loc[:].div(export_corn_states.sum(axis=1), axis=0)
df_new = df_new.mul(MUL_FACTOR, axis=0)

df_tu_use = export_corn_states.round(3)+df_new.round(3)
df_tu_use.insert(0, "Mês", export_corn.iloc[:,0:1])
df_tu_use.insert(0, "Ano", export_corn.iloc[:,1:2])


for i in state_name["State"].unique():
    
    if i not in df_tu_use.columns[2:]:
        df_tu_use[i] = 0


df_tu_use.to_csv("../Data Directory/Comex_state_corn_use.csv",index = False)

### Soya Data Transformation

export_soya = soya_data_to_use_final.copy()
export_soya_col = export_soya.columns[2:]

my_list = state_name["Full_Name"].values
new_columns = []
new_columns.append("Mês")
new_columns.append("Ano")
for i in export_soya_col:
    if i.title() in my_list:
        
        value = state_name[state_name["Full_Name"]== i.title()].reset_index(drop=True)["State"][0]        
        new_columns.append(value)
        
    else:
        new_columns.append(i.title())
        
export_soya.columns = new_columns
export_soya = export_soya.round(3)
export_soya["Unallocated"] = export_soya["Mercadoria Nacionalizada"] + export_soya["Não Declarada"]
del export_soya["Mercadoria Nacionalizada"]
del export_soya["Não Declarada"] 

export_soya_states = export_soya.copy()
del export_soya_states["Ano"]
del export_soya_states["Mês"]
del export_soya_states["Unallocated"]


MUL_FACTOR = []
for i in export_soya["Unallocated"]:
    MUL_FACTOR.append(float(i))
df_new = export_soya_states.loc[:].div(export_soya_states.sum(axis=1), axis=0)
df_new = df_new.mul(MUL_FACTOR, axis=0)

df_tu_use = export_soya_states.round(3)+df_new.round(3)
df_tu_use.insert(0, "Mês", export_corn.iloc[:,0:1])
df_tu_use.insert(0, "Ano", export_corn.iloc[:,1:2])


for i in state_name["State"].unique():
    
    if i not in df_tu_use.columns[2:]:
        df_tu_use[i] = 0

df_tu_use.to_csv("../Data Directory/Comex_state_soya_use.csv",index = False)