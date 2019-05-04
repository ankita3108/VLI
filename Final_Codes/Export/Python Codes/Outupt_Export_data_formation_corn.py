import pandas as pd
import numpy as np
import os

export_fraction_corn = pd.read_csv('../Input Files/EXPORT_FRACTION_CORN.csv',encoding='latin-1')

## Corn ratio for state to zone
list_ = []
only_one_zone = []
export_fraction = export_fraction_corn.copy()
for i in unique_state:
    
    dd = export_fraction[export_fraction["State"]==i].reset_index(drop=True)
    if len(dd) ==1:
        only_one_zone.append(i)
    
    new_export_fraction = pd.DataFrame(index = range(0,len(dd)),columns= dd.columns)
    new_export_fraction = new_export_fraction.fillna(0)
    new_export_fraction["State"] = dd["State"]
    new_export_fraction["Zones"] = dd["Zones"]
    
    new_export_fraction["2012"] = round(dd["2012"]/dd["2012"].sum(),3)
    new_export_fraction["2013"] = round(dd["2013"]/dd["2013"].sum(),3)
    new_export_fraction["2014"] = round(dd["2014"]/dd["2014"].sum(),3)
    new_export_fraction["2015"] = round(dd["2015"]/dd["2015"].sum(),3)
    
    new_export_fraction["2016"] = round(dd["2016"]/dd["2016"].sum(),3)
    new_export_fraction["2017"] = round(dd["2017"]/dd["2017"].sum(),3)
    new_export_fraction["2018"] = round(dd["2018"]/dd["2018"].sum(),3)
    new_export_fraction["2019"] = round(dd["2019"]/dd["2019"].sum(),3)
    new_export_fraction["2020"] = round(dd["2020"]/dd["2020"].sum(),3)
    new_export_fraction = new_export_fraction.fillna(0)
    
    del dd
    list_.append(new_export_fraction)

frame_ratio_corn = pd.concat(list_, axis = 0, ignore_index = True)
del list_
del export_fraction

list_ = []
for i in unique_state:
    
    if i not in only_one_zone:
        
        dd = frame_ratio_corn[frame_ratio_corn["State"]==i].reset_index(drop=True)
        list_.append(dd)
        
    else:
        dd = frame_ratio_corn[frame_ratio_corn["State"]==i].reset_index(drop=True)
        dd["2012"] = 1
        dd["2013"] = 1
        dd["2014"] = 1
        dd["2015"] = 1
        dd["2016"] = 1
        dd["2017"] = 1
        dd['2018'] = 1
        dd['2019'] = 1
        dd['2020'] = 1
        list_.append(dd)
        
frame_ratio_corn = pd.concat(list_, axis = 0,ignore_index = True)


## Reading real data Corn
export_data_real_milho = pd.read_csv('../Data Directory/Comex_state_corn_use.csv',encoding='latin-1')
export_data_real_milho = export_data_real_milho.round(3)

column_export = ["Produto","Mês","Ano","UF","Microrregião","Exportação Mín (Kt)","Exportação Med (Kt)","Exportação Max (Kt)","Tipo","Tipo Ajustado","%Confiança"]
final_export_table = pd.DataFrame(columns= column_export)
final_export_table

## corn Real
list_ = []
years = [2012,2013,2014,2015,2016,2017,2018]

export_data_real = export_data_real_milho.copy()
frame_ratio = frame_ratio_corn.copy()
for year in years:
    
    exx = export_data_real[export_data_real["Ano"] == year].reset_index(drop=True)
    
    for i in export_data_real.columns[2:]:
        
        ss = frame_ratio[frame_ratio["State"] == i].reset_index(drop=True)
        
        zones = ss["Zones"].unique()
        
        for z in zones:
            
            final_export_table = pd.DataFrame(columns= column_export)
            
            value = ss[ss["Zones"] == z].reset_index(drop=True)[str(year)][0]
            
            final_export_table["Exportação Med (Kt)"] = round(exx[i]*value,3)
            final_export_table["Produto"] = "Milho"
            final_export_table["Mês"] = range(1,13)
            final_export_table["Ano"] = year
            final_export_table["UF"] = i
            final_export_table["Microrregião"] = z
            final_export_table["Exportação Mín (Kt)"] = 0
            final_export_table["Exportação Max (Kt)"] = 0
            final_export_table["Tipo"] = "Real"
            final_export_table["Tipo Ajustado"] = "Real/Calculada"
            final_export_table["%Confiança"] = 0
            
            list_.append(final_export_table)
            
            
for i in export_data_real.columns[2:]:
    
    ss = frame_ratio[frame_ratio["State"] == i].reset_index(drop=True)
    exx = export_data_real[export_data_real["Ano"] == 2019].reset_index(drop=True)
    
    zones = ss["Zones"].unique()
    for z in zones:
        final_export_table = pd.DataFrame(columns= column_export)
        value = ss[ss["Zones"] == z].reset_index(drop=True)["2019"][0]
        final_export_table["Exportação Med (Kt)"] = round(exx[i]*value,3)
        final_export_table["Produto"] = "Milho"
        final_export_table["Mês"] = range(1,2)
        final_export_table["Ano"] = 2019
        final_export_table["UF"] = i
        final_export_table["Microrregião"] = z
        final_export_table["Exportação Mín (Kt)"] = 0
        final_export_table["Exportação Max (Kt)"] = 0
        final_export_table["Tipo"] = "Milho"
        final_export_table["Tipo Ajustado"] = "Real/Calculada"
        final_export_table["%Confiança"] = 0
        
        list_.append(final_export_table)
        
frame_export_actual_corn = pd.concat(list_, axis = 0, ignore_index = True)
del frame_ratio
del export_data_real

column_export = ["Produto","Mês","Ano","UF","Microrregião","Exportação Mín (Kt)","Exportação Med (Kt)","Exportação Max (Kt)","Tipo","Tipo Ajustado","%Confiança"]
final_export_table = pd.DataFrame(columns= column_export)
final_export_table


mypath = "../Data Directory/export_results_corn"
from os import listdir
from os.path import isfile, join
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]


list_ = []
for file_ in onlyfiles:
    df = pd.read_csv('../Data Directory/export_results_corn'+"/"+file_,index_col=None, header=0)
    df["Year"] = 2019
    df["Month"] = range(2,13)
    del df["Unnamed: 0"]
    
    list_.append(df)

frame_export_pred_corn = pd.concat(list_, axis = 0, ignore_index = True)


unique_state_pred =frame_export_pred_corn["State"].unique()


intervals = [70,75,80,85,95]
list_ = []
frame_export_pred = frame_export_pred_corn.copy()
frame_ratio = frame_ratio_corn.copy()

for k in intervals:
    
    for i in unique_state_pred:
        
        ss1 = frame_export_pred[frame_export_pred["State"] == i].reset_index(drop=True)
        ss = frame_ratio[frame_ratio["State"] == i].reset_index(drop=True)
        
        zones = ss["Zones"].unique()
        for z in zones:
            final_export_table = pd.DataFrame(columns= column_export)
            value = ss[ss["Zones"] == z].reset_index(drop=True)["2019"][0]
            final_export_table["Exportação Med (Kt)"] = round(ss1["Point Forecast"]*value,3)
            final_export_table["Produto"] = "Milho"
            final_export_table["Mês"] = range(2,13)
            final_export_table["Ano"] = 2019
            final_export_table["UF"] = i
            final_export_table["Microrregião"] = z
            final_export_table["Exportação Mín (Kt)"] = round(ss1["Lo"+" "+str(k)]*value,3)
            final_export_table["Exportação Max (Kt)"] = round(ss1["Hi"+" "+str(k)]*value,3)
            final_export_table["Tipo"] = "Prevista"
            final_export_table["Tipo Ajustado"] = "Prevista"
            final_export_table["%Confiança"] = k
        
            list_.append(final_export_table)
            
frame_export_pred_corn = pd.concat(list_, axis = 0, ignore_index = True)
del frame_export_pred
del frame_ratio


full_states = frame_export_actual_corn["Microrregião"].unique()
predicted_states = frame_export_pred_corn["Microrregião"].unique()

not_model =[]
for i in full_states:
    if i not in predicted_states:
        not_model.append(i)
        
not_model.append('Year')
to_use_calulated = set(not_model)
export_estimated = pd.DataFrame(columns=to_use_calulated,index=range(0,1))
export_estimated['Year'] = 2019

for i in export_estimated.columns:
    if i != 'Year':
        value = export_fraction_soja[export_fraction_corn['State']==i].reset_index(drop=True)['2019'][0]
        export_estimated[i].iloc[0] = round(value,3)    


list_ = []
intervals = [70,75,80,85,95]
not_model =[]
for i in full_states:
    if i not in predicted_states:
        not_model.append(i)

for inter in intervals:
    for i in not_model:
        ss1 = export_estimated[export_estimated["Year"] == 2019].reset_index(drop=True)
        ss = frame_ratio_corn[frame_ratio_corn["State"] == i].reset_index(drop=True)
        
        zones = ss["Zones"].unique()
        for z in zones:
            final_export_table = pd.DataFrame(columns= column_export)
            value = ss[ss["Zones"] == z].reset_index(drop=True)["2019"][0]
            final_export_table["Mês"] = range(2,13)
            final_export_table["Produto"] = "Milho"
            final_export_table["Ano"] = 2019
            final_export_table["UF"] = i
            final_export_table["Microrregião"] = z
            final_export_table["Exportação Mín (Kt)"] = 0
            final_export_table["Exportação Max (Kt)"] = 0
            final_export_table["Tipo"] = "Prevista"
            final_export_table["Tipo Ajustado"] = "Prevista"
            final_export_table["%Confiança"] = inter
            final_export_table["Exportação Med (Kt)"] = round(ss1[i][0]/12,3)
        
            list_.append(final_export_table)


frame_export_estimated_corn = pd.concat(list_, axis = 0, ignore_index = True)


frame_export_full_predicted = pd.concat([frame_export_pred_corn,frame_export_estimated_corn], ignore_index=True)


mypath = "../Data Directory/export_fitted_results_corn"
from os import listdir
from os.path import isfile, join
onlyfiles = [f for f in listdir(mypath) if isfile(join(mypath, f))]

list_ = []
year = []
for j in range(2012,2019):
    for i in range(12):
        year.append(j)

month = []
for j in range(7):
    for i in range(1,13):
        month.append(i)
        
for file_ in onlyfiles:
    
    df = pd.read_csv('../Data Directory/export_fitted_results_corn'+"/"+file_,index_col=None, header=0)[:84].reset_index(drop=True)
    df["Year"] = year
    df["Month"] = month
    del df["Unnamed: 0"]
    
    list_.append(df)
    
for file_ in onlyfiles:
    
    df = pd.read_csv('../Data Directory/export_fitted_results_corn'+"/"+file_,index_col=None, header=0)[84:].reset_index(drop=True)
    df["Year"] = 2019
    df["Month"] = 1
    del df["Unnamed: 0"]
    
    list_.append(df)

frame_export_actual_fitted_corn = pd.concat(list_, axis = 0, ignore_index = True)

fitted = pd.DataFrame()
unique_states= frame_export_actual_fitted_corn["State"].unique()

for i in unique_states:
    
    DF = frame_export_actual_fitted_corn[frame_export_actual_fitted_corn["State"]==i].reset_index(drop=True)
    fitted["Year"] = DF["Year"]
    fitted["Month"] = DF["Month"]
    value = DF["x"]
    fitted[i] = value

list_ = []
years = [2012,2013,2014,2015,2016,2017,2018]
for year in years:
    
    exx = fitted[fitted["Year"] == year].reset_index(drop=True)
    
    for i in fitted.columns[2:]:
        
        ss = frame_ratio_corn[frame_ratio_corn["State"] == i].reset_index(drop=True)
        
        zones = ss["Zones"].unique()
        
        for z in zones:
            
            final_export_table = pd.DataFrame(columns= column_export)
            
            value = ss[ss["Zones"] == z].reset_index(drop=True)[str(year)][0]
            
            final_export_table["Exportação Med (Kt)"] = round(exx[i]*value,3)
            final_export_table["Produto"] = "Milho"
            final_export_table["Mês"] = range(1,13)
            final_export_table["Ano"] = year
            final_export_table["UF"] = i
            final_export_table["Microrregião"] = z
            final_export_table["Exportação Mín (Kt)"] = 0
            final_export_table["Exportação Max (Kt)"] = 0
            final_export_table["Tipo"] = "Calculada"
            final_export_table["Tipo Ajustado"] = "Real/Calculada"
            final_export_table["%Confiança"] = 0
            
            list_.append(final_export_table)
            
for i in fitted.columns[2:]:
    
    ss = frame_ratio_corn[frame_ratio_corn["State"] == i].reset_index(drop=True)
    exx = fitted[fitted["Year"] == 2019].reset_index(drop=True)
    
    zones = ss["Zones"].unique()
    for z in zones:
        final_export_table = pd.DataFrame(columns= column_export)
        value = ss[ss["Zones"] == z].reset_index(drop=True)["2019"][0]
        final_export_table["Exportação Med (Kt)"] = round(exx[i]*value,3)
        final_export_table["Produto"] = "Milho"
        final_export_table["Mês"] = range(1,2)
        final_export_table["Ano"] = 2019
        final_export_table["UF"] = i
        final_export_table["Microrregião"] = z
        final_export_table["Exportação Mín (Kt)"] = 0
        final_export_table["Exportação Max (Kt)"] = 0
        final_export_table["Tipo"] = "Calculada"
        final_export_table["Tipo Ajustado"] = "Real/Calculada"
        final_export_table["%Confiança"] = 0
        
        list_.append(final_export_table)

frame_export_fitted_corn = pd.concat(list_, axis = 0, ignore_index = True)


final_export_corn = pd.concat([frame_export_actual_corn,frame_export_fitted_corn,frame_export_full_predicted], ignore_index=True)


state_name = pd.read_csv('../Input Files/states_names.csv',encoding='latin-1')
state_name.head(3)

state_names = []

for i in final_export_corn["UF"]:
    
    name = state_name[state_name["State"]== i].reset_index(drop=True)["Full_Name"][0]
    state_names.append(name)

final_export_corn.insert(3, "Estado", state_names)
final_export_corn.head(2)


final_export_soya = pd.read_csv('../Data Directory/Export_final_soya.csv',encoding='latin-1')

final_export_both_crop = final_export_soya.append(final_export_corn,ignore_index=True)

import datetime
current_date =datetime.datetime.today().strftime('%Y%m%d')


writer = pd.ExcelWriter(current_date+'_Exportação.xlsx', engine='xlsxwriter')

# Convert the dataframe to an XlsxWriter Excel object.
final_export_both_crop.to_excel(writer, sheet_name='Sheet1')

# Close the Pandas Excel writer and output the Excel file.
writer.save()

