#!/usr/bin/env python
# coding: utf-8

# In[20]:


import pandas as pd
from datetime import datetime
import numpy as np


# In[21]:


df1 = pd.read_csv('./data/CLIENT_191102.tsv', sep = '\t')
df_case = df1.drop(columns = ['EE Provider ID', 'Client Unique ID'])  #EE UID is unique for each case
df_clients = pd.DataFrame(df_case.groupby('Client ID').size()).rename(columns = {0: 'frequency'}) #some client came for more than once
#2364 unique clients
df = df_case.drop(columns = ['EE UID', 'Client Age at Entry', 'Client Age at Exit']).drop_duplicates()
df_clients = pd.merge(df_clients, df, on = 'Client ID', how = 'left')


# In[22]:


df2 = pd.read_csv('./data/DISABILITY_ENTRY_191102.tsv', sep = '\t')
df3 = pd.read_csv('./data/DISABILITY_EXIT_191102.tsv', sep = '\t')

df_diab_entry = df2.rename(columns = {'Disability Determination (Entry)': 'disability_determination_entry', 'Disability Type (Entry)': 'disability_type'})
df_diab_entry = df_diab_entry[df_diab_entry.disability_determination_entry == 'Yes (HUD)'] #filter clients with disability when entry

df_diab_exit = df3.rename(columns = {'Disability Determination (Exit)': 'disability_determination_exit', 'Disability Type (Exit)': 'disability_type'})
df_diab_exit = df_diab_exit[df_diab_exit.disability_determination_exit == 'Yes (HUD)'] #filter clients with disability when exit

df_disable = pd.merge(df_diab_exit,  df_diab_entry, on = ['Client ID', 'disability_type'], how = 'left') #join two dataframe
df_disable = df_disable[['Client ID', 'disability_determination_exit', 'disability_type', 'disability_determination_entry']]
df_disable[df_disable.disability_determination_exit != df_disable.disability_determination_entry] #find out the difference between entry and exit
#data show no information in the change of disability


# In[23]:


df_disable_indicator = pd.DataFrame(df_diab_exit.groupby('Client ID').size()).rename(columns = {0: 'disability_size'})
df_clients = pd.merge(df_clients, df_disable_indicator, on = 'Client ID', how = 'left')
df_clients = df_clients.fillna(0)
df_clients['disability'] = round(df_clients['disability_size']/df_clients['frequency'])
df_clients = df_clients.drop(columns = ['disability_size'])


# In[24]:


df_entry_exit = pd.read_csv('./data/ENTRY_EXIT_191102.tsv', sep = '\t')
df_entry_exit = df_entry_exit[['EE UID', 'Client ID', 'Entry Date', 'Exit Date', 'Destination', 'Reason for Leaving']]
df_entry_exit['Entry Date'] = pd.to_datetime(df_entry_exit['Entry Date'], format = '%m/%d/%Y')
df_entry_exit['Exit Date'] = pd.to_datetime(df_entry_exit['Exit Date'], format = '%m/%d/%Y')
df_entry_exit['Exit Date'] = df_entry_exit['Exit Date'].replace(np.datetime64('NaT'), datetime.now())
df_entry_exit['Period'] = (df_entry_exit['Exit Date'] - df_entry_exit['Entry Date']).dt.days


# In[25]:


df = df_entry_exit.groupby('Client ID').mean().drop(columns = ['EE UID'])
df_clients = pd.merge(df_clients, df, on = 'Client ID', how = 'left')


# In[26]:


df_entry_exit['Entry_Year'] = df_entry_exit['Entry Date'].map(lambda x: x.year)
df_entry_exit['Entry_Month'] = df_entry_exit['Entry Date'].map(lambda x: x.month)
df_entry_exit['Day'] = df_entry_exit['Entry Date'].map(lambda x: x.day)
df_entry_exit['Exit_Year'] = df_entry_exit['Exit Date'].map(lambda x: x.year)
df_entry_exit['Exit_Month'] = df_entry_exit['Exit Date'].map(lambda x: x.month)
df_entry_exit['Exit_Day'] = df_entry_exit['Exit Date'].map(lambda x: x.day)


# In[27]:


df_nclient = pd.DataFrame(df_entry_exit.groupby('Entry_Year', as_index=False).size()).rename(columns = {0: 'number_cases'})
df_year = df_entry_exit[['Client ID', 'Entry_Year']]
df_ncase = df_year.groupby(['Entry_Year'], as_index=False).agg({"Client ID": pd.Series.nunique}).rename(columns = {'Client ID': 'number_clients'})
df_n = pd.merge(df_nclient, df_ncase, on = ['Entry_Year'], how = 'left')
df_n = df_n[df_n['Entry_Year'] > 2012]


# In[28]:


df6 = pd.read_csv('./data/HEALTH_INS_ENTRY_191102.tsv', sep = '\t')
df7 = pd.read_csv('./data/HEALTH_INS_EXIT_191102.tsv', sep = '\t')
l = [] 
for Ins_type, sub_df in df6.groupby('Health Insurance Type (Entry)'):
    l.append(Ins_type)
print(l)

ls = []
for Ins_type, sub_df in df7.groupby('Health Insurance Type (Exit)'):
    ls.append(Ins_type)
print(ls)
print(l == ls)
#type of health insurance are same


# In[29]:


df6s = df6[df6['Covered (Entry)'] == 'Yes'].rename(columns = {'Health Insurance Type (Entry)': 'health_insurance_type'})
df6s = df6s[['Client ID', 'Covered (Entry)', 'health_insurance_type']]
df7s = df7[df7['Covered (Exit)'] == 'Yes'].rename(columns = {'Health Insurance Type (Exit)': 'health_insurance_type'})
df7s = df7s[['Client ID', 'Covered (Exit)', 'health_insurance_type']]
df_healthins = pd.merge(df7s, df6s, on = ['Client ID', 'health_insurance_type'], how = 'inner').drop_duplicates()
df_healthins[df_healthins['Covered (Exit)'] != df_healthins['Covered (Entry)']]
#no difference in health insurance covered when entry and exit


# In[30]:


df_healthins_types = pd.DataFrame(df_healthins.groupby('Client ID').size()).rename(columns = {0: 'healthins_types'})
df_clients = pd.merge(df_clients, df_healthins_types, on = ['Client ID'], how = 'left')  #add health insurance


# In[31]:


df8 = pd.read_csv('./data/INCOME_ENTRY_191102.tsv', sep = '\t')
df8 = df8[['Client ID', 'Receiving Income (Entry)', 'Income Source (Entry)', 'Monthly Amount (Entry)']]
df_income_entry = pd.DataFrame(df8.groupby('Client ID', as_index=False).mean())
df9 = pd.read_csv('./data/INCOME_EXIT_191102.tsv', sep = '\t')
df9 = df9[['Client ID', 'ReceivingIncome (Exit)', 'Source of Income (Exit)', 'Monthly Amount (Exit)']]
df_income_exit = pd.DataFrame(df9.groupby('Client ID', as_index=False).mean())


# In[32]:


df_income = pd.merge(df_income_exit, df_income_entry, on = ['Client ID'], how = 'left')
df_income = df_income.dropna(thresh = 2)
df_income.fillna(0, inplace = True)
df_income['changes'] = df_income['Monthly Amount (Exit)'] - df_income['Monthly Amount (Entry)']
df_clients = pd.merge(df_clients, df_income, on = ['Client ID'], how = 'left').fillna(0) #add income


# In[33]:


df5 = pd.read_csv('./data/EE_UDES_191102.tsv', sep = '\t')
df_udess = df5[['EE UID', 'Client ID', 'Prior Living Situation(43)', 
               'Total number of months homeless on the street, in ES or SH in the past three years(5168)', 
              'Housing Status(2703)', 'Domestic violence victim/survivor(341)', 'Does the client have a disabling condition?(1935)',
               'Covered by Health Insurance(4376)']]
df_udess = df_udess.rename(columns = {'Prior Living Situation(43)': 'prior_living_situation', 
               'Total number of months homeless on the street, in ES or SH in the past three years(5168)': 'months_on_street', 
              'Housing Status(2703)': 'housing_status', 'Domestic violence victim/survivor(341)': 'domestic_violence', 
                                      'Does the client have a disabling condition?(1935)': 'disabiltiy', 'Covered by Health Insurance(4376)': 'health_insurance'})
df_udess['months_on_street'] = df_udess['months_on_street'].replace(["Client doesn't know (HUD)",
                                                                     'Client refused (HUD)', "Data not collected (HUD)"],np.nan)
df_udess['prior_living_situation'] = df_udess['prior_living_situation'].replace('Data not collected (HUD)', np.nan).replace('Other (HUD)', np.nan)


# In[34]:


df_case = pd.merge(df_case, df_udess, on = ['EE UID', 'Client ID'], how = 'left').merge(df_entry_exit, on = ['EE UID', 'Client ID'], how = 'left')
df_case = pd.merge(df_case, df_diab_exit, on = ['EE UID', 'Client ID'], how = 'left')
df_case = df_case.replace('More than 12 months (HUD)', '> 12').replace('One month (this time is the first month) (HUD)', '<1')


# In[35]:


df_clients.to_csv('./CLIENTS.csv')
df_case.to_csv('./CASE.csv')
df_n.to_csv('./number.csv')

