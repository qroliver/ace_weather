#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  9 14:08:48 2024

@author: oliver
"""

# importing packages

import requests
import pandas as pd
import datetime as dt
from dateutil.relativedelta import relativedelta
import time
import random


# api key
api_key = '' # request on https://opendata.aemet.es/centrodedescargas/inicio


# url
base_url = 'https://opendata.aemet.es/opendata/api'
endpoint = 'valores/climatologicos/diarios/datos'


# station code for Lanzarote airport
station_code = 'C029O'


# getting data
data = pd.DataFrame()


# start date
start = pd.to_datetime("1970-01-01")
#start_date = start.strftime("%Y-%m-%d") + "T00:00:00UTC"

# end date (6 months period)
end = start + relativedelta(months=6, days=-1)
#end_date = end.strftime("%Y-%m-%d") + "T23:59:59UTC"

# today's date
today = pd.to_datetime(dt.date.today())


while end < today:
    
    start_date = start.strftime("%Y-%m-%d") + "T00:00:00UTC"
    end_date = end.strftime("%Y-%m-%d") + "T23:59:59UTC"
    
    print(f"Period: {start_date} - {end_date}")

    # api url for the request
    api_url = f"{base_url}/{endpoint}/fechaini/{start_date}/fechafin/{end_date}/estacion/{station_code}?api_key={api_key}"

    # get request
    response = requests.get(api_url)

    # checking if there is data
    if response.json()['estado'] == 200:
    
        # extracting url for the data from the response
        data_url = response.json()['datos']
    
        # requesting data
        data_response = requests.get(data_url)
    
        # extracting data 
        data_content = data_response.json()
        
        # creating a pandas df and binding it with previous
        period_data = pd.DataFrame(data_content)
        
        data = pd.concat([data, period_data], ignore_index = True)
    
    else:
        print(f"Estado: {response.json()['estado']}")
        print(response.json()['descripcion'])
        
    # calculating new start and end dates
    start = end + relativedelta(days=1)
    end = start + relativedelta(months= 6, days = -1)
    
    
    time.sleep(1 + random.uniform(0, 5))


# getting data until today (last period)
start_date = start.strftime("%Y-%m-%d") + "T00:00:00UTC"
end = today + relativedelta(days=-1)
end_date = end.strftime("%Y-%m-%d") + "T23:59:59UTC"


print(f"Period: {start_date} - {end_date}")

# api url for the request
api_url = f"{base_url}/{endpoint}/fechaini/{start_date}/fechafin/{end_date}/estacion/{station_code}?api_key={api_key}"

# get request
response = requests.get(api_url)

# extracting url for the data from the response
data_url = response.json()['datos']

# requesting data
data_response = requests.get(data_url)

# extracting data 
data_content = data_response.json()

# creating a pandas df and binding it with previous
period_data = pd.DataFrame(data_content)

data = pd.concat([data, period_data], ignore_index = True)


# saving as csv
data.to_csv("data/aemet_ace_data_raw.csv", index=False)































































