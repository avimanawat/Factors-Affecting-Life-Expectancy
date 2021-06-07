# -*- coding: utf-8 -*-
"""
Created on Sun Dec  8 15:45:47 2019

@author: Puneet Kochar
"""

import pandas as pd
life=pd.read_csv('life expectancy.csv')
life

life[life['GDP'].isnull()].index

life['GDP'][2035]

import re
import urllib3

url='https://www.google.com/search?q=senegal+gdp+of+2003&rlz=1C1CHBF_enUS863US863&oq=senegal+gdp+of+2003&aqs=chrome..69i57j33l3.7662j1j7&sourceid=chrome&ie=UTF-8'
http=urllib3.PoolManager()
response=http.request('GET',url)
webcontent=str(response.data)

position=webcontent.find('Gross domestic product (2003)')

interest=webcontent[position:position+1000]

res=re.findall('\d\.\d+',interest)

value=float(res[0])*1000000000

life['GDP'][2035]=value
