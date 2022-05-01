import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import matplotlib.pyplot as plt # for plotting the data 
import seaborn as sns # Advanced data plotting on top of matplotlib
import os
from pathlib import Path
import datatable as dt
import plotly.express as px

%matplotlib inline
from plotly.offline import init_notebook_mode, iplot 
import plotly.graph_objs as go
import plotly.offline as py
py.init_notebook_mode(connected=True)
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator, ImageColorGenerator
%matplotlib inline

f_area = pd.read_csv("../input/global-environmental-indicators/Forests/Forest Area.csv")

f_area = f_area.drop(f_area.index[:1])

f_area["%change"] = ((f_area["Forest Area, 2020 (1000 ha)"] - f_area["Forest Area, 1990 (1000 ha)"])/ f_area["Forest Area, 1990 (1000 ha)"])*100

plt.figure(figsize = (20,20))
fig = go.Figure(data=go.Choropleth(
    locations=f_area['Country and Area'], # Spatial coordinates
    z = f_area['%change'].astype(float), # Data to be color-coded
    locationmode = 'country names', # set of locations match entries in `locations`
    colorscale = 'RdYlGn',
    colorbar_title = "%change",
    reversescale=True
))
title = '<b>Forest Area % Change</b><br><sup>1990 vs 2020</sup>'

fig.update_layout(
    template="plotly_white",
    title = {'text' : title, 
                            'x':0.5, 'xanchor': 'center'}, 
                   font = {"color" : 'black'}
)

fig.show()
