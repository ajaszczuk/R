library(ggplot2)
library(dplyr)
library(eurostat)

#####Downloading data and generating dataframe
df = get_eurostat(
  id = 'prc_hicp_manr'
)

#Filtering UK and choosing classifaction of goods = All-items 
bezUK = df %>% filter(geo != 'UK', coicop == 'CP00')


#Setting time frame of data
df = bezUK %>% filter (time >= '2000-01-01' & time <= '2022-09-01')


#########Creating line plot

#Customizing name for plot
df = df %>% rename(country = geo)
df

#Line chart
x11()
ggplot(data=df, aes(x=time, y=values, color = country)) +
   geom_line(linewidth = 0.75)


######### Dendrogram

xtab = xtabs(values~country + time, df)
odl = dist(xtab, method = 'minkowski', p = 1.5)
hc = hclust(odl, method = 'complete')
x11()
plot(hc, main = 'Clustering countries based on HICP', sub = '', ylab = NULL, xlab='')
dendrogram = rect.hclust(hc, k=4, border = 'red')
