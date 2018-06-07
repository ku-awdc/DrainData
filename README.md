# DrainData

To install the software:

install.packages(c('devtools','tidyverse','readxl','rlang'))
devtools::install_github('mdenwood/DrainData')
library('DrainData')

To import 2 files in the current working directory:

dd <- DrainData(c('2.xlsx','3.xlsx'))

Or to import all files in a given folder:

dd <- DrainData('Data')

To extract the data and plot:

df <- dd$GetData()

str(df)
summary(df)

ggplot(df, aes(x=DateTime, y=AdjustedFluid, col=Identifier)) +
	geom_line() +
	geom_point()
	