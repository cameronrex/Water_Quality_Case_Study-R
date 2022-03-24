# Water_Quality_Case_Study-R
Austin TX Water Quality Case Study in R


### Load in the libraries that we'll need
library(tidyverse)
library(stringr)
library(lubridate)

### Read in the dataset
water <- read_csv('http://594442.youcanlearnit.net/austinwater.csv')

### Let's take a look at what we have
glimpse(water)

### First, let's get rid of a lot of columns that we don't need
### I'm going to do that by building a new tibble with just
### siteName, siteType, parameter, result and unit

water <- tibble('siteName'=water$SITE_NAME,
                'siteType'=water$SITE_TYPE,
                'sampleTime'=water$SAMPLE_DATE,
                'parameterType'=water$PARAM_TYPE,
                'parameter'=water$PARAMETER,
                'result'=water$RESULT,
                'unit'=water$UNIT)

glimpse(water)

### Now let's start finding the rows that we need.
### First, we need pH.  I might start by trying to look at all unique parameter names

unique(water$parameter)

### but that's way too long... what if we try searching for names that contain PH?

unique(water[which(str_detect(water$parameter,'PH')),]$parameter)

### still a mess... let's backtrack and look at parameter types

unique(water$parameterType)

### OK, what if I filter this down to look only at parameter types of Alkalinity/Hardness/pH
### and Conventionals

filtered_water <- subset(water,(parameterType=='Alkalinity/Hardness/pH') |
                                  parameterType=='Conventionals')

### Notice that this is much smaller in size.  let's check what parameters we have now

unique(filtered_water$parameter)

### I want only two of these, (PH and water temp), so let's filter those

filtered_water <- subset(filtered_water, ((parameter=='PH') |
                                            (parameter=='WATER TEMPERATURE')))

glimpse(filtered_water)

### Let's take a look at the data a different way
summary(filtered_water)

### It would be helpful to convert some of these to factors
filtered_water$siteType <- as.factor(filtered_water$siteType)
filtered_water$parameterType <- as.factor(filtered_water$parameterType)
filtered_water$parameter <- as.factor(filtered_water$parameter)
filtered_water$unit <- as.factor(filtered_water$unit)

summary(filtered_water)

### And sampleTime should be a date/time object
filtered_water$sampleTime <- mdy_hms(filtered_water$sampleTime)

summary(filtered_water)

### Why are some of these measurements in feet?
subset(filtered_water,unit=='Feet')

### Looks like that is supposed to be Farenheit. Convert to that.
convert <- which(filtered_water$unit=='Feet')
filtered_water$unit[convert] <- 'Deg. Fahrenheit'
### Check it got converted
summary(filtered_water)

### What about the MG/L?
subset(filtered_water,unit=='MG/L')
subset(filtered_water,unit=='MG/L' & parameter=='PH')

convert <- which(filtered_water$unit=='MG/L' & filtered_water$parameter=='PH')
filtered_water$unit[convert] <- 'Standard units'

subset(filtered_water,unit=='MG/L')
subset(filtered_water,unit=='MG/L' & filtered_water$result>70)
convert <- which(filtered_water$unit=='MG/L' & filtered_water$result>70)
filtered_water$unit[convert] <- 'Deg. Fahrenheit'

subset(filtered_water,unit=='MG/L')
convert <- which(filtered_water$unit=='MG/L')
filtered_water$unit[convert] <- 'Deg. Celsius'

summary(filtered_water)

ggplot(filtered_water, mapping=aes(x=sampleTime, y=result)) +
  geom_point()

glimpse(subset(filtered_water, result>1000000))

remove <- which(filtered_water$result>1000000 |
                  is.na(filtered_water$result))

filtered_water <- filtered_water[-remove, ]
summary(filtered_water)

glimpse(subset(filtered_water, result>1000))

remove <- which(filtered_water$result>1000)
filtered_water <- filtered_water[-remove, ]

summary(filtered_water)

ggplot(data=filtered_water, mapping=aes(x=unit, y=result)) +
  geom_boxplot()

convert <- which(filtered_water$result>60 &
                   filtered_water$unit=='Deg. Celsius')

filtered_water$unit[convert] <- 'Deg. Fahrenheit'

ggplot(data=filtered_water, mapping=aes(x=unit, y=result)) +
  geom_boxplot()



