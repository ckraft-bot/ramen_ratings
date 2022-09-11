# laod packages
library(ggplot2)
library(readr)

# import dataset
#ramen_data <- read.csv("C:\\Users\\ckraft\\Downloads\\Ramen_Ratings.csv")
Ramen_Ratings <- read_csv("Ramen_Ratings.csv")
View(Ramen_Ratings)

# preview table
head(Ramen_Ratings, 10)

# quick facts
summary(Ramen_Ratings)

# -------------- Data Cleaning
# missing values
is.na(Ramen_Ratings) <- Ramen_Ratings == ''

# removing missing values
Ramen_Ratings <- Ramen_Ratings[!(is.na(Ramen_Ratings$Style) | Ramen_Ratings$Style==''),]

# checking for dirty data in the 'Stars' col
Ramen_Ratings[grep('a', Ramen_Ratings$Stars), ]
# deleting dirty data rows (3) in the 'Stars' col
Ramen_Ratings <- subset(Ramen_Ratings, Stars != 'Unrated')
# converting col dt to numeric from char
Ramen_Ratings$Stars <- as.numeric(as.character(Ramen_Ratings$Stars))
# standardizing the country values, cities and country abbreviations are renamed 
Ramen_Ratings['Country'][Ramen_Ratings['Country'] == 'USA'] <- 'United States'
Ramen_Ratings['Country'][Ramen_Ratings['Country'] == 'UK'] <- 'United Kingdom'
Ramen_Ratings['Country'][Ramen_Ratings['Country'] == 'Sarawak'] <- 'Malaysia'
Ramen_Ratings['Country'][Ramen_Ratings['Country'] == 'Holland'] <- 'Netherlands'
Ramen_Ratings['Country'][Ramen_Ratings['Country'] == 'Dubai'] <- 'United Arab Emirates'

### grouping countries into continents
# Oceania
Ramen_Ratings$Continent[Ramen_Ratings$Country == 'Australia' | Ramen_Ratings$Country=='Fiji'] <- 'Oceania'

# Asia
Ramen_Ratings$Continent[Ramen_Ratings$Country == 'Bangladesh' | Ramen_Ratings$Country == 'Cambodia' 
                        | Ramen_Ratings$Country == 'China' | Ramen_Ratings$Country == 'United Arab Emirates' | Ramen_Ratings$Country == 'Hong Kong' 
                        | Ramen_Ratings$Country == 'India' | Ramen_Ratings$Country == 'Indonesia' | Ramen_Ratings$Country == 'Japan' 
                        | Ramen_Ratings$Country == 'Malaysia' | Ramen_Ratings$Country == 'Myanmar' | Ramen_Ratings$Country == 'Nepal'
                        | Ramen_Ratings$Country == 'Pakistan' | Ramen_Ratings$Country == 'Philippines' | Ramen_Ratings$Country == 'Singapore' 
                        | Ramen_Ratings$Country == 'South Korea' | Ramen_Ratings$Country == 'Taiwan' | Ramen_Ratings$Country == 'Thailand' 
                        | Ramen_Ratings$Country == 'Vietnam'] <- 'Asia'

# South America
Ramen_Ratings$Continent[Ramen_Ratings$Country == 'Brazil' | Ramen_Ratings$Country == 'Colombia'] <- 'South America'

# North America
Ramen_Ratings$Continent[Ramen_Ratings$Country == 'Canada' | Ramen_Ratings$Country == 'Mexico' | Ramen_Ratings$Country == 'United States'] <- 'North America'

# Africa
Ramen_Ratings$Continent[Ramen_Ratings$Country == 'Ghana' | Ramen_Ratings$Country == 'Nigeria'] <- 'Africa'

# Europe
Ramen_Ratings$Continent[Ramen_Ratings$Country == 'Estonia' | Ramen_Ratings$Country == 'Finland' 
                        | Ramen_Ratings$Country == 'Germany' | Ramen_Ratings$Country == 'Hungary' | Ramen_Ratings$Country == 'Netherlands' 
                        | Ramen_Ratings$Country == 'Poland' | Ramen_Ratings$Country == 'Sweden' | Ramen_Ratings$Country == 'United Kingdom'] <- 'Europe'

### grouping meats
chicken <- grepl(pattern = 'Chicken|Ayam', Ramen_Ratings$Variety)
beef <- grepl(pattern = 'Beef|Cow', Ramen_Ratings$Variety)
duck <- grepl('Duck', Ramen_Ratings$Variety)
pork <- grepl('Pork', Ramen_Ratings$Variety)
seafood <- grepl(pattern = 'Shrimp|Squid|Scallop|Prawn|Seafood|Crab|Fish', Ramen_Ratings$Variety)
other <- grepl('Meatball', Ramen_Ratings$Variety)
# if meat is blank then fill NA
Ramen_Ratings$Meat_Type <- NA
# rename meats
Ramen_Ratings$Meat_Type[chicken] <- 'Chicken'
Ramen_Ratings$Meat_Type[beef] <- 'Beef'
Ramen_Ratings$Meat_Type[duck] <- 'Duck'
Ramen_Ratings$Meat_Type[pork] <- 'Pork'
Ramen_Ratings$Meat_Type[seafood] <- 'Seafood'
Ramen_Ratings$Meat_Type[other] <- 'Other'



### grouping spicy
spicy <- grepl(pattern = 'Spicy|Hot|Flamin|Chili|Chilli|Sambal|Pedas', Ramen_Ratings$Variety)
# binary coding spicy Y/N
Ramen_Ratings$Spicy <- 'No'
Ramen_Ratings$Spicy[spicy] <- 'Yes'



### grouping noodle types
udon <- grepl('Udon', Ramen_Ratings$Variety)
soba <- grepl('Soba', Ramen_Ratings$Variety)
wonton <- grepl(pattern = 'Wantan|Wonton', Ramen_Ratings$Variety)
shirataki <- grepl('Shirataki', Ramen_Ratings$Variety)
hokkien <- grepl('Hokkien', Ramen_Ratings$Variety)
noodle <- grepl('Noodle|mian', Ramen_Ratings$Variety)
mi <- grepl(pattern = 'Mi|Mee', Ramen_Ratings$Variety)
rice_noodle <- grepl(pattern = 'Rice|Bihun', Ramen_Ratings$Variety)
ramyeon <- grepl(pattern = 'Ramyun|Ramyeon|Ramyon|Ramen', Ramen_Ratings$Variety)
# if noodle is blank then fill NA
Ramen_Ratings$Noodle_Type <- NA
# rename noodles
Ramen_Ratings$Noodle_Type[udon] <- 'Udon'
Ramen_Ratings$Noodle_Type[soba] <- 'Soba'
Ramen_Ratings$Noodle_Type[wonton] <- 'Wonton'
Ramen_Ratings$Noodle_Type[shirataki] <- 'Shirataki'
Ramen_Ratings$Noodle_Type[hokkien] <- 'Hokkien'
Ramen_Ratings$Noodle_Type[noodle] <- 'Noodle'
Ramen_Ratings$Noodle_Type[mi] <- 'Mi'
Ramen_Ratings$Noodle_Type[rice_noodle] <- 'Rice Noodle'
Ramen_Ratings$Noodle_Type[ramyeon] <- 'Ramyeon'

# preview new table
head(Ramen_Ratings, 10)

# export new table
#write.csv(Ramen_Ratings, 'Ramen2.csv', row.names = FALSE)
View(Ramen_Ratings)

# -------------- Data Visualizing
# distribution of stars given by continent
ggplot(data = Ramen_Ratings)+
  geom_point(mapping = aes(x=Stars, y=Continent, color=Stars))
# Conclusion: African Ramen are not well liked. So don't waste time buying those kinds of ramen.

# popular noodle style
ggplot(data = Ramen_Ratings)+geom_bar(mapping = aes(y=Stars, color=Noodle_Type, fill=Noodle_Type))
# Diversification of ramen with noodle types at the shop will be good. But be sure to have more of these noodle types: Mi, noodle, and Ramyeon.

# ramen packaging
ggplot(data = Ramen_Ratings)+
  geom_bar(mapping=aes(x=Style,fill=Style))
# Looks like ramen packs are most common. This can impact they way ramen are shelved at teh market.

# meat popularity
ggplot(data = Ramen_Ratings)+
  geom_point(mapping = aes(x=Spicy, y=Stars, color=Meat_Type))
# Conclusion: For non spicy ramen Chicken, pork, and seafood are the popular proteins; for spicy ramen beef and seafood are the favored proteins.

