
getwd()
setwd("/Users/veronicamandelli/Desktop/project_Texas")

# carico tutte le librerie
#install.packages("ggplot")
library(ggplot2)
#install.packages("magrittr")
library(magrittr)
#install.packages("dplyr")
library(dplyr)

### IMPORTO DATASET
dati <- read.csv("Real Estate Texas.csv", sep = ",", encoding = 'latin1')
attach(dati) 
dati


### INDICI
summary(dati)
num_osservazioni <- nrow(dati)

# City
table(city) #moda
ni = table(city) #freq assoluta
N = dim(dati)[1] #num righe e num colonne->prendo le righe
fi = table(city)/N # frequ relativa
Ni = cumsum(ni) #freq cumulata
Fi = Ni/N #fre relative cumulate
# visualizziamo combinazione frequenze complete in tabella
table_complete = cbind(ni, fi, Ni, Fi)

# Year
ni = table(year) #freq assoluta
N = dim(dati)[1] #num righe e num colonne->prendo le righe
fi = table(year)/N # freq relativa
Ni = cumsum(ni) #freq cumulata
Fi = Ni/N #freq relative cumulate
table_complete = cbind(ni, fi, Ni, Fi)
table_complete

# Month
ni = table(month) #freq assoluta 
N = dim(dati)[1] #num righe e num colonne->prendo le righe
fi = table(month)/N # freq relativa
Ni = cumsum(ni) #freq cumulata
Fi = Ni/N #freq relative cumulate
# visualizziamo combinazione frequenze complete in tabella
table_complete = cbind(ni, fi, Ni, Fi)
table_complete

# Sales
summary(sales)

indici_variabilità_sales <- data.frame(
  R = max(sales) - min(sales),  # range o intervallo di variazione
  diff_interquartile = IQR(sales), #diff tra 3° e 1° quartile
  varianza = var(sales),
  dev_standard = sd(sales), #deviazione standard
  coeff_variazione = sd(sales)/mean(sales) * 100
)
print(indici_variabilità_sales)

indici_forma_sales <- data.frame(
  asimmetria = skewness(sales),
  curtosi = kurtosis(sales) - 3
)
print(indici_forma_sales)

#Volume
summary(volume)

indici_variabilità_volume <- data.frame(
  R = max(volume) - min(volume),  # range o intervallo di variazione
  diff_interquartile = IQR(volume), #diff tra 3° e 1° quartile
  varianza = var(volume),
  dev_standard = sd(volume), #deviazione standard
  coeff_variazione = sd(volume)/mean(volume) * 100
)
print(indici_variabilità_volume)

indici_forma_volume <- data.frame(
  asimmetria = skewness(volume),
  curtosi = kurtosis(volume) - 3
)
print(indici_forma_volume)

#Median price
summary(median_price)

indici_variabilità_price <- data.frame(
  R = max(median_price) - min(median_price),  # range o intervallo di variazione
  diff_interquartile = IQR(median_price), #diff tra 3° e 1° quartile
  varianza = var(median_price),
  dev_standard = sd(median_price), #deviazione standard
  coeff_variazione = sd(median_price)/mean(median_price) * 100
)
print(indici_variabilità_price)

indici_forma_median_price <- data.frame(
  asimmetria = skewness(median_price),
  curtosi = kurtosis(median_price) - 3
)
print(indici_forma_median_price)

# Listings
summary(listings)

indici_variabilità_listings <- data.frame(
  R = max(listings) - min(listings),  # range o intervallo di variazione
  diff_interquartile = IQR(listings), #diff tra 3° e 1° quartile
  varianza = var(listings),
  dev_standard = sd(listings), #deviazione standard
  coeff_variazione = sd(listings)/mean(listings) * 100
)
print(indici_variabilità_listings)

indici_forma_listings <- data.frame(
  asimmetria = skewness(listings),
  curtosi = kurtosis(listings) - 3
)
print(indici_forma_listings)

# Months inventory
summary(months_inventory)

indici_variabilità_inventory <- data.frame(
  R = max(months_inventory) - min(months_inventory),  # range o intervallo di variazione
  diff_interquartile = IQR(months_inventory), #diff tra 3° e 1° quartile
  varianza = var(months_inventory),
  dev_standard = sd(months_inventory), #deviazione standard
  coeff_variazione = sd(months_inventory)/mean(months_inventory) * 100
)
print(indici_variabilità_inventory)

indici_forma_months_inventory <- data.frame(
  asimmetria = skewness(months_inventory),
  curtosi = kurtosis(months_inventory) - 3
)
print(indici_forma_months_inventory)


### SUDDIVISIONE IN CLASSI
min(sales)
max(sales)
class = seq(50, 450, by=50)
class_sales = cut(sales, breaks = class)

# distribuzione di frequenze
ni_class = table(class_sales) #freq assoluta
N = dim(dati)[1] #num righe e num colonne->prendo le righe
fi_class = table(class_sales)/N # frequ relativa
Ni_class = cumsum(ni_class) #freq cumulata
Fi_class = Ni_class/N #fre relative cumulate
table_complete = cbind(ni_class, fi_class, Ni_class, Fi_class)

# grafico a barre
distr_freq = as.data.frame(table_complete)

?barplot
barplot(ni_class, 
        main = "Distribuzione delle classi di lunghezza", 
        xlab = "classi di lunghezza", 
        ylab = "frequenze assolute",
        col = "orange")

# indice di Gini #fi uguali allora gini =1
gini.index <- function(x){
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.norm = gini/((J-1)/J)
  
  return(gini.norm)
}
table(class_sales)
gini.index(class_sales)


### PROBABILITA'
# Probabilità city "Beaumont"
num_osservazioni <- nrow(dati) 
num_city_beaumont <- nrow(subset(dati, city == "Beaumont")) 
p_city_Beaumont = num_city_beaumont/num_osservazioni
p_city_Beaumont

# Probabilità month "Luglio"
num_month_luglio <- nrow(subset(dati, month == "7"))
p_month_july = num_month_luglio/num_osservazioni
p_month_july

# probabilità mese "dicembre" e anno "2012"
num_dicembre_2012 <- nrow(subset(dati, month == "12" & year == 2012))
p_dicembre_2012 <- num_dicembre_2012/num_osservazioni
p_dicembre_2012

### NUOVA COLONNA
# nuova colonna "mean_price"
mutate(
  dati, 
  mean_price = volume / sales) %>% 
  select(volume, sales, mean_price) %>% head(5)

# nuova colonna  "efficenza di vendita" 
mutate(
  dati, 
  efficiency_listings = sales / listings)%>% 
  select(sales, listings, efficiency_listings) %>% head(5)


### SUMMARY
# summary by year
dati %>%
  group_by(year) %>%
  summarise(
    mean = mean(listings, na.rm = TRUE),
    sd = sd(listings, na.rm = TRUE),
    min = min(listings), 
    max = max(listings))

# summary by city, year and month
dati %>%
  group_by(city, year, month) %>%
  summarise(
    media_sales = mean(sales, na.rm = TRUE),  .groups = "rowwise")

#################### GRAFICI ########################
# boxplot (prezzo mediano tra le città)
ggplot(dati, aes(x = city, y = median_price)) +
  geom_boxplot(fill = "pink", ) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Distribuzione del prezzo mediano delle case tra le varie città",
       x = "Città",
       y = "Prezzo mediano")+
  theme_minimal()

# boxplot valore totale delle vendite tra le cità e i vari anni
ggplot(dati, aes(x = factor(year), y = volume, fill = city)) +
  geom_boxplot() +
  labs(title = "Distribuzione del Valore Totale delle Vendite per Città e Anno",
       x = "Città",
       y = "Valore Totale Vendite",
       fill = "Città") +
  theme_minimal() 

# grafico a barre sovrapposte
ggplot(dati, aes(x = factor(month), y = sales, fill = city)) +
  geom_bar(stat = "identity", position = "stack") + #stack per barre sovrapposte
  facet_wrap(~year, scales = "free", ncol = 2) +
  labs(title = "Vendite mensili per Città (2010-2014)",
       x = "Mese",
       y = "Totale Vendite",
       fill = "Città") +
  theme_minimal()


# grafico a barre normalizzato
ggplot(dati, aes(x = factor(month), y = sales, fill = city)) +
  geom_bar(stat = "identity", position = "fill") +  # "fill" per normalizzare le barre
  facet_wrap(~year, scales = "free", ncol = 2) +
  labs(title = "Vendite mensili normalizzate per Città (2010-2014)",
       x = "Mese",
       y = "Percentuale di Vendite",
       fill = "Città") 


# grafico line chart
year_month <- as.Date(paste(dati$year, dati$month, "01", sep = "-"))
length(year_month)
length(dati$sales)

ggplot(dati, aes(x = year_month, y = sales, group=city, color=city)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5, shape = 16) +  # Aggiungi i punti
  labs(title = "Andamento delle vendite nel tempo",
       x = "Date",
       y = "Sales") +
  scale_x_date(labels = scales::date_format("%d %B %y"), date_breaks = "1 year", date_minor_breaks = "1 month", limits = as.Date(c("2010-01-01", "2014-12-31")) ) +
  theme_linedraw()+
  theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1, size = 8))



