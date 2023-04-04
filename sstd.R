################################### TRACKING SUSTAINABLE DEVELOPMENT GOALS (SDGs) IN BRAZIL ################################### 
################################### ACOMPANHANDO OS OBJETIVOS DE DESENVOLVIMENTO SUSTENTÁVEL (ODS) NO BRASIL ###################################

#GOAL: Calculate the urban population for the years 2001 and 2021 in municipalities in the Mato Grosso (MT) state in Brazil (BR)
#Obtain, clean, and standardize Census data from official and reliable data sources from Brazilian institutions.
#Use the projected populations to calculate the SDG 11.3.1: Ratio of land consumption rate to population growth rate

#OBJETIVO: Calcular a população urbana para os anos de 2001 e 2021 nos municípios do estado de Mato Grosso (MT) no Brasil (BR)
#Obter, limpar e padronizar dados do Censo de fontes de dados oficiais e confiáveis de instituições brasileiras.
#Usar as populações projetadas para calcular o ODS 11.3.1: Razão entre a taxa de consumo da terra e a taxa de crescimento populacional

##WARNING: do not run the code once. Go until #Google Drive Authentication step# first, authenticate your Google Accont and then, proceed.
##Installing packages 
install.packages('sidrar') #Working with Census Data from Brazil official database
install.packages("dplyr")
install.packages("googledrive") #To further work with census data on Google Earth Engine (GEE), login to you Google account is needed

#Installing packages and libraries
library(sidrar)
library(dplyr)
library(googledrive)

#Google Drive Authentication
drive_auth()

#==========================================OBTAINING DATA========================================#
#========================================OBTENÇÃO DE DADOS ====================================#

#Getting data from SIDRA System: Resident Population (urban, rural)
#Acssess to the original table website: https://sidra.ibge.gov.br/tabela/200
#This table below is exclusively to Mato Grosso state.
x <- get_sidra(api = '/t/200/n3/51/n6/5103007,5103502,5105309,5105903,5106802,5102504,5105507,5100508,5101308,5106000,5100102,5103403,5106109,5106505,5107701,5108402,5100201,5102702,5102850,5105101,5105150,5106307,5107305,5107776,5107859,5107909,5108501,5101258,5105002,5105622,5106752,5107107,5107206,5107750,5103452,5107958,5104906,5106778,5108600,5101902,5102637,5103304,5105259,5106224,5107925,5108006,5102603,5103106,5107180,5103809,5104500,5106232,5106828,5107156,5102686,5103700,5103858,5106182,5106240,5107875,5100359,5102694,5103353,5103957,5105234,5106851,5107065,5107248,5107263,5107354,5107941,5108857,5108907,5107743,5107768,5107883,5108352,5101852,5103361,5103437,5106174,5106315,5104526,5104542/v/allxp/p/last%203/c2/0/c1/all/c58/0/d/v93%200')

#==========================================CLEANING DATA========================================#
#========================================LIMPEZA DE DADOS ====================================#

#Check for missing or NaN values in the dataframe
missing_summary <- x %>%
  summarise(
    missing_values = sum(is.na(Valor)),
    nan_values = sum(is.nan(Valor))
  )
print(missing_summary)

#Remove rows with NA or NaN values in the 'Valor' column
x <- x %>%
  filter(!is.na(Valor) & !is.nan(Valor))

#See the first 6 rows of the table
#head(x)

#Open the table in a separate window
#View(x)

#Visualizing column names
#print(colnames(x))

#remove columns
x <- x[, -c(13, 12, 17, 16)]

#Create new dataframe
df <- as.data.frame(x)

#Print dataframe
#print(df)

# Open the table in a separate window
#print(colnames(df))

#==========================================ORGANIZING THE DATA========================================#
#========================================ORGANIZANDO OS DADOS ====================================#

#Reorganizing columns

df <- df %>%
  select(`Nível Territorial (Código)`, `Nível Territorial`,
         `Unidade de Medida (Código)`, `Unidade da Federação e Município (Código)`,
         `Unidade da Federação e Município`, `Variável (Código)`, `Variável`,
         `Situação do domicílio (Código)`, `Ano (Código)`, `Ano`,
         `Situação do domicílio`, Valor, `Unidade de Medida`)

#Open the table in a separate window
#View(df)

# Rename columns
df <- df %>% 
  rename(
    territorial_level_code = `Nível Territorial (Código)`,
    territorial_level = `Nível Territorial`,
    measurement_unit_code = `Unidade de Medida (Código)`,
    geocode = `Unidade da Federação e Município (Código)`,
    municipality = `Unidade da Federação e Município`,
    variable_code = `Variável (Código)`,
    variable = `Variável`,
    household_situation_code = `Situação do domicílio (Código)`,
    year_code = `Ano (Código)`,
    year = `Ano`,
    household_situation = `Situação do domicílio`,
    Population_count = `Valor`,
    measurement_unit = `Unidade de Medida`
  )

# View renamed columns
#names(df)
#View(df)

#==========================================CLEANING DATA========================================#
#========================================LIMPEZA DE DADOS ====================================#

#Selecting data of municipalities that have data for all the three years: 1991, 2000 and 2010
df <- df %>%
  group_by(municipality) %>%
  filter(n_distinct(year) == 3)

# Filter the data frame to only include rows where household_situation equals "Urbana"
df_urban <- subset(df, household_situation == 'Urbana')


#==========================================POPLATION PROJECTIONS RATES========================================#
#========================================TAXAS DE PROJEÇÕES POPULACIONAIS====================================#

#Calculating Linear Population Growth Rates for each municipality
#Linear Population Growth Rates for projecting 2001 and 2021 (using 1991-2000 rate, and 2000-2010 rates, respectvely)

#Formula Linear Population Growth Rate:
#

df_urban <- df_urban %>%
  group_by(municipality) %>%
  mutate(population_growth_rate_1991_2000 = (Population_count[year == 2000] - Population_count[year == 1991]) / (2000 - 1991))

df_urban <- df_urban %>%
  group_by(municipality) %>%
  mutate(population_growth_rate_2000_2010 = (Population_count[year == 2010] - Population_count[year == 2000]) / (2010 - 2000))

#Convert "population_growth_rate_1991_2000" column to numeric
df_urban$population_growth_rate_1991_2000 <- as.numeric(df_urban$population_growth_rate_1991_2000)
# Reduce decimal values in "population_growth_rate_1991_2000" column to 1
df_urban$population_growth_rate_1991_2000 <- round(df_urban$population_growth_rate_1991_2000, 1) 

#Convert "population_growth_rate_2000_2010" column to numeric
df_urban$population_growth_rate_2000_2010 <- as.numeric(df_urban$population_growth_rate_2000_2010)
# Reduce decimal values in "population_growth_rate_2000_2010" column to 1
df_urban$population_growth_rate_2000_2010 <- round(df_urban$population_growth_rate_2000_2010, 1)
#View(df_urban)

#Create a vector of unique municipalities
municipalities <- unique(df_urban$municipality)

#Create a new dataframe with one row 2001 for each municipality
new_rows <- data.frame(municipality = municipalities,
                       territorial_level_code = df_urban$territorial_level_code[1],
                       territorial_level = rep("Município", length(municipalities)),
                       measurement_unit_code = df_urban$measurement_unit_code[1],
                       geocode = df_urban$geocode[1],
                       variable_code = df_urban$variable_code[1],
                       variable = df_urban$variable[1],
                       household_situation_code = df_urban$household_situation_code[1],
                       household_situation = df_urban$household_situation[1],
                       measurement_unit = rep("Pessoas", length(municipalities)),
                       year_code = rep("2001", length(municipalities)),
                       year = rep("2001", length(municipalities)),
                       Population_count = rep(NA, length(municipalities)))

#Bind the new rows to the original dataframe
df_urban <- bind_rows(df_urban, new_rows) %>%
  arrange(municipality)

#Create a new dataframe with one row 2011 for each municipality
new_rows <- data.frame(municipality = municipalities,
                       territorial_level_code = df_urban$territorial_level_code[1],
                       territorial_level = rep("Município", length(municipalities)),
                       measurement_unit_code = df_urban$measurement_unit_code[1],
                       geocode = df_urban$geocode[1],
                       variable_code = df_urban$variable_code[1],
                       variable = df_urban$variable[1],
                       household_situation_code = df_urban$household_situation_code[1],
                       household_situation = df_urban$household_situation[1],
                       measurement_unit = rep("Pessoas", length(municipalities)),
                       year_code = rep("2011", length(municipalities)),
                       year = rep("2011", length(municipalities)),
                       Population_count = rep(NA, length(municipalities)))

#Bind the new rows to the original dataframe
df_urban <- bind_rows(df_urban, new_rows) %>%
  arrange(municipality)

#Create a new dataframe with one row 2021 for each municipality
new_rows <- data.frame(municipality = municipalities,
                       territorial_level_code = df_urban$territorial_level_code[1],
                       territorial_level = rep("Município", length(municipalities)),
                       measurement_unit_code = df_urban$measurement_unit_code[1],
                       geocode = df_urban$geocode[1],
                       variable_code = df_urban$variable_code[1],
                       variable = df_urban$variable[1],
                       household_situation_code = df_urban$household_situation_code[1],
                       household_situation = df_urban$household_situation[1],
                       measurement_unit = rep("Pessoas", length(municipalities)),
                       year_code = rep("2021", length(municipalities)),
                       year = rep("2021", length(municipalities)),
                       Population_count = rep(NA, length(municipalities)))

#Bind the new rows to the original dataframe
df_urban <- bind_rows(df_urban, new_rows) %>%
  arrange(municipality)

#==========================================SLECTING THE DATA & POPULATION PROJECTIONS========================================#
#========================================SELECIONANDO OS DADOS & REALIZANDO PROJEÇÃO POPULACIONAL====================================#

#Formula of Linear population prjection:
#

#Group the dataframe by municipality and year (2000)
df_urban_grouped <- df_urban %>%
  filter(year == 2000) %>%
  group_by(municipality)

#Calculate the new population counts for each municipality in 2001
new_population_counts <- df_urban_grouped %>%
  summarize(new_population_count = (Population_count + population_growth_rate_1991_2000 * (2001 - 2000)))

#Join the new_population_counts dataframe with the original dataframe
df_urban <- df_urban %>%
  left_join(new_population_counts, by = "municipality") %>%
  mutate(Population_count = if_else(year == 2001, new_population_count, Population_count)) %>%
  select(-new_population_count)

#Group the dataframe by municipality and year (2010)
df_urban_grouped <- df_urban %>%
  filter(year == 2010) %>%
  group_by(municipality)

#Calculate the new population counts for each municipality in 2011
new_population_counts <- df_urban_grouped %>%
  summarize(new_population_count = (Population_count + population_growth_rate_2000_2010 * (2011 - 2010)))

#Join the new_population_counts dataframe with the original dataframe
df_urban <- df_urban %>%
  left_join(new_population_counts, by = "municipality") %>%
  mutate(Population_count = if_else(year == 2011, new_population_count, Population_count)) %>%
  select(-new_population_count)

#Group the dataframe by municipality and year (2010)
df_urban_grouped <- df_urban %>%
  filter(year == 2010) %>%
  group_by(municipality)

#Calculate the new population counts for each municipality in 2021
new_population_counts <- df_urban_grouped %>%
  summarize(new_population_count = (Population_count + population_growth_rate_2000_2010 * (2021 - 2010)))

#Join the new_population_counts dataframe with the original dataframe
df_urban <- df_urban %>%
  left_join(new_population_counts, by = "municipality") %>%
  mutate(Population_count = if_else(year == 2021, new_population_count, Population_count)) %>%
  select(-new_population_count)

View(df_urban)

################################# EXPORTING DATA ###################################
################################# EXPORTANDO OS DADOS ###################################

#Exporting table with selected columns:
#Select the desired columns
export_data <- df_urban %>%
  select(
    municipality, geocode, year, household_situation,
    Population_count, population_growth_rate_1991_2000, population_growth_rate_2000_2010
  )

# Export the table to a CSV file in the workspace
write.csv(export_data, "exported_data.csv", row.names = FALSE)

# Find or create the Google Drive folder
folder_name <- "R_results"
folder <- drive_get(path = folder_name)

# If the folder doesn't exist, create it
if (is.null(folder)) {
  folder <- drive_mkdir(folder_name)
}
# Upload the file to the Google Drive folder
local_file <- "exported_data.csv"
uploaded_file <- drive_upload(media = local_file, type = "text/csv", path = as_id(folder))

#rm(list = ls())
