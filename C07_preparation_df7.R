#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
    )
  )

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 250))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
df7_dist_lordo_sconto <- df_7_tic_clean_final %>% 
  group_by(COD_REPARTO) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))
df7_dist_lordo_sconto

ggplot(data=df7_dist_lordo_sconto, aes(x=COD_REPARTO, y=AVG_IMPORTO_LORDO)) +
    geom_bar(stat="identity", fill="orange", color="black") +
    labs(x="Cod reparto", y="Average importo lordo", 
         title="Average importo lordo medio for cod reparto") +
    theme_minimal()

ggplot(data=df7_dist_lordo_sconto, aes(x=COD_REPARTO, y=AVG_SCONTO)) +
    geom_bar(stat="identity", fill="orange", color="black") +
    labs(x="Cod reparto", y="Average sconto", 
         title="Average sconto for cod reparto") +
    theme_minimal()

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)
df7_dist_articoli <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))
df7_dist_articoli

df7_dist_articoli <- df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO, DIREZIONE) %>%
  summarize(NUM_TICS = n_distinct(ID_SCONTRINO)
            , NUM_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = NUM_TICS/ALL_TOT_TICs
         , PERCENT_CLIs = NUM_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs) %>%
  arrange(desc(NUM_TICS))
df7_dist_articoli

# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI
df7_dist_cli <- df_7_tic_clean_final %>% 
  group_by(ID_CLI) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO)) %>%
 arrange(desc(AVG_IMPORTO_LORDO))
df7_dist_cli

ggplot(data=df7_dist_cli, aes(x=ID_CLI, y=AVG_IMPORTO_LORDO)) +
    geom_bar(stat="identity", fill="orange", color="black") +
    labs(x="Cod cliente", y="Average importo lordo", 
         title="Average importo lordo for cliente") +
    theme_minimal()

ggplot(data=df7_dist_cli
         , aes(x=ID_CLI, y=AVG_SCONTO)) +
    geom_bar(stat="identity", fill="orange", color="black") +
    labs(x="Cod cliente", y="Average sconto", 
         title="Average sconto for cliente") +
    theme_minimal()

# compute the distribution of customers by number of purchases (as described in the slides)
df7_numpurchase <- df_7_tic_clean_final %>% 
  filter(DIREZIONE == 1) %>%
  select(c("ID_CLI", "ID_ARTICOLO")) %>%
  unique() %>%
  group_by(ID_CLI) %>%
  summarise(num = n())
df7_numpurchase
#df7_numpurchase %>% summarise(quantile(num, .75))

df7_numpurchase <- df7_numpurchase %>%
  mutate(PURCHASE = case_when((num >= 1) & (num < 10) ~ "At least 1",
                              (num >= 10) & (num < 20) ~ "At least 10",
                              (num >= 20) & (num < 50) ~ "At least 20",
                               num >= 50 ~ "At least 50")) %>%
  group_by(PURCHASE) %>% 
  summarise(TOT = sum(num)) %>% 
  arrange(desc(TOT))

ggplot(df7_numpurchase, aes(x = PURCHASE, y = cumsum(TOT)/sum(TOT))) + 
  geom_bar(stat="identity", fill="gold2", color="black") +
  labs(x="Number of purchases", y="Cumulative frequency", 
       title="Customer distribution based on the number of purchases") +
  theme_minimal() 

# compute the days for next purchase curve (as described in the slides)
df7_dates <- df_7_tic_clean_final %>% 
  filter(DIREZIONE == 1) %>%
  select(c("ID_CLI", "TIC_DATE"))  %>%
  unique() %>%
  group_by(ID_CLI) %>%
  summarize(AVG_PURCHASE_DATE = mean(diff(TIC_DATE)),
            LAST_DATE = max(TIC_DATE),
            FIRST_DATE = min(TIC_DATE)) %>%
  mutate(AVG_PURCHASE_DATE = round(as.numeric(AVG_PURCHASE_DATE, units = "days")))

df7_dates$AVG_PURCHASE_DATE <- df7_dates$AVG_PURCHASE_DATE %>% 
  replace(is.na(.), 0)

df7_dates <- df7_dates %>%
  group_by(AVG_PURCHASE_DATE) %>%
  summarize(NUM_CLI = sum(n_distinct(ID_CLI)))

df7_dates

df7_dates$CUM_SUM <- cumsum(df7_dates$NUM_CLI)
df7_dates$PERC_CUM <- (df7_dates$CUM_SUM / max(df7_dates$CUM_SUM)) * 100

df7_dates %>%
  ggplot(aes(x = AVG_PURCHASE_DATE, y = PERC_CUM)) + 
  geom_line(color = "gold2", size = 0.8) +
  geom_vline(xintercept=60, color = "black", size = 0.5) + 
  geom_hline(yintercept=80, color = "black", size = 0.5) +
  labs(x="Average days next purchase", y="%Customers", 
       title="Cumulative repurchase percentage") +
  scale_x_continuous(breaks=seq(0,300,30)) +
  theme_minimal() 

#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)
