---
title: 'Processing forest inventory measurements'
output:
  html_document:
    df_print: paged
  pdf_document: default
---

Setup and connect to DB


```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.4.1     v dplyr   0.7.4
## v tidyr   0.7.2     v stringr 1.2.0
## v readr   1.1.1     v forcats 0.2.0
```

```
## -- Conflicts ------------------------------------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(RSQLite)
```

```
## Warning: package 'RSQLite' was built under R version 3.4.4
```

```r
DBname <- "RilieviDendrometria_v03.sqlite"
DBconn <- dbConnect(RSQLite::SQLite(), DBname)
DBI::dbListTables(DBconn)
```

```
##  [1] "AdS_Rilevat"       "Altezze"           "Boschi"           
##  [4] "Cav_specie"        "Cavallettamento"   "Diradamento"      
##  [7] "Rilevatori"        "Rilievi"           "Specie"           
## [10] "V_Altezze"         "V_Cavallettamento" "prova"            
## [13] "prova2"            "sqlite_sequence"
```

Synthesis tables   ##  COULD not find the 'pivot function', is it special for EXPLORATORY?

```{# could not find pivot() as R function!   r}
dbGetQuery(DBconn, 'select * from V_Cavallettamento') %>%
  pivot(Cod_bosco + AdS ~ sp, value = Id_ceppaia, fun.aggregate = length)
```

Initial stand tables


```r
v_cav <- dbGetQuery(DBconn, 'select * from V_Cavallettamento')
v_cav %>%
  group_by(Cod_bosco, AdS, cod_specie) %>%
  summarise(n= n(), dg = sqrt(mean(d130^2))) %>%
  mutate(G = n * dg^2 * pi/40000)
```

```
## # A tibble: 5 x 6
## # Groups: Cod_bosco, AdS [5]
##   Cod_bosco AdS   cod_specie     n    dg     G
##   <chr>     <chr> <chr>      <int> <dbl> <dbl>
## 1 ANE1      1     CEDRUS       158  18.4  4.22
## 2 ANE1      2     CEDRUS       154  17.8  3.82
## 3 ANE1      3     CEDRUS       153  17.5  3.68
## 4 ANE1      4     CEDRUS       170  20.6  5.69
## 5 SOLO      1     QUEILE        62  28.8  4.05
```

Estimation of dbh-heigth models


```r
v_heigths <- dbGetQuery(DBconn, 'select * from V_Altezze')
h_mods <- v_heigths %>%
  group_by(Cod_bosco, AdS, cod_specie) %>%
    do(fit = lm(h_dendrometrica ~ log(d130), .))
```

Stand tables summary by dbh class
and Estimation of average heigths (by class)


```r
library(broom)
acl <- 3 # dbh class intervals width

standTables1 <- v_cav %>%
  mutate(dbh = d130, d130 = acl*floor(.5 + d130/acl)) %>%
  group_by(Cod_bosco, AdS, cod_specie, d130) %>%
  summarise(frq = n()) %>%
  nest() %>%
  inner_join(h_mods, .) %>%
  mutate(avg_h = list(augment(fit, newdata = data))) %>%
  unnest(avg_h) %>%
  rename(avg_h = .fitted, avg_h.se = .se.fit)
```

```
## Joining, by = c("Cod_bosco", "AdS", "cod_specie")
```


Estimation of wood resource using INFC volume (tables) functions
[Species matching
Quil for QUEILE
Piab for CEDRUS]


```r
# Funzione che converte un fattore nel vettore dei corrispondenti livelli
#  (quindi in un vettore di stringhe 'chr', "character")
factor2chr <- function(x) levels(x)[x]


library(ForIT)
#Elenco delle SPECIE e dei corrispondenti codici
INFCspecies <- INFCstats %$% 
  unique(data.frame(spg = factor2chr(spg), specie = factor2chr(specie))) %>%
  mutate(rownames = NULL)

lkup <- setNames(c('Quil', 'Piab'), c('QUEILE','CEDRUS'))

standTables2 <- standTables1 %>%
  group_by(Cod_bosco, AdS) %>%
  nest() %>%
  mutate(v = map(data, 
                 function(x) as.data.frame(x) %$%
                   INFCvpe(lkup[cod_specie], d130 , avg_h, mod='v', frq, aggr=FALSE)[['mainData']])) %>%
  unnest()
```

```
## Error in mutate_impl(.data, dots): Evaluation error: non trovo la funzione "%$%".
```

```r
# with(data, data.frame(cod_specie, d130, avg_h)))
#   mutate(v[] = with( .data[[1]], INFCvpe(lkup[cod_specie], d130 , avg_h, mod='v', 1, aggr=FALSE)[['mainData']])) %>%

standTables2 %T>% 
  {if (any(.$in.range == 'n')) warning("There are OUTofRANGE values")} %<>%
  filter(!in.range == 'n') %>%
  group_by(Cod_bosco, AdS, cod_specie) %>%
  summarise(n= sum(frq), dg = sqrt(mean(d130^2)), Vol = sum(T_0)/10000) %>%
  mutate(G = n * dg^2 * pi/40000) %>%
  select(Cod_bosco, AdS, cod_specie, n, dg, G, Vol)
```

```
## Error in eval(lhs, parent, parent): oggetto "standTables2" non trovato
```

