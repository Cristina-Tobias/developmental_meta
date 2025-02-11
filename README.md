A systematic review of short-term and working memory abilities in
children with intellectual disability; to what extent do the result
support the delay or difference hypotheses?
================
Cris
Last Updated: 11, February, 2025 at 13:28

## Aim

The aim of the study is to investigate if short-term and working memory
abilities exhibit a developmentally delayed or developmentally different
pattern in children with ID. This will be done by an overall analysis
and subgroup analyses based on type of memory (short-term or working
memory), aetiology of ID (familial or organic). In addition, the overall
analysis will also be repeated with level of IQ as a moderator.

### Aims and Research questions

The aim of the study is to investigate if short-term and working memory
abilities exhibit a developmentally delayed or developmentally different
pattern in children with ID. This will be done by an overall analysis
and subgroup analyses based on the type of memory (short-term or working
memory) and the aetiology of ID (familial or organic). In addition, the
overall analysis will also be repeated with the level of the severity of
the intellectual disability as a moderator.

Our research questions are:

1.  Do short-term and working memory abilities exhibit a developmentally
    delayed or developmentally different pattern in children with ID
    (compared to mental age-matched groups)?

2.  Are these results consistent across the aetiology (familial and
    organic) of ID or level of IQ?

For research question 1, we cannot make a prediction as to whether there
will be significant differences in short-term and working memory
abilities between children with ID and a mental-age-matched comparison
group. However, we will use equivalence testing to find out whether
within the literature, there is significant statistical evidence to
support the delay and the difference hypotheses. Different statistical
outcomes and possible conclusions are further described in method
section below. For research question 2, we hypothesise, that both IQ and
ID origin will significantly moderate STM and WM performance.

## Methods

### Data extraction

We extracted data from stuff

### Statistical analysis

We tested the difference in performance moderating for type of task and
also for type of ID

### Equivalence testing

Yes

## Results

### Descriptives

We conducted and reported this systematic review according to NIRO-SR
guidelines for conducting and reporting systematic reviews of
non-intervention research (REF). See table X for descriptives of the
obtained studies.

``` r
skim_data <- skim(data[, columns_of_interest])
skim_data
```

|  |  |
|:---|:---|
| Name | data\[, columns_of_interes… |
| Number of rows | 99 |
| Number of columns | 14 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |  |
| Column type frequency: |  |
| numeric | 14 |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |  |
| Group variables | None |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| study_year | 0 | 1.00 | 2009.13 | 11.15 | 1962.00 | 2010.00 | 2012.00 | 2013.00 | 2020.00 | ▁▁▁▁▇ |
| n_id | 0 | 1.00 | 22.46 | 11.19 | 10.00 | 15.00 | 19.00 | 26.00 | 68.00 | ▇▆▁▁▁ |
| age_id | 3 | 0.97 | 16.31 | 6.24 | 4.34 | 13.59 | 14.98 | 17.73 | 49.88 | ▃▇▁▁▁ |
| mean_ma_id | 2 | 0.98 | 6.35 | 1.26 | 1.88 | 5.75 | 6.10 | 6.74 | 11.17 | ▁▂▇▁▁ |
| sd_ma_id | 29 | 0.71 | 2.06 | 4.82 | 0.37 | 0.67 | 0.93 | 1.56 | 24.61 | ▇▁▁▁▁ |
| iq_id | 10 | 0.90 | 48.78 | 8.50 | 19.92 | 44.17 | 53.00 | 53.30 | 65.75 | ▁▂▂▇▁ |
| n_magroup | 0 | 1.00 | 25.78 | 14.07 | 10.00 | 15.00 | 22.00 | 28.00 | 65.00 | ▇▅▁▁▁ |
| age_magroup | 9 | 0.91 | 6.57 | 1.32 | 1.93 | 5.75 | 6.27 | 7.55 | 11.17 | ▁▂▇▃▁ |
| mean_ma_magroup | 13 | 0.87 | 6.58 | 1.24 | 1.90 | 5.98 | 6.71 | 6.90 | 9.76 | ▁▂▇▇▂ |
| iq_magroup | 50 | 0.49 | 102.73 | 8.78 | 94.00 | 95.70 | 95.70 | 109.00 | 123.56 | ▇▁▅▁▁ |
| mean_id | 0 | 1.00 | 53.96 | 164.97 | 0.04 | 3.84 | 9.93 | 22.74 | 1096.20 | ▇▁▁▁▁ |
| mean_magroup | 0 | 1.00 | 52.19 | 165.84 | 0.03 | 3.78 | 9.67 | 22.81 | 1258.00 | ▇▁▁▁▁ |
| sd_id | 0 | 1.00 | 15.64 | 36.80 | 0.19 | 2.38 | 5.01 | 10.50 | 251.50 | ▇▁▁▁▁ |
| sd_magroup | 0 | 1.00 | 12.10 | 32.96 | 0.13 | 1.90 | 3.50 | 5.52 | 277.90 | ▇▁▁▁▁ |

``` r
missing_summary <- miss_var_summary(data[, columns_of_interest])
iq_missing <- missing_summary[missing_summary$variable == "iq_id", "pct_miss"][[1]]
iq_ma_missing <- missing_summary[missing_summary$variable == "iq_magroup", "pct_miss"][[1]]
```

<!---
For both groups, we extracted the IQ scores, number of participants, mean and SD of the performance in the tasks, and the mean and SD of the chronological and mental age. The variable with the most missing data was  with % data missing. IQ scores for the ID group were missing %, and for the MA group, % of IQ scores were missing.
-->

``` r
missing_data <- vis_miss(data[,columns_of_interest])
missing_data
```

![](README_files/figure-gfm/descriptives-1.png)<!-- -->

### Statistical analysis

We are performing a multilevel meta-analysis Meta-analysis per outcome
(WM and STM)

``` r
res_inhibition <- rma.mv(yi, vi, random = list(~ 1 | effectsize_id, ~1 | study_id), 
                         subset = ef_type == "inhibition",
                          tdist = TRUE, data = stats_test) #this will be our STM
res_attention <- rma.mv(yi, vi, random = list(~ 1 | effectsize_id, ~1 | study_id), 
                         subset = ef_type == "attention",
                          tdist = TRUE, data = stats_test) #this will be our WM

dat.comp <- data.frame(alloc    = c("Short-term memory", "Working memory"
                                    ), 
                       estimate = c(coef(res_inhibition), coef(res_attention)
                                    ), 
                       stderror = c(res_inhibition$se, res_attention$se
                                    ),
                       tau2     = c(res_inhibition$tau2, res_attention$tau2
                                    ))
dfround(dat.comp, 3)
```

    ##               alloc estimate stderror tau2
    ## 1 Short-term memory   -0.105    0.180    0
    ## 2    Working memory   -0.054    0.203    0

``` r
id_tests <- t(data.frame(inhibition = c(res_inhibition$b, res_inhibition$se,res_inhibition$ci.lb, res_inhibition$ci.ub), #this will be our STM
                        attention = c(res_attention$b, res_attention$se,res_attention$ci.lb, res_attention$ci.ub),  #this will be our WM
                         row.names = c('b', 'se', 'ci.lb', 'ci.ub')))
for (id in c("inhibition", "attention"
             )){
  df <- stats_test[stats_test$ef_type == id, c('cite', 'yi', 'sei', 'n_magroup', 'n_id')]
  
  # Compute confidence intervals and format labels
  
  df$cite <- paste0(df$cite, '.', ave(df$cite, df$cite, FUN = seq_along))
  df <- df %>%
    mutate(
      lower = yi - 1.96 * sei,  
      upper = yi + 1.96 * sei,  
      ci_text = paste0(round(yi,2), "  [", round(lower, 2), ", ", round(upper, 2), "]")
    )
  FE_model <- paste0('RE Model for the ', id, ' outcome')
  df <- df %>%
    arrange(yi)
  df <- rbind(df, c(FE_model, id_tests[id,'b'], id_tests[id,'se'], '','', 
                    id_tests[id,'ci.lb'], id_tests[id,'ci.ub'], 
                    paste0(round(id_tests[id,'b'],2), 
                           "  [", round(id_tests[id,'ci.lb'], 2), ", ", 
                           round(id_tests[id,'ci.ub'], 2), "]")))
  df[, c('yi','sei', 'lower', 'upper')] <- lapply(df[, c('yi','sei', 'lower', 'upper')], as.numeric)
  df$desired_order <- seq(dim(df)[1],1)
  p_right <- ggplot(df, aes(y = reorder(cite, desired_order))) +
    geom_point(aes(x=yi), size = 3, shape=15, color = "darkred") +  
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "grey") +  
    labs(
      x = "Effect Size (Hedges' g)",
      y = "Study",
      caption = "Error bars represent 95% confidence intervals"
    )   + theme(axis.line.y = element_blank(),
                axis.ticks.y= element_blank(),
                axis.text.y= element_blank(),
                axis.title.y= element_blank(),
                panel.background = element_rect(fill = "white"
                )) + coord_cartesian(xlim = c(min(df$lower), max(df$upper)), ylim = c(0,dim(df)[1] + 1))
  p_left <- ggplot(df, aes(y = reorder(cite, desired_order))) + labs(x = '') +
    geom_text(aes(x = 0, label = cite), hjust = 0, fontface = "bold") +
    annotate(geom="text", x=0.3, y=dim(df)[1] + 1, label="Study", fontface = "bold") +
    geom_text(aes(x = 0.8, label = n_id), hjust = 0) +
    annotate(geom="text", x=0.8, y=dim(df)[1] + 1, label="N (ID)", fontface = "bold") +
    geom_text(aes(x = 1, label = n_magroup), hjust = 0) +
    annotate(geom="text", x=1, y=dim(df)[1] + 1, label="N (Control)", fontface = "bold") +
    geom_text(
      aes(x = 1.3, label = ci_text),
      hjust = 0
    ) +
    annotate(geom="text", x=1.4, y=dim(df)[1] + 1, label="Hedges' g [95% CI]", fontface = "bold") +
    theme_void() +
    coord_cartesian(xlim = c(0, 2), ylim = c(0,dim(df)[1] + 1))
  
  layout <- c(
    area(t = 0, l = 0, b = dim(df)[1], r = 5), 
    area(t = 0, l = 6, b = dim(df)[1], r = 8) 
  )
  print(p_left + p_right + plot_layout(design = layout))
  Sys.sleep(2)
}
```

![](README_files/figure-gfm/plots%20type%20of%20memory-1.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20memory-2.png)<!-- -->

#### Moderator analyses

Moderating type of ID origin for STM (both STM and WM will have separate
subgroup analyses)

``` r
#subgroup analysis (RE meta-analysis of each group)
res_DS <- rma.mv(yi, vi, random = list(~ 1 | effectsize_id, ~1 | study_id),
                          subset = group_id == "DS" & ef_type =="inhibition",
                          tdist = TRUE, data = stats_test) # this will be our organic origin
res_NSID <- rma.mv(yi, vi, random = list(~ 1 | effectsize_id, ~1 | study_id),
                          subset = group_id == "NSID" & ef_type =="inhibition",
                          tdist = TRUE, data = stats_test) # this will be our non-specific origin


dat.comp <- data.frame(alloc    = c("DS", "NSID"
                                    ), 
                       estimate = c(coef(res_DS), coef(res_NSID)
                                    ), 
                       stderror = c(res_DS$se, res_NSID$se
                                    ),
                       tau2     = c(res_DS$tau2, res_NSID$tau2
                                   )
                       )
dfround(dat.comp, 3)
```

    ##   alloc estimate stderror tau2
    ## 1    DS   -0.318    0.198    0
    ## 2  NSID    0.674    0.936    0

``` r
#fixed effects meta-analysis of the subgroup estimates
# taken from: https://www.metafor-project.org/doku.php/tips:comp_two_independent_estimates#separate_meta-analyses
rma(estimate, sei=stderror, mods = ~ alloc, method="FE", data=dat.comp, digits=3)
```

    ## 
    ## Fixed-Effects with Moderators Model (k = 2)
    ## 
    ## I^2 (residual heterogeneity / unaccounted variability): 0.00%
    ## H^2 (unaccounted variability / sampling variability):   1.00
    ## R^2 (amount of heterogeneity accounted for):            NA%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 0) = 0.000, p-val = 1.000
    ## 
    ## Test of Moderators (coefficient 2):
    ## QM(df = 1) = 1.074, p-val = 0.300
    ## 
    ## Model Results:
    ## 
    ##            estimate     se    zval   pval   ci.lb  ci.ub    
    ## intrcpt      -0.318  0.198  -1.606  0.108  -0.706  0.070    
    ## allocNSID     0.992  0.957   1.036  0.300  -0.884  2.868    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
id_tests <- t(data.frame(DS = c(res_DS$b, res_DS$se,res_DS$ci.lb, res_DS$ci.ub),
                         NSID = c(res_NSID$b, res_NSID$se,res_NSID$ci.lb, res_NSID$ci.ub),
                         row.names = c('b', 'se', 'ci.lb', 'ci.ub')))
for (id in c('DS', 'NSID'
            )
     ){
  df <- stats_test[stats_test$group_id == id, c('cite', 'yi', 'sei', 'n_magroup', 'n_id')]
  
  # Compute confidence intervals and format labels
  
  df$cite <- paste0(df$cite, '.', ave(df$cite, df$cite, FUN = seq_along))
  df <- df %>%
    mutate(
      lower = yi - 1.96 * sei,  
      upper = yi + 1.96 * sei,  
      ci_text = paste0(round(yi,2), "  [", round(lower, 2), ", ", round(upper, 2), "]")
    )
  FE_model <- paste0('RE Model for ', id, ' group')
  df <- df %>%
    arrange(yi)
  df <- rbind(df, c(FE_model, id_tests[id,'b'], id_tests[id,'se'], '','', 
                    id_tests[id,'ci.lb'], id_tests[id,'ci.ub'], 
                    paste0(round(id_tests[id,'b'],2), 
                           "  [", round(id_tests[id,'ci.lb'], 2), ", ", 
                           round(id_tests[id,'ci.ub'], 2), "]")))
  df[, c('yi','sei', 'lower', 'upper')] <- lapply(df[, c('yi','sei', 'lower', 'upper')], as.numeric)
  df$desired_order <- seq(dim(df)[1],1)
  p_right <- ggplot(df, aes(y = reorder(cite, desired_order))) +
    geom_point(aes(x=yi), size = 3, shape=15, color = "darkred") +  
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "grey") +  
    labs(
      x = "Effect Size (Hedges' g)",
      y = "Study",
      caption = "Error bars represent 95% confidence intervals"
    )   + theme(axis.line.y = element_blank(),
                axis.ticks.y= element_blank(),
                axis.text.y= element_blank(),
                axis.title.y= element_blank(),
                panel.background = element_rect(fill = "white"
                )) + coord_cartesian(xlim = c(min(df$lower), max(df$upper)), ylim = c(0,dim(df)[1] + 1))
  p_left <- ggplot(df, aes(y = reorder(cite, desired_order))) + labs(x = '') +
    geom_text(aes(x = 0, label = cite), hjust = 0, fontface = "bold") +
    annotate(geom="text", x=0.3, y=dim(df)[1] + 1, label="Study", fontface = "bold") +
    geom_text(aes(x = 0.8, label = n_id), hjust = 0) +
    annotate(geom="text", x=0.8, y=dim(df)[1] + 1, label="N (ID)", fontface = "bold") +
    geom_text(aes(x = 1, label = n_magroup), hjust = 0) +
    annotate(geom="text", x=1, y=dim(df)[1] + 1, label="N (Control)", fontface = "bold") +
    geom_text(
      aes(x = 1.3, label = ci_text),
      hjust = 0
    ) +
    annotate(geom="text", x=1.4, y=dim(df)[1] + 1, label="Hedges' g [95% CI]", fontface = "bold") +
    theme_void() +
    coord_cartesian(xlim = c(0, 2), ylim = c(0,dim(df)[1] + 1))
  
  layout <- c(
    area(t = 0, l = 0, b = dim(df)[1], r = 5), 
    area(t = 0, l = 6, b = dim(df)[1], r = 8) 
  )
  print(p_left + p_right + plot_layout(design = layout))
  Sys.sleep(2)
}
```

![](README_files/figure-gfm/plots%20type%20of%20ID,%20-1.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20ID,%20-2.png)<!-- -->

### Equivalence testing

The studies looked at different types of executive functions (CHANGE
THIS LATER TO SUIT OUR OWN ANALYSIS). In the next table you can see the
extracted descriptives separated by executive function.

``` r
describeBy(data[,columns_of_interest], data$ef_type)
```

    ## 
    ##  Descriptive statistics by group 
    ## group: attention
    ##                 vars  n    mean    sd  median trimmed  mad     min     max
    ## study_year         1 16 2011.94  2.24 2012.00 2011.57 1.48 2010.00 2019.00
    ## n_id               2 16   22.06  9.44   15.00   21.29 0.00   15.00   40.00
    ## age_id             3 16   16.51  5.08   17.60   16.19 2.92    9.59   27.81
    ## mean_ma_id         4 16    5.74  0.82    5.92    5.77 0.86    4.47    6.70
    ## sd_ma_id           5 10    0.84  0.30    0.90    0.83 0.15    0.37    1.38
    ## iq_id              6 12   46.37 12.02   53.00   48.32 0.44   19.92   53.30
    ## n_magroup          7 16   24.38 10.97   16.00   23.93 1.48   15.00   40.00
    ## age_magroup        8 16    6.20  1.45    6.58    6.24 1.52    4.15    7.60
    ## mean_ma_magroup    9 13    6.06  1.15    6.90    6.13 0.00    4.45    6.90
    ## iq_magroup        10  8   94.85  0.91   94.85   94.85 1.26   94.00   95.70
    ## mean_id           11 16   26.95 34.79   10.06   22.99 6.62    2.13  107.30
    ## mean_magroup      12 16   26.12 33.38    8.50   22.80 3.72    1.96   96.80
    ## sd_id             13 16    6.90  7.16    3.72    6.05 4.43    0.42   25.20
    ## sd_magroup        14 16    5.03  6.51    1.64    4.29 1.80    0.35   20.10
    ##                  range  skew kurtosis   se
    ## study_year        9.00  1.79     3.42 0.56
    ## n_id             25.00  0.78    -1.04 2.36
    ## age_id           18.22  0.25    -0.62 1.27
    ## mean_ma_id        2.23 -0.38    -1.33 0.20
    ## sd_ma_id          1.01 -0.16    -0.83 0.10
    ## iq_id            33.38 -1.25    -0.16 3.47
    ## n_magroup        25.00  0.45    -1.70 2.74
    ## age_magroup       3.45 -0.33    -1.71 0.36
    ## mean_ma_magroup   2.45 -0.56    -1.72 0.32
    ## iq_magroup        1.70  0.00    -2.23 0.32
    ## mean_id         105.17  1.40     0.43 8.70
    ## mean_magroup     94.84  1.25    -0.14 8.34
    ## sd_id            24.78  1.10     0.18 1.79
    ## sd_magroup       19.75  1.35     0.15 1.63
    ## ------------------------------------------------------------ 
    ## group: fluency
    ##                 vars  n    mean    sd  median trimmed   mad     min     max
    ## study_year         1 12 2011.17  4.57 2012.00 2011.10  1.48 2003.00 2020.00
    ## n_id               2 12   26.50 13.71   24.00   23.50  2.97   15.00   68.00
    ## age_id             3 12   14.31  1.41   13.64   14.05  0.70   13.17   18.10
    ## mean_ma_id         4 12    6.39  1.19    6.38    6.38  1.24    4.50    8.39
    ## sd_ma_id           5 10    1.15  0.36    1.10    1.16  0.54    0.67    1.58
    ## iq_id              6 12   45.00  9.26   45.26   45.30 10.91   30.65   56.33
    ## n_magroup          7 12   26.50 12.03   26.00   24.10  4.45   15.00   62.00
    ## age_magroup        8 12    6.31  0.96    6.14    6.30  1.17    4.92    7.84
    ## mean_ma_magroup    9 10    6.77  1.42    6.53    6.83  2.19    4.50    8.55
    ## iq_magroup        10  3  109.00  0.00  109.00  109.00  0.00  109.00  109.00
    ## mean_id           11 12   17.78  9.16   16.60   16.79 10.85    7.13   38.30
    ## mean_magroup      12 12   18.01  8.74   14.99   17.11  9.01    8.00   37.00
    ## sd_id             13 12    7.50  2.76    8.59    7.52  3.42    2.77   12.10
    ## sd_magroup        14 12    5.74  1.65    5.31    5.68  1.55    3.26    8.73
    ##                 range  skew kurtosis   se
    ## study_year      17.00 -0.33    -0.13 1.32
    ## n_id            53.00  2.20     4.07 3.96
    ## age_id           4.93  1.47     1.45 0.41
    ## mean_ma_id       3.89 -0.14    -1.13 0.34
    ## sd_ma_id         0.92  0.00    -1.73 0.12
    ## iq_id           25.68 -0.22    -1.41 2.67
    ## n_magroup       47.00  1.98     3.47 3.47
    ## age_magroup      2.92  0.09    -1.37 0.28
    ## mean_ma_magroup  4.05 -0.41    -1.30 0.45
    ## iq_magroup       0.00   NaN      NaN 0.00
    ## mean_id         31.17  0.69    -0.49 2.65
    ## mean_magroup    29.00  0.68    -0.73 2.52
    ## sd_id            9.33 -0.12    -1.32 0.80
    ## sd_magroup       5.47  0.28    -1.35 0.48
    ## ------------------------------------------------------------ 
    ## group: inhibition
    ##                 vars  n    mean     sd  median trimmed  mad     min     max
    ## study_year         1 37 2011.95   3.94 2013.00 2012.29 1.48 2001.00 2018.00
    ## n_id               2 37   20.70   8.11   19.00   20.13 7.41   11.00   42.00
    ## age_id             3 37   16.64   7.91   14.68   15.30 1.70    4.34   49.88
    ## mean_ma_id         4 35    6.51   1.71    6.10    6.44 0.82    1.88   11.17
    ## sd_ma_id           5 24    4.10   7.93    1.10    2.40 0.74    0.54   24.61
    ## iq_id              6 34   48.88   8.41   53.00   49.48 9.04   30.65   59.10
    ## n_magroup          7 37   27.30  16.24   21.00   24.90 8.90   13.00   65.00
    ## age_magroup        8 34    6.63   1.63    6.14    6.60 1.76    1.93   11.17
    ## mean_ma_magroup    9 32    6.53   1.48    6.53    6.60 1.25    1.90    9.76
    ## iq_magroup        10 18  103.57   8.09  107.20  102.92 4.37   94.00  123.56
    ## mean_id           11 37   64.21 189.01    7.08   13.01 9.27    0.45  948.90
    ## mean_magroup      12 37   57.89 161.58    7.00   12.47 9.00    0.25  706.70
    ## sd_id             13 37   16.03  36.81    4.79    7.46 4.54    0.19  205.40
    ## sd_magroup        14 37    9.30  20.68    3.27    3.74 2.15    0.13   95.40
    ##                  range  skew kurtosis    se
    ## study_year       17.00 -0.83     0.82  0.65
    ## n_id             31.00  0.67    -0.55  1.33
    ## age_id           45.54  2.82     8.75  1.30
    ## mean_ma_id        9.30  0.51     1.43  0.29
    ## sd_ma_id         24.07  2.11     2.60  1.62
    ## iq_id            28.45 -0.57    -0.89  1.44
    ## n_magroup        52.00  1.36     0.48  2.67
    ## age_magroup       9.24  0.07     1.27  0.28
    ## mean_ma_magroup   7.86 -0.54     1.42  0.26
    ## iq_magroup       29.56  0.45    -0.34  1.91
    ## mean_id         948.45  3.46    11.67 31.07
    ## mean_magroup    706.45  3.04     7.88 26.56
    ## sd_id           205.21  3.94    16.47  6.05
    ## sd_magroup       95.27  3.08     8.43  3.40
    ## ------------------------------------------------------------ 
    ## group: other
    ##                 vars n    mean    sd  median trimmed   mad     min     max
    ## study_year         1 9 2013.00  4.03 2012.00 2013.00  2.97 2008.00 2020.00
    ## n_id               2 9   25.00 16.72   18.00   25.00  4.45   15.00   65.00
    ## age_id             3 9   17.58  5.72   17.60   17.58  2.24   10.10   30.30
    ## mean_ma_id         4 9    6.89  0.91    6.70    6.89  1.07    5.54    8.43
    ## sd_ma_id           5 6    1.15  0.44    1.17    1.15  0.51    0.52    1.58
    ## iq_id              6 8   53.52  3.22   53.30   53.52  2.45   46.57   56.33
    ## n_magroup          7 9   26.00 17.02   18.00   26.00  4.45   15.00   62.00
    ## age_magroup        8 8    7.51  0.18    7.50    7.51  0.15    7.33    7.84
    ## mean_ma_magroup    9 9    7.23  0.95    6.90    7.23  1.04    5.30    8.55
    ## iq_magroup        10 6   99.57  7.35   95.70   99.57  2.52   94.00  109.00
    ## mean_id           11 9   18.46 26.54    7.80   18.46 11.51    0.04   84.90
    ## mean_magroup      12 9   24.43 29.30   12.50   24.43 15.42    0.03   87.00
    ## sd_id             13 9    6.06  5.81    3.80    6.06  3.11    0.83   15.98
    ## sd_magroup        14 9    9.05 15.32    2.20    9.05  2.10    0.78   47.75
    ##                 range  skew kurtosis   se
    ## study_year      12.00  0.64    -1.11 1.34
    ## n_id            50.00  1.47     0.74 5.57
    ## age_id          20.20  0.87     0.12 1.91
    ## mean_ma_id       2.89  0.13    -1.34 0.30
    ## sd_ma_id         1.06 -0.20    -1.94 0.18
    ## iq_id            9.76 -1.01    -0.07 1.14
    ## n_magroup       47.00  1.17    -0.40 5.67
    ## age_magroup      0.51  0.49    -1.24 0.06
    ## mean_ma_magroup  3.25 -0.52    -0.60 0.32
    ## iq_magroup      15.00  0.51    -1.96 3.00
    ## mean_id         84.86  1.63     1.43 8.85
    ## mean_magroup    86.97  1.08    -0.37 9.77
    ## sd_id           15.15  0.84    -1.08 1.94
    ## sd_magroup      46.97  1.74     1.57 5.11
    ## ------------------------------------------------------------ 
    ## group: shifting
    ##                 vars  n    mean     sd  median trimmed   mad     min     max
    ## study_year         1 24 2000.42  19.51 2010.00 2002.70  4.45 1962.00 2019.00
    ## n_id               2 24   22.25  12.95   15.50   20.35  7.41   10.00   67.00
    ## age_id             3 21   15.12   3.11   15.17   15.18  3.60   10.10   20.02
    ## mean_ma_id         4 24    6.31   0.70    6.10    6.24  0.52    5.27    8.33
    ## sd_ma_id           5 19    0.91   0.32    0.90    0.89  0.35    0.54    1.58
    ## iq_id              6 23   50.20   6.58   53.00   50.35  5.01   37.90   65.75
    ## n_magroup          7 24   23.83  13.12   18.00   22.10  5.93   10.00   62.00
    ## age_magroup        8 19    6.58   0.95    6.14    6.59  1.54    5.10    7.84
    ## mean_ma_magroup    9 21    6.63   0.82    6.53    6.58  0.55    5.30    8.55
    ## iq_magroup        10 14  106.18  10.60  109.00  106.19 13.79   94.00  118.30
    ## mean_id           11 24   89.61 235.97    8.98   29.25  8.93    0.81 1096.20
    ## mean_magroup      12 24   90.14 268.75    7.00   20.05  7.87    1.10 1258.00
    ## sd_id             13 24   29.02  57.31    3.19   15.11  3.14    0.85  251.50
    ## sd_magroup        14 24   25.77  59.82    3.35   11.11  2.18    1.30  277.90
    ##                   range  skew kurtosis    se
    ## study_year        57.00 -1.05    -0.76  3.98
    ## n_id              57.00  1.89     3.50  2.64
    ## age_id             9.92  0.00    -1.38  0.68
    ## mean_ma_id         3.06  1.06     0.71  0.14
    ## sd_ma_id           1.04  0.78    -0.42  0.07
    ## iq_id             27.85 -0.24     0.10  1.37
    ## n_magroup         52.00  1.31     0.92  2.68
    ## age_magroup        2.74 -0.13    -1.69  0.22
    ## mean_ma_magroup    3.25  0.61    -0.11  0.18
    ## iq_magroup        24.30 -0.09    -1.95  2.83
    ## mean_id         1095.39  3.37    11.08 48.17
    ## mean_magroup    1256.90  3.55    12.09 54.86
    ## sd_id            250.65  2.70     7.10 11.70
    ## sd_magroup       276.60  3.23    10.38 12.21
    ## ------------------------------------------------------------ 
    ## group: updating
    ##                 vars n    mean sd  median trimmed mad     min     max range
    ## study_year         1 1 2010.00 NA 2010.00 2010.00   0 2010.00 2010.00     0
    ## n_id               2 1   28.00 NA   28.00   28.00   0   28.00   28.00     0
    ## age_id             3 1   38.30 NA   38.30   38.30   0   38.30   38.30     0
    ## mean_ma_id         4 1    6.17 NA    6.17    6.17   0    6.17    6.17     0
    ## sd_ma_id           5 1    1.50 NA    1.50    1.50   0    1.50    1.50     0
    ## iq_id              6 0     NaN NA      NA     NaN  NA     Inf    -Inf  -Inf
    ## n_magroup          7 1   28.00 NA   28.00   28.00   0   28.00   28.00     0
    ## age_magroup        8 1    6.40 NA    6.40    6.40   0    6.40    6.40     0
    ## mean_ma_magroup    9 1    6.50 NA    6.50    6.50   0    6.50    6.50     0
    ## iq_magroup        10 0     NaN NA      NA     NaN  NA     Inf    -Inf  -Inf
    ## mean_id           11 1    5.04 NA    5.04    5.04   0    5.04    5.04     0
    ## mean_magroup      12 1    7.29 NA    7.29    7.29   0    7.29    7.29     0
    ## sd_id             13 1    3.76 NA    3.76    3.76   0    3.76    3.76     0
    ## sd_magroup        14 1    3.98 NA    3.98    3.98   0    3.98    3.98     0
    ##                 skew kurtosis se
    ## study_year        NA       NA NA
    ## n_id              NA       NA NA
    ## age_id            NA       NA NA
    ## mean_ma_id        NA       NA NA
    ## sd_ma_id          NA       NA NA
    ## iq_id             NA       NA NA
    ## n_magroup         NA       NA NA
    ## age_magroup       NA       NA NA
    ## mean_ma_magroup   NA       NA NA
    ## iq_magroup        NA       NA NA
    ## mean_id           NA       NA NA
    ## mean_magroup      NA       NA NA
    ## sd_id             NA       NA NA
    ## sd_magroup        NA       NA NA

Equivalence testing for the different types of tasks.

``` r
# Overall meta-analyses of STM and WM
inhibition <- stats_test[stats_test$ef_type == 'inhibition',]
inhibition_model <- rma(yi, vi, data = inhibition)
inhibition_results <- ma_pipe_sei(
  inhibition,
  true_effect = inhibition_model$b,
  rep_lower = inhibition_model$ci.lb,
  rep_upper = inhibition_model$ci.ub,
  analysis_title = "Short-term memory tasks",
  plot = TRUE
)
```

![](README_files/figure-gfm/equivalence%20test%20plots-1.png)<!-- -->

    ## 

    ## 

    ## NHST: don't reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
inhibition_et_dat <- inhibition_results$et

 attention <- stats_test[stats_test$ef_type == 'attention',]
 attention_model <- rma(yi, vi, data = attention)
 attention_results <- ma_pipe_sei(
   attention,
   true_effect = attention_model$b,
   rep_lower = attention_model$ci.lb,
   rep_upper = attention_model$ci.ub,
   analysis_title = "Working memory tasks",
   plot = TRUE
 )
```

![](README_files/figure-gfm/equivalence%20test%20plots-2.png)<!-- -->

    ## 

    ## 

    ## NHST: don't reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

``` r
attention_et_dat <- attention_results$et

# Subgroup analyses for each outcome (here only presented one)
inhibition_DS <- stats_test[stats_test$ef_type == 'inhibition' & stats_test$group_id == "DS",]
inhibition_model_DS <- rma(yi, vi, data = inhibition_DS)
inhibition_results_DS <- ma_pipe_sei(
  inhibition_DS,
  true_effect = inhibition_model_DS$b,
  rep_lower = inhibition_model_DS$ci.lb,
  rep_upper = inhibition_model_DS$ci.ub,
  analysis_title = "STM tasks for organic origin",
  plot = TRUE
)
```

![](README_files/figure-gfm/equivalence%20test%20plots-3.png)<!-- -->

    ## 

    ## 

    ## NHST: don't reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

``` r
inhibition_et_dat_DS <- inhibition_results_DS$et

inhibition_NSID <- stats_test[stats_test$ef_type == 'inhibition' & stats_test$group_id == "NSID",]
inhibition_model_NSID <- rma(yi, vi, data = inhibition_NSID)
inhibition_results_NSID <- ma_pipe_sei(
  inhibition_NSID,
  true_effect = inhibition_model_NSID$b,
  rep_lower = inhibition_model_NSID$ci.lb,
  rep_upper = inhibition_model_NSID$ci.ub,
  analysis_title = "STM tasks for familial origin",
  plot = TRUE
)
```

![](README_files/figure-gfm/equivalence%20test%20plots-4.png)<!-- -->

    ## 

    ## 

    ## NHST: don't reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

``` r
inhibition_et_dat_NSID <- inhibition_results_NSID$et

## equivalence forest plot
com1 <- rbind(inhibition_et_dat, attention_et_dat,
              inhibition_et_dat_DS, inhibition_et_dat_NSID)

com1 <- combine_et(com1)

com1 <- com1 + ggtitle("Summary effect sizes and equivalence bounds") +
  theme(plot.title = element_text(hjust = 0.5))

com1
```

![](README_files/figure-gfm/equivalence%20test%20plots-5.png)<!-- -->

## Sensitivity analysis

``` r
sens_inh <- cooks.distance.rma.mv(res_inhibition)
sens_att <- cooks.distance.rma.mv(res_attention)

plot(sens_inh, type="o", pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
```

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
plot(sens_att, type="o", pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
```

![](README_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
sessionInfo()
```

    ## R version 4.4.2 (2024-10-31)
    ## Platform: aarch64-apple-darwin20
    ## Running under: macOS Sequoia 15.3
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: Europe/Stockholm
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] psych_2.4.12        skimr_2.1.5         naniar_1.1.0       
    ##  [4] dmetar_0.1.0        ggrepel_0.9.6       devtools_2.4.5     
    ##  [7] usethis_3.1.0       tidyr_1.3.1         dplyr_1.1.4        
    ## [10] TOSTER_0.8.4        patchwork_1.3.0     ggthemes_5.1.0     
    ## [13] ggplot2_3.5.1       readr_2.1.5         metaviz_0.3.1      
    ## [16] reshape_0.8.9       janitor_2.2.1       readxl_1.4.3       
    ## [19] metafor_4.8-0       numDeriv_2016.8-1.1 metadat_1.4-0      
    ## [22] Matrix_1.7-1       
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] mathjaxr_1.6-0       jsonlite_1.8.9       rstudioapi_0.17.1   
    ##   [4] magrittr_2.0.3       modeltools_0.2-23    farver_2.1.2        
    ##   [7] nloptr_2.1.1         rmarkdown_2.29       fs_1.6.5            
    ##  [10] vctrs_0.6.5          memoise_2.0.1        minqa_1.2.8         
    ##  [13] base64enc_0.1-3      CompQuadForm_1.4.3   htmltools_0.5.8.1   
    ##  [16] distributional_0.5.0 cellranger_1.1.0     htmlwidgets_1.6.4   
    ##  [19] plyr_1.8.9           poibin_1.6           lubridate_1.9.4     
    ##  [22] cachem_1.1.0         igraph_2.1.4         mime_0.12           
    ##  [25] lifecycle_1.0.4      pkgconfig_2.0.3      R6_2.5.1            
    ##  [28] fastmap_1.2.0        rbibutils_2.3        shiny_1.10.0        
    ##  [31] magic_1.6-1          snakecase_0.11.1     digest_0.6.37       
    ##  [34] colorspace_2.1-1     pkgload_1.4.0        labeling_0.4.3      
    ##  [37] timechange_0.3.0     abind_1.4-8          compiler_4.4.2      
    ##  [40] remotes_2.5.0        withr_3.0.2          meta_8.0-2          
    ##  [43] pkgbuild_1.4.6       MASS_7.3-61          sessioninfo_1.2.3   
    ##  [46] tools_4.4.2          prabclus_2.3-4       httpuv_1.6.15       
    ##  [49] visdat_0.6.0         nnet_7.3-19          glue_1.8.0          
    ##  [52] nlme_3.1-166         promises_1.3.2       grid_4.4.2          
    ##  [55] cluster_2.1.6        generics_0.1.3       gtable_0.3.6        
    ##  [58] tzdb_0.4.0           class_7.3-22         hms_1.1.3           
    ##  [61] xml2_1.3.6           flexmix_2.3-19       pillar_1.10.1       
    ##  [64] ggdist_3.3.2         stringr_1.5.1        later_1.4.1         
    ##  [67] robustbase_0.99-4-1  splines_4.4.2        lattice_0.22-6      
    ##  [70] tidyselect_1.2.1     pbapply_1.7-2        miniUI_0.1.1.1      
    ##  [73] knitr_1.49           reformulas_0.4.0     gridExtra_2.3       
    ##  [76] stats4_4.4.2         xfun_0.50            diptest_0.77-1      
    ##  [79] DEoptimR_1.1-3-1     MuMIn_1.48.4         netmeta_3.1-1       
    ##  [82] stringi_1.8.4        yaml_2.3.10          boot_1.3-31         
    ##  [85] evaluate_1.0.3       kernlab_0.9-33       tibble_3.2.1        
    ##  [88] cli_3.6.3            xtable_1.8-4         Rdpack_2.6.2        
    ##  [91] repr_1.1.7           munsell_0.5.1        Rcpp_1.0.14         
    ##  [94] parallel_4.4.2       ellipsis_0.3.2       mclust_6.1.1        
    ##  [97] profvis_0.4.0        urlchecker_1.0.1     lme4_1.1-36         
    ## [100] mvtnorm_1.3-3        scales_1.3.0         crayon_1.5.3        
    ## [103] purrr_1.0.4          fpc_2.2-13           rlang_1.1.5         
    ## [106] mnormt_2.1.1         cowplot_1.1.3
