A systematic review of short-term and working memory abilities in
children with intellectual disability; to what extent do the result
support the delay or difference hypotheses?
================
Cris
Last Updated: 04, February, 2025 at 18:10

## Introduction

## Aim

The aim of the study is to investigate if short-term and working memory
abilities exhibit a developmentally delayed or developmentally different
pattern in children with ID. This will be done by an overall analysis
and subgroup analyses based on type of memory (short-term or working
memory), aetiology of ID (familial or organic). In addition, the overall
analysis will also be repeated with level of IQ as a moderator.

### Research questions

1.  Do short-term and working memory abilities exhibit a developmentally
    delayed or developmentally different pattern in children with ID
    (compared to mental age matched groups)? 1.1 Is this pattern
    moderated by origin of ID (familial and organic) or level of IQ?
2.  Do origin (familial and organic) of ID or level of IQ moderate
    whether STM and WM performance are delayed or different?

### Hypotheses

1.  The STM delay hypothesis will be supported if STM performance of
    participants with ID will not differ significantly from that of the
    mental age-matched group

2.  The WM delay hypothesis will be supported if WM performance of
    participants with ID will not differ significantly from that of the
    mental age-matched group

3.  The STM difference hypothesis will be supported if STM performance
    of participants with ID will differ significantly from that of the
    mental age-matched group. We expect that the difference may occur in
    any direction.

4.  The WM difference hypothesis will be supported if WM performance of
    participants with ID will differ significantly from that of the
    mental age-matched group. We expect that the difference may occur in
    any direction.

## Methods

### Data extraction

We conducted and reported this systematic review according to NIRO-SR
guidelines for conducting and reporting systematic reviews of
non-intervention research (Topor & Pickering et al., 2023). Elements
pertaining to the method and results of the meta-analysis were reported
following the PRISMA 2020 guidelines (Page et al., 2021).

### Search Strategy

To capture all studies relevant to our research questions we conducted
literature searches in the following databases: MEDLINE PubMed Central
(PMC), and NCBI Bookshelf, through PUBMED, SCOPUS (Elsevier), Web of
Science Core Collection (All Editions), ERIC and PsycINFO through
EBSCOHost. All databases were accessed through the Linköping University
library.

Further, we searched grey literature through the SCOPUS database,
including the preprint feature, as well as Google Scholar and BASE
(Bielefeld Academic Search Engine) for additional grey literature. For
search in Google Scholar, we limited the retrieval of references to the
first 500 hits to ensure relevance and limit redundancy. We also
performed backward and forward reference searches of the included
studies, which we conducted in Lens.org. Lastly, we screened references
from systematic reviews that were caught during the literature search
looking for studies that fell within our inclusion criteria, also done
in Lens.org.

Database specific search strings are available in the project’s OSF
(Supplement XX Search Strategy). The general search terms that were used
are:

- Population – diagnosis: intellectual disability, Trisomy 21, Fragile
  X, Williams syndrome, 22q11.2 deletion syndrome, Angelmans syndrome,
  Retts syndrome, Prader Willis syndrome, CHARGE syndrome, Cri du chat
  syndrome, Dandy-Walker Syndrome, Tuberous sclerosis, Wiedemann-Steiner

- Population – age: children, adolescent, under 18

- Independent variable: mental age match

- Dependent variable: short term memory, working memory

### Statistical analysis

All statistical analyses and data processing were performed using R
(4.3.1, R Core Team, 2024), and all effect size calculations and coding
procedures are available on OSF. For the effect size, we calculated
standardized mean differences (as Hedges’ g) using the metafor package
(Viechtabuer, 2010). All other packages used to visualise and present
results are cited in the RMarkdown on OSF. We conducted a multi-level
random-effects meta-analysis using the restricted maximum likelihood
variance (tau2) estimator (REML). We conducted separate meta-analyses
for the following: one meta-analysis including STM tasks, and one
including WM tasks. Calculated effects are plotted together with their
95% confidence intervals. See Table 2 for a description of all conducted
analyses. We conducted a 3-level RE meta-analysis to account for
multiple effect sizes (i.e., multiple tasks measuring the same outcome)
nested within studies. Following recommendations by Assink & Wibbelink
(2016) and Viechtbauer (metafor documentation, Viechtbauer, 2010), we
modelled random effects for the study level and random effects for the
tasks nested within those studies.

To be able to completely answer the primary research question, we
conducted equivalence tests of the average effect size. Following
recommendations from Lakens (2017, 2018), we conducted the equivalence
test using TOSTER package (Caldwell, 2022) to perform two one-sided
tests (TOST). This allows us to falsify the delay or difference
hypothesis by assessing if the effects statistically differ and if they
differ “meaningfully” from no effect (i.e., nil null-hypothesis).
Meaningful is however a subjective description, and literature has so
far not focused on establishing guidelines on evaluating the strength of
the relationship between executive functions and ID population. Thus, we
decided to set the equivalence bounds to the average effect size of g =
0.34 found in a meta-analysis by Spaniol & Danielsson (2022). This
meta-analysis had similar, albeit broader, inclusion criteria, focusing
on more aspects of executive function. Alpha level was set to α = .05.

The criteria to falsify the delay and difference hypotheses for STM and
WM would be as follows: 1. a statistically nonsignificant and not
equivalent effect supports the delay hypothesis.

2.  a statistically significant (in any direction) and not equivalent
    effect supports the difference hypothesis.

3.  a statistically nonsignificant and equivalent effect supports the
    delay hypothesis.

4.  a statistically significant (in any direction) and equivalent effect
    would indicate designs that have many participants or otherwise high
    power to find any effects significant, although not practically
    meaningful. This finding would again support the delay hypothesis.

Moderator analyses

To address the secondary research questions, we conducted moderator
analyses using metafor. We conducted a meta-regression with IQ of the ID
group as a continuous predictor centred around the grand mean of the ID
group, once for each meta-analysis (i.e., one with STM outcomes and one
with WM outcomes). Here, the equivalence test was not applicable as only
the potential presence of the effect of IQ (and not its lack) was
relevant to our research question.

We also conducted subgroup analyses based on the origin of ID
(i.e. familial or organic). This solution was appropriate as only if
there were more than five studies per group\*. We conducted a
complete-case meta-regression as we expected that potential missing
values will only be randomly missing (e.g., simply not reported).
Subgroup analyseis wereas conducted by modelling two independent
random-effect meta-analyses for each subgroup. Studies with mixed
populations that did not provide individual estimates for each group
were excluded from the subgroup analysis. We then compared the effect
sizes by conducting a fixed-effects meta-analysis of the two estimates,
essentially conducting a Wald-type test, as suggested by Viechtbauer
(2024, see Comparing Estimates of Independent Meta-Analyses or
Subgroups). Agan, the equivalence test was not applicable as only the
potential presence of the effect of ID origin (and not its lack) was
relevant to our research question.

## Results

### Descriptives

We conducted and reported this systematic review according to NIRO-SR
guidelines for conducting and reporting systematic reviews of
non-intervention research (REF). See table X for descriptives of the
obtained studies.

``` r
skim(data[, columns_of_interest])
```

|                                                  |                             |
|:-------------------------------------------------|:----------------------------|
| Name                                             | data\[, columns_of_interes… |
| Number of rows                                   | 99                          |
| Number of columns                                | 14                          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                             |
| Column type frequency:                           |                             |
| numeric                                          | 14                          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                             |
| Group variables                                  | None                        |

Data summary

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate |    mean |     sd |      p0 |     p25 |     p50 |     p75 |    p100 | hist  |
|:----------------|----------:|--------------:|--------:|-------:|--------:|--------:|--------:|--------:|--------:|:------|
| study_year      |         0 |          1.00 | 2009.13 |  11.15 | 1962.00 | 2010.00 | 2012.00 | 2013.00 | 2020.00 | ▁▁▁▁▇ |
| n_id            |         0 |          1.00 |   22.46 |  11.19 |   10.00 |   15.00 |   19.00 |   26.00 |   68.00 | ▇▆▁▁▁ |
| age_id          |         3 |          0.97 |   16.31 |   6.24 |    4.34 |   13.59 |   14.98 |   17.73 |   49.88 | ▃▇▁▁▁ |
| mean_ma_id      |         2 |          0.98 |    6.35 |   1.26 |    1.88 |    5.75 |    6.10 |    6.74 |   11.17 | ▁▂▇▁▁ |
| sd_ma_id        |        29 |          0.71 |    2.06 |   4.82 |    0.37 |    0.67 |    0.93 |    1.56 |   24.61 | ▇▁▁▁▁ |
| iq_id           |        10 |          0.90 |   48.78 |   8.50 |   19.92 |   44.17 |   53.00 |   53.30 |   65.75 | ▁▂▂▇▁ |
| n_magroup       |         0 |          1.00 |   25.78 |  14.07 |   10.00 |   15.00 |   22.00 |   28.00 |   65.00 | ▇▅▁▁▁ |
| age_magroup     |         9 |          0.91 |    6.57 |   1.32 |    1.93 |    5.75 |    6.27 |    7.55 |   11.17 | ▁▂▇▃▁ |
| mean_ma_magroup |        13 |          0.87 |    6.58 |   1.24 |    1.90 |    5.98 |    6.71 |    6.90 |    9.76 | ▁▂▇▇▂ |
| iq_magroup      |        50 |          0.49 |  102.73 |   8.78 |   94.00 |   95.70 |   95.70 |  109.00 |  123.56 | ▇▁▅▁▁ |
| mean_id         |         0 |          1.00 |   53.96 | 164.97 |    0.04 |    3.84 |    9.93 |   22.74 | 1096.20 | ▇▁▁▁▁ |
| mean_magroup    |         0 |          1.00 |   52.19 | 165.84 |    0.03 |    3.78 |    9.67 |   22.81 | 1258.00 | ▇▁▁▁▁ |
| sd_id           |         0 |          1.00 |   15.64 |  36.80 |    0.19 |    2.38 |    5.01 |   10.50 |  251.50 | ▇▁▁▁▁ |
| sd_magroup      |         0 |          1.00 |   12.10 |  32.96 |    0.13 |    1.90 |    3.50 |    5.52 |  277.90 | ▇▁▁▁▁ |

For both groups, we extracted the iq, number of participants, mean and
sd of the performance in the tasks, and the mean and sd of the
chronological and mental age. There was a total of an 8.4% of missing
data with the iq of the control group missing 51% of the times

``` r
vis_miss(data[,columns_of_interest])
```

![](README_files/figure-gfm/descriptives-1.png)<!-- -->

### Statistical analysis

We are performing a multilevel analysis. When the data is extracted, we
will separate WM from STM tasks.

``` r
overall_3_level <- rma.mv(yi, vi, random = list(~ 1 | effectsize_id, ~1 | study_id), 
                          tdist = TRUE, data = stats_test)
 
 
#two-level model without within-study variance (to estimate the between-study variance)
modelnovar2 <- rma.mv(yi, vi, random = list(~ 1 | effectsize_id, ~1 | study_id), 
                      sigma2 = c(0,NA), tdist = TRUE, data = stats_test)
 
anova(overall_3_level,modelnovar2)
```

    ## 
    ##         df      AIC      BIC     AICc    logLik      LRT   pval       QE 
    ## Full     3 287.2253 294.9802 287.4807 -140.6127                 434.0747 
    ## Reduced  2 414.6042 419.7741 414.7305 -205.3021 129.3788 <.0001 434.0747

``` r
#build a two-level model without between-study variance (to estimate the within-study variance);
 
modelnovar3 <- rma.mv(yi, vi, random = list(~ 1 | effectsize_id, ~ 1 | study_id), 
                      sigma2 = c(NA,0), tdist = TRUE, data = stats_test)
 
anova(overall_3_level,modelnovar3)
```

    ## 
    ##         df      AIC      BIC     AICc    logLik    LRT   pval       QE 
    ## Full     3 287.2253 294.9802 287.4807 -140.6127               434.0747 
    ## Reduced  2 285.2253 290.3953 285.3517 -140.6127 0.0000 1.0000 434.0747

Moderating type of ID: in our study we will have familial and organic
ID.

``` r
res_DS <- rma(yi, vi, data=stats_test, subset=group_id=="DS")
res_NSID <- rma(yi, vi, data=stats_test, subset=group_id=="NSID")
res_FXS <- rma(yi, vi, data=stats_test, subset=group_id=="FXS")
res_WS <- rma(yi, vi, data=stats_test, subset=group_id=="WS")

dat.comp <- data.frame(alloc    = c("DS", "NSID", "FXS", "WS"), 
                       estimate = c(coef(res_DS), coef(res_NSID), coef(res_FXS), coef(res_WS)), 
                       stderror = c(res_DS$se, res_NSID$se, res_FXS$se, res_WS$se),
                       tau2     = c(res_DS$tau2, res_NSID$tau2, res_FXS$tau2, res_WS$tau2))
dfround(dat.comp, 3)
```

    ##   alloc estimate stderror  tau2
    ## 1    DS   -0.175    0.119 0.301
    ## 2  NSID   -0.106    0.120 0.243
    ## 3   FXS   -0.287    0.526 1.550
    ## 4    WS    0.044    0.210 1.120

``` r
rma(estimate, sei=stderror, mods = ~ alloc, method="FE", data=dat.comp, digits=3)
```

    ## 
    ## Fixed-Effects with Moderators Model (k = 4)
    ## 
    ## I^2 (residual heterogeneity / unaccounted variability): 0.00%
    ## H^2 (unaccounted variability / sampling variability):   1.00
    ## R^2 (amount of heterogeneity accounted for):            NA%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 0) = 0.000, p-val = 1.000
    ## 
    ## Test of Moderators (coefficients 2:4):
    ## QM(df = 3) = 0.939, p-val = 0.816
    ## 
    ## Model Results:
    ## 
    ##            estimate     se    zval   pval   ci.lb  ci.ub    
    ## intrcpt      -0.175  0.119  -1.474  0.140  -0.408  0.058    
    ## allocFXS     -0.112  0.539  -0.208  0.835  -1.169  0.945    
    ## allocNSID     0.069  0.169   0.409  0.682  -0.262  0.400    
    ## allocWS       0.219  0.241   0.908  0.364  -0.254  0.692    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
id_tests <- t(data.frame(DS = c(res_DS$b, res_DS$se,res_DS$ci.lb, res_DS$ci.ub),
                         NSID = c(res_NSID$b, res_NSID$se,res_NSID$ci.lb, res_NSID$ci.ub),
                         FXS = c(res_FXS$b, res_FXS$se,res_FXS$ci.lb, res_FXS$ci.ub),
                         WS = c(res_WS$b, res_WS$se,res_WS$ci.lb, res_WS$ci.ub),
                         row.names = c('b', 'se', 'ci.lb', 'ci.ub')))
for (id in c('DS', 'NSID', 'FXS', 'WS')){
  df <- stats_test[stats_test$group_id == id, c('cite', 'yi', 'sei', 'n_magroup', 'n_id')]
  
  # Compute confidence intervals and format labels
  
  df$cite <- paste0(df$cite, '.', ave(df$cite, df$cite, FUN = seq_along))
  df <- df %>%
    mutate(
      lower = yi - 1.96 * sei,  
      upper = yi + 1.96 * sei,  
      ci_text = paste0(round(yi,2), "  [", round(lower, 2), ", ", round(upper, 2), "]")
    )
  FE_model <- paste0('F.E Model for ', id, ' group')
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
      x = "Effect Size (Cohen's d)",
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
    annotate(geom="text", x=1.4, y=dim(df)[1] + 1, label="E.F [95% CI]", fontface = "bold") +
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

![](README_files/figure-gfm/plots%20type%20of%20ID,%20-1.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20ID,%20-2.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20ID,%20-3.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20ID,%20-4.png)<!-- -->

Moderating type of executive function: in our study we will have WM and
STM tasks

``` r
res_inhibition <- rma(yi, vi, data=stats_test, subset=ef_type=="inhibition")
res_updating <- rma(yi, vi, data=stats_test, subset=ef_type=="updating")
res_shifting <- rma(yi, vi, data=stats_test, subset=ef_type=="shifting")
res_fluency <- rma(yi, vi, data=stats_test, subset=ef_type=="fluency")
res_attention <- rma(yi, vi, data=stats_test, subset=ef_type=="attention")
res_other <- rma(yi, vi, data=stats_test, subset=ef_type=="other")

dat.comp <- data.frame(alloc    = c("Inhibition", "Updating", "Shifting", "Fluency", 'Attention', 'Other'), 
                       estimate = c(coef(res_inhibition), coef(res_updating), coef(res_shifting), coef(res_fluency), coef(res_attention), coef(res_other)), 
                       stderror = c(res_inhibition$se, res_updating$se, res_shifting$se, res_fluency$se, res_attention$se, res_other$se),
                       tau2     = c(res_inhibition$tau2, res_updating$tau2, res_shifting$tau2, res_fluency$tau2, res_attention$tau2, res_other$tau2))
dfround(dat.comp, 3)
```

    ##        alloc estimate stderror  tau2
    ## 1 Inhibition   -0.105    0.180 0.900
    ## 2   Updating   -0.549    0.275 0.000
    ## 3   Shifting    0.173    0.154 0.406
    ## 4    Fluency   -0.108    0.207 0.379
    ## 5  Attention   -0.054    0.203 0.437
    ## 6      Other   -0.624    0.261 0.456

``` r
rma(estimate, sei=stderror, mods = ~ alloc, method="FE", data=dat.comp, digits=3)
```

    ## 
    ## Fixed-Effects with Moderators Model (k = 6)
    ## 
    ## I^2 (residual heterogeneity / unaccounted variability): 0.00%
    ## H^2 (unaccounted variability / sampling variability):   1.00
    ## R^2 (amount of heterogeneity accounted for):            NA%
    ## 
    ## Test for Residual Heterogeneity:
    ## QE(df = 0) = 0.000, p-val = 1.000
    ## 
    ## Test of Moderators (coefficients 2:6):
    ## QM(df = 5) = 9.882, p-val = 0.079
    ## 
    ## Model Results:
    ## 
    ##                  estimate     se    zval   pval   ci.lb  ci.ub    
    ## intrcpt            -0.054  0.203  -0.268  0.788  -0.452  0.343    
    ## allocFluency       -0.053  0.290  -0.184  0.854  -0.621  0.515    
    ## allocInhibition    -0.051  0.272  -0.187  0.852  -0.583  0.481    
    ## allocOther         -0.570  0.331  -1.723  0.085  -1.218  0.079  . 
    ## allocShifting       0.228  0.255   0.893  0.372  -0.272  0.727    
    ## allocUpdating      -0.495  0.342  -1.448  0.148  -1.165  0.175    
    ## 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
id_tests <- t(data.frame(inhibition = c(res_inhibition$b, res_inhibition$se,res_inhibition$ci.lb, res_inhibition$ci.ub),
                         updating = c(res_updating$b, res_updating$se,res_updating$ci.lb, res_updating$ci.ub),
                         shifting = c(res_shifting$b, res_shifting$se,res_shifting$ci.lb, res_shifting$ci.ub),
                         fluency = c(res_fluency$b, res_fluency$se,res_fluency$ci.lb, res_fluency$ci.ub),
                         attention = c(res_attention$b, res_attention$se,res_attention$ci.lb, res_attention$ci.ub),
                         other = c(res_other$b, res_other$se,res_other$ci.lb, res_other$ci.ub),
                         
                         row.names = c('b', 'se', 'ci.lb', 'ci.ub')))
for (id in c("inhibition", "updating", "shifting", "fluency", 'attention', 'other')){
  df <- stats_test[stats_test$ef_type == id, c('cite', 'yi', 'sei', 'n_magroup', 'n_id')]
  
  # Compute confidence intervals and format labels
  
  df$cite <- paste0(df$cite, '.', ave(df$cite, df$cite, FUN = seq_along))
  df <- df %>%
    mutate(
      lower = yi - 1.96 * sei,  
      upper = yi + 1.96 * sei,  
      ci_text = paste0(round(yi,2), "  [", round(lower, 2), ", ", round(upper, 2), "]")
    )
  FE_model <- paste0('F.E Model for ', id, ' group')
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
      x = "Effect Size (Cohen's d)",
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
    annotate(geom="text", x=1.4, y=dim(df)[1] + 1, label="E.F [95% CI]", fontface = "bold") +
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

![](README_files/figure-gfm/plots%20type%20of%20EF-1.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20EF-2.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20EF-3.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20EF-4.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20EF-5.png)<!-- -->![](README_files/figure-gfm/plots%20type%20of%20EF-6.png)<!-- -->

### Equivalence testing

The studies looked at different types of executive functions (CHANGE
THIS LATER TO SUIT OUR OWN ANALYSIS). In the next table you can see the
extracted descriptives separated by executive function

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
inhibition <- stats_test[stats_test$ef_type == 'inhibition',]
inhibition_model <- rma(yi, vi, data = inhibition)
inhibition_results <- ma_pipe_sei(
  inhibition,
  true_effect = inhibition_model$b,
  rep_lower = inhibition_model$ci.lb,
  rep_upper = inhibition_model$ci.ub,
  analysis_title = "inhibition tasks",
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
inhibition_power_median_dat <- inhibition_results$power_median_dat
inhibition_power_median_dat$ES <- mean(inhibition$yi)
inhibition_et_dat <- inhibition_results$et

updating <- stats_test[stats_test$ef_type == 'updating',]
updating_model <- rma(yi, vi, data = updating)
updating_results <- ma_pipe_sei(
  updating,
  true_effect = updating_model$b,
  rep_lower = updating_model$ci.lb,
  rep_upper = updating_model$ci.ub,
  analysis_title = "updating tasks",
  plot = TRUE
)
```

![](README_files/figure-gfm/equivalence%20test%20plots-2.png)<!-- -->

    ## 

    ## 

    ## NHST: reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

``` r
updating_power_median_dat <- updating_results$power_median_dat
updating_power_median_dat$ES <- mean(updating$yi)
updating_et_dat <- updating_results$et

shifting <- stats_test[stats_test$ef_type == 'shifting',]
shifting_model <- rma(yi, vi, data = shifting)
shifting_results <- ma_pipe_sei(
  shifting,
  true_effect = shifting_model$b,
  rep_lower = shifting_model$ci.lb,
  rep_upper = shifting_model$ci.ub,
  analysis_title = "shifting tasks",
  plot = TRUE
)
```

![](README_files/figure-gfm/equivalence%20test%20plots-3.png)<!-- -->

    ## 

    ## 

    ## NHST: don't reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

``` r
shifting_power_median_dat <- shifting_results$power_median_dat
shifting_power_median_dat$ES <- mean(shifting$yi)
shifting_et_dat <- shifting_results$et

fluency <- stats_test[stats_test$ef_type == 'fluency',]
fluency_model <- rma(yi, vi, data = fluency)
fluency_results <- ma_pipe_sei(
  fluency,
  true_effect = fluency_model$b,
  rep_lower = fluency_model$ci.lb,
  rep_upper = fluency_model$ci.ub,
  analysis_title = "fluency tasks",
  plot = TRUE
)
```

![](README_files/figure-gfm/equivalence%20test%20plots-4.png)<!-- -->

    ## 

    ## 

    ## NHST: don't reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

``` r
fluency_power_median_dat <- fluency_results$power_median_dat
fluency_power_median_dat$ES <- mean(fluency$yi)
fluency_et_dat <- fluency_results$et

attention <- stats_test[stats_test$ef_type == 'attention',]
attention_model <- rma(yi, vi, data = attention)
attention_results <- ma_pipe_sei(
  attention,
  true_effect = attention_model$b,
  rep_lower = attention_model$ci.lb,
  rep_upper = attention_model$ci.ub,
  analysis_title = "attention tasks",
  plot = TRUE
)
```

![](README_files/figure-gfm/equivalence%20test%20plots-5.png)<!-- -->

    ## 

    ## 

    ## NHST: don't reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

``` r
attention_power_median_dat <- attention_results$power_median_dat
attention_power_median_dat$ES <- mean(attention$yi)
attention_et_dat <- attention_results$et


other <- stats_test[stats_test$ef_type == 'other',]
other_model <- rma(yi, vi, data = other)
other_results <- ma_pipe_sei(
  other,
  true_effect = other_model$b,
  rep_lower = other_model$ci.lb,
  rep_upper = other_model$ci.ub,
  analysis_title = "other tasks",
  plot = TRUE
)
```

![](README_files/figure-gfm/equivalence%20test%20plots-6.png)<!-- -->

    ## 

    ## 

    ## NHST: reject null significance hypothesis that the effect is equal to 0 
    ## TOST: don't reject null equivalence hypothesis

``` r
other_power_median_dat <- other_results$power_median_dat
other_power_median_dat$ES <- mean(other$yi)
other_et_dat <- other_results$et

com1 <- rbind(inhibition_et_dat,
              updating_et_dat,
              shifting_et_dat,
              fluency_et_dat,
              attention_et_dat, 
              other_et_dat)
com1 <- combine_et(com1)
com1 <- com1 + ggtitle("Summary effect sizes and equivalence bounds") +
  theme(plot.title = element_text(hjust = 0.5))


# Create the meta-analysis power tile supplement 

power_med <- rbind(inhibition_power_median_dat,
              updating_power_median_dat,
              shifting_power_median_dat,
              fluency_power_median_dat,
              attention_power_median_dat, 
              other_power_median_dat)

power_med_long <- gather(power_med, effect, power, observed:e66, factor_key=TRUE)

tile <- ggplot(data = power_med_long, aes(x = effect, y = reorder(analysis, -ES),)) + 
  geom_tile(aes(fill = power)) + 
  coord_equal(ratio = 0.8) + 
  scale_fill_gradient(name = "Power", low = "#FDF0FF", high = "#E200FD") + theme_tufte(base_family="Helvetica")

tile <- tile + theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank())

tile <- tile + ggtitle("Median statistical power") +
  xlab("Effect size") +
  theme(plot.title = element_text(hjust = 0.5))

tile <- tile + scale_x_discrete(labels=c("observed" = "Observed \n effect", "e33" = "0.33",
                                         "e66" = "0.66"))
tile
```

![](README_files/figure-gfm/equivalence%20test%20plots-7.png)<!-- -->

``` r
# Combine the two plots
com1 + tile
```

![](README_files/figure-gfm/equivalence%20test%20plots-8.png)<!-- -->

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16)
    ## Platform: aarch64-apple-darwin20 (64-bit)
    ## Running under: macOS 15.2
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
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
    ##  [7] usethis_2.2.3       tidyr_1.3.1         dplyr_1.1.4        
    ## [10] TOSTER_0.8.3        patchwork_1.3.0     ggthemes_5.1.0     
    ## [13] ggplot2_3.5.1       readr_2.1.5         metaviz_0.3.1      
    ## [16] reshape_0.8.9       janitor_2.2.1       readxl_1.4.3       
    ## [19] metafor_4.6-0       numDeriv_2016.8-1.1 metadat_1.2-0      
    ## [22] Matrix_1.6-5       
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] RColorBrewer_1.1-3   mathjaxr_1.6-0       rstudioapi_0.15.0   
    ##   [4] jsonlite_1.8.9       magrittr_2.0.3       modeltools_0.2-23   
    ##   [7] farver_2.1.2         nloptr_2.1.1         rmarkdown_2.29      
    ##  [10] fs_1.6.5             vctrs_0.6.5          memoise_2.0.1       
    ##  [13] minqa_1.2.8          CompQuadForm_1.4.3   base64enc_0.1-3     
    ##  [16] htmltools_0.5.8.1    distributional_0.4.0 cellranger_1.1.0    
    ##  [19] htmlwidgets_1.6.4    plyr_1.8.9           poibin_1.6          
    ##  [22] lubridate_1.9.4      cachem_1.1.0         mime_0.12           
    ##  [25] lifecycle_1.0.4      pkgconfig_2.0.3      R6_2.5.1            
    ##  [28] fastmap_1.2.0        rbibutils_2.3        shiny_1.8.0         
    ##  [31] magic_1.6-1          snakecase_0.11.1     digest_0.6.37       
    ##  [34] colorspace_2.1-1     pkgload_1.3.4        labeling_0.4.3      
    ##  [37] timechange_0.3.0     abind_1.4-8          compiler_4.3.1      
    ##  [40] remotes_2.4.2.1      withr_3.0.2          meta_8.0-1          
    ##  [43] pkgbuild_1.4.3       MASS_7.3-60          sessioninfo_1.2.2   
    ##  [46] tools_4.3.1          prabclus_2.3-4       httpuv_1.6.14       
    ##  [49] visdat_0.6.0         nnet_7.3-19          glue_1.8.0          
    ##  [52] nlme_3.1-162         promises_1.2.1       grid_4.3.1          
    ##  [55] cluster_2.1.4        generics_0.1.3       gtable_0.3.6        
    ##  [58] tzdb_0.4.0           class_7.3-22         hms_1.1.3           
    ##  [61] xml2_1.3.6           flexmix_2.3-19       pillar_1.10.1       
    ##  [64] ggdist_3.3.2         stringr_1.5.1        later_1.3.2         
    ##  [67] robustbase_0.99-4-1  splines_4.3.1        lattice_0.21-8      
    ##  [70] tidyselect_1.2.1     miniUI_0.1.1.1       knitr_1.49          
    ##  [73] reformulas_0.4.0     gridExtra_2.3        stats4_4.3.1        
    ##  [76] xfun_0.50            diptest_0.77-1       DEoptimR_1.1-3-1    
    ##  [79] MuMIn_1.48.4         netmeta_2.9-0        stringi_1.8.4       
    ##  [82] yaml_2.3.10          boot_1.3-28.1        evaluate_1.0.3      
    ##  [85] kernlab_0.9-33       tibble_3.2.1         cli_3.6.3           
    ##  [88] xtable_1.8-4         Rdpack_2.6.2         repr_1.1.6          
    ##  [91] munsell_0.5.1        Rcpp_1.0.14          parallel_4.3.1      
    ##  [94] ellipsis_0.3.2       mclust_6.1.1         profvis_0.3.8       
    ##  [97] urlchecker_1.0.1     lme4_1.1-36          scales_1.3.0        
    ## [100] purrr_1.0.2          fpc_2.2-13           rlang_1.1.4         
    ## [103] mnormt_2.1.1         cowplot_1.1.3
