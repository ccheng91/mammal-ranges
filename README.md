Manmal range explore
================
Cheng
5/26/2021

## Preliminary Modeling

### Modeling framework

We assigned all species that are detected at each site to three
categories: Type B match: Species detected by both Camera + IUCN ( coded
as `0` in the model). Type A match: Species detected by camera only
(coded as `1)`. Type C match: Species detected by IUCN only (also coded
as `1` in the model).

We ran two separate binomial mixed-effect models to examine what factors
are associated with errors of the IUCN range map. We Compared type A,
type B matches to assess the potential with <u>omission error</u> and
type C and type B for <u>commission error</u>.

The binomial mixed-effects model looks like this:

### Candidate variables

| Variable                    | Description                                                                                 | Prediction                                                                                                                                      |
|-----------------------------|---------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|
| <u>First tier variable</u>  |                                                                                             |                                                                                                                                                 |
| Body mass                   | Previous study found that body mass is associated with two types of error of IUCN range map | Body mass will have negative effect on both Commission and Omission errors: large mammal’s distribution data is more accurate. Hence more match |
| Realm                       | Previous studies show that well IUCN range map is more accurate in well studied region      | Some realm will have significantly better match. Realm inculdes Afrotropical, Indo-Malay, Nearctic, Neotropical                                 |
| IUCN\_frq                   | Total IUCN assessment of a species                                                          | Negative effect on both errors. More assessment                                                                                                 |
| IUCN\_year                  | Year of the latest IUCN assessment of a species                                             | Negative effect on both errors. Recent IUCN range map is more accurate                                                                          |
| Camera trap sampling effort | Camera trap days                                                                            | Negative effect. The more sampling efforts less                                                                                                 |
| <u>Second tier variable</u> |                                                                                             |                                                                                                                                                 |
| ForStrat.Value              | Forgoing stratum. Forgoing strata G (ground), S (sub-canopy), Ar (Arboreal)                 | Arboreal species has more error                                                                                                                 |
| Activity Nocturnal          | Nocturnality: Nocturnal (1), other (0)                                                      | Positive effect                                                                                                                                 |
| diet.breadth                | Number of diet a species has, 1 - 6. large -&gt; generalists                                | Negative effect                                                                                                                                 |
| Tree\_mean                  | Three height: Index of habitat complexity. From 1.8 to 38.8 (m)                             | Positive effect                                                                                                                                 |
| <u>Random effect</u>        |                                                                                             |                                                                                                                                                 |
| Species                     | Potential random effects                                                                    | Same species has same species covariates                                                                                                        |
| ProjectID                   | Potential random effects 2                                                                  | Same sites has same site covariates                                                                                                             |

### Do we need random effect?

We generated fixed-effects minimal base-line models and three base-line
mixed-model using the “glmer” function with random intercepts for
species, projectID, and species + projectID. We can then check if
including the random effect is permitted by comparing the AICs from the
glm to AIC from the glmer model. If the AIC of the glmer object is
smaller than the AIC of the glm object, then this indicates that
including random intercepts is justified.

``` r
# baseline model glm for omission error
m0.glm.omi <-  glm(type ~ 1, family = binomial, data = omi) 

# add ´control = glmerControl(optimizer = “bobyqa”)´ to avoid unnecessary failures to converge.
# base-line mixed-model 1
m0.glmer1.omi<- glmer(type ~ (1|speciesScientificName), data = omi, family = binomial, control=glmerControl(optimizer="bobyqa"))

# base-line mixed-model 2
m0.glmer2.omi<- glmer(type ~ (1|projectID) , data = omi, family = binomial, control=glmerControl(optimizer="bobyqa"))

# base-line mixed-model 3
m0.glmer3.omi<- glmer(type ~ (1|speciesScientificName) + (1|projectID) , data = omi, family = binomial, control=glmerControl(optimizer="bobyqa"))



# baseline model glm commission error
m0.glm.com <-  glm(type ~ 1, family = binomial, data = com) 

# base-line mixed-model 1
m0.glmer1.com<- glmer(type ~ (1|speciesScientificName), data = com, family = binomial, control=glmerControl(optimizer="bobyqa"))

# base-line mixed-model 2
m0.glmer2.com<- glmer(type ~ (1|projectID) , data = com, family = binomial, control=glmerControl(optimizer="bobyqa"))

# base-line mixed-model 3
m0.glmer3.com<- glmer(type ~ (1|speciesScientificName) + (1|projectID) , data = com, family = binomial, control=glmerControl(optimizer="bobyqa"))
```

Now, we check if including the random effect is permitted by comparing
the AICs from the glm to AIC from the glmer model. If the AIC of the
glmer object is smaller than the AIC of the glm object, then this
indicates that including random intercepts is justified.

``` r
# Omission error
aic.glm <- AIC(logLik(m0.glm.omi))
aic.glmer1 <- AIC(logLik(m0.glmer1.omi))
aic.glmer2 <- AIC(logLik(m0.glmer2.omi))
aic.glmer3 <- AIC(logLik(m0.glmer3.omi))

aic.glm;aic.glmer1;aic.glmer2;aic.glmer3
```

    ## [1] 943.08

    ## [1] 626.3468

    ## [1] 939.5412

    ## [1] 599.0271

Same thing with the commission error.

``` r
# Commission error
aic.glm <- AIC(logLik(m0.glm.com))
aic.glmer1 <- AIC(logLik(m0.glmer1.com))
aic.glmer2 <- AIC(logLik(m0.glmer2.com))
aic.glmer3 <- AIC(logLik(m0.glmer3.com))

aic.glm;aic.glmer1;aic.glmer2;aic.glmer3
```

    ## [1] 2829.176

    ## [1] 2377.584

    ## [1] 2509.332

    ## [1] 2106.953

##### The AIC of the glmer3 object is smallest shows that including the two random intercepts (species + project) is justified.

## Model structure

We fit the full model of all first tier variables.

``` r
z1 <- glmer(type ~ BodyMass.Value + realm + IUCN_frq + IUCN_year + sampling_effort_t + (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = omi )

summary(z1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## type ~ BodyMass.Value + realm + IUCN_frq + IUCN_year + sampling_effort_t +  
    ##     (1 | speciesScientificName) + (1 | projectID)
    ##    Data: omi
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    608.3    658.0   -294.1    588.3     1053 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2677 -0.0121 -0.0066 -0.0021  4.3085 
    ## 
    ## Random effects:
    ##  Groups                Name        Variance Std.Dev.
    ##  speciesScientificName (Intercept) 281.791  16.787  
    ##  projectID             (Intercept)   3.432   1.852  
    ## Number of obs: 1063, groups:  speciesScientificName, 319; projectID, 46
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -8.56988    1.48422  -5.774 7.74e-09 ***
    ## BodyMass.Value     0.21030    0.74060   0.284    0.776    
    ## realmIndo-Malay   -2.25812    1.99259  -1.133    0.257    
    ## realmNearctic      0.46038    1.31946   0.349    0.727    
    ## realmNeotropical  -0.69394    1.50594  -0.461    0.645    
    ## IUCN_frq          -0.31663    0.29299  -1.081    0.280    
    ## IUCN_year         -0.30838    0.53728  -0.574    0.566    
    ## sampling_effort_t -0.04277    0.46271  -0.092    0.926    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) BdyM.V rlmI-M rlmNrc rlmNtr IUCN_f IUCN_y
    ## BodyMass.Vl  0.013                                          
    ## relmInd-Mly -0.143  0.033                                   
    ## realmNerctc -0.362  0.088  0.350                            
    ## realmNtrpcl -0.091  0.045  0.231  0.364                     
    ## IUCN_frq    -0.712 -0.083  0.038  0.052 -0.105              
    ## IUCN_year    0.044 -0.139  0.002 -0.023  0.091 -0.037       
    ## smplng_ffr_  0.077 -0.002  0.095 -0.267 -0.092 -0.040 -0.010

In a better table:

``` r
# summarize tabulated form 
sjPlot::tab_model(z1)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
type
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00 – 0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>&lt;0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
BodyMass.Value
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.23
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.29 – 5.27
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.776
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Indo-Malay\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00 – 5.19
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.257
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Nearctic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.58
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.12 – 21.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.727
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Neotropical\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.50
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.03 – 9.56
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.645
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_frq
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.73
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.41 – 1.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.280
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_year
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.73
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.26 – 2.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.566
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
sampling\_effort\_t
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.96
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.39 – 2.37
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.926
</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.29
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
281.79
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.43
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.99
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
319
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
46
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
1063
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.006 / 0.989
</td>
</tr>
</table>

<u>None of factors are significant.</u>

#### What about the commission error??

``` r
z2 <- glmer(type ~ BodyMass.Value + realm + IUCN_frq + IUCN_year + sampling_effort_t + (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com)

summary(z2)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## type ~ BodyMass.Value + realm + IUCN_frq + IUCN_year + sampling_effort_t +  
    ##     (1 | speciesScientificName) + (1 | projectID)
    ##    Data: com
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2096.0   2152.3  -1038.0   2076.0     2058 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5530 -0.4583  0.1199  0.4005  5.0511 
    ## 
    ## Random effects:
    ##  Groups                Name        Variance Std.Dev.
    ##  speciesScientificName (Intercept) 4.646    2.155   
    ##  projectID             (Intercept) 3.077    1.754   
    ## Number of obs: 2068, groups:  speciesScientificName, 485; projectID, 55
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)        1.94870    0.63384   3.074  0.00211 **
    ## BodyMass.Value    -0.22709    0.14428  -1.574  0.11550   
    ## realmIndo-Malay    1.01193    0.83752   1.208  0.22695   
    ## realmNearctic     -0.85257    0.74775  -1.140  0.25421   
    ## realmNeotropical   0.55065    0.91522   0.602  0.54740   
    ## IUCN_frq          -0.20772    0.06899  -3.011  0.00260 **
    ## IUCN_year          0.33035    0.13351   2.474  0.01334 * 
    ## sampling_effort_t -0.14456    0.25413  -0.569  0.56947   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) BdyM.V rlmI-M rlmNrc rlmNtr IUCN_f IUCN_y
    ## BodyMass.Vl  0.040                                          
    ## relmInd-Mly -0.585  0.004                                   
    ## realmNerctc -0.681  0.036  0.517                            
    ## realmNtrpcl -0.523  0.028  0.418  0.480                     
    ## IUCN_frq    -0.456 -0.150 -0.022  0.035 -0.038              
    ## IUCN_year   -0.010 -0.175  0.016  0.021  0.055 -0.031       
    ## smplng_ffr_ -0.068  0.003  0.163  0.062 -0.009  0.008 -0.005

``` r
# summarize tabulated form 
sjPlot::tab_model(z2)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
type
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
7.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.03 – 24.31
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.002</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
BodyMass.Value
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.80
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.60 – 1.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.116
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Indo-Malay\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.75
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.53 – 14.20
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.227
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Nearctic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.43
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.10 – 1.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.254
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Neotropical\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.73
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.29 – 10.43
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.547
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_frq
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.81
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.71 – 0.93
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.003</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_year
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.39
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.07 – 1.81
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.013</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
sampling\_effort\_t
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.87
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.53 – 1.42
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.569
</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.29
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
4.65
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.08
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.70
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
485
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
55
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
2068
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.066 / 0.721
</td>
</tr>
</table>

The two index of IUCN assessment history are significant.

The IUCN frequency has negative effect of `-0.20`. Make sense. The more
assessment that IUCN conducted for a species the more it’s range map
will match camera trap result.

BUt the IUCN year has positive effect of `0.33`. THis does not make
sense, it means there are more mismatches in more recent IUCN
assessment. I wonder if there is also a temporal mismatches. i.e. Camera
survey was conducted in 2016, IUCN assessed in 2020. (<u>Need take a
closer look</u>)

## Next, let’s fit the full model

Let’s start with the omission error:

``` r
z3 <- glmer(type ~ BodyMass.Value + realm + IUCN_frq + IUCN_year + sampling_effort_t + ForStrat.Value + Activity.Nocturnal + diet.breadth + Tree_mean+ (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = omi )

summary(z3)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## type ~ BodyMass.Value + realm + IUCN_frq + IUCN_year + sampling_effort_t +  
    ##     ForStrat.Value + Activity.Nocturnal + diet.breadth + Tree_mean +  
    ##     (1 | speciesScientificName) + (1 | projectID)
    ##    Data: omi
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    614.7    689.2   -292.3    584.7     1048 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3021 -0.0145 -0.0060 -0.0019  4.3041 
    ## 
    ## Random effects:
    ##  Groups                Name        Variance Std.Dev.
    ##  speciesScientificName (Intercept) 254.878  15.965  
    ##  projectID             (Intercept)   3.483   1.866  
    ## Number of obs: 1063, groups:  speciesScientificName, 319; projectID, 46
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)        -6.27018    2.21307  -2.833  0.00461 **
    ## BodyMass.Value      0.50049    0.67081   0.746  0.45561   
    ## realmIndo-Malay    -2.21001    1.88677  -1.171  0.24147   
    ## realmNearctic       0.78683    1.42068   0.554  0.57969   
    ## realmNeotropical   -0.03781    1.60987  -0.023  0.98126   
    ## IUCN_frq           -0.37141    0.32304  -1.150  0.25025   
    ## IUCN_year          -0.68779    0.62741  -1.096  0.27298   
    ## sampling_effort_t  -0.04349    0.46797  -0.093  0.92595   
    ## ForStrat.ValueG    -1.83700    1.58263  -1.161  0.24575   
    ## ForStrat.ValueS    -3.09474    2.33126  -1.327  0.18434   
    ## Activity.Nocturnal -0.61524    1.30217  -0.472  0.63659   
    ## diet.breadth        0.46138    0.53647   0.860  0.38978   
    ## Tree_mean          -0.09176    0.45087  -0.204  0.83873   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 13 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
# summarize tabulated form 
sjPlot::tab_model(z3)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
type
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00 – 0.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.005</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
BodyMass.Value
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.65
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.44 – 6.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.456
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Indo-Malay\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00 – 4.43
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.241
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Nearctic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.20
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.14 – 35.56
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.580
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Neotropical\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.96
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04 – 22.59
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.981
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_frq
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.69
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.37 – 1.30
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.250
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_year
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.50
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.15 – 1.72
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.273
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
sampling\_effort\_t
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.96
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.38 – 2.40
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.926
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
ForStrat.Value \[G\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.16
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01 – 3.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.246
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
ForStrat.Value \[S\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00 – 4.37
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.184
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Activity.Nocturnal
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04 – 6.94
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.637
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
diet.breadth
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.59
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.55 – 4.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.390
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Tree\_mean
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.91
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.38 – 2.21
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.839
</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.29
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
254.88
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.48
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.99
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
319
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
46
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
1063
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.013 / 0.988
</td>
</tr>
</table>

#### <u>None of the candidates variable are significant in explaining omission error.</u>

``` r
z4 <- glmer(type ~ BodyMass.Value + realm + IUCN_frq + IUCN_year + sampling_effort_t + ForStrat.Value + Activity.Nocturnal + diet.breadth + Tree_mean+ (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )

summary(z4)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## type ~ BodyMass.Value + realm + IUCN_frq + IUCN_year + sampling_effort_t +  
    ##     ForStrat.Value + Activity.Nocturnal + diet.breadth + Tree_mean +  
    ##     (1 | speciesScientificName) + (1 | projectID)
    ##    Data: com
    ## Control: glmerControl(optimizer = "bobyqa")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   2067.6   2152.2  -1018.8   2037.6     2053 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6215 -0.4408  0.1100  0.3846  5.1149 
    ## 
    ## Random effects:
    ##  Groups                Name        Variance Std.Dev.
    ##  speciesScientificName (Intercept) 4.416    2.101   
    ##  projectID             (Intercept) 3.055    1.748   
    ## Number of obs: 2068, groups:  speciesScientificName, 485; projectID, 55
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         3.15026    0.74465   4.231 2.33e-05 ***
    ## BodyMass.Value     -0.18441    0.14685  -1.256  0.20919    
    ## realmIndo-Malay     1.23605    0.83813   1.475  0.14027    
    ## realmNearctic      -0.68516    0.74743  -0.917  0.35931    
    ## realmNeotropical    1.05002    0.97892   1.073  0.28344    
    ## IUCN_frq           -0.21533    0.06957  -3.095  0.00197 ** 
    ## IUCN_year           0.12543    0.14929   0.840  0.40082    
    ## sampling_effort_t  -0.19314    0.25692  -0.752  0.45219    
    ## ForStrat.ValueG    -1.70511    0.38880  -4.386 1.16e-05 ***
    ## ForStrat.ValueS    -2.78666    0.52358  -5.322 1.02e-07 ***
    ## Activity.Nocturnal  0.11223    0.32863   0.342  0.73272    
    ## diet.breadth       -0.26275    0.14510  -1.811  0.07017 .  
    ## Tree_mean          -0.44031    0.30208  -1.458  0.14495    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 13 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
# summarize tabulated form 
sjPlot::tab_model(z4)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
type
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
23.34
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
5.42 – 100.46
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>&lt;0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
BodyMass.Value
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.83
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.62 – 1.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.209
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Indo-Malay\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
3.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.67 – 17.79
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.140
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Nearctic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.50
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.12 – 2.18
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.359
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Neotropical\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.86
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.42 – 19.47
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.283
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_frq
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.81
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.70 – 0.92
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.002</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_year
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.13
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.85 – 1.52
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.401
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
sampling\_effort\_t
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.82
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.50 – 1.36
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.452
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
ForStrat.Value \[G\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.18
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.08 – 0.39
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>&lt;0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
ForStrat.Value \[S\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02 – 0.17
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>&lt;0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Activity.Nocturnal
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.12
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.59 – 2.13
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.733
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
diet.breadth
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.77
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.58 – 1.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.070
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Tree\_mean
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.64
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.36 – 1.16
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.145
</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.29
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
4.42
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.05
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.69
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
485
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
55
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
2068
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.123 / 0.732
</td>
</tr>
</table>

#### IUCN frequency remain significant, but not IUCN year.

#### Forgoing stratum matters.

The dummy level of the forgoing strata is ForStrat.Value \[Arboreal\] in
our model. ForStrat.Value \[Ground\] and ForStrat.Value \[Sub-canopy\]
have negative effects `-1.70` and `-2.78` compare to ForStrat.Value
\[Arboreal\]. This means we are more likely to get a mismatch for
arboreal species.

This could indicate IUCN range map often over predict distribution of
arboreal. But we cannot rule out the fact that camera trap can be less
efficient in detecting arboreal species.

## Any thoughts on model selection?

I an trying to find the ‘best’ model using i.e. full combination,
stepwise regression. But it feels like data dredging.
