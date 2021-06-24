Mammal range
================
Cheng
6/23/2021

## Preliminary Modeling

### Modeling framework

We can think of this as: we surveyed **55** sites by using camera traps
and ‘surveyed’ it again by using IUCN range map. We now have two species
list from the two method.

We matched these two species lists, all species ( **453** in total) can
be assigned into three categories: Both: Species detected by both Camera
and IUCN (coded as `0` in the model). Camera only: Species detected by
camera only (coded as `1)`. IUCN only: Species detected by IUCN only
(also coded as `1` in the model).

We had 1954 matches in total.

There were **760** of Both, **89** of Camera only, and **1105** of IUCN
only.

We ran two separate binomial mixed-effect models to examine what factors
are associated with errors of the IUCN range map. We Compared type A,
type B matches to assess the potential with <u>omission error</u> and
type C and type B for <u>commission error</u>.

The binomial mixed-effects models structed like this:

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

## Rationale for random effects

The first question is how many of the species are sampled in different
sites (more than one time)?

``` r
#Species sample more than once
nrow(tab[which(tab$total  >= 1),])
```

    ## [1] 453

``` r
#Species sample more than 2 times
nrow(tab[which(tab$total  >= 2),])
```

    ## [1] 301

``` r
#Species sample more than 3 times
nrow(tab[which(tab$total  >= 3),])
```

    ## [1] 221

One species have one set of species covariates of interest. If **453**
species are sampled more than once. This can a potential
pseudoreplication.

We also followed this tutorial to think a bit more about the random
effect
(<https://bookdown.org/steve_midway/DAR/random-effects.html#should-i-consider-random-effects>).

1.  Can the factor(s) be viewed as a random sample from a probability
    distribution?

Species can be viewed as a random sample of all species, site can be
viewed as a random sample of sites. So **YES**.

2.  Does the intended scope of inference extend beyond the levels of a
    factor included in the current analysis to the entire population of
    a factor?

We are interested in general relationship between species traits
(i.e.bodymass) and the “match”. Not just these 500 + species. So
**YES**.

3.  Are the coefficients of a given factor going to be modeled?

If there is variation among species in probability of “matching”. Then
it is hypothesized that this might be due to species traits, we’ve added
traits in our model.

So **YES**.

4.  Is there a lack of statistical independence due to multiple
    observations from the same level within a factor over space and/or
    time?

YES, in our study, multiple observations (same species different place)
existed for most of species.

### We can’t use disaggregated model because it’s pseudoreplication. We can’t use species or site as fixed effects becasue we are no interested in effects of species or site pre se.

We can also look at this from the AIC perspective. We generated
fixed-effects minimal base-line models and three base-line mixed-model
using the “glmer” function with random intercepts for species,
projectID, and species + projectID. We can then check if including the
random effect is permitted by comparing the AICs from the glm to AIC
from the glmer model. If the AIC of the glmer object is smaller than the
AIC of the glm object, then this indicates that including random
intercepts is justified.

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

    ## [1] 571.7912

    ## [1] 470.1384

    ## [1] 564.526

    ## [1] 446.0467

Same thing with the commission error.

``` r
# Commission error
aic.glm <- AIC(logLik(m0.glm.com))
aic.glmer1 <- AIC(logLik(m0.glmer1.com))
aic.glmer2 <- AIC(logLik(m0.glmer2.com))
aic.glmer3 <- AIC(logLik(m0.glmer3.com))

aic.glm;aic.glmer1;aic.glmer2;aic.glmer3
```

    ## [1] 2523.25

    ## [1] 2169.423

    ## [1] 2318.229

    ## [1] 1971.706

### The AIC of the glmer3 object is smallest shows that including the two random intercepts (species + project) is justified.

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
    ##    458.3    505.7   -219.1    438.3      839 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9814 -0.0178 -0.0080 -0.0047  4.4817 
    ## 
    ## Random effects:
    ##  Groups                Name        Variance Std.Dev.
    ##  speciesScientificName (Intercept) 134.271  11.588  
    ##  projectID             (Intercept)   3.255   1.804  
    ## Number of obs: 849, groups:  speciesScientificName, 274; projectID, 46
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -8.6705     1.5129  -5.731 9.97e-09 ***
    ## BodyMass.Value      0.3054     0.4841   0.631    0.528    
    ## realmIndo-Malay    -1.3551     1.7613  -0.769    0.442    
    ## realmNearctic      -0.1749     1.3992  -0.125    0.901    
    ## realmNeotropical   -0.8031     1.5107  -0.532    0.595    
    ## IUCN_frq           -0.1398     0.2564  -0.545    0.586    
    ## IUCN_year          -0.1268     0.5038  -0.252    0.801    
    ## sampling_effort_t   0.1851     0.4844   0.382    0.702    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) BdyM.V rlmI-M rlmNrc rlmNtr IUCN_f IUCN_y
    ## BodyMass.Vl  0.048                                          
    ## relmInd-Mly -0.163 -0.027                                   
    ## realmNerctc -0.377  0.049  0.286                            
    ## realmNtrpcl -0.060  0.032  0.248  0.344                     
    ## IUCN_frq    -0.650 -0.198  0.055  0.096 -0.072              
    ## IUCN_year    0.043 -0.191  0.027  0.020  0.128 -0.090       
    ## smplng_ffr_  0.047  0.021  0.106 -0.331 -0.108 -0.054 -0.013

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
1.36
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.53 – 3.51
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.528
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Indo-Malay\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01 – 8.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.442
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Nearctic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.84
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.05 – 13.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.901
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Neotropical\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.45
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02 – 8.65
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.595
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_frq
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.87
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.53 – 1.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.586
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_year
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.88
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.33 – 2.36
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.801
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
sampling\_effort\_t
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.20
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.47 – 3.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.702
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
134.27
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.25
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.98
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
274
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
849
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.004 / 0.977
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
    ##   1965.7   2021.1   -972.9   1945.7     1855 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4358 -0.4778  0.1501  0.4273  3.4217 
    ## 
    ## Random effects:
    ##  Groups                Name        Variance Std.Dev.
    ##  speciesScientificName (Intercept) 4.148    2.037   
    ##  projectID             (Intercept) 2.627    1.621   
    ## Number of obs: 1865, groups:  speciesScientificName, 433; projectID, 55
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)        1.50492    0.59709   2.520   0.0117 *
    ## BodyMass.Value    -0.19870    0.14550  -1.366   0.1721  
    ## realmIndo-Malay    1.23501    0.78373   1.576   0.1151  
    ## realmNearctic     -0.72266    0.70896  -1.019   0.3080  
    ## realmNeotropical   0.54583    0.85699   0.637   0.5242  
    ## IUCN_frq          -0.16391    0.06725  -2.437   0.0148 *
    ## IUCN_year          0.28852    0.13400   2.153   0.0313 *
    ## sampling_effort_t -0.07172    0.23721  -0.302   0.7624  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) BdyM.V rlmI-M rlmNrc rlmNtr IUCN_f IUCN_y
    ## BodyMass.Vl  0.049                                          
    ## relmInd-Mly -0.585  0.009                                   
    ## realmNerctc -0.671  0.041  0.508                            
    ## realmNtrpcl -0.520  0.033  0.418  0.475                     
    ## IUCN_frq    -0.469 -0.161 -0.018  0.040 -0.037              
    ## IUCN_year   -0.020 -0.183  0.013  0.009  0.051 -0.007       
    ## smplng_ffr_ -0.079  0.002  0.161  0.058 -0.010  0.007 -0.003

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
4.50
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.40 – 14.52
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.012</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
BodyMass.Value
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.82
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.62 – 1.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.172
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
0.74 – 15.98
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.115
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Nearctic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.49
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.12 – 1.95
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.308
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
0.32 – 9.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.524
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_frq
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.74 – 0.97
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.015</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_year
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.33
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.03 – 1.74
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.031</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
sampling\_effort\_t
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.93
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.58 – 1.48
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.762
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
4.15
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
2.63
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.67
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
433
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
1865
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.066 / 0.695
</td>
</tr>
</table>

The two index of IUCN assessment history are significant.

The IUCN frequency has negative effect of `-0.20`. Make sense. The more
assessment that IUCN conducted for a species the more it’s range map
will match camera trap result.

But the IUCN year has positive effect of `0.33`. THis does not make
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
    ##    466.9    538.1   -218.5    436.9      834 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9850 -0.0194 -0.0091 -0.0043  4.4996 
    ## 
    ## Random effects:
    ##  Groups                Name        Variance Std.Dev.
    ##  speciesScientificName (Intercept) 126.027  11.226  
    ##  projectID             (Intercept)   3.234   1.798  
    ## Number of obs: 849, groups:  speciesScientificName, 274; projectID, 46
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)        -7.99752    2.10625  -3.797 0.000146 ***
    ## BodyMass.Value      0.36409    0.50066   0.727 0.467089    
    ## realmIndo-Malay    -1.25698    1.72424  -0.729 0.465998    
    ## realmNearctic      -0.09689    1.45318  -0.067 0.946839    
    ## realmNeotropical   -0.39046    1.63580  -0.239 0.811341    
    ## IUCN_frq           -0.12121    0.27440  -0.442 0.658684    
    ## IUCN_year          -0.23835    0.57915  -0.412 0.680664    
    ## sampling_effort_t   0.19551    0.48341   0.404 0.685886    
    ## ForStrat.ValueG    -0.52128    1.57371  -0.331 0.740463    
    ## ForStrat.ValueS    -2.27304    2.42117  -0.939 0.347822    
    ## Activity.Nocturnal -0.15842    1.31427  -0.121 0.904056    
    ## diet.breadth        0.25205    0.52724   0.478 0.632612    
    ## Tree_mean          -0.08110    0.46377  -0.175 0.861185    
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
0.00 – 0.02
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
1.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.54 – 3.84
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.467
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Indo-Malay\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.28
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01 – 8.35
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.466
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Nearctic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.91
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.05 – 15.66
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.947
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Neotropical\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.68
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.03 – 16.70
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.811
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_frq
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.89
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.52 – 1.52
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.659
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_year
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.79
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.25 – 2.45
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.681
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
sampling\_effort\_t
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.22
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.47 – 3.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.686
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
ForStrat.Value \[G\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.59
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.03 – 12.98
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.740
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
ForStrat.Value \[S\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00 – 11.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.348
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Activity.Nocturnal
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.06 – 11.22
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.904
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
diet.breadth
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.46 – 3.62
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.633
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Tree\_mean
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.92
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.37 – 2.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.861
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
126.03
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
3.23
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.98
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
274
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
849
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.009 / 0.975
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
    ##   1937.0   2020.0   -953.5   1907.0     1850 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5254 -0.4670  0.1430  0.4186  3.4577 
    ## 
    ## Random effects:
    ##  Groups                Name        Variance Std.Dev.
    ##  speciesScientificName (Intercept) 3.886    1.971   
    ##  projectID             (Intercept) 2.619    1.618   
    ## Number of obs: 1865, groups:  speciesScientificName, 433; projectID, 55
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         2.77097    0.70663   3.921 8.80e-05 ***
    ## BodyMass.Value     -0.16568    0.14757  -1.123   0.2616    
    ## realmIndo-Malay     1.42681    0.78507   1.817   0.0692 .  
    ## realmNearctic      -0.58816    0.70829  -0.830   0.4063    
    ## realmNeotropical    0.98010    0.91772   1.068   0.2855    
    ## IUCN_frq           -0.16991    0.06761  -2.513   0.0120 *  
    ## IUCN_year           0.10412    0.14966   0.696   0.4866    
    ## sampling_effort_t  -0.12192    0.24025  -0.507   0.6118    
    ## ForStrat.ValueG    -1.67731    0.38481  -4.359 1.31e-05 ***
    ## ForStrat.ValueS    -2.73095    0.51849  -5.267 1.39e-07 ***
    ## Activity.Nocturnal  0.03603    0.33328   0.108   0.9139    
    ## diet.breadth       -0.28391    0.14277  -1.989   0.0467 *  
    ## Tree_mean          -0.45779    0.28173  -1.625   0.1042    
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
15.97
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.00 – 63.81
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
0.85
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.63 – 1.13
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.262
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Indo-Malay\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.17
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.89 – 19.40
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.069
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Nearctic\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.56
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.14 – 2.23
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.406
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
realm \[Neotropical\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.66
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.44 – 16.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.286
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_frq
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.84
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.74 – 0.96
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.012</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
IUCN\_year
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.83 – 1.49
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.487
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
sampling\_effort\_t
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.89
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.55 – 1.42
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.612
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
ForStrat.Value \[G\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.19
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.09 – 0.40
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
0.07
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02 – 0.18
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
1.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.54 – 1.99
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.914
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
diet.breadth
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.75
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.57 – 1.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.047</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Tree\_mean
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.63
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.36 – 1.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.104
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
3.89
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>projectID</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
2.62
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.66
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>speciesScientificName</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
433
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
1865
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.131 / 0.708
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
