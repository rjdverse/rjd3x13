# X-13 Dictionary

Functions to provide information for all output objects (series,
diagnostics, parameters) available with
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md) function.

## Usage

``` r
x13_dictionary()

x13_full_dictionary()
```

## Value

`x13_dictionary()` returns a character vector containing the names of
all output objects (series, diagnostics, parameters) available with the
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md) function,
whereas `x13_full_dictionary()` returns a `data.frame` with format and
description, for all the output objects.

## Details

These functions provide lists of output names (series, diagnostics,
parameters) available with the
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md) function.
These names can be used to generate customized outputs with the
userdefined option of the
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md) function
(see examples). The `x13_full_dictionary` function provides additional
information on object format and description.

## Examples

``` r
# \donttest{
# Visualize the dictionary
print(x13_dictionary())
#>   [1] "period"                              
#>   [2] "span.start"                          
#>   [3] "span.end"                            
#>   [4] "span.n"                              
#>   [5] "span.missing"                        
#>   [6] "log"                                 
#>   [7] "adjust"                              
#>   [8] "likelihood.ll"                       
#>   [9] "likelihood.adjustedll"               
#>  [10] "likelihood.ssqerr"                   
#>  [11] "likelihood.aic"                      
#>  [12] "likelihood.bic"                      
#>  [13] "likelihood.aicc"                     
#>  [14] "likelihood.bicc"                     
#>  [15] "likelihood.bic2"                     
#>  [16] "likelihood.hannanquinn"              
#>  [17] "likelihood.nparams"                  
#>  [18] "likelihood.nobs"                     
#>  [19] "likelihood.neffectiveobs"            
#>  [20] "likelihood.df"                       
#>  [21] "arima.p"                             
#>  [22] "arima.d"                             
#>  [23] "arima.q"                             
#>  [24] "arima.bp"                            
#>  [25] "arima.bd"                            
#>  [26] "arima.bq"                            
#>  [27] "arima.theta(*)"                      
#>  [28] "arima.phi(*)"                        
#>  [29] "arima.btheta(*)"                     
#>  [30] "arima.bphi(*)"                       
#>  [31] "regression.espan.start"              
#>  [32] "regression.espan.end"                
#>  [33] "regression.espan.n"                  
#>  [34] "regression.espan.missing"            
#>  [35] "regression.mean"                     
#>  [36] "regression.nlp"                      
#>  [37] "regression.ntd"                      
#>  [38] "regression.leaster"                  
#>  [39] "regression.nmh"                      
#>  [40] "regression.nout"                     
#>  [41] "regression.nao"                      
#>  [42] "regression.nls"                      
#>  [43] "regression.ntc"                      
#>  [44] "regression.nso"                      
#>  [45] "regression.nusers"                   
#>  [46] "regression.mu"                       
#>  [47] "regression.lp"                       
#>  [48] "regression.td(*)"                    
#>  [49] "regression.td-derived"               
#>  [50] "regression.td-ftest"                 
#>  [51] "regression.easter"                   
#>  [52] "regression.outlier(*)"               
#>  [53] "regression.user(*)"                  
#>  [54] "regression.missing(*)"               
#>  [55] "residuals.res"                       
#>  [56] "residuals.tsres"                     
#>  [57] "residuals.n"                         
#>  [58] "residuals.df"                        
#>  [59] "residuals.dfc"                       
#>  [60] "residuals.ser"                       
#>  [61] "residuals.ser_ml"                    
#>  [62] "residuals.type"                      
#>  [63] "residuals.mean"                      
#>  [64] "residuals.skewness"                  
#>  [65] "residuals.kurtosis"                  
#>  [66] "residuals.doornikhansen"             
#>  [67] "residuals.lb"                        
#>  [68] "residuals.bp"                        
#>  [69] "residuals.lb2"                       
#>  [70] "residuals.bp2"                       
#>  [71] "residuals.seaslb"                    
#>  [72] "residuals.seasbp"                    
#>  [73] "residuals.nruns"                     
#>  [74] "residuals.lruns"                     
#>  [75] "residuals.nudruns"                   
#>  [76] "residuals.ludruns"                   
#>  [77] "regression.ml.parameters"            
#>  [78] "regression.ml.pcovar"                
#>  [79] "regression.ml.pcovar-ml"             
#>  [80] "regression.ml.pcorr"                 
#>  [81] "regression.ml.pscore"                
#>  [82] "regression.details.description"      
#>  [83] "regression.details.type"             
#>  [84] "regression.details.coefficients"     
#>  [85] "regression.details.covar"            
#>  [86] "regression.details.covar-ml"         
#>  [87] "y"                                   
#>  [88] "y_f(?)"                              
#>  [89] "y_ef(?)"                             
#>  [90] "y_b(?)"                              
#>  [91] "y_eb(?)"                             
#>  [92] "yc"                                  
#>  [93] "yc_f(?)"                             
#>  [94] "yc_b(?)"                             
#>  [95] "ylin"                                
#>  [96] "ylin_f(?)"                           
#>  [97] "ylin_b(?)"                           
#>  [98] "det"                                 
#>  [99] "det_f(?)"                            
#> [100] "det_b(?)"                            
#> [101] "cal"                                 
#> [102] "cal_f(?)"                            
#> [103] "cal_b(?)"                            
#> [104] "ycal"                                
#> [105] "ycal_f(?)"                           
#> [106] "ycal_b(?)"                           
#> [107] "tde"                                 
#> [108] "tde_f(?)"                            
#> [109] "tde_b(?)"                            
#> [110] "ee"                                  
#> [111] "ee_f(?)"                             
#> [112] "ee_b(?)"                             
#> [113] "omhe"                                
#> [114] "omhe_f(?)"                           
#> [115] "omhe_b(?)"                           
#> [116] "mhe"                                 
#> [117] "mhe_f(?)"                            
#> [118] "mhe_b(?)"                            
#> [119] "out"                                 
#> [120] "out_f(?)"                            
#> [121] "out_b(?)"                            
#> [122] "reg"                                 
#> [123] "reg_f(?)"                            
#> [124] "reg_b(?)"                            
#> [125] "l"                                   
#> [126] "l_f(?)"                              
#> [127] "l_b(?)"                              
#> [128] "full_res"                            
#> [129] "mode"                                
#> [130] "seasonal"                            
#> [131] "sa"                                  
#> [132] "t"                                   
#> [133] "s"                                   
#> [134] "i"                                   
#> [135] "sa_f"                                
#> [136] "t_f"                                 
#> [137] "s_f"                                 
#> [138] "i_f"                                 
#> [139] "out_t"                               
#> [140] "out_s"                               
#> [141] "out_i"                               
#> [142] "reg_t"                               
#> [143] "reg_s"                               
#> [144] "reg_i"                               
#> [145] "reg_sa"                              
#> [146] "reg_u"                               
#> [147] "reg_y"                               
#> [148] "det_t"                               
#> [149] "det_s"                               
#> [150] "det_i"                               
#> [151] "out_t_f(?)"                          
#> [152] "out_s_f(?)"                          
#> [153] "out_i_f(?)"                          
#> [154] "reg_t_f(?)"                          
#> [155] "reg_s_f(?)"                          
#> [156] "reg_i_f(?)"                          
#> [157] "reg_sa_f(?)"                         
#> [158] "reg_u_f(?)"                          
#> [159] "reg_y_f(?)"                          
#> [160] "det_t_f(?)"                          
#> [161] "det_s_f(?)"                          
#> [162] "det_i_f(?)"                          
#> [163] "out_t_b(?)"                          
#> [164] "out_s_b(?)"                          
#> [165] "out_i_b(?)"                          
#> [166] "reg_t_b(?)"                          
#> [167] "reg_s_b(?)"                          
#> [168] "reg_i_b(?)"                          
#> [169] "reg_sa_b(?)"                         
#> [170] "reg_u_b(?)"                          
#> [171] "reg_y_b(?)"                          
#> [172] "det_t_b(?)"                          
#> [173] "det_s_b(?)"                          
#> [174] "det_i_b(?)"                          
#> [175] "decomposition.y_cmp"                 
#> [176] "decomposition.y_cmp_f"               
#> [177] "decomposition.y_cmp_b"               
#> [178] "decomposition.sa_cmp"                
#> [179] "decomposition.t_cmp"                 
#> [180] "decomposition.s_cmp"                 
#> [181] "decomposition.i_cmp"                 
#> [182] "preadjustment.a1"                    
#> [183] "preadjustment.a1a"                   
#> [184] "preadjustment.a1b"                   
#> [185] "preadjustment.a6"                    
#> [186] "preadjustment.a7"                    
#> [187] "preadjustment.a8"                    
#> [188] "preadjustment.a8t"                   
#> [189] "preadjustment.a8i"                   
#> [190] "preadjustment.a8s"                   
#> [191] "preadjustment.a9"                    
#> [192] "preadjustment.a9"                    
#> [193] "preadjustment.a9cal"                 
#> [194] "preadjustment.a9u"                   
#> [195] "preadjustment.a9sa"                  
#> [196] "preadjustment.a9ser"                 
#> [197] "decomposition.b1"                    
#> [198] "decomposition.b2"                    
#> [199] "decomposition.b3"                    
#> [200] "decomposition.b4"                    
#> [201] "decomposition.b5"                    
#> [202] "decomposition.b6"                    
#> [203] "decomposition.b7"                    
#> [204] "decomposition.b8"                    
#> [205] "decomposition.b9"                    
#> [206] "decomposition.b10"                   
#> [207] "decomposition.b11"                   
#> [208] "decomposition.b13"                   
#> [209] "decomposition.b17"                   
#> [210] "decomposition.b20"                   
#> [211] "decomposition.c1"                    
#> [212] "decomposition.c2"                    
#> [213] "decomposition.c4"                    
#> [214] "decomposition.c5"                    
#> [215] "decomposition.c6"                    
#> [216] "decomposition.c7"                    
#> [217] "decomposition.c9"                    
#> [218] "decomposition.c10"                   
#> [219] "decomposition.c11"                   
#> [220] "decomposition.c13"                   
#> [221] "decomposition.c17"                   
#> [222] "decomposition.c20"                   
#> [223] "decomposition.d1"                    
#> [224] "decomposition.d2"                    
#> [225] "decomposition.d4"                    
#> [226] "decomposition.d5"                    
#> [227] "decomposition.d6"                    
#> [228] "decomposition.d7"                    
#> [229] "decomposition.d8"                    
#> [230] "decomposition.d9"                    
#> [231] "decomposition.d10"                   
#> [232] "decomposition.d11"                   
#> [233] "decomposition.d12"                   
#> [234] "decomposition.d13"                   
#> [235] "decomposition.x11-all"               
#> [236] "decomposition.icratio"               
#> [237] "decomposition.trend-filter"          
#> [238] "decomposition.seasonal-filters"      
#> [239] "decomposition.d9-global-msr"         
#> [240] "decomposition.d9-msr"                
#> [241] "decomposition.d9-msr-table"          
#> [242] "finals.d11"                          
#> [243] "finals.d12"                          
#> [244] "finals.d13"                          
#> [245] "finals.d16"                          
#> [246] "finals.d18"                          
#> [247] "finals.d11a"                         
#> [248] "finals.d12a"                         
#> [249] "finals.d16a"                         
#> [250] "finals.d18a"                         
#> [251] "finals.d11b"                         
#> [252] "finals.d12b"                         
#> [253] "finals.d16b"                         
#> [254] "finals.d18b"                         
#> [255] "finals.e1"                           
#> [256] "finals.e2"                           
#> [257] "finals.e3"                           
#> [258] "finals.e11"                          
#> [259] "diagnostics.seas-lin-combined"       
#> [260] "diagnostics.seas-lin-evolutive"      
#> [261] "diagnostics.seas-lin-stable"         
#> [262] "diagnostics.seas-si-combined"        
#> [263] "diagnostics.seas-si-combined3"       
#> [264] "diagnostics.seas-si-evolutive"       
#> [265] "diagnostics.seas-si-stable"          
#> [266] "diagnostics.seas-res-combined"       
#> [267] "diagnostics.seas-res-combined3"      
#> [268] "diagnostics.seas-res-evolutive"      
#> [269] "diagnostics.seas-res-stable"         
#> [270] "diagnostics.seas-sa-combined"        
#> [271] "diagnostics.seas-sa-combined3"       
#> [272] "diagnostics.seas-sa-evolutive"       
#> [273] "diagnostics.seas-sa-stable"          
#> [274] "diagnostics.seas-i-combined"         
#> [275] "diagnostics.seas-i-combined3"        
#> [276] "diagnostics.seas-i-evolutive"        
#> [277] "diagnostics.seas-i-stable"           
#> [278] "diagnostics.seas-lin-qs"             
#> [279] "diagnostics.seas-lin-f"              
#> [280] "diagnostics.seas-lin-friedman"       
#> [281] "diagnostics.seas-lin-kw"             
#> [282] "diagnostics.seas-lin-periodogram"    
#> [283] "diagnostics.seas-lin-spectralpeaks"  
#> [284] "diagnostics.seas-res-qs"             
#> [285] "diagnostics.seas-res-f"              
#> [286] "diagnostics.seas-res-friedman"       
#> [287] "diagnostics.seas-res-kw"             
#> [288] "diagnostics.seas-res-periodogram"    
#> [289] "diagnostics.seas-res-spectralpeaks"  
#> [290] "diagnostics.seas-sa-qs"              
#> [291] "diagnostics.seas-sa-f"               
#> [292] "diagnostics.seas-sa-friedman"        
#> [293] "diagnostics.seas-sa-kw"              
#> [294] "diagnostics.seas-sa-periodogram"     
#> [295] "diagnostics.seas-sa-spectralpeaks"   
#> [296] "diagnostics.seas-i-qs"               
#> [297] "diagnostics.seas-i-f"                
#> [298] "diagnostics.seas-i-friedman"         
#> [299] "diagnostics.seas-i-kw"               
#> [300] "diagnostics.seas-i-periodogram"      
#> [301] "diagnostics.seas-i-spectralpeaks"    
#> [302] "diagnostics.seas-sa-ac1"             
#> [303] "diagnostics.td-res-all"              
#> [304] "diagnostics.td-res-last"             
#> [305] "diagnostics.td-sa-all"               
#> [306] "diagnostics.td-sa-last"              
#> [307] "diagnostics.td-i-all"                
#> [308] "diagnostics.td-i-last"               
#> [309] "diagnostics.fcast-insample-mean"     
#> [310] "diagnostics.fcast-outsample-mean"    
#> [311] "diagnostics.fcast-outsample-variance"
#> [312] "m-statistics.m1"                     
#> [313] "m-statistics.m2"                     
#> [314] "m-statistics.m3"                     
#> [315] "m-statistics.m4"                     
#> [316] "m-statistics.m5"                     
#> [317] "m-statistics.m6"                     
#> [318] "m-statistics.m7"                     
#> [319] "m-statistics.m8"                     
#> [320] "m-statistics.m9"                     
#> [321] "m-statistics.m10"                    
#> [322] "m-statistics.m11"                    
#> [323] "m-statistics.q"                      
#> [324] "m-statistics.q-m2"                   
#> [325] "variancedecomposition.cycle"         
#> [326] "variancedecomposition.seasonality"   
#> [327] "variancedecomposition.irregular"     
#> [328] "variancedecomposition.tdh"           
#> [329] "variancedecomposition.others"        
#> [330] "variancedecomposition.total"         
#> [331] "quality.summary"                     
#> [332] "benchmarking.original"               
#> [333] "benchmarking.target"                 
#> [334] "benchmarking.result"                 
#> attr(,"class")
#> [1] "JD3_DICTIONARY"
summary(x13_dictionary())
#>         Length          Class           Mode 
#>            334 JD3_DICTIONARY      character 

# first 10 lines
head(x13_full_dictionary(), n = 10)
#>                     name
#> 1                 period
#> 2             span.start
#> 3               span.end
#> 4                 span.n
#> 5           span.missing
#> 6                    log
#> 7                 adjust
#> 8          likelihood.ll
#> 9  likelihood.adjustedll
#> 10     likelihood.ssqerr
#>                                                    description detail
#> 1                                         period of the series       
#> 2                     start of the considered (partial) series       
#> 3                       end of the considered (partial) series       
#> 4         number of periods in the considered (partial) series       
#> 5  number of missing values in the considered (partial) series       
#> 6                                            log-transformtion       
#> 7                                 pre-adjustment for leap year       
#> 8                                               log-likelihood       
#> 9                                      adjusted log-likelihood       
#> 10                                              sum of squares       
#>                                         output   type              fullname
#> 1                            java.lang.Integer Normal                period
#> 2  jdplus.toolkit.base.api.timeseries.TsPeriod Normal            span.start
#> 3  jdplus.toolkit.base.api.timeseries.TsPeriod Normal              span.end
#> 4                            java.lang.Integer Normal                span.n
#> 5                            java.lang.Integer Normal          span.missing
#> 6                            java.lang.Integer Normal                   log
#> 7                             java.lang.String Normal                adjust
#> 8                             java.lang.Double Normal         likelihood.ll
#> 9                             java.lang.Double Normal likelihood.adjustedll
#> 10                            java.lang.Double Normal     likelihood.ssqerr
# For more structured information call `View(x13_full_dictionary())`

# Extract names of output of interest
user_defined_output <- x13_dictionary()[c(65, 95, 135)]
user_defined_output
#> [1] "residuals.kurtosis" "ylin"               "sa_f"              

# Generate the corresponding output in an estimation
y <- rjd3toolkit::ABS$X0.2.09.10.M
m <- x13(y,"rsa3", userdefined=user_defined_output)

# Retrieve user defined output
tail(m$user_defined$ylin)
#>         Mar    Apr    May    Jun    Jul    Aug
#> 2017 1370.3 1522.6 1452.4 1557.2 1445.5 1303.1
m$user_defined$residuals.kurtosis
#> Value: 3.143851 
#> P-Value: 0.5512 
m$user_defined$sa_f
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 2017                                                                        
#> 2018 1545.102 1550.995 1544.872 1558.419 1556.812 1546.513 1552.880 1555.686
#>           Sep      Oct      Nov      Dec
#> 2017 1559.713 1541.278 1551.031 1550.678
#> 2018                                    
# }
```
