# X-13 Dictionary

Function providing the names all output objects (series, diagnostics,
parameters) available with
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md) function.
Can be used to generate an output non available by default with
userdefined option in
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md)function
(see examples).

## Usage

``` r
x13_dictionary()
```

## Value

returns a vector containing the names of all output objects (series,
diagnostics, parameters) available with
[`x13()`](https://rjdverse.github.io/rjd3x13/reference/x13.md) function.

## See also

`x13_full_dictionary` for a detailed version of the output description

## Examples

``` r
# visualize the list of names
summary(x13_dictionary())
#> List of possible outputs:
#> 
#> - period
#> - span.start
#> - span.end
#> - span.n
#> - span.missing
#> - log
#> - adjust
#> - likelihood.ll
#> - likelihood.adjustedll
#> - likelihood.ssqerr
#> - likelihood.aic
#> - likelihood.bic
#> - likelihood.aicc
#> - likelihood.bicc
#> - likelihood.bic2
#> - likelihood.hannanquinn
#> - likelihood.nparams
#> - likelihood.nobs
#> - likelihood.neffectiveobs
#> - likelihood.df
#> - arima.p
#> - arima.d
#> - arima.q
#> - arima.bp
#> - arima.bd
#> - arima.bq
#> - arima.theta(*)
#> - arima.phi(*)
#> - arima.btheta(*)
#> - arima.bphi(*)
#> - regression.espan.start
#> - regression.espan.end
#> - regression.espan.n
#> - regression.espan.missing
#> - regression.mean
#> - regression.nlp
#> - regression.ntd
#> - regression.leaster
#> - regression.nmh
#> - regression.nout
#> - regression.nao
#> - regression.nls
#> - regression.ntc
#> - regression.nso
#> - regression.nusers
#> - regression.mu
#> - regression.lp
#> - regression.td(*)
#> - regression.td-derived
#> - regression.td-ftest
#> - regression.easter
#> - regression.outlier(*)
#> - regression.user(*)
#> - regression.missing(*)
#> - residuals.res
#> - residuals.tsres
#> - residuals.n
#> - residuals.df
#> - residuals.dfc
#> - residuals.ser
#> - residuals.ser_ml
#> - residuals.type
#> - residuals.mean
#> - residuals.skewness
#> - residuals.kurtosis
#> - residuals.doornikhansen
#> - residuals.lb
#> - residuals.bp
#> - residuals.lb2
#> - residuals.bp2
#> - residuals.seaslb
#> - residuals.seasbp
#> - residuals.nruns
#> - residuals.lruns
#> - residuals.nudruns
#> - residuals.ludruns
#> - regression.ml.parameters
#> - regression.ml.pcovar
#> - regression.ml.pcovar-ml
#> - regression.ml.pcorr
#> - regression.ml.pscore
#> - regression.details.description
#> - regression.details.type
#> - regression.details.coefficients
#> - regression.details.covar
#> - regression.details.covar-ml
#> - y
#> - y_f(?)
#> - y_ef(?)
#> - y_b(?)
#> - y_eb(?)
#> - yc
#> - yc_f(?)
#> - yc_b(?)
#> - ylin
#> - ylin_f(?)
#> - ylin_b(?)
#> - det
#> - det_f(?)
#> - det_b(?)
#> - cal
#> - cal_f(?)
#> - cal_b(?)
#> - ycal
#> - ycal_f(?)
#> - ycal_b(?)
#> - tde
#> - tde_f(?)
#> - tde_b(?)
#> - ee
#> - ee_f(?)
#> - ee_b(?)
#> - omhe
#> - omhe_f(?)
#> - omhe_b(?)
#> - mhe
#> - mhe_f(?)
#> - mhe_b(?)
#> - out
#> - out_f(?)
#> - out_b(?)
#> - reg
#> - reg_f(?)
#> - reg_b(?)
#> - l
#> - l_f(?)
#> - l_b(?)
#> - full_res
#> - mode
#> - seasonal
#> - sa
#> - t
#> - s
#> - i
#> - sa_f
#> - t_f
#> - s_f
#> - i_f
#> - out_t
#> - out_s
#> - out_i
#> - reg_t
#> - reg_s
#> - reg_i
#> - reg_sa
#> - reg_u
#> - reg_y
#> - det_t
#> - det_s
#> - det_i
#> - out_t_f(?)
#> - out_s_f(?)
#> - out_i_f(?)
#> - reg_t_f(?)
#> - reg_s_f(?)
#> - reg_i_f(?)
#> - reg_sa_f(?)
#> - reg_u_f(?)
#> - reg_y_f(?)
#> - det_t_f(?)
#> - det_s_f(?)
#> - det_i_f(?)
#> - out_t_b(?)
#> - out_s_b(?)
#> - out_i_b(?)
#> - reg_t_b(?)
#> - reg_s_b(?)
#> - reg_i_b(?)
#> - reg_sa_b(?)
#> - reg_u_b(?)
#> - reg_y_b(?)
#> - det_t_b(?)
#> - det_s_b(?)
#> - det_i_b(?)
#> - decomposition.y_cmp
#> - decomposition.y_cmp_f
#> - decomposition.y_cmp_b
#> - decomposition.sa_cmp
#> - decomposition.t_cmp
#> - decomposition.s_cmp
#> - decomposition.i_cmp
#> - preadjustment.a1
#> - preadjustment.a1a
#> - preadjustment.a1b
#> - preadjustment.a6
#> - preadjustment.a7
#> - preadjustment.a8
#> - preadjustment.a8t
#> - preadjustment.a8i
#> - preadjustment.a8s
#> - preadjustment.a9
#> - preadjustment.a9
#> - preadjustment.a9cal
#> - preadjustment.a9u
#> - preadjustment.a9sa
#> - preadjustment.a9ser
#> - decomposition.b1
#> - decomposition.b2
#> - decomposition.b3
#> - decomposition.b4
#> - decomposition.b5
#> - decomposition.b6
#> - decomposition.b7
#> - decomposition.b8
#> - decomposition.b9
#> - decomposition.b10
#> - decomposition.b11
#> - decomposition.b13
#> - decomposition.b17
#> - decomposition.b20
#> - decomposition.c1
#> - decomposition.c2
#> - decomposition.c4
#> - decomposition.c5
#> - decomposition.c6
#> - decomposition.c7
#> - decomposition.c9
#> - decomposition.c10
#> - decomposition.c11
#> - decomposition.c13
#> - decomposition.c17
#> - decomposition.c20
#> - decomposition.d1
#> - decomposition.d2
#> - decomposition.d4
#> - decomposition.d5
#> - decomposition.d6
#> - decomposition.d7
#> - decomposition.d8
#> - decomposition.d9
#> - decomposition.d10
#> - decomposition.d11
#> - decomposition.d12
#> - decomposition.d13
#> - decomposition.x11-all
#> - decomposition.icratio
#> - decomposition.trend-filter
#> - decomposition.seasonal-filters
#> - decomposition.d9-global-msr
#> - decomposition.d9-msr
#> - decomposition.d9-msr-table
#> - finals.d11
#> - finals.d12
#> - finals.d13
#> - finals.d16
#> - finals.d18
#> - finals.d11a
#> - finals.d12a
#> - finals.d16a
#> - finals.d18a
#> - finals.d11b
#> - finals.d12b
#> - finals.d16b
#> - finals.d18b
#> - finals.e1
#> - finals.e2
#> - finals.e3
#> - finals.e11
#> - diagnostics.seas-lin-combined
#> - diagnostics.seas-lin-evolutive
#> - diagnostics.seas-lin-stable
#> - diagnostics.seas-si-combined
#> - diagnostics.seas-si-combined3
#> - diagnostics.seas-si-evolutive
#> - diagnostics.seas-si-stable
#> - diagnostics.seas-res-combined
#> - diagnostics.seas-res-combined3
#> - diagnostics.seas-res-evolutive
#> - diagnostics.seas-res-stable
#> - diagnostics.seas-sa-combined
#> - diagnostics.seas-sa-combined3
#> - diagnostics.seas-sa-evolutive
#> - diagnostics.seas-sa-stable
#> - diagnostics.seas-i-combined
#> - diagnostics.seas-i-combined3
#> - diagnostics.seas-i-evolutive
#> - diagnostics.seas-i-stable
#> - diagnostics.seas-lin-qs
#> - diagnostics.seas-lin-f
#> - diagnostics.seas-lin-friedman
#> - diagnostics.seas-lin-kw
#> - diagnostics.seas-lin-periodogram
#> - diagnostics.seas-lin-spectralpeaks
#> - diagnostics.seas-res-qs
#> - diagnostics.seas-res-f
#> - diagnostics.seas-res-friedman
#> - diagnostics.seas-res-kw
#> - diagnostics.seas-res-periodogram
#> - diagnostics.seas-res-spectralpeaks
#> - diagnostics.seas-sa-qs
#> - diagnostics.seas-sa-f
#> - diagnostics.seas-sa-friedman
#> - diagnostics.seas-sa-kw
#> - diagnostics.seas-sa-periodogram
#> - diagnostics.seas-sa-spectralpeaks
#> - diagnostics.seas-i-qs
#> - diagnostics.seas-i-f
#> - diagnostics.seas-i-friedman
#> - diagnostics.seas-i-kw
#> - diagnostics.seas-i-periodogram
#> - diagnostics.seas-i-spectralpeaks
#> - diagnostics.seas-sa-ac1
#> - diagnostics.td-res-all
#> - diagnostics.td-res-last
#> - diagnostics.td-sa-all
#> - diagnostics.td-sa-last
#> - diagnostics.td-i-all
#> - diagnostics.td-i-last
#> - diagnostics.fcast-insample-mean
#> - diagnostics.fcast-outsample-mean
#> - diagnostics.fcast-outsample-variance
#> - m-statistics.m1
#> - m-statistics.m2
#> - m-statistics.m3
#> - m-statistics.m4
#> - m-statistics.m5
#> - m-statistics.m6
#> - m-statistics.m7
#> - m-statistics.m8
#> - m-statistics.m9
#> - m-statistics.m10
#> - m-statistics.m11
#> - m-statistics.q
#> - m-statistics.q-m2
#> - variancedecomposition.cycle
#> - variancedecomposition.seasonality
#> - variancedecomposition.irregular
#> - variancedecomposition.tdh
#> - variancedecomposition.others
#> - variancedecomposition.total
#> - quality.summary
#> - benchmarking.original
#> - benchmarking.target
#> - benchmarking.result 
#> 
#>  For a detailled summary of all outputs, please use the function `x13_full_dictionary()` or `tramoseats_full_dictionary()`
# set up vector with names of output objects of interest
user_defined_output <- c("ylin", "residuals.kurtosis")
# generate the corresponding output in an estimation
library(rjd3toolkit)
#> 
#> Attaching package: ‘rjd3toolkit’
#> The following objects are masked from ‘package:stats’:
#> 
#>     aggregate, mad
y <- rjd3toolkit::ABS$X0.2.09.10.M
m<-x13(y,"rsa3", userdefined=user_defined_output)
# retrieve user defined output
tail(m$user_defined$ylin)
#>         Mar    Apr    May    Jun    Jul    Aug
#> 2017 1370.3 1522.6 1452.4 1557.2 1445.5 1303.1
m$user_defined$residuals.kurtosis
#> Value: 3.143851 
#> P-Value: 0.5512 
```
