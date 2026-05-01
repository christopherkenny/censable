# Recode ANSI by FIPS

Replaces state fips with state ansi

## Usage

``` r
recode_ansi_fips(.data, .fips)
```

## Arguments

- .data:

  data.frame or tibble

- .fips:

  column with state fips

## Value

.data with column .fips replaced with state ansi

## Examples

``` r
data('stata')
stata |> recode_ansi_fips(fips)
#>       fips abb                        name    ansi    region           division
#> 1  1779775  AL                     Alabama 1779775     South East South Central
#> 2  1785533  AK                      Alaska 1785533      West            Pacific
#> 3  1779777  AZ                     Arizona 1779777      West           Mountain
#> 4  0068085  AR                    Arkansas 0068085     South West South Central
#> 5  1779778  CA                  California 1779778      West            Pacific
#> 6  1779779  CO                    Colorado 1779779      West           Mountain
#> 7  1779780  CT                 Connecticut 1779780 Northeast        New England
#> 8  1779781  DE                    Delaware 1779781     South     South Atlantic
#> 9  1702382  DC        District of Columbia 1702382     South     South Atlantic
#> 10 0294478  FL                     Florida 0294478     South     South Atlantic
#> 11 1705317  GA                     Georgia 1705317     South     South Atlantic
#> 12 1779782  HI                      Hawaii 1779782      West            Pacific
#> 13 1779783  ID                       Idaho 1779783      West           Mountain
#> 14 1779784  IL                    Illinois 1779784   Midwest East North Central
#> 15 0448508  IN                     Indiana 0448508   Midwest East North Central
#> 16 1779785  IA                        Iowa 1779785   Midwest West North Central
#> 17 0481813  KS                      Kansas 0481813   Midwest West North Central
#> 18 1779786  KY                    Kentucky 1779786     South East South Central
#> 19 1629543  LA                   Louisiana 1629543     South West South Central
#> 20 1779787  ME                       Maine 1779787 Northeast        New England
#> 21 1714934  MD                    Maryland 1714934     South     South Atlantic
#> 22 0606926  MA               Massachusetts 0606926 Northeast        New England
#> 23 1779789  MI                    Michigan 1779789   Midwest East North Central
#> 24 0662849  MN                   Minnesota 0662849   Midwest West North Central
#> 25 1779790  MS                 Mississippi 1779790     South East South Central
#> 26 1779791  MO                    Missouri 1779791   Midwest West North Central
#> 27 0767982  MT                     Montana 0767982      West           Mountain
#> 28 1779792  NE                    Nebraska 1779792   Midwest West North Central
#> 29 1779793  NV                      Nevada 1779793      West           Mountain
#> 30 1779794  NH               New Hampshire 1779794 Northeast        New England
#> 31 1779795  NJ                  New Jersey 1779795 Northeast       Mid-Atlantic
#> 32 0897535  NM                  New Mexico 0897535      West           Mountain
#> 33 1779796  NY                    New York 1779796 Northeast       Mid-Atlantic
#> 34 1027616  NC              North Carolina 1027616     South     South Atlantic
#> 35 1779797  ND                North Dakota 1779797   Midwest West North Central
#> 36 1085497  OH                        Ohio 1085497   Midwest East North Central
#> 37 1102857  OK                    Oklahoma 1102857     South West South Central
#> 38 1155107  OR                      Oregon 1155107      West            Pacific
#> 39 1779798  PA                Pennsylvania 1779798 Northeast       Mid-Atlantic
#> 40 1219835  RI                Rhode Island 1219835 Northeast        New England
#> 41 1779799  SC              South Carolina 1779799     South     South Atlantic
#> 42 1785534  SD                South Dakota 1785534   Midwest West North Central
#> 43 1325873  TN                   Tennessee 1325873     South East South Central
#> 44 1779801  TX                       Texas 1779801     South West South Central
#> 45 1455989  UT                        Utah 1455989      West           Mountain
#> 46 1779802  VT                     Vermont 1779802 Northeast        New England
#> 47 1779803  VA                    Virginia 1779803     South     South Atlantic
#> 48 1779804  WA                  Washington 1779804      West            Pacific
#> 49 1779805  WV               West Virginia 1779805     South     South Atlantic
#> 50 1779806  WI                   Wisconsin 1779806   Midwest East North Central
#> 51 1779807  WY                     Wyoming 1779807      West           Mountain
#> 52 1802701  AS              American Samoa 1802701      <NA>               <NA>
#> 53 1802705  GU                        Guam 1802705      <NA>               <NA>
#> 54 1779809  MP    Northern Mariana Islands 1779809      <NA>               <NA>
#> 55 1779808  PR                 Puerto Rico 1779808      <NA>               <NA>
#> 56 1878752  UM U.S. Minor Outlying Islands 1878752      <NA>               <NA>
#> 57 1802710  VI         U.S. Virgin Islands 1802710      <NA>               <NA>
```
