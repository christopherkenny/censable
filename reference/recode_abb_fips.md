# Recode Abb by FIPS

Replaces state fips with state abb

## Usage

``` r
recode_abb_fips(.data, .fips)
```

## Arguments

- .data:

  data.frame or tibble

- .fips:

  column with state fips

## Value

.data with column .fips replaced with state abb

## Examples

``` r
data('stata')
stata |> recode_abb_fips(fips)
#>    fips abb                        name    ansi    region           division
#> 1    AL  AL                     Alabama 1779775     South East South Central
#> 2    AK  AK                      Alaska 1785533      West            Pacific
#> 3    AZ  AZ                     Arizona 1779777      West           Mountain
#> 4    AR  AR                    Arkansas 0068085     South West South Central
#> 5    CA  CA                  California 1779778      West            Pacific
#> 6    CO  CO                    Colorado 1779779      West           Mountain
#> 7    CT  CT                 Connecticut 1779780 Northeast        New England
#> 8    DE  DE                    Delaware 1779781     South     South Atlantic
#> 9    DC  DC        District of Columbia 1702382     South     South Atlantic
#> 10   FL  FL                     Florida 0294478     South     South Atlantic
#> 11   GA  GA                     Georgia 1705317     South     South Atlantic
#> 12   HI  HI                      Hawaii 1779782      West            Pacific
#> 13   ID  ID                       Idaho 1779783      West           Mountain
#> 14   IL  IL                    Illinois 1779784   Midwest East North Central
#> 15   IN  IN                     Indiana 0448508   Midwest East North Central
#> 16   IA  IA                        Iowa 1779785   Midwest West North Central
#> 17   KS  KS                      Kansas 0481813   Midwest West North Central
#> 18   KY  KY                    Kentucky 1779786     South East South Central
#> 19   LA  LA                   Louisiana 1629543     South West South Central
#> 20   ME  ME                       Maine 1779787 Northeast        New England
#> 21   MD  MD                    Maryland 1714934     South     South Atlantic
#> 22   MA  MA               Massachusetts 0606926 Northeast        New England
#> 23   MI  MI                    Michigan 1779789   Midwest East North Central
#> 24   MN  MN                   Minnesota 0662849   Midwest West North Central
#> 25   MS  MS                 Mississippi 1779790     South East South Central
#> 26   MO  MO                    Missouri 1779791   Midwest West North Central
#> 27   MT  MT                     Montana 0767982      West           Mountain
#> 28   NE  NE                    Nebraska 1779792   Midwest West North Central
#> 29   NV  NV                      Nevada 1779793      West           Mountain
#> 30   NH  NH               New Hampshire 1779794 Northeast        New England
#> 31   NJ  NJ                  New Jersey 1779795 Northeast       Mid-Atlantic
#> 32   NM  NM                  New Mexico 0897535      West           Mountain
#> 33   NY  NY                    New York 1779796 Northeast       Mid-Atlantic
#> 34   NC  NC              North Carolina 1027616     South     South Atlantic
#> 35   ND  ND                North Dakota 1779797   Midwest West North Central
#> 36   OH  OH                        Ohio 1085497   Midwest East North Central
#> 37   OK  OK                    Oklahoma 1102857     South West South Central
#> 38   OR  OR                      Oregon 1155107      West            Pacific
#> 39   PA  PA                Pennsylvania 1779798 Northeast       Mid-Atlantic
#> 40   RI  RI                Rhode Island 1219835 Northeast        New England
#> 41   SC  SC              South Carolina 1779799     South     South Atlantic
#> 42   SD  SD                South Dakota 1785534   Midwest West North Central
#> 43   TN  TN                   Tennessee 1325873     South East South Central
#> 44   TX  TX                       Texas 1779801     South West South Central
#> 45   UT  UT                        Utah 1455989      West           Mountain
#> 46   VT  VT                     Vermont 1779802 Northeast        New England
#> 47   VA  VA                    Virginia 1779803     South     South Atlantic
#> 48   WA  WA                  Washington 1779804      West            Pacific
#> 49   WV  WV               West Virginia 1779805     South     South Atlantic
#> 50   WI  WI                   Wisconsin 1779806   Midwest East North Central
#> 51   WY  WY                     Wyoming 1779807      West           Mountain
#> 52   AS  AS              American Samoa 1802701      <NA>               <NA>
#> 53   GU  GU                        Guam 1802705      <NA>               <NA>
#> 54   MP  MP    Northern Mariana Islands 1779809      <NA>               <NA>
#> 55   PR  PR                 Puerto Rico 1779808      <NA>               <NA>
#> 56   UM  UM U.S. Minor Outlying Islands 1878752      <NA>               <NA>
#> 57   VI  VI         U.S. Virgin Islands 1802710      <NA>               <NA>
```
