# Recode Name by FIPS

Replaces state fips with state name

## Usage

``` r
recode_name_fips(.data, .fips)
```

## Arguments

- .data:

  data.frame or tibble

- .fips:

  column with state fips

## Value

.data with column .fips replaced with state name

## Examples

``` r
data('stata')
stata |> recode_name_fips(fips)
#>                           fips abb                        name    ansi
#> 1                      Alabama  AL                     Alabama 1779775
#> 2                       Alaska  AK                      Alaska 1785533
#> 3                      Arizona  AZ                     Arizona 1779777
#> 4                     Arkansas  AR                    Arkansas 0068085
#> 5                   California  CA                  California 1779778
#> 6                     Colorado  CO                    Colorado 1779779
#> 7                  Connecticut  CT                 Connecticut 1779780
#> 8                     Delaware  DE                    Delaware 1779781
#> 9         District of Columbia  DC        District of Columbia 1702382
#> 10                     Florida  FL                     Florida 0294478
#> 11                     Georgia  GA                     Georgia 1705317
#> 12                      Hawaii  HI                      Hawaii 1779782
#> 13                       Idaho  ID                       Idaho 1779783
#> 14                    Illinois  IL                    Illinois 1779784
#> 15                     Indiana  IN                     Indiana 0448508
#> 16                        Iowa  IA                        Iowa 1779785
#> 17                      Kansas  KS                      Kansas 0481813
#> 18                    Kentucky  KY                    Kentucky 1779786
#> 19                   Louisiana  LA                   Louisiana 1629543
#> 20                       Maine  ME                       Maine 1779787
#> 21                    Maryland  MD                    Maryland 1714934
#> 22               Massachusetts  MA               Massachusetts 0606926
#> 23                    Michigan  MI                    Michigan 1779789
#> 24                   Minnesota  MN                   Minnesota 0662849
#> 25                 Mississippi  MS                 Mississippi 1779790
#> 26                    Missouri  MO                    Missouri 1779791
#> 27                     Montana  MT                     Montana 0767982
#> 28                    Nebraska  NE                    Nebraska 1779792
#> 29                      Nevada  NV                      Nevada 1779793
#> 30               New Hampshire  NH               New Hampshire 1779794
#> 31                  New Jersey  NJ                  New Jersey 1779795
#> 32                  New Mexico  NM                  New Mexico 0897535
#> 33                    New York  NY                    New York 1779796
#> 34              North Carolina  NC              North Carolina 1027616
#> 35                North Dakota  ND                North Dakota 1779797
#> 36                        Ohio  OH                        Ohio 1085497
#> 37                    Oklahoma  OK                    Oklahoma 1102857
#> 38                      Oregon  OR                      Oregon 1155107
#> 39                Pennsylvania  PA                Pennsylvania 1779798
#> 40                Rhode Island  RI                Rhode Island 1219835
#> 41              South Carolina  SC              South Carolina 1779799
#> 42                South Dakota  SD                South Dakota 1785534
#> 43                   Tennessee  TN                   Tennessee 1325873
#> 44                       Texas  TX                       Texas 1779801
#> 45                        Utah  UT                        Utah 1455989
#> 46                     Vermont  VT                     Vermont 1779802
#> 47                    Virginia  VA                    Virginia 1779803
#> 48                  Washington  WA                  Washington 1779804
#> 49               West Virginia  WV               West Virginia 1779805
#> 50                   Wisconsin  WI                   Wisconsin 1779806
#> 51                     Wyoming  WY                     Wyoming 1779807
#> 52              American Samoa  AS              American Samoa 1802701
#> 53                        Guam  GU                        Guam 1802705
#> 54    Northern Mariana Islands  MP    Northern Mariana Islands 1779809
#> 55                 Puerto Rico  PR                 Puerto Rico 1779808
#> 56 U.S. Minor Outlying Islands  UM U.S. Minor Outlying Islands 1878752
#> 57         U.S. Virgin Islands  VI         U.S. Virgin Islands 1802710
#>       region           division
#> 1      South East South Central
#> 2       West            Pacific
#> 3       West           Mountain
#> 4      South West South Central
#> 5       West            Pacific
#> 6       West           Mountain
#> 7  Northeast        New England
#> 8      South     South Atlantic
#> 9      South     South Atlantic
#> 10     South     South Atlantic
#> 11     South     South Atlantic
#> 12      West            Pacific
#> 13      West           Mountain
#> 14   Midwest East North Central
#> 15   Midwest East North Central
#> 16   Midwest West North Central
#> 17   Midwest West North Central
#> 18     South East South Central
#> 19     South West South Central
#> 20 Northeast        New England
#> 21     South     South Atlantic
#> 22 Northeast        New England
#> 23   Midwest East North Central
#> 24   Midwest West North Central
#> 25     South East South Central
#> 26   Midwest West North Central
#> 27      West           Mountain
#> 28   Midwest West North Central
#> 29      West           Mountain
#> 30 Northeast        New England
#> 31 Northeast       Mid-Atlantic
#> 32      West           Mountain
#> 33 Northeast       Mid-Atlantic
#> 34     South     South Atlantic
#> 35   Midwest West North Central
#> 36   Midwest East North Central
#> 37     South West South Central
#> 38      West            Pacific
#> 39 Northeast       Mid-Atlantic
#> 40 Northeast        New England
#> 41     South     South Atlantic
#> 42   Midwest West North Central
#> 43     South East South Central
#> 44     South West South Central
#> 45      West           Mountain
#> 46 Northeast        New England
#> 47     South     South Atlantic
#> 48      West            Pacific
#> 49     South     South Atlantic
#> 50   Midwest East North Central
#> 51      West           Mountain
#> 52      <NA>               <NA>
#> 53      <NA>               <NA>
#> 54      <NA>               <NA>
#> 55      <NA>               <NA>
#> 56      <NA>               <NA>
#> 57      <NA>               <NA>
```
