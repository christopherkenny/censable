# Join Abb by FIPS

Adds a column with state abbreviation joining by a column with state
fips

## Usage

``` r
join_abb_fips(.data, .fips)
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
stata |> join_abb_fips(fips)
#>    fips abb.x                        name    ansi    region           division
#> 1    01    AL                     Alabama 1779775     South East South Central
#> 2    02    AK                      Alaska 1785533      West            Pacific
#> 3    04    AZ                     Arizona 1779777      West           Mountain
#> 4    05    AR                    Arkansas 0068085     South West South Central
#> 5    06    CA                  California 1779778      West            Pacific
#> 6    08    CO                    Colorado 1779779      West           Mountain
#> 7    09    CT                 Connecticut 1779780 Northeast        New England
#> 8    10    DE                    Delaware 1779781     South     South Atlantic
#> 9    11    DC        District of Columbia 1702382     South     South Atlantic
#> 10   12    FL                     Florida 0294478     South     South Atlantic
#> 11   13    GA                     Georgia 1705317     South     South Atlantic
#> 12   15    HI                      Hawaii 1779782      West            Pacific
#> 13   16    ID                       Idaho 1779783      West           Mountain
#> 14   17    IL                    Illinois 1779784   Midwest East North Central
#> 15   18    IN                     Indiana 0448508   Midwest East North Central
#> 16   19    IA                        Iowa 1779785   Midwest West North Central
#> 17   20    KS                      Kansas 0481813   Midwest West North Central
#> 18   21    KY                    Kentucky 1779786     South East South Central
#> 19   22    LA                   Louisiana 1629543     South West South Central
#> 20   23    ME                       Maine 1779787 Northeast        New England
#> 21   24    MD                    Maryland 1714934     South     South Atlantic
#> 22   25    MA               Massachusetts 0606926 Northeast        New England
#> 23   26    MI                    Michigan 1779789   Midwest East North Central
#> 24   27    MN                   Minnesota 0662849   Midwest West North Central
#> 25   28    MS                 Mississippi 1779790     South East South Central
#> 26   29    MO                    Missouri 1779791   Midwest West North Central
#> 27   30    MT                     Montana 0767982      West           Mountain
#> 28   31    NE                    Nebraska 1779792   Midwest West North Central
#> 29   32    NV                      Nevada 1779793      West           Mountain
#> 30   33    NH               New Hampshire 1779794 Northeast        New England
#> 31   34    NJ                  New Jersey 1779795 Northeast       Mid-Atlantic
#> 32   35    NM                  New Mexico 0897535      West           Mountain
#> 33   36    NY                    New York 1779796 Northeast       Mid-Atlantic
#> 34   37    NC              North Carolina 1027616     South     South Atlantic
#> 35   38    ND                North Dakota 1779797   Midwest West North Central
#> 36   39    OH                        Ohio 1085497   Midwest East North Central
#> 37   40    OK                    Oklahoma 1102857     South West South Central
#> 38   41    OR                      Oregon 1155107      West            Pacific
#> 39   42    PA                Pennsylvania 1779798 Northeast       Mid-Atlantic
#> 40   44    RI                Rhode Island 1219835 Northeast        New England
#> 41   45    SC              South Carolina 1779799     South     South Atlantic
#> 42   46    SD                South Dakota 1785534   Midwest West North Central
#> 43   47    TN                   Tennessee 1325873     South East South Central
#> 44   48    TX                       Texas 1779801     South West South Central
#> 45   49    UT                        Utah 1455989      West           Mountain
#> 46   50    VT                     Vermont 1779802 Northeast        New England
#> 47   51    VA                    Virginia 1779803     South     South Atlantic
#> 48   53    WA                  Washington 1779804      West            Pacific
#> 49   54    WV               West Virginia 1779805     South     South Atlantic
#> 50   55    WI                   Wisconsin 1779806   Midwest East North Central
#> 51   56    WY                     Wyoming 1779807      West           Mountain
#> 52   60    AS              American Samoa 1802701      <NA>               <NA>
#> 53   66    GU                        Guam 1802705      <NA>               <NA>
#> 54   69    MP    Northern Mariana Islands 1779809      <NA>               <NA>
#> 55   72    PR                 Puerto Rico 1779808      <NA>               <NA>
#> 56   74    UM U.S. Minor Outlying Islands 1878752      <NA>               <NA>
#> 57   78    VI         U.S. Virgin Islands 1802710      <NA>               <NA>
#>    abb.y
#> 1     AL
#> 2     AK
#> 3     AZ
#> 4     AR
#> 5     CA
#> 6     CO
#> 7     CT
#> 8     DE
#> 9     DC
#> 10    FL
#> 11    GA
#> 12    HI
#> 13    ID
#> 14    IL
#> 15    IN
#> 16    IA
#> 17    KS
#> 18    KY
#> 19    LA
#> 20    ME
#> 21    MD
#> 22    MA
#> 23    MI
#> 24    MN
#> 25    MS
#> 26    MO
#> 27    MT
#> 28    NE
#> 29    NV
#> 30    NH
#> 31    NJ
#> 32    NM
#> 33    NY
#> 34    NC
#> 35    ND
#> 36    OH
#> 37    OK
#> 38    OR
#> 39    PA
#> 40    RI
#> 41    SC
#> 42    SD
#> 43    TN
#> 44    TX
#> 45    UT
#> 46    VT
#> 47    VA
#> 48    WA
#> 49    WV
#> 50    WI
#> 51    WY
#> 52    AS
#> 53    GU
#> 54    MP
#> 55    PR
#> 56    UM
#> 57    VI
```
