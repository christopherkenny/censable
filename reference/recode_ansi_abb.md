# Recode ANSI by Abb

Replaces state abbreviation with state ansi

## Usage

``` r
recode_ansi_abb(.data, .abb)
```

## Arguments

- .data:

  data.frame or tibble

- .abb:

  column with state abbrevaition

## Value

.data with column .abb replaced with state ansi

## Examples

``` r
data('stata')
stata |> recode_ansi_abb(abb)
#>    fips     abb                        name    ansi    region
#> 1    01 1779775                     Alabama 1779775     South
#> 2    02 1785533                      Alaska 1785533      West
#> 3    04 1779777                     Arizona 1779777      West
#> 4    05 0068085                    Arkansas 0068085     South
#> 5    06 1779778                  California 1779778      West
#> 6    08 1779779                    Colorado 1779779      West
#> 7    09 1779780                 Connecticut 1779780 Northeast
#> 8    10 1779781                    Delaware 1779781     South
#> 9    11 1702382        District of Columbia 1702382     South
#> 10   12 0294478                     Florida 0294478     South
#> 11   13 1705317                     Georgia 1705317     South
#> 12   15 1779782                      Hawaii 1779782      West
#> 13   16 1779783                       Idaho 1779783      West
#> 14   17 1779784                    Illinois 1779784   Midwest
#> 15   18 0448508                     Indiana 0448508   Midwest
#> 16   19 1779785                        Iowa 1779785   Midwest
#> 17   20 0481813                      Kansas 0481813   Midwest
#> 18   21 1779786                    Kentucky 1779786     South
#> 19   22 1629543                   Louisiana 1629543     South
#> 20   23 1779787                       Maine 1779787 Northeast
#> 21   24 1714934                    Maryland 1714934     South
#> 22   25 0606926               Massachusetts 0606926 Northeast
#> 23   26 1779789                    Michigan 1779789   Midwest
#> 24   27 0662849                   Minnesota 0662849   Midwest
#> 25   28 1779790                 Mississippi 1779790     South
#> 26   29 1779791                    Missouri 1779791   Midwest
#> 27   30 0767982                     Montana 0767982      West
#> 28   31 1779792                    Nebraska 1779792   Midwest
#> 29   32 1779793                      Nevada 1779793      West
#> 30   33 1779794               New Hampshire 1779794 Northeast
#> 31   34 1779795                  New Jersey 1779795 Northeast
#> 32   35 0897535                  New Mexico 0897535      West
#> 33   36 1779796                    New York 1779796 Northeast
#> 34   37 1027616              North Carolina 1027616     South
#> 35   38 1779797                North Dakota 1779797   Midwest
#> 36   39 1085497                        Ohio 1085497   Midwest
#> 37   40 1102857                    Oklahoma 1102857     South
#> 38   41 1155107                      Oregon 1155107      West
#> 39   42 1779798                Pennsylvania 1779798 Northeast
#> 40   44 1219835                Rhode Island 1219835 Northeast
#> 41   45 1779799              South Carolina 1779799     South
#> 42   46 1785534                South Dakota 1785534   Midwest
#> 43   47 1325873                   Tennessee 1325873     South
#> 44   48 1779801                       Texas 1779801     South
#> 45   49 1455989                        Utah 1455989      West
#> 46   50 1779802                     Vermont 1779802 Northeast
#> 47   51 1779803                    Virginia 1779803     South
#> 48   53 1779804                  Washington 1779804      West
#> 49   54 1779805               West Virginia 1779805     South
#> 50   55 1779806                   Wisconsin 1779806   Midwest
#> 51   56 1779807                     Wyoming 1779807      West
#> 52   60 1802701              American Samoa 1802701      <NA>
#> 53   66 1802705                        Guam 1802705      <NA>
#> 54   69 1779809    Northern Mariana Islands 1779809      <NA>
#> 55   72 1779808                 Puerto Rico 1779808      <NA>
#> 56   74 1878752 U.S. Minor Outlying Islands 1878752      <NA>
#> 57   78 1802710         U.S. Virgin Islands 1802710      <NA>
#>              division
#> 1  East South Central
#> 2             Pacific
#> 3            Mountain
#> 4  West South Central
#> 5             Pacific
#> 6            Mountain
#> 7         New England
#> 8      South Atlantic
#> 9      South Atlantic
#> 10     South Atlantic
#> 11     South Atlantic
#> 12            Pacific
#> 13           Mountain
#> 14 East North Central
#> 15 East North Central
#> 16 West North Central
#> 17 West North Central
#> 18 East South Central
#> 19 West South Central
#> 20        New England
#> 21     South Atlantic
#> 22        New England
#> 23 East North Central
#> 24 West North Central
#> 25 East South Central
#> 26 West North Central
#> 27           Mountain
#> 28 West North Central
#> 29           Mountain
#> 30        New England
#> 31       Mid-Atlantic
#> 32           Mountain
#> 33       Mid-Atlantic
#> 34     South Atlantic
#> 35 West North Central
#> 36 East North Central
#> 37 West South Central
#> 38            Pacific
#> 39       Mid-Atlantic
#> 40        New England
#> 41     South Atlantic
#> 42 West North Central
#> 43 East South Central
#> 44 West South Central
#> 45           Mountain
#> 46        New England
#> 47     South Atlantic
#> 48            Pacific
#> 49     South Atlantic
#> 50 East North Central
#> 51           Mountain
#> 52               <NA>
#> 53               <NA>
#> 54               <NA>
#> 55               <NA>
#> 56               <NA>
#> 57               <NA>
```
