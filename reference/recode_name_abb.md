# Recode Name by Abb

Replaces state abbreviation with state name

## Usage

``` r
recode_name_abb(.data, .abb)
```

## Arguments

- .data:

  data.frame or tibble

- .abb:

  column with state abbrevaition

## Value

.data with column .abb replaced with state name

## Examples

``` r
data('stata')
stata |> recode_name_abb(abb)
#>    fips                         abb                        name    ansi
#> 1    01                     Alabama                     Alabama 1779775
#> 2    02                      Alaska                      Alaska 1785533
#> 3    04                     Arizona                     Arizona 1779777
#> 4    05                    Arkansas                    Arkansas 0068085
#> 5    06                  California                  California 1779778
#> 6    08                    Colorado                    Colorado 1779779
#> 7    09                 Connecticut                 Connecticut 1779780
#> 8    10                    Delaware                    Delaware 1779781
#> 9    11        District of Columbia        District of Columbia 1702382
#> 10   12                     Florida                     Florida 0294478
#> 11   13                     Georgia                     Georgia 1705317
#> 12   15                      Hawaii                      Hawaii 1779782
#> 13   16                       Idaho                       Idaho 1779783
#> 14   17                    Illinois                    Illinois 1779784
#> 15   18                     Indiana                     Indiana 0448508
#> 16   19                        Iowa                        Iowa 1779785
#> 17   20                      Kansas                      Kansas 0481813
#> 18   21                    Kentucky                    Kentucky 1779786
#> 19   22                   Louisiana                   Louisiana 1629543
#> 20   23                       Maine                       Maine 1779787
#> 21   24                    Maryland                    Maryland 1714934
#> 22   25               Massachusetts               Massachusetts 0606926
#> 23   26                    Michigan                    Michigan 1779789
#> 24   27                   Minnesota                   Minnesota 0662849
#> 25   28                 Mississippi                 Mississippi 1779790
#> 26   29                    Missouri                    Missouri 1779791
#> 27   30                     Montana                     Montana 0767982
#> 28   31                    Nebraska                    Nebraska 1779792
#> 29   32                      Nevada                      Nevada 1779793
#> 30   33               New Hampshire               New Hampshire 1779794
#> 31   34                  New Jersey                  New Jersey 1779795
#> 32   35                  New Mexico                  New Mexico 0897535
#> 33   36                    New York                    New York 1779796
#> 34   37              North Carolina              North Carolina 1027616
#> 35   38                North Dakota                North Dakota 1779797
#> 36   39                        Ohio                        Ohio 1085497
#> 37   40                    Oklahoma                    Oklahoma 1102857
#> 38   41                      Oregon                      Oregon 1155107
#> 39   42                Pennsylvania                Pennsylvania 1779798
#> 40   44                Rhode Island                Rhode Island 1219835
#> 41   45              South Carolina              South Carolina 1779799
#> 42   46                South Dakota                South Dakota 1785534
#> 43   47                   Tennessee                   Tennessee 1325873
#> 44   48                       Texas                       Texas 1779801
#> 45   49                        Utah                        Utah 1455989
#> 46   50                     Vermont                     Vermont 1779802
#> 47   51                    Virginia                    Virginia 1779803
#> 48   53                  Washington                  Washington 1779804
#> 49   54               West Virginia               West Virginia 1779805
#> 50   55                   Wisconsin                   Wisconsin 1779806
#> 51   56                     Wyoming                     Wyoming 1779807
#> 52   60              American Samoa              American Samoa 1802701
#> 53   66                        Guam                        Guam 1802705
#> 54   69    Northern Mariana Islands    Northern Mariana Islands 1779809
#> 55   72                 Puerto Rico                 Puerto Rico 1779808
#> 56   74 U.S. Minor Outlying Islands U.S. Minor Outlying Islands 1878752
#> 57   78         U.S. Virgin Islands         U.S. Virgin Islands 1802710
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
