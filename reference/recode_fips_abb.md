# Recode FIPS by Abb

Replaces state abbreviation with state fips

## Usage

``` r
recode_fips_abb(.data, .abb)
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
stata |> recode_fips_abb(abb)
#>    fips abb                        name    ansi    region           division
#> 1    01  01                     Alabama 1779775     South East South Central
#> 2    02  02                      Alaska 1785533      West            Pacific
#> 3    04  04                     Arizona 1779777      West           Mountain
#> 4    05  05                    Arkansas 0068085     South West South Central
#> 5    06  06                  California 1779778      West            Pacific
#> 6    08  08                    Colorado 1779779      West           Mountain
#> 7    09  09                 Connecticut 1779780 Northeast        New England
#> 8    10  10                    Delaware 1779781     South     South Atlantic
#> 9    11  11        District of Columbia 1702382     South     South Atlantic
#> 10   12  12                     Florida 0294478     South     South Atlantic
#> 11   13  13                     Georgia 1705317     South     South Atlantic
#> 12   15  15                      Hawaii 1779782      West            Pacific
#> 13   16  16                       Idaho 1779783      West           Mountain
#> 14   17  17                    Illinois 1779784   Midwest East North Central
#> 15   18  18                     Indiana 0448508   Midwest East North Central
#> 16   19  19                        Iowa 1779785   Midwest West North Central
#> 17   20  20                      Kansas 0481813   Midwest West North Central
#> 18   21  21                    Kentucky 1779786     South East South Central
#> 19   22  22                   Louisiana 1629543     South West South Central
#> 20   23  23                       Maine 1779787 Northeast        New England
#> 21   24  24                    Maryland 1714934     South     South Atlantic
#> 22   25  25               Massachusetts 0606926 Northeast        New England
#> 23   26  26                    Michigan 1779789   Midwest East North Central
#> 24   27  27                   Minnesota 0662849   Midwest West North Central
#> 25   28  28                 Mississippi 1779790     South East South Central
#> 26   29  29                    Missouri 1779791   Midwest West North Central
#> 27   30  30                     Montana 0767982      West           Mountain
#> 28   31  31                    Nebraska 1779792   Midwest West North Central
#> 29   32  32                      Nevada 1779793      West           Mountain
#> 30   33  33               New Hampshire 1779794 Northeast        New England
#> 31   34  34                  New Jersey 1779795 Northeast       Mid-Atlantic
#> 32   35  35                  New Mexico 0897535      West           Mountain
#> 33   36  36                    New York 1779796 Northeast       Mid-Atlantic
#> 34   37  37              North Carolina 1027616     South     South Atlantic
#> 35   38  38                North Dakota 1779797   Midwest West North Central
#> 36   39  39                        Ohio 1085497   Midwest East North Central
#> 37   40  40                    Oklahoma 1102857     South West South Central
#> 38   41  41                      Oregon 1155107      West            Pacific
#> 39   42  42                Pennsylvania 1779798 Northeast       Mid-Atlantic
#> 40   44  44                Rhode Island 1219835 Northeast        New England
#> 41   45  45              South Carolina 1779799     South     South Atlantic
#> 42   46  46                South Dakota 1785534   Midwest West North Central
#> 43   47  47                   Tennessee 1325873     South East South Central
#> 44   48  48                       Texas 1779801     South West South Central
#> 45   49  49                        Utah 1455989      West           Mountain
#> 46   50  50                     Vermont 1779802 Northeast        New England
#> 47   51  51                    Virginia 1779803     South     South Atlantic
#> 48   53  53                  Washington 1779804      West            Pacific
#> 49   54  54               West Virginia 1779805     South     South Atlantic
#> 50   55  55                   Wisconsin 1779806   Midwest East North Central
#> 51   56  56                     Wyoming 1779807      West           Mountain
#> 52   60  60              American Samoa 1802701      <NA>               <NA>
#> 53   66  66                        Guam 1802705      <NA>               <NA>
#> 54   69  69    Northern Mariana Islands 1779809      <NA>               <NA>
#> 55   72  72                 Puerto Rico 1779808      <NA>               <NA>
#> 56   74  74 U.S. Minor Outlying Islands 1878752      <NA>               <NA>
#> 57   78  78         U.S. Virgin Islands 1802710      <NA>               <NA>
```
