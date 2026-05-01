# Recode FIPS by Name

Replaces state name with state fips

## Usage

``` r
recode_fips_name(.data, .name)
```

## Arguments

- .data:

  data.frame or tibble

- .name:

  column with state name

## Value

.data with column .name replaced with fips

## Examples

``` r
data('stata')
stata |> recode_fips_name(name)
#>    fips abb name    ansi    region           division
#> 1    01  AL   01 1779775     South East South Central
#> 2    02  AK   02 1785533      West            Pacific
#> 3    04  AZ   04 1779777      West           Mountain
#> 4    05  AR   05 0068085     South West South Central
#> 5    06  CA   06 1779778      West            Pacific
#> 6    08  CO   08 1779779      West           Mountain
#> 7    09  CT   09 1779780 Northeast        New England
#> 8    10  DE   10 1779781     South     South Atlantic
#> 9    11  DC   11 1702382     South     South Atlantic
#> 10   12  FL   12 0294478     South     South Atlantic
#> 11   13  GA   13 1705317     South     South Atlantic
#> 12   15  HI   15 1779782      West            Pacific
#> 13   16  ID   16 1779783      West           Mountain
#> 14   17  IL   17 1779784   Midwest East North Central
#> 15   18  IN   18 0448508   Midwest East North Central
#> 16   19  IA   19 1779785   Midwest West North Central
#> 17   20  KS   20 0481813   Midwest West North Central
#> 18   21  KY   21 1779786     South East South Central
#> 19   22  LA   22 1629543     South West South Central
#> 20   23  ME   23 1779787 Northeast        New England
#> 21   24  MD   24 1714934     South     South Atlantic
#> 22   25  MA   25 0606926 Northeast        New England
#> 23   26  MI   26 1779789   Midwest East North Central
#> 24   27  MN   27 0662849   Midwest West North Central
#> 25   28  MS   28 1779790     South East South Central
#> 26   29  MO   29 1779791   Midwest West North Central
#> 27   30  MT   30 0767982      West           Mountain
#> 28   31  NE   31 1779792   Midwest West North Central
#> 29   32  NV   32 1779793      West           Mountain
#> 30   33  NH   33 1779794 Northeast        New England
#> 31   34  NJ   34 1779795 Northeast       Mid-Atlantic
#> 32   35  NM   35 0897535      West           Mountain
#> 33   36  NY   36 1779796 Northeast       Mid-Atlantic
#> 34   37  NC   37 1027616     South     South Atlantic
#> 35   38  ND   38 1779797   Midwest West North Central
#> 36   39  OH   39 1085497   Midwest East North Central
#> 37   40  OK   40 1102857     South West South Central
#> 38   41  OR   41 1155107      West            Pacific
#> 39   42  PA   42 1779798 Northeast       Mid-Atlantic
#> 40   44  RI   44 1219835 Northeast        New England
#> 41   45  SC   45 1779799     South     South Atlantic
#> 42   46  SD   46 1785534   Midwest West North Central
#> 43   47  TN   47 1325873     South East South Central
#> 44   48  TX   48 1779801     South West South Central
#> 45   49  UT   49 1455989      West           Mountain
#> 46   50  VT   50 1779802 Northeast        New England
#> 47   51  VA   51 1779803     South     South Atlantic
#> 48   53  WA   53 1779804      West            Pacific
#> 49   54  WV   54 1779805     South     South Atlantic
#> 50   55  WI   55 1779806   Midwest East North Central
#> 51   56  WY   56 1779807      West           Mountain
#> 52   60  AS   60 1802701      <NA>               <NA>
#> 53   66  GU   66 1802705      <NA>               <NA>
#> 54   69  MP   69 1779809      <NA>               <NA>
#> 55   72  PR   72 1779808      <NA>               <NA>
#> 56   74  UM   74 1878752      <NA>               <NA>
#> 57   78  VI   78 1802710      <NA>               <NA>
```
