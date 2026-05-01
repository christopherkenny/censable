# Recode Abb by Name

Replaces state name with state abbreviation

## Usage

``` r
recode_abb_name(.data, .name)
```

## Arguments

- .data:

  data.frame or tibble

- .name:

  column with state name

## Value

.data with column .name replaced with abbreviation

## Examples

``` r
data('stata')
stata |> recode_abb_name(name)
#>    fips abb name    ansi    region           division
#> 1    01  AL   AL 1779775     South East South Central
#> 2    02  AK   AK 1785533      West            Pacific
#> 3    04  AZ   AZ 1779777      West           Mountain
#> 4    05  AR   AR 0068085     South West South Central
#> 5    06  CA   CA 1779778      West            Pacific
#> 6    08  CO   CO 1779779      West           Mountain
#> 7    09  CT   CT 1779780 Northeast        New England
#> 8    10  DE   DE 1779781     South     South Atlantic
#> 9    11  DC   DC 1702382     South     South Atlantic
#> 10   12  FL   FL 0294478     South     South Atlantic
#> 11   13  GA   GA 1705317     South     South Atlantic
#> 12   15  HI   HI 1779782      West            Pacific
#> 13   16  ID   ID 1779783      West           Mountain
#> 14   17  IL   IL 1779784   Midwest East North Central
#> 15   18  IN   IN 0448508   Midwest East North Central
#> 16   19  IA   IA 1779785   Midwest West North Central
#> 17   20  KS   KS 0481813   Midwest West North Central
#> 18   21  KY   KY 1779786     South East South Central
#> 19   22  LA   LA 1629543     South West South Central
#> 20   23  ME   ME 1779787 Northeast        New England
#> 21   24  MD   MD 1714934     South     South Atlantic
#> 22   25  MA   MA 0606926 Northeast        New England
#> 23   26  MI   MI 1779789   Midwest East North Central
#> 24   27  MN   MN 0662849   Midwest West North Central
#> 25   28  MS   MS 1779790     South East South Central
#> 26   29  MO   MO 1779791   Midwest West North Central
#> 27   30  MT   MT 0767982      West           Mountain
#> 28   31  NE   NE 1779792   Midwest West North Central
#> 29   32  NV   NV 1779793      West           Mountain
#> 30   33  NH   NH 1779794 Northeast        New England
#> 31   34  NJ   NJ 1779795 Northeast       Mid-Atlantic
#> 32   35  NM   NM 0897535      West           Mountain
#> 33   36  NY   NY 1779796 Northeast       Mid-Atlantic
#> 34   37  NC   NC 1027616     South     South Atlantic
#> 35   38  ND   ND 1779797   Midwest West North Central
#> 36   39  OH   OH 1085497   Midwest East North Central
#> 37   40  OK   OK 1102857     South West South Central
#> 38   41  OR   OR 1155107      West            Pacific
#> 39   42  PA   PA 1779798 Northeast       Mid-Atlantic
#> 40   44  RI   RI 1219835 Northeast        New England
#> 41   45  SC   SC 1779799     South     South Atlantic
#> 42   46  SD   SD 1785534   Midwest West North Central
#> 43   47  TN   TN 1325873     South East South Central
#> 44   48  TX   TX 1779801     South West South Central
#> 45   49  UT   UT 1455989      West           Mountain
#> 46   50  VT   VT 1779802 Northeast        New England
#> 47   51  VA   VA 1779803     South     South Atlantic
#> 48   53  WA   WA 1779804      West            Pacific
#> 49   54  WV   WV 1779805     South     South Atlantic
#> 50   55  WI   WI 1779806   Midwest East North Central
#> 51   56  WY   WY 1779807      West           Mountain
#> 52   60  AS   AS 1802701      <NA>               <NA>
#> 53   66  GU   GU 1802705      <NA>               <NA>
#> 54   69  MP   MP 1779809      <NA>               <NA>
#> 55   72  PR   PR 1779808      <NA>               <NA>
#> 56   74  UM   UM 1878752      <NA>               <NA>
#> 57   78  VI   VI 1802710      <NA>               <NA>
```
