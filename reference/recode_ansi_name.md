# Recode ANSI by Name

Replaces state name with state ansi

## Usage

``` r
recode_ansi_name(.data, .name)
```

## Arguments

- .data:

  data.frame or tibble

- .name:

  column with state name

## Value

.data with column .name replaced with ansi

## Examples

``` r
data('stata')
stata |> recode_ansi_name(name)
#>    fips abb    name    ansi    region           division
#> 1    01  AL 1779775 1779775     South East South Central
#> 2    02  AK 1785533 1785533      West            Pacific
#> 3    04  AZ 1779777 1779777      West           Mountain
#> 4    05  AR 0068085 0068085     South West South Central
#> 5    06  CA 1779778 1779778      West            Pacific
#> 6    08  CO 1779779 1779779      West           Mountain
#> 7    09  CT 1779780 1779780 Northeast        New England
#> 8    10  DE 1779781 1779781     South     South Atlantic
#> 9    11  DC 1702382 1702382     South     South Atlantic
#> 10   12  FL 0294478 0294478     South     South Atlantic
#> 11   13  GA 1705317 1705317     South     South Atlantic
#> 12   15  HI 1779782 1779782      West            Pacific
#> 13   16  ID 1779783 1779783      West           Mountain
#> 14   17  IL 1779784 1779784   Midwest East North Central
#> 15   18  IN 0448508 0448508   Midwest East North Central
#> 16   19  IA 1779785 1779785   Midwest West North Central
#> 17   20  KS 0481813 0481813   Midwest West North Central
#> 18   21  KY 1779786 1779786     South East South Central
#> 19   22  LA 1629543 1629543     South West South Central
#> 20   23  ME 1779787 1779787 Northeast        New England
#> 21   24  MD 1714934 1714934     South     South Atlantic
#> 22   25  MA 0606926 0606926 Northeast        New England
#> 23   26  MI 1779789 1779789   Midwest East North Central
#> 24   27  MN 0662849 0662849   Midwest West North Central
#> 25   28  MS 1779790 1779790     South East South Central
#> 26   29  MO 1779791 1779791   Midwest West North Central
#> 27   30  MT 0767982 0767982      West           Mountain
#> 28   31  NE 1779792 1779792   Midwest West North Central
#> 29   32  NV 1779793 1779793      West           Mountain
#> 30   33  NH 1779794 1779794 Northeast        New England
#> 31   34  NJ 1779795 1779795 Northeast       Mid-Atlantic
#> 32   35  NM 0897535 0897535      West           Mountain
#> 33   36  NY 1779796 1779796 Northeast       Mid-Atlantic
#> 34   37  NC 1027616 1027616     South     South Atlantic
#> 35   38  ND 1779797 1779797   Midwest West North Central
#> 36   39  OH 1085497 1085497   Midwest East North Central
#> 37   40  OK 1102857 1102857     South West South Central
#> 38   41  OR 1155107 1155107      West            Pacific
#> 39   42  PA 1779798 1779798 Northeast       Mid-Atlantic
#> 40   44  RI 1219835 1219835 Northeast        New England
#> 41   45  SC 1779799 1779799     South     South Atlantic
#> 42   46  SD 1785534 1785534   Midwest West North Central
#> 43   47  TN 1325873 1325873     South East South Central
#> 44   48  TX 1779801 1779801     South West South Central
#> 45   49  UT 1455989 1455989      West           Mountain
#> 46   50  VT 1779802 1779802 Northeast        New England
#> 47   51  VA 1779803 1779803     South     South Atlantic
#> 48   53  WA 1779804 1779804      West            Pacific
#> 49   54  WV 1779805 1779805     South     South Atlantic
#> 50   55  WI 1779806 1779806   Midwest East North Central
#> 51   56  WY 1779807 1779807      West           Mountain
#> 52   60  AS 1802701 1802701      <NA>               <NA>
#> 53   66  GU 1802705 1802705      <NA>               <NA>
#> 54   69  MP 1779809 1779809      <NA>               <NA>
#> 55   72  PR 1779808 1779808      <NA>               <NA>
#> 56   74  UM 1878752 1878752      <NA>               <NA>
#> 57   78  VI 1802710 1802710      <NA>               <NA>
```
