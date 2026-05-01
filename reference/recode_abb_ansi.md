# Recode Abb by ANSI

Replaces state ansi with state abbreviation

## Usage

``` r
recode_abb_ansi(.data, .ansi)
```

## Arguments

- .data:

  data.frame or tibble

- .ansi:

  column with state ansi

## Value

.data with column .ansi replaced with state abbreviation

## Examples

``` r
data('stata')
stata |> recode_abb_ansi(ansi)
#>    fips abb                        name ansi    region           division
#> 1    01  AL                     Alabama   AL     South East South Central
#> 2    02  AK                      Alaska   AK      West            Pacific
#> 3    04  AZ                     Arizona   AZ      West           Mountain
#> 4    05  AR                    Arkansas   AR     South West South Central
#> 5    06  CA                  California   CA      West            Pacific
#> 6    08  CO                    Colorado   CO      West           Mountain
#> 7    09  CT                 Connecticut   CT Northeast        New England
#> 8    10  DE                    Delaware   DE     South     South Atlantic
#> 9    11  DC        District of Columbia   DC     South     South Atlantic
#> 10   12  FL                     Florida   FL     South     South Atlantic
#> 11   13  GA                     Georgia   GA     South     South Atlantic
#> 12   15  HI                      Hawaii   HI      West            Pacific
#> 13   16  ID                       Idaho   ID      West           Mountain
#> 14   17  IL                    Illinois   IL   Midwest East North Central
#> 15   18  IN                     Indiana   IN   Midwest East North Central
#> 16   19  IA                        Iowa   IA   Midwest West North Central
#> 17   20  KS                      Kansas   KS   Midwest West North Central
#> 18   21  KY                    Kentucky   KY     South East South Central
#> 19   22  LA                   Louisiana   LA     South West South Central
#> 20   23  ME                       Maine   ME Northeast        New England
#> 21   24  MD                    Maryland   MD     South     South Atlantic
#> 22   25  MA               Massachusetts   MA Northeast        New England
#> 23   26  MI                    Michigan   MI   Midwest East North Central
#> 24   27  MN                   Minnesota   MN   Midwest West North Central
#> 25   28  MS                 Mississippi   MS     South East South Central
#> 26   29  MO                    Missouri   MO   Midwest West North Central
#> 27   30  MT                     Montana   MT      West           Mountain
#> 28   31  NE                    Nebraska   NE   Midwest West North Central
#> 29   32  NV                      Nevada   NV      West           Mountain
#> 30   33  NH               New Hampshire   NH Northeast        New England
#> 31   34  NJ                  New Jersey   NJ Northeast       Mid-Atlantic
#> 32   35  NM                  New Mexico   NM      West           Mountain
#> 33   36  NY                    New York   NY Northeast       Mid-Atlantic
#> 34   37  NC              North Carolina   NC     South     South Atlantic
#> 35   38  ND                North Dakota   ND   Midwest West North Central
#> 36   39  OH                        Ohio   OH   Midwest East North Central
#> 37   40  OK                    Oklahoma   OK     South West South Central
#> 38   41  OR                      Oregon   OR      West            Pacific
#> 39   42  PA                Pennsylvania   PA Northeast       Mid-Atlantic
#> 40   44  RI                Rhode Island   RI Northeast        New England
#> 41   45  SC              South Carolina   SC     South     South Atlantic
#> 42   46  SD                South Dakota   SD   Midwest West North Central
#> 43   47  TN                   Tennessee   TN     South East South Central
#> 44   48  TX                       Texas   TX     South West South Central
#> 45   49  UT                        Utah   UT      West           Mountain
#> 46   50  VT                     Vermont   VT Northeast        New England
#> 47   51  VA                    Virginia   VA     South     South Atlantic
#> 48   53  WA                  Washington   WA      West            Pacific
#> 49   54  WV               West Virginia   WV     South     South Atlantic
#> 50   55  WI                   Wisconsin   WI   Midwest East North Central
#> 51   56  WY                     Wyoming   WY      West           Mountain
#> 52   60  AS              American Samoa   AS      <NA>               <NA>
#> 53   66  GU                        Guam   GU      <NA>               <NA>
#> 54   69  MP    Northern Mariana Islands   MP      <NA>               <NA>
#> 55   72  PR                 Puerto Rico   PR      <NA>               <NA>
#> 56   74  UM U.S. Minor Outlying Islands   UM      <NA>               <NA>
#> 57   78  VI         U.S. Virgin Islands   VI      <NA>               <NA>
```
