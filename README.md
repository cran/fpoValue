# fpoValue

Farmer Producer Organisation economics: determine when collective marketing,
aggregation, grading and bulk procurement improve member-farmer returns, and
distinguish the total surplus an FPO generates from the benefits farmers
actually receive.

## Installation

```r
install.packages("fpoValue")   # once accepted on CRAN
```

## Functions

| Function | Purpose |
|---|---|
| `transaction_costs()` | Per-farmer transaction costs of a marketing channel |
| `aggregation_savings()` | Transport, bulk-input and grading economies of aggregation |
| `member_benefit()` | FPO surplus vs member benefits; distribution across members |
| `break_even_volume()` | Break-even throughput, membership and procurement price |
| `collective_marketing_scenarios()` | Individual vs collective channels compared |

## Example

```r
library(fpoValue)
q <- c(5, 8, 12, 20, 35, 60)
member_benefit(q, outside_price = 1400, member_price = 1480,
               sale_price = 1650, outside_cost = 140, member_cost = 40,
               handling_cost = 60, fixed_cost = 8000, dividend_share = 0.5)
```
