##### hmw2:

## >>> TRY A: Find the 5 fastest turnaround times overall, across
##            all results and demo groups.

## >>> TRY B: Create a new variable for turnaround time in MINUTES
##            instead of hours, then select just patient age, result,
##            and the new column.

## >>> TRY C: Group by payor_group and summarize the average patient
##            age and the number of tests per group. Which payor
##            group has the oldest average patient age?

## >>> TRY D (stretch): Use slice_max() to find the single slowest
##            test PER CLINIC (hint: group_by(clinic_name) first,
##            then slice_max(col_rec_tat, n = 1)).
