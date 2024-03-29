
## Prepare test data
set.seed(1001)
N <- 300

trialno <- sample(c(1000:2000), N)
exc1 <- rep(NA, N)
exc1[sample(1:N, 15)] <- sample(c("Sample not collected", "MRI not collected", "Other"),
  15,
  replace = TRUE, prob = c(0.4, 0.4, 0.2)
)

induc <- rep(NA, N)
induc[is.na(exc1)] <- trialno[is.na(exc1)]

exc2 <- rep(NA, N)
exc2[sample(1:N, 20)] <- sample(c(
  "Sample not collected", "Dead",
  "Other"
), 20,
replace = TRUE,
prob = c(0.4, 0.4, 0.2)
)
exc2[is.na(induc)] <- NA

exc <- ifelse(is.na(exc2), exc1, exc2)

arm <- rep(NA, N)
arm[is.na(exc)] <- sample(c("Conc", "Seq"), sum(is.na(exc)), replace = TRUE)
arm3 <- sample(c("Trt A", "Trt B", "Trt C"), N, replace = TRUE)
arm3[is.na(arm)] <- NA

fow1 <- rep(NA, N)
fow1[!is.na(arm)] <- sample(c("Withdraw", "Discontinued", "Death", "Other", NA),
  sum(!is.na(arm)),
  replace = TRUE,
  prob = c(0.05, 0.05, 0.05, 0.05, 0.8)
)
fow2 <- rep(NA, N)
fow2[!is.na(arm) & is.na(fow1)] <- sample(c("Protocol deviation", "Outcome missing", NA),
  sum(!is.na(arm) & is.na(fow1)),
  replace = TRUE,
  prob = c(0.05, 0.05, 0.9)
)


df <- data.frame(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2)
rm(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2, N)

## Multiple phase
p <- consort_plot(
  data = df,
  order = list(
    trialno = "Population",
    exc1 = "Excluded",
    induc = "Induction",
    exc2 = "Excluded",
    arm3 = "Randomized patient",
    fow1 = "Lost of Follow-up",
    trialno = "Finished Followup",
    fow2 = "Not evaluable",
    trialno = "Final Analysis"
  ),
  side_box = c("exc1", "exc2", "fow1", "fow2"),
  allocation = "arm3",
  labels = c(
    "1" = "Screening", "2" = "Month 4",
    "3" = "Randomization", "5" = "Month 24",
    "6" = "End of study"
  ),
  cex = 0.7
)