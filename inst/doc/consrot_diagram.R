## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(consort)

## -----------------------------------------------------------------------------
set.seed(1001)
N <- 300

trialno <- sample(c(1000:2000), N)
exc1 <- rep(NA, N)
exc1[sample(1:N, 15)] <- sample(c("Sample not collected", "MRI not collected",
                                  "Other"), 15, replace = T, prob = c(0.4, 0.4, 0.2))

induc <- rep(NA, N)
induc[is.na(exc1)] <- trialno[is.na(exc1)]

exc2 <- rep(NA, N)
exc2[sample(1:N, 20)] <- sample(c("Sample not collected", "Dead",
                                  "Other"), 20, replace = T, prob = c(0.4, 0.4, 0.2))
exc2[is.na(induc)] <- NA

exc <- ifelse(is.na(exc2), exc1, exc2)

arm <- rep(NA, N)
arm[is.na(exc)] <- sample(c("Conc", "Seq"), sum(is.na(exc)), replace = T)
arm3 <- sample(c("Trt A", "Trt B", "Trt C"), N, replace = T)
arm3[is.na(arm)] <- NA

fow1 <- rep(NA, N)
fow1[!is.na(arm)] <- sample(c("Withdraw", "Discontinued", "Death", "Other", NA),
                            sum(!is.na(arm)), replace = T, 
                            prob = c(0.05, 0.05, 0.05, 0.05, 0.8))
fow2 <- rep(NA, N)
fow2[!is.na(arm) & is.na(fow1)] <- sample(c("Protocol deviation", "Outcome missing", NA),
                                          sum(!is.na(arm) & is.na(fow1)), replace = T, 
                                          prob = c(0.05, 0.05, 0.9))


df <- data.frame(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2)
rm(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2, N)



## ----cars, echo=FALSE---------------------------------------------------------
head(df)

## ----eval=FALSE---------------------------------------------------------------
#  consort_plot(data,
#               orders,
#               side_box,
#               allocation = NULL,
#               labels = NULL,
#               coords = NULL,
#               dist = 0.05,
#               cex = 0.8,
#               text_width = NULL,
#               widths = c(0.1, 0.9))

## ----message=FALSE, fig.width  = 7, fig.height = 6----------------------------
out <- consort_plot(data = df,
             orders = c(trialno = "Population",
                          exc1    = "Excluded",
                          arm     = "Allocated",
                          fow1    = "Lost of Follow-up",
                          trialno = "Finished Followup",
                          fow2    = "Not evaluable for the final analysis",
                          trialno = "Final Analysis"),
             side_box = c("exc1", "fow1", "fow2"),
             cex = 0.9)
plot(out)


## ----fig.width  = 8, fig.height = 7-------------------------------------------
out <- consort_plot(data = df,
             orders = c(trialno = "Population",
                          exc    = "Excluded",
                          arm     = "Randomized patient",
                          fow1    = "Lost of Follow-up",
                          trialno = "Finished Followup",
                          fow2    = "Not evaluable",
                          trialno = "Final Analysis"),
             side_box = c("exc", "fow1", "fow2"),
             allocation = "arm",
             coords = c(0.4, 0.6),
             labels = c("1" = "Screening", "2" = "Randomization",
                        "5" = "Final"))

plot(out)

## ----fig.width  = 9.5, fig.height = 7-----------------------------------------
g <- consort_plot(data = df,
             orders = c(trialno = "Population",
                          exc    = "Excluded",
                          arm3     = "Randomized patient",
                          fow1    = "Lost of Follow-up",
                          trialno = "Finished Followup",
                          fow2    = "Not evaluable",
                          trialno = "Final Analysis"),
             side_box = c("exc", "fow1", "fow2"),
             allocation = "arm3",
             labels = c("1" = "Screening", "2" = "Randomization",
                        "5" = "Final"))
plot(g)

## ----fig.width  = 9, fig.height = 9-------------------------------------------
g <- consort_plot(data = df,
             orders = list(trialno = "Population",
                          exc1    = "Excluded",
                          induc   = "Induction",
                          exc2    = "Excluded",
                          arm3     = "Randomized patient",
                          fow1    = "Lost of Follow-up",
                          trialno = "Finished Followup",
                          fow2    = "Not evaluable",
                          trialno = "Final Analysis"),
             side_box = c("exc1", "exc2", "fow1", "fow2"),
             allocation = "arm3",
             labels = c("1" = "Screening", "2" = "Month 4",
                        "3" = "Randomization", "5" = "Month 24",
                        "6" = "End of study"),
             dist = 0.02,
             cex = 0.7)
plot(g)

## ----fig.width  = 7, fig.height = 5-------------------------------------------
library(grid)
# Might want to change some settings
options(txt_gp = gpar(cex = 0.8))

txt1 <- "Population (n=300)"
txt1_side <- "Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)"

# supports pipeline operator
g <- add_box(txt = txt1) |>
  add_side_box(txt = txt1_side) |> 
  add_box(txt = "Randomized (n=200)") |> 
  add_split(txt = c("Arm A (n=100)", "Arm B (n=100)")) |> 
  add_side_box(txt = c("Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)",
                       "Excluded (n=7):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)")) |> 
  add_box(txt = c("Final analysis (n=85)", "Final analysis (n=93)")) |> 
  add_label_box(txt = c("1" = "Screening",
                        "3" = "Randomized",
                        "4" = "Final analysis"))
plot(g)

## ----fig.width  = 7, fig.height = 7-------------------------------------------

df$arm <- factor(df$arm)

txt <- gen_text(df$trialno, label = "Patient consented")
g <- add_box(txt = txt)

txt <- gen_text(df$exc, label = "Excluded", bullet = TRUE)
g <- add_side_box(g, txt = txt)   

# Exclude subjects
df <- df[is.na(df$exc), ]

g <- add_box(g, txt = gen_text(df$arm, label = "Patients randomised")) 

txt <- gen_text(df$arm)
g <- add_split(g, txt = txt)

txt <- gen_text(split(df$fow1, df$arm),
                label = "Lost to follow-up", bullet = TRUE)
g <- add_box(g, txt = txt, just = "left")

df <- df[is.na(df$fow1), ]
txt <- gen_text(split(df$trialno, df$arm),
                label = "Primary analysis")
g <- add_box(g, txt = txt)

g <- add_label_box(g, txt = c("3" = "Baseline",
                              "4" = "First Stage"))

plot(g)

## ----eval=FALSE---------------------------------------------------------------
#  # save plots
#  png("consort_diagram.png", width = 29,
#      height = 21, res = 300, units = "cm", type = "cairo")
#  plot(g)
#  dev.off()
#  

## ----eval=FALSE---------------------------------------------------------------
#  ggplot2::ggsave("consort_diagram.pdf", plot = g)

