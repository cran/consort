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

df$reas1[!is.na(arm)] <- sample(c("Protocol deviation", "Outcome missing", NA),
                                sum(!is.na(arm)), replace = T, 
                                prob = c(0.08, 0.07, 0.85))

df$reas2[!is.na(df$reas1)] <- sample(c("Withdraw", "Discontinued", "Death", "Other"),
                                     sum(!is.na(df$reas1)), replace = T, 
                                     prob = c(0.05, 0.05, 0.05, 0.05))

rm(trialno, exc1, induc, exc2, exc, arm, arm3, fow1, fow2, N)

## ----cars, echo=FALSE---------------------------------------------------------
head(df)

## ----eval=FALSE---------------------------------------------------------------
#  consort_plot(data,
#               orders,
#               side_box,
#               allocation = NULL,
#               labels = NULL,
#               cex = 0.8,
#               text_width = NULL,
#               widths = c(0.1, 0.9))

## ----message=FALSE, fig.width  = 6, fig.height = 6----------------------------
out <- consort_plot(data = df,
             orders = c(trialno = "Population",
                          exc1    = "Excluded",
                          trialno = "Allocated",
                          fow1    = "Lost of Follow-up",
                          trialno = "Finished Followup",
                          fow2    = "Not evaluable for the final analysis",
                          trialno = "Final Analysis"),
             side_box = c("exc1", "fow1", "fow2"),
             cex = 0.9)
plot(out)


## ----fig.width  = 7, fig.height = 6-------------------------------------------
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
             labels = c("1" = "Screening", "2" = "Randomization",
                        "5" = "Final"))

plot(out)

## ----fig.width  = 9, fig.height = 6-------------------------------------------
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
             cex = 0.7)
plot(g)

## ----fig.width  = 7, fig.height = 4-------------------------------------------
library(grid)
# Might want to change some settings
options(txt_gp = gpar(cex = 0.8))

txt0 <- c("Study 1 (n=160)", "Study 2 (n=140)")
txt1 <- "Population (n=300)"
txt1_side <- "Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)"

# supports pipeline operator
g <- add_box(txt = txt0) |>
  add_box(txt = txt1) |>
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

## -----------------------------------------------------------------------------
g <- add_box(txt = c("Study 1 (n=8)", "Study 2 (n=12)", "Study 3 (n=12)"))
g <- add_box(g, txt = "Included All (n=20)")
g <- add_side_box(g, txt = "Excluded (n=7):\n\u2022 MRI not collected (n=3)")
g <- add_box(g, txt = "Randomised")
g <- add_split(g, txt = c("Arm A (n=143)", "Arm B (n=142)"))
g <- add_box(g, txt = c("", "From Arm B"))
g <- add_box(g, txt = "Combine all")
g <- add_split(g, txt = c("Process 1 (n=140)", "Process 2 (n=140)", "Process 3 (n=142)"))

plot(g, grViz = TRUE)


## ----fig.width  = 7, fig.height = 6-------------------------------------------

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

txt <- gen_text(split(df[,c("reas1", "reas2")], df$arm),
                label = "Lost to follow-up")
g <- add_box(g, txt = txt, just = "left")

df <- df[complete.cases(df[,c("reas1", "reas2")]), ]
txt <- gen_text(split(df$trialno, df$arm),
                label = "Primary analysis")
g <- add_box(g, txt = txt)

g <- add_label_box(g, txt = c("1" = "Baseline",
                              "3" = "First Stage"))

plot(g)

## -----------------------------------------------------------------------------
plot(g, grViz = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  # save plots
#  png("consort_diagram.png", width = 29,
#      height = 21, res = 300, units = "cm", type = "cairo")
#  plot(g)
#  dev.off()
#  

## ----eval=FALSE---------------------------------------------------------------
#  ggplot2::ggsave("consort_diagram.pdf", plot = build_grid(g))

## ----eval=FALSE---------------------------------------------------------------
#  plot(g, grViz = TRUE) |>
#      DiagrammeRsvg::export_svg() |>
#      charToRaw() |>
#      rsvg::rsvg_pdf("svg_graph.pdf")

