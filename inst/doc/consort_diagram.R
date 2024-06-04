## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(consort)
data(dispos.data)

## ----data-head, echo=FALSE----------------------------------------------------
head(dispos.data)

## ----example, eval=FALSE------------------------------------------------------
#  consort_plot(data,
#               orders,
#               side_box,
#               allocation = NULL,
#               labels = NULL,
#               cex = 0.8,
#               text_width = NULL,
#               widths = c(0.1, 0.9))

## ----patchwork, eval=FALSE----------------------------------------------------
#  library(patchwork)
#  
#  wrap_elements(build_grid(g)) + plot_annotation(
#    title = 'Consort diagram',
#    subtitle = 'Flow chart of the XX study',
#    caption = 'Disclaimer: None of these plots are insightful'
#  )

## ----dump-dotfile, eval = FALSE-----------------------------------------------
#  cat(build_grviz(g), file = "consort.gv")

## ----single-arms, message=FALSE, fig.width  = 6, fig.height = 6---------------
out <- consort_plot(data = dispos.data,
                    orders = c(trialno = "Population",
                               exclusion = "Excluded",
                               trialno = "Allocated",
                               subjid_notdosed = "Not dosed",
                               followup = "Followup",
                               lost_followup = "Not evaluable for the final analysis",
                               mitt = "Final Analysis"),
                    side_box = c("exclusion", "subjid_notdosed", "lost_followup"),
                    cex = 0.9)
plot(out)

## ----multiple-phase, fig.width  = 9, fig.height = 6---------------------------
g <- consort_plot(data = dispos.data,
                  orders = c(trialno = "Population",
                             exclusion1    = "Excluded",
                             induction   = "Induction",
                             exclusion2    = "Excluded",
                             arm3     = "Randomized patient",
                             subjid_notdosed = "Not dosed",
                             mitt = "Final miTT Analysis"),
                  side_box = c("exclusion1", "exclusion2", "subjid_notdosed"),
                  allocation = "arm3",
                  labels = c("1" = "Screening", "2" = "Month 4",
                             "3" = "Randomization", 
                             "5" = "End of study"),
                  cex = 0.7)
plot(g)

## ----multiple-split, fig.width  = 12, fig.height = 8, out.width = "95%"-------
df <- dispos.data[!dispos.data$arm3 %in% "Trt C",]
g <- consort_plot(data = df,
                  orders = c(trialno = "Population",
                             exclusion1    = "Excluded",
                             induction   = "Induction",
                             exclusion2    = "Excluded",
                             arm = "Randomized patient",
                             arm3     = "",
                             subjid_notdosed = "Not dosed",
                             mitt = "Final miTT Analysis"),
                  side_box = c("exclusion1", "exclusion2", "subjid_notdosed"),
                  allocation = c("arm", "arm3"),
                  labels = c("1" = "Screening", "2" = "Month 4",
                             "3" = "Randomization", 
                             "6" = "End of study"),
                  cex = 0.7)
plot(g)

## ----multiple-split-stack, fig.width  = 13, fig.height = 10, out.width = "95%"----
# We will exclude one arm to avoid too many arms.
df <- dispos.data[!dispos.data$arm3 %in% "Trt C",]
p <- consort_plot(data = df,
                  orders = list(c(trialno = "Population"),
                                c(exclusion = "Excluded"),
                                c(arm     = "Randomized patient"),
                                # The following two variables will be stacked together
                                c(arm3     = "", # Should not provide a value to show the actual arm
                                  subjid_notdosed="Participants not treated"),
                                # The following two variables will be stacked together
                                c(followup    = "Pariticpants planned for follow-up",
                                  lost_followup = "Reason for tot followed"),
                                c(assessed = "Assessed for final outcome"),
                                c(no_value = "Reason for not assessed"),
                                c(mitt = "Included in the mITT analysis")),
                  side_box = c("exclusion", "no_value"), 
                  allocation = c("arm", "arm3"), # Two level randomisation
                  kickoff_sidebox = FALSE,
                  labels = c("1" = "Screening", "2" = "Randomization",
                             "5" = "Follow-up", "7" = "Final analysis"),
                  cex = 0.7)

plot(p)

## ----providetext1, fig.width  = 7, fig.height = 4-----------------------------
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

## ----providetext2-------------------------------------------------------------
g <- add_box(txt = c("Study 1 (n=8)", "Study 2 (n=12)", "Study 3 (n=12)"))
g <- add_box(g, txt = "Included All (n=20)")
g <- add_side_box(g, txt = "Excluded (n=7):\n\u2022 MRI not collected (n=3)")
g <- add_box(g, txt = "Randomised")
g <- add_split(g, txt = c("Arm A (n=143)", "Arm B (n=142)"))
g <- add_box(g, txt = c("", "From Arm B"))
g <- add_box(g, txt = "Combine all")
# Length needs to be the same as previous one, use a list here.
g <- add_split(g, txt = list(c("Process 1 (n=140)", "Process 2 (n=140)",
                               "Process 3 (n=142)")))

plot(g, grViz = TRUE)


## ----manual-multisplit, fig.width  = 13, fig.height = 6, out.width = "80%"----
g <- add_box(txt = "Patient consented (n=200)")
g <- add_side_box(g, txt = "Excluded (n=10):\n\u2022 MRI not collected (n=3)\n\u2022 Other (n=7)")
g <- add_box(g, txt = "Randomised (n=190)")
g <- add_split(g, txt = c("Randomised study (n=144)", "Preference study (n=146)"))

# Provide a list here
g <- add_split(g, txt = list(c("Allocated to surgery (n=70)", 
                                "Allocated to medicine (n=74)"),
                             c("Allocated to surgery (n=47)", 
                              "Allocated to medicine (n=48)",
                               "Placebo (n=51)")))

g <- add_side_box(g, txt = c("Excluded (n=3):\n\u2022 Withdrawn before surgery (n=2)\n\u2022 Declined (n=1)", 
                             "", 
                             "Excluded (n=1):\n\u2022 Withdrawn before surgery (n=1)",
                             "Excluded (n=3):\n\u2022 Withdrawn before surgery (n=1)\n\u2022 Declined (n=1)",
                             "Excluded (n=10):6\n\u2022 Declined (n=10)"),
                  side = rep("right", 5))
g <- add_box(g, txt = c("Analysed (n=67)", "Analysed (n=74)",
                        "Analysed (n=46)", "Analysed (n=45)", "Analysed (n=41)"))

plot(g)


## ----disposition-data, fig.width  = 7, fig.height = 6.5-----------------------
options(txt_gp = gpar(cex = 0.8))

dispos.data$arm <- factor(dispos.data$arm)

txt <- gen_text(dispos.data$trialno, label = "Patient consented")
g <- add_box(txt = txt)

txt <- gen_text(dispos.data$exclusion, label = "Excluded", bullet = TRUE)
g <- add_side_box(g, txt = txt)   

g <- add_box(g, txt = gen_text(dispos.data$arm, label = "Patients randomised")) 

txt <- gen_text(dispos.data$arm)
g <- add_split(g, txt = txt)

# Exclude subjects
dispos.data <- dispos.data[is.na(dispos.data$exclusion), ]

txt <- gen_text(split(dispos.data[,c("subjid_notdosed")], dispos.data$arm),
                label = "Not dosed", bullet = TRUE)
g <- add_box(g, txt = txt, just = "left")

txt <- gen_text(split(dispos.data$mitt, dispos.data$arm),
                label = "Primary mITT analysis")
g <- add_box(g, txt = txt)

g <- add_label_box(g, txt = c("1" = "Baseline",
                              "5" = "Final analysis"))

plot(g)

## ----save-plot----------------------------------------------------------------
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

