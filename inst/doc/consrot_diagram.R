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
#               dist = 0.05,
#               cex = 0.8)

## ----eval=FALSE---------------------------------------------------------------
#  add_box(prev_box = NULL, # Previous node, left blank if this is the first node.
#          txt,             # Text in the node
#          dist = 0.02)     # Distance between box
#  
#  
#  add_side_box(prev_box,
#               txt,
#               dist = 0.02)
#  
#  add_split(prev_box,
#            txt,
#            dist = 0.02)
#  
#  add_label_box(ref_box, # Reference node to horizontally align with
#                txt)
#  
#  build_consort(consort_list,      # A list of flow chart nodes
#                label_list = NULL), # A list of node labels
#  

## ----message=FALSE, fig.width  = 7, fig.height = 6----------------------------
out <- consort_plot(data = df,
             order = c(trialno = "Population",
                          exc1    = "Excluded",
                          arm     = "Allocated",
                          fow1    = "Lost of Follow-up",
                          trialno = "Finished Followup",
                          fow2    = "Not evaluable for the final analysis",
                          trialno = "Final Analysis"),
             side_box = c("exc1", "fow1", "fow2"),
             cex = 0.9)
out


## ----fig.width  = 10, fig.height = 7------------------------------------------
out <- consort_plot(data = df,
             order = c(trialno = "Population",
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

out

## ----fig.width  = 11, fig.height = 7------------------------------------------
consort_plot(data = df,
             order = c(trialno = "Population",
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

## ----fig.width  = 11, fig.height = 9------------------------------------------
consort_plot(data = df,
             order = list(trialno = "Population",
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


## ----fig.width  = 9, fig.height = 7-------------------------------------------
library(grid)
# Might want to change some settings
options(boxGrobTxt = gpar(color = "black", cex = 0.8),
          boxGrob  = gpar(color = "black", cex = 0.8),
          connectGrobArrow = arrow(length = unit(0.1, "inches"),
                                   type = "closed"))

txt1 <- "Population (n=300)"
txt1_side <- "Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)"

node1 <- add_box(txt = txt1)

node3 <- add_side_box(node1, txt = txt1_side)    

node4 <- add_box(node3, txt = "Randomized (n=200)")

node1_sp <- add_split(node4, txt = c("Arm A (n=100)", "Arm B (n=100"))
side1_sp <- add_side_box(node1_sp, txt = c("Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)",
                                           "Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)"))

node2_sp <- add_box(side1_sp, txt = c("Final analysis (n=100)", "Final analysis (n=100"))

lab1 <- add_label_box(node1, txt = "Screening")
lab2 <- add_label_box(node4, txt = "Randomized")
lab3 <- add_label_box(node2_sp, txt = "Final analysis")

g <- build_consort(list(node1,
                   node3,
                   node4,
                   node1_sp,
                   side1_sp,
                   node2_sp),
              list(lab1, lab2, lab3))
g

## ----fig.width  = 9, fig.height = 7-------------------------------------------

df$arm <- factor(df$arm)

txt <- box_text(df$trialno, label = "Patient consented")
node1 <- add_box(txt = txt)

txt <- box_text(df$exc, label = "Excluded", bullet = TRUE)
node_sd1 <- add_side_box(node1, txt = txt)   

# Exclude subjects
df <- df[is.na(df$exc), ]

node2 <- add_box(node_sd1, txt = box_text(df$arm, label = "Patients randomised")) 

txt <- box_text(df$arm)
node3 <- add_split(node2, txt = txt)

txt <- box_text(split(df$fow1, df$arm),
                label = "Lost to follow-up", bullet = TRUE)
node4 <- add_box(node3, txt = txt, just = "left")

df <- df[is.na(df$fow1), ]
txt <- box_text(split(df$trialno, df$arm),
                label = "Primary analysis")
node5 <- add_box(node4, txt = txt)

lab1 <- add_label_box(node4, txt = "Baseline")
lab2 <- add_label_box(node5, txt = "First Stage")


g <- build_consort(list(node1,
                        node_sd1,
                        node2,
                        node3,
                        node4,
                        node5),
                   list(lab1, lab2))
g

## ----eval=FALSE---------------------------------------------------------------
#  # save plots
#  png("consort_diagram.png", width = 29,
#      height = 21, res = 300, units = "cm", type = "cairo")
#  out
#  dev.off()
#  

## ----eval=FALSE---------------------------------------------------------------
#  ggplot2::ggsave("consort_diagram.pdf", plot = p)

