library(treemap)


setwd("C://Users//fatem//Downloads/customer_insurance")
library(readxl)
y <- read_excel("treamap.xlsx",sheet = "Sheet2")
head(y)
treemap(y, index=c("shobe","agent"), vSize="avg_sat", type="index",
        fontsize.labels=c(16,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","black"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ),                                   # Where to place labels in the rectangle?
        overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=F                        # If true, labels are bigger when rectangle is bigger.
        )

