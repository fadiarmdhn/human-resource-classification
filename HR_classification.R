# Load data
nursery <- read.csv("nursery.csv", header = TRUE, sep = ",")

# Ringkasan Data
str(nursery)

# Lihat 6 baris pertama
head(nursery)

summary(nursery)

library(arules)
rules <- apriori(nursery)

rules <- sort(rules, by="confidence")
inspect(rules[1:10])

# Mengatur Parameter
params = list(minlen = 2, 
              support = 0.05,
              confidence = 0.8)

# Mengatur appearance
rules <- apriori(nursery, parameter = params,
                 appearance = list(rhs = c("class=not_recom", "class=very_recom",
                                           "class=priority", "class=spec_prior"),
                                            default = "lhs"))
rules <- sort(rules, by = "confidence")
inspect(rules)

# Membentuk matriks hubungan subset
subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE

# Menentukan rules yang redundan
redundant <- colSums(subset_matrix) > 1
which(redundant)

# Ambil rules yang tidak redundan
rules_pruned <- rules[!redundant]
inspect(rules_pruned)

# Visualisasi
library(arulesViz)
plot(rules_pruned)
plot(rules_pruned, method="grouped")
