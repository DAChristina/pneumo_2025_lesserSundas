# test power
library(pwr)

p1 <- 0.46*0.56  # VT carriage in unvaccinated children (Lombok 2012, which was based on the 1997 study, was not using serotype identification)
p2 <- p1*0.5  # estimated reduction of VT after vaccination; minimum clinically meaningful difference? How about just 10% reduction (0.35)

# plus oversample 10%-20%
h <- pwr::ES.h(p1, p2)
# group sizes (total = 300; 80% vaccination coverage)
total <- 450
n1 <- total*0.2   # unvaccinated
n2 <- total*0.8  # vaccinated

pwr.2p2n.test(h = h,
              n1 = n1,
              n2 = n2,
              sig.level = 0.05,
              alternative = "two.sided") # can be "greater" (p1 > p2; reduction effect of vaccination) but I would like to keep it "two.sided"
# power has already >= 0.8; 300 samples are sufficient but have to check p1 & p2 based on previous study(es).

total_plus_oversample <- total + total*0.1
total_plus_oversample

# test required samples
p1 <- 0.45  # baseline VT carriage
coverage <- 0.80
VE_values <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9) # vaccine effectiveness
alpha <- 0.05
power <- 0.8

# Calculate sample size for each VE
results <- data.frame(VE = numeric(),
                      PostCarriage = numeric(),
                      Cohen_h = numeric(),
                      SampleSizePerGroup = numeric(),
                      TotalSampleSize = numeric())

for (VE in VE_values) {
  p2 <- post_vt_carriage(p1, coverage, VE)
  h <- ES.h(p1, p2)
  test <- pwr.2p.test(h = h, power = power, sig.level = alpha, alternative = "two.sided")
  results <- rbind(results, data.frame(
    VE = VE,
    PostCarriage = round(p2, 3),
    Cohen_h = round(h, 3),
    SampleSizePerGroup = ceiling(test$n),
    TotalSampleSize = ceiling(2 * test$n)
  ))
}

print(results)
