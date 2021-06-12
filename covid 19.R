library(covid19.analytics)
#data
ag <- covid19.data(case = "aggregated")
tsc <- covid19.data(case = "ts-confirmed")
# summary
report.summary(Nentries = 10,
               graphical.output = TRUE)
#total cases as per location
tots.per.location(tsc,geo.loc = "India")
tots.per.location(tsc,geo.loc = c("Iran","India","France"))

# growth rate
growth.rate(tsc,geo.loc = "India")
growth.rate(tsc,geo.loc = c("india","France"))

#world map
live.map(tsc)

# sir model (healthy but susceptible to disease)
generate.SIR.model(tsc, "India", tot.population = 170000000)

