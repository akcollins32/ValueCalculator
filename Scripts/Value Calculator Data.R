require(readr)
require(data.table)
require(tidyverse)
require(rvest)
require(pdftables)
require(filesstrings)


physician.fees <- fread(file.choose(), keepLeadingZeros = T)
colnames(physician.fees) <- c("Year", "CarrierNumber", "Locality", "HCPCS", "Modifier", "NonFacilityFeeAmt", "FacilityFeeAmt", "PCTC", "StatusCode", "MulitpleSurgery", "TherapyReductionAmt", "TherapyAdjustmentAmtV2", "OPPSIndicator", "OPPSNonFacilityAmt", "OPPSFacilityAmt", "Trailer")

physician.fees <- physician.fees%>%
  mutate(LocalityMatch = paste(physician.fees$CarrierNumber, physician.fees$Locality, sep = ""))


physician.fees$FacilityFeeAmt <- trim_anything(physician.fees$FacilityFeeAmt, "0+",side = "left")
physician.fees$NonFacilityFeeAmt <- trim_anything(physician.fees$NonFacilityFeeAmt, "0+",side = "left")
  


phys.fee.local <- left_join(physician.fees, local.f, by = c("LocalityMatch" = "CARRIER"))


#HCPCS Codes
hcpcs <- fread(file.choose())
hcpcs <- hcpcs[,c(1,3,4)] #Removing "action code change" col

physfee.hcpcs <- left_join(phys.fee.local, hcpcs, by = ("HCPCS"))

#CPTs
cpts <- read_html("https://www.sites.google.com/site/cptcodes/codelist")

cpt.table <- cpts %>% html_nodes("table")%>%
  .[[4]]%>%
  html_table(fill = TRUE)
  
  
cpt.table <- cpt.table%>%
  arrange(cpt.table$Code)

#write.csv(cpt.table, "CPT Descriptions.csv", row.names = F)
cpts <- fread("CPT Descriptions.csv")
cpts$Code <- as.character(cpts$Code)

physfee.final <- left_join(physfee.hcpcs, cpts, by = c("HCPCS" = "Code"))
physfee.final <- physfee.final%>%
  mutate(FullDesc = unite(physfee.final,FullDesc, c(physfee.final$`LONG DESCRIPTION`, physfee.final$Description), na.rm = T, remove = F))

physfee.final$Desc <- NA

# 
# i= 1
# for (i in 1:nrow(physfee.final)) {
#   print(i)
#   if(!is.na(physfee.final$`LONG DESCRIPTION`[[i]]) == TRUE){
#     physfee.final$Desc[[i]] = physfee.final$`LONG DESCRIPTION`[[i]]
#   }else{
#     physfee.final$Desc[[i]] = physfee.final$Description[[i]]
#   }
#   
# }


write_rds(physfee.final, "Final Physician Fee Schedule.rds")
write.csv(physfee.final, "Final Physician Fee Schedule.csv")

physfee.final <- read_rds("Final Physician Fee Schedule.rds")


physfee.final.working <- physfee.final%>%
  select(c(17:19,4:7,22:23))%>%
  filter(physfee.final$Modifier == "  ")%>%#only keeping rows with empty modifiers (full charge for service)
  na.omit()#removes nonexistant locality and a number of cpts that did not have descriptions


write_rds(physfee.final.working, "Final Physician Fee Schedule (App).rds")
write.csv(physfee.final.working, "Final Physician Fee Schedule (App).csv")




medicare_labs <- fread(file.choose())



# emory <- read_json(file.choose())
# 
# emory <- fromJSON('https://www.emoryhealthcare.org/ui/pricing-transparency/json/2022/110078_emory-university-hospital-midtown_standardcharges.json')
# sd.mercy <- fread(file.choose())

# Process localities after copying from PDF
localities <- fread("Localities.txt", header = F)
local.f <- as.data.frame(matrix(, nrow=113, ncol = 3))
i = 1
j = 0
for (i in 1:nrow(local.f)) {
  j = j+1 
  local.f[i,1] = localities[j]
  local.f[i,2] = localities[j+1]
  local.f[i,3] = localities[j+2]
  j = j+2
}


setnames(local.f, as.character(local.f[1,]))
local.f <- local.f[-1,]

write.csv(local.f, "Localities Lookup.csv", row.names = F)

#Converting pdf table from ACR to excel with pdftables
convert_pdf('Source/Final Rule Impact Table 70000 Series Website.pdf', output_file = NULL, format = "xlsx-single", message = TRUE, api_key = "xv5n9r3v6tv4")

get_remaining("xv5n9r3v6tv4")
