require(readr)
require(data.table)

physician.fees <- fread(file.choose())
colnames(physician.fees) <- c("Year", "CarrierNumber", "Locality", "HCPCS", "Modifier", "NonFacilityFeeAmt", "FacilityFeeAmt", "PCTC", "StatusCode", "MulitpleSurgery", "TherapyReductionAmt", "TherapyAdjustmentAmtV2", "OPPSIndicator", "OPPSNonFacilityAmt", "OPPSFacilityAmt", "Trailer")


emory <- read_json(file.choose())

emory <- fromJSON('https://www.emoryhealthcare.org/ui/pricing-transparency/json/2022/110078_emory-university-hospital-midtown_standardcharges.json')
sd.mercy <- fread(file.choose())
