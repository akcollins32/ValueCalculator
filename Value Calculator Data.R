require(readr)
require(data.table)

physician.fees <- fread(file.choose())
colnames(physician.fees) <- c("Year", "CarrierNumber", "Locality", "HCPCS", "Modifier", "NonFacilityFeeAmt", "FacilityFeeAmt", "PCTC", "StatusCode", "MulitpleSurgery", "TherapyReductionAmt", "TherapyAdjustmentAmtV2", "OPPSIndicator", "OPPSNonFacilityAmt", "OPPSFacilityAmt", "Trailer")
