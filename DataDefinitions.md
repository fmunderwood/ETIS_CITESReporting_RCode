## Definitions and data
An expanded version of this is given in the document [Notes_Rscripts_ETIS_analysis CITES_CoP18.pdf]( Notes_RScripts_ETIS_analysis_CITES_CoP18.pdf)

The ETIS data are held in a PostgreSQL relational database.

### Basic data
For every seizure in the ETIS database the minimal information for its inclusion in the analysis is that it describes:

*	The year in which the seizure was made
*	The country that made the seizure – the country of discovery (which can be any country in the trade chain or, sometimes, the only country known in a trade chain)
*	Quantity – either weight (kgs) or number of pieces – of raw and/or worked ivory
*	Is status 3 or above – indicating that the record has been validated by ETIS

For most analytical purposes seizures that contain both raw and worked ivory are counted as two separate seizures. 

### Countries on the trade chain
Other information about the seizure that is used in the analysis concerns other countries that are listed on the trade chain. For any seizure the following information may also be provided:

*	Countries of origin – where the ivory originated from and the proportion of ivory in the seizure that comes from this country
*	Countries of export – where the shipment was, or was to be, exported (or re-exported) from
*	Countries of transit – where the shipment passed, or was passing through, between export and destination.  In each case it is noted whether or not the country falls within the chain of custody; in other words, the consignment was in the legal jurisdiction of the country for a period of time and could have been seized if detected. If it was, it is marked as having a seizure opportunity. 
*	Country of destination – the final destination of the shipment

This information is not always available. Sometimes the same country can have multiple roles in the trade chain (for example, it could be the country of origin, export and discovery, or the country of discovery and destination). 

### Country codes
All countries have a unique identifier code and are also described using ISO 3166-1 alpha-2 country codes. In the database they are recorded in capitals – e.g. US, KE. Country codes are converted to lower case within R. This makes it easier to track Namibia (ISO code NA – which means Not Available in R).

In some cases a pseudo country is listed – for example a country of origin marked as XF means that it came from a country in Africa but the particular country is not known. These pseudo-countries are excluded when analysis is carried out at country level.

### Seizures in and out
For analysis, a seizure that is made in a particular country is listed as a *seizure in* for that country (country of discovery). A seizure is listed as a *seizure out* for countries on the trade chain that are not the country of discovery. For example consider the following hypothetical shipment:

> The shipment of one tonne of raw ivory was seized in SG. The ivory was from UG (20%) and KE (80%). It was exported from KE and then passed through AE where it was part of the chain of custody and MY where it was not part of the chain of custody before being seized in SG. The destination of the shipment was CN.

There are two ways of counting *seizures out* depending on what is being measured.

*	To estimate a measure of law enforcement and effectiveness the seizure is counted as a *seizure out* only for countries that had an opportunity to make a seizure. Currently this is all countries listed on the trade chain prior to the country of destination. In the hypothetical example for the countries UG, KE and AE this seizure would count as a *seizure out*. It does not count as a *seizure out* for CN because there was no chance for law enforcement activity in CN because the consignment never reached CN. 

*	To estimate trade flows the seizure is counted as a *seizure out* for all countries on the trade chain (that are not the country of discovery) because the intent is to capture the intended known pathway of each transaction. In the hypothetical example it would cound as a *seizure out* for UG, KE, AE and CN.

In neither case would this seizure count as a *seizure out* for MY because it is never part of the chain of custody and so the consignment was never officially within the jurisdiction of MY. In general, transit countries with no seizure opportunity are not included in any analysis unless specifically noted. 

### Weights in and out

In the cluster analysis *weights in* and *weights out* are also calculated for each country. A seizure’s weight counts towards the *weights in* total for the country of discovery. So in the hypothetical example the shipment contributes a *weight in* of one tonne to SG.

A seizure’s weight contributes to the *weights out* total for the countries in which it counts as a *seizure out*. However, for countries which are only listed as a country of origin, only the portion of the ivory that comes from that country is included. In the hypothetical example above, a *weight out* of 200kg (rather than one tonne) is used for UG. For all other countries – KE, AE and CN – a weight of one tonne is used. Although KE was the country of origin for 800kg of the ivory, the whole shipment including the 200kg from UG was exported from KE. So for this shipment the *weight out* for KE is one tonne.
