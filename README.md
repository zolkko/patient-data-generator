# patient-data-generator

This script takes NAMCS data (in raw folder), and uses it to generate a set of representative EHR patient data.  The config section of the script allows you to change the number of patient records to generate, add in randomization, change the sampling weight methodology, and randomly remove data (to more realistically simulate EHR data).

To run, first unzip raw/namcs2012-stata.dta.zip, and then run generate.R

    R CMD BATCH generate.R

The following tables will be generated:

1. patient: contains information about the patient
2. diagnosis: contains information about diagnoses the patient may have
3. prescription: contains inforamtion about prescriptions the patient is currently on
4. encounter: contains information about the patient's visits or encounters (note: currently there's a 1:1 relationship between patients and encounters)
5. encountermeasure: contains measurements taken during the encounter
6. labresult: contains lab results for the patient

Note that data is coded using:

* measurement/lab: LOINC
* prescription: Multum Lexicon Plus
* diagnosis: ICD9
