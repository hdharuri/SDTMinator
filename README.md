# SDTMinator
## Conversion of legacy clinical datasets to SDTM format

SDTM is a data standard developed by CDISC (Clinical Data Interchange Standards Consortium). It is a framework for organizing clinical trial information for submission to FDA. 

Data is organized into 3 general observation classes and special-purpose domains:
	
*	Interventions
*	Events
*	Findings

Special-purpose domains
*	Demographics
*	Comments
*	Subject Elements
*	Subject Visits

The following image displays the various classes and the domains within these classes:

![Alt text](/Images/sdtm_domains.png?raw=true "Title")


###	Strategy to map raw clinical dataset to the SDTM format
####	Scenario 1
![Alt text](/certical_to_horizontal.png?raw=true "Title")

####	Scenario 2

![Alt text](/maintain_association_of_columns.png?raw=true "Title")

###	The mapper file

![Alt text](/what_is_mapper_file.png?raw=true "Title")


