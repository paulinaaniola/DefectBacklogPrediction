# DefectBacklogPrediction

#### Data

Defect reports comes from open source projects and were collected using Bugzilla bugtracker.  

| Component   | Bug tracking period |
| ------------- | ------------- |
| Eclipse Platform  | 10-10-2001 - 01-01-2019 |
| Eclipse Birt  | 15-03-2005 - 01-01-2019  |
| Eclipse Jdt  | 03-03-2005 - 01-01-2019 |
| Eclipse Data tools  | 03-03-2005 - 01-01-2019  |
| Eclipse PDE  | 20-11-2001 - 01-01-2019 |
| Mozilla Firefox  | 30-07-1999 - 01-01-2019 |
| Mozilla Core  | 28-03-1997 - 01-01-2019 |
| Mozilla Thunderbird  | 02-01-2000 - 01-01-2019 |
| Mozilla Calendar  | 09-11-2000 - 01-01-2019 |
| Kernel File System  | 18-11-2002 - 01-01-2019 |
| Kernel Networking  | 15-11-2005 - 01-01-2019 |
| Kernel IO Storage  | 14-11-2002 - 01-01-2019 |
| Open Office Writer  | 30-10-2000 - 01-01-2019 |
| Open Office Calc  | 23-10-2000 - 01-01-2019 |
| Open Office Draw  | 30-10-2000 - 01-01-2019 |
| Apache Ant  | 11-09-2000 - 01-01-2019 |
| Apache Apache2  | 15-01-2001 - 01-01-2019 |
| Libre Office Writer  | 15-01-2001 - 01-01-2019 |
| Libre Office Calc  | 08-10-2010 - 01-01-2019 |
| Libre Office Draw  | 15-01-2011 - 01-01-2019 |


Data related to the single component consists of three files:
- *component_name*_reports - defect reports from Bugzilla
- *component_name*_backlog - defect reports converted into weekly defect backlog
- *component_name*_samples - set of week numbers from defect backlog for further predictions

These files are located in Data/*project_name*/*component_name* folder. 

#### Defect backlog

Defect reports were converted into weekly defect backlog. Every week in the backlog includes the following metrics:
- backlog_all - the number of all defects in the backlog
- inflow_all - the number of defects that were reported in corresponding week
- outflow_all - the number of defects that were resolved in the corresponding week

- backlog_normal - the number of defects labeled as *normal* in the backlog
- inflow_normal - the number of defects labeled as *normal* that were reported in corresponding week
- outflow_normal - the number of defects labeled as *normal* that were resolved in the corresponding week

- backlog_major - the number of labeled as *major*  defects in the backlog
- inflow_major - the number of defects labeled as *major* that were reported in corresponding week
- outflow_major - the number of defects labeled as *major* that were resolved in the corresponding week

#### Samples

For each defect backlog one week was drawn for each month. Week numbers which were drawn are saved in the file *component_name*_samples.
The numbers refers to the week number in the corresponding defect backlog. 
