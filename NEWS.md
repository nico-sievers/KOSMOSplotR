# KOSMOSplotR 2.4.0
* A new style template has been added for KOSMOSsetup: "KOSMOS2024KielQuartzSideExperiment"
* Trying to implement more flexibility towards experimental designs in the coming patches. Currently, regressionplots and stats-calculation in lineplots is not yet working for the Quartz experiment.

---
# KOSMOSplotR 2.3.4
* 1) the user can now choose whether during side-by-side plotting, 0dTA controls are plotted in both panels or just one via "showControlsBothTimes"
* 2) should the data table contain trailing sampling day rows that dont have data yet, these will not be included in the x-axis nor will they trigger warning messages

---
# KOSMOSplotR 2.3.3
* 1) plotting groups side-by-side now actually displays 0dTA controls, the functionality was not correctly implemented before...
* 2) the defaults for customising y-limits as well as the headspace parameter were optimised in order to better deal with data sets containing negative values

---
# KOSMOSplotR 2.3.2
* if line plots show treatment groups side-by-side, then the 0dTA controls are depicted in both panels now

---
# KOSMOSplotR 2.3.1
* fixed a bug in the line plot stats module where only one group would be drawn

---
# KOSMOSplotR 2.3.0
* added the option to plot the two treatment groups side by side in two sub-plots for the timeline plot

---
# KOSMOSplotR 2.2.0
* added the option to subset the data by any column and value before plotting

---
# KOSMOSplotR 2.1.1
* fixed a bug if multiple entries per day and meso where present, and added a warning if such a situation occurs

---
# KOSMOSplotR 2.1.0
* Cleaned the repository by moving functions under development to the new R2HU package. Adjusted some minor mistakes in the documentation

---
# KOSMOSplotR 2.0.0
* Introduced the KOSMOSselect() function and more style templates to allow plotting of data from the Bergen and Helgoland campaigns. Furthermore, introduced the KOSMOSadjustColumnnames() function in combination with further modifications to drastically enhance the ability of the code to process data sets that don't follow exactly the template data sheet. Also added many warning and info outputs to increase transparency of operations to the user.

---
# KOSMOSplotR 1.1.2
* Added the option to exclude days from the timeline graph

---
# KOSMOSplotR 1.1.1
* Fixed two errors in the documentation

---
# KOSMOSplotR 1.1.0
* Regression plot and p-value functions added. Prepared for first sharing with the group.

---
# KOSMOSplotR 1.0.5
* Package renamed.

---
# KOSMOSplotR 1.0.4
* Attempt to patch the below once more time again, to test with Brohannes.

---
# KOSMOSplotR 1.0.3
* Attempt to patch the below once more, to test with Brohannes.

---
# KOSMOSplotR 1.0.2
* Attempt to patch the below.

---
# KOSMOSplotR 1.0.1
* Failed attempt to test updating.

---
# KOSMOSplotR 1.0.0
* Initial Git submission.
