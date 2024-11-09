/*Importing Data Using Proc import*/
proc import
	datafile = '/home/u63766652/FinalProject/freshman-15.csv'
	out=weight
    dbms=csv
    replace;
run;

/* Checking normality using QQ plot */
proc univariate data= weight normal;
var InitialWeight TerminalWeight;
qqplot / normal (mu=est sigma=est);
run;

/* Shapiro-Wilk Test for Normality */
proc univariate data=weight normal;
   var InitialWeight TerminalWeight;
run;

/*HOMOGENEITY OF VARIANCE CHECK*/
/* Sorting the data */
proc sort data=weight;
    by InitialWeight TerminalWeight;
run;

/* Create box plot for InitialWeight */
proc sgplot data=weight;
    vbox InitialWeight / ;
    xaxis label='Initial Weight';
    yaxis label='Weight';
    title 'Box Plot of Initial Weight';
run;
/* Create box plot for TerminalWeight */
proc sgplot data=weight;
    vbox TerminalWeight / ;
    xaxis label='Terminal Weight';
    yaxis label='Weight';
    title 'Box Plot of Terminal Weight';
run;


/* Assumptions for wilcoxon signed rank test */
/*Randomness*/
/*Independence*/
/*At least ordinal scale*/
/* Create new dataset with difference between two fuel treatments */
data weight2;
set weight;
diff = InitialWeight - TerminalWeight ;
run;
/*Normality*/
proc univariate data=weight2;
   var diff;
   histogram / normal;
run;

/* Perform Wilcoxon Signed-Rank Test */
proc univariate data=weight2 mu0=0;
   var Diff;
   ods select TestsForLocation;
run;











