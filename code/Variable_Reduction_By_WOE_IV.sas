libname dataloc 'the designated folder where the SAS data set is stored';
%let inset=model_sample; /* data set name */
%let target=called; /* target variable (y) */
%let libout=C:/output folder; /* folder for export outputs */
%let varall= var1 var2 var3 бнбн varn; /* list of variables to be assessed */
%let tiermax=10; /* max number of bins to assign to variables */
%let ivthresh=0.1; /* set to 0 if you want to see output graphs for all variables */
%let outgraph=iv_woe_graph; /* pdf graph for top predictors */
%let ivout=iv_ranked; /* output file in txt for Information Value */
%let woeout=woe_ranked; /* output file in txt for Weight of Evidence */
%let libdata=dataloc; /* name of library where the data set is stored. */
%let outcome=pct_cust_called; /* name of target for summary tables */
%let outname=% Customers Called; /* label of target for summary tables and graphs */
*********** Changes are needed for underlined codes above only;
***********There is no need to change the following part of the program;
ods output nlevels=checkfreq;
proc freq data=&libdata..&inset nlevels;
tables &varall/noprint;
run;
ods output close;
data varcnt;
set checkfreq;
varcnt+1; run;
proc univariate data=varcnt;
 var varcnt;
 output out=pctscore pctlpts=0 10 20 30 40 50 60 70 80 90 100
 pctlpre=pct_;
run;
data _null_;
set pctscore;
call symputx('start1', 1);
call symputx('end1', int(pct_10)-1);
call symputx('start2', int(pct_10));
call symputx('end2', int(pct_20)-1);
call symputx('start3', int(pct_20));
call symputx('end3', int(pct_30)-1);
call symputx('start4', int(pct_30));
call symputx('end4', int(pct_40)-1);
call symputx('start5', int(pct_40));
call symputx('end5', int(pct_50)-1);
call symputx('start6', int(pct_50));
call symputx('end6', int(pct_60)-1);
call symputx('start7', int(pct_60));
call symputx('end7', int(pct_70)-1);
call symputx('start8', int(pct_70));
call symputx('end8', int(pct_80)-1);
call symputx('start9', int(pct_80));
call symputx('end9', int(pct_90)-1);
call symputx('start10', int(pct_90));
call symputx('end10', pct_100);
run;
** get some important macro values;
** rename the variables;
** select variables with less than needed number of tiers such as 10 in this example;
proc sql;
select tablevar
into :varmore separated by ' '
from varcnt
where nlevels > &tiermax; quit;
proc sql;
create table vcnt as select count(*) as vcnt
from varcnt where nlevels > &tiermax; quit;
data _null_;
set vcnt;
call symputx('vmcnt', vcnt); run;
proc sql;
select tablevar
into :v1-:v&vmcnt
from varcnt
where nlevels > &tiermax; quit;
proc sql;
select max(varcnt), compress('&x'||put(varcnt, 10.))
into :varcount, :tempvar separated by ' '
from varcnt
order by varcnt;
quit;
proc sql;
select tablevar
into :x1-:x&end10
from varcnt; quit;
proc sql;
select count(*)
into :obscnt
from &libdata..&inset; quit;
%macro stkorig;
%do i=1 %to &vmcnt;
data v&i;
length tablevar $32.;
set &libdata..&inset(keep=&&v&i rename=(&&v&i=origvalue));
tablevar="&&v&i";
format tablevar $32.;
attrib _all_ label='';
run;
proc rank data=v&i groups=&tiermax out=v&i;
by tablevar;
var origvalue;
ranks rankvmore;
run;
proc means data=v&i median mean min max nway noprint;
class tablevar rankvmore;
var origvalue;
output out=vmoreranked&i(drop=_type_ _freq_)
 median=med_origv
 mean=mean_origv
 min=min_origv
 max=max_origv;
run;
%end;
%mend;
%stkorig;
data stackorig;
set vmoreranked1-vmoreranked&vmcnt; run;
** make a permanent dataset just in case;
data &libdata..stackorig;
set stackorig; run;
** only rank these variables with more than 10 values;
** the following dataset is for later aggregation in a sas macro;
proc rank data=&libdata..&inset groups=&tiermax out=try_model(keep=&tempvar &target);
var &varmore;
ranks &varmore;
run;
** generate Information Value and Weight of Evidence;
%macro outshell;
%do i=1 %to &varcount;
** count good and bad;
data try_model;
set try_model;
if &&x&i=. then &&x&i=-1000000000;
run;
proc sql;
select sum(case when &target=1 then 1 else 0 end), sum(case when &target=0 then 1 else
0 end), count(*)
 into :tot_bad, :tot_good, :tot_both
from try_model;
quit;
proc sql;
select count(*) into :nonmiss
from try_model
where &&x&i ne -1000000000;
quit;
** compute Weight of Evidence (WoE);
proc sql;
create table woe&i as
(select "&&x&i" as tablevar,
 &&x&i as tier,
 count(*) as cnt,
count(*)/&tot_both as cnt_pct,
 sum(case when &target=0 then 1 else 0 end) as sum_good,
 sum(case when &target=0 then 1 else 0 end)/&tot_good as dist_good,
 sum(case when &target=1 then 1 else 0 end) as sum_bad,
 sum(case when &target=1 then 1 else 0 end)/&tot_bad as dist_bad,
 log((sum(case when &target=0 then 1 else 0 end)/&tot_good)/(sum(case when
&target=1 then 1 else 0 end)/&tot_bad))*100 as woe,
 ((sum(case when &target=0 then 1 else 0 end)/&tot_good)-(sum(case when
&target=1 then 1 else 0 end)/&tot_bad))
*log((sum(case when &target=0 then 1 else 0
end)/&tot_good)/(sum(case when &target=1 then 1 else 0 end)/&tot_bad)) as pre_iv,
sum(case when &target=1 then 1 else 0 end)/count(*) as &outcome
 from try_model
 group by "&&x&i", &&x&i
)
order by &&x&i;
quit;
** compute Information Value (IV);
proc sql;
create table iv&i as select "&&x&i" as tablevar,
 sum(pre_iv) as iv,
 (1-&nonmiss/&obscnt) as pct_missing
from woe&i; quit;
%end;
%mend outshell;
%outshell;
%macro stackset;
%do j=1 %to 10;
data tempiv&j;
length tablevar $32.;
set iv&&start&j-iv&&end&j;
format tablevar $32.;
run;
data tempwoe&j;
length tablevar $32.;
set woe&&start&j-woe&&end&j;
format tablevar $32.;
run;
%end;
%mend;
%stackset;
data &libdata..ivall; set tempiv1-tempiv10; run;
data &libdata..woeall; set tempwoe1-tempwoe10; run;
proc sort data=&libdata..ivall; by descending iv; run;
data &libdata..ivall; set &libdata..ivall; ivrank+1; run;
proc sort data=&libdata..ivall nodupkey out=ivtemp(keep=iv); by descending iv; run;
data ivtemp; set ivtemp; ivtier+1; run;
proc sort data=ivtemp; by iv; run;
proc sort data=&libdata..ivall; by iv; run;
data &ivout;
merge &libdata..ivall ivtemp; by iv; run;
proc sort data=&ivout; by tablevar; run;
proc sort data=&libdata..woeall; by tablevar; run;
data &libdata..iv_woe_all;
merge &ivout &libdata..woeall;
by tablevar; run;
proc sort data=&libdata..iv_woe_all; by tablevar tier; run;
proc sort data=&libdata..stackorig; by tablevar rankvmore; run;
data &libdata..iv_woe_all2;
merge &libdata..iv_woe_all(in=t) &libdata..stackorig(in=s rename=(rankvmore=tier));
by tablevar tier;
if t;
if s then tier=med_origv;
run;
proc sort data=&libdata..iv_woe_all2; by ivrank tier; run;
%let retvar=tablevar iv ivrank ivtier tier cnt cnt_pct dist_good dist_bad woe
 &outcome pct_missing;
data &libdata..&woeout(keep=&retvar);
retain &retvar;
set &libdata..iv_woe_all2;
label tablevar="Variable";
label iv="Information Value";
label ivrank="IV Rank";
label tier="Tier/Bin";
label cnt="# Customers";
label cnt_pct="% Custoemrs";
label dist_good="% Good";
label dist_bad="% Bad";
label woe="Weight of Evidence";
label &outcome="&outname";
label pct_missing="% Missing Values";
run;
** examine KS;
proc npar1way data=&libdata..&inset /* specify the input dataset */
 edf noprint;
 var &varall; /* type your list of predictors(x) here */
 class &target; /* target variable such as BAD */
 output out=ks101(keep= _var_ _D_ rename=(_var_=tablevar _D_=ks));
run;
proc sort data=ks101; by tablevar; run;
proc sort data=&ivout; by tablevar; run;
data &libdata..&ivout;
retain tablevar iv ivrank ivtier ks pct_missing;
merge ks101 &ivout;
by tablevar;
keep tablevar iv ivrank ivtier ks pct_missing;
run;
proc contents data=&libdata..&woeout varnum; run;
proc contents data=&libdata..&ivout varnum; run;
proc sort data=&libdata..&woeout out=&woeout(drop=ivrank rename=(ivtier=iv_rank)); by
ivtier tablevar; run;
proc sort data=&libdata..&ivout out=&ivout(drop=ivrank rename=(ivtier=iv_rank)); by
ivtier; run;
%macro to_excel(data_sum);
PROC EXPORT DATA=&data_sum
 OUTFILE="&libout/&data_sum"
 DBMS=tab REPLACE;
run;
%mend;
%to_excel(&ivout);
%to_excel(&woeout);
proc sql;
select count(distinct ivrank)
into :cntgraph
from &libdata..&ivout
where iv > &ivthresh; quit;
data _null_;
call symputx('endlabel', &cntgraph); run;
** add tier label;
proc sql;
select tablevar, iv
into :tl1-:tl&endlabel, :ivr1-:ivr&endlabel
from &libdata..&ivout
where ivrank le &cntgraph
order by ivrank; quit;
proc template;
define style myfont;
parent=styles.default;
style GraphFonts /
'GraphDataFont'=("Helvetica",8pt)
'GraphUnicodeFont'=("Helvetica",6pt)
'GraphValueFont'=("Helvetica",9pt)
'GraphLabelFont'=("Helvetica",12pt,bold)
'GraphFootnoteFont' = ("Helvetica",6pt,bold)
'GraphTitleFont'=("Helvetica",10pt,bold)
'GraphAnnoFont' = ("Helvetica",6pt)
;
end;
run;

ods pdf file="&libout/&outgraph..pdf" style=myfont;
%macro drgraph;
%do j=1 %to &cntgraph;
proc sgplot data=&libdata..&woeout(where=(ivrank=&j));
vbar tier / response=cnt nostatlabel nooutline fillattrs=(color="salmon");
vline tier / response=&outcome datalabel y2axis lineattrs=(color="blue" thickness=2)
nostatlabel;
label cnt="# Customers";
label &outcome="&outname";
label tier="&&tl&j";
keylegend / location = outside
position = top
noborder
title = "&outname & Acct Distribution: &&tl&j";
format cnt_pct percent7.4;
format &outcome percent7.3;
run;
proc sgplot data=&libdata..&woeout(where=(ivrank=&j));
vbar tier / response=woe datalabel nostatlabel;
vline tier / response=cnt_pct y2axis lineattrs=(color="salmon" thickness=2)
nostatlabel;
label woe="Weight of Evidence";
label cnt_pct="% Customers";
label &outcome="&outname";
label tier="&&tl&j";
keylegend / location = outside
position = top
noborder
title = "Information Value: &&ivr&j (Rank: &j)";
format cnt_pct percent7.4;
run;
%end;
%mend;
%drgraph;
ods pdf close;
