
clear global

global B wm wi1 wi2 u um beta b1 phi d1 d2 rr1 rr2 ri2 ri3 al v2 boost wA;  




load("model_input_data.mat")   % all input data



%loop over states 
for istate=1:6
states=["US" "CA" "IL" "MA" "MS" "TX"];


% preallocate output 
inci_per_10000=zeros(40,55);
annual_cases=zeros(40,55);
monthly_cases=zeros(480,55);
annual_pop=zeros(40,55);
jul_jun_inci_per_10000=zeros(39,55);
jul_jun_pop=zeros(39,55);
numb_vac_1st_dose=zeros(39,55);
    
age=[0:1/12:11/12 1:4 7.5 15 30 50 70];
al=length(age);
avgage=age;
agep=[(1/960)*ones(1,12) (1/80)*ones(1,4) 1/16 1/8 1/4 1/4 1/4];

 
total_pop_1980=pop_1980(istate); %1980 total population 

 

 crude_birth_rate=annual_birth_rate(:,istate)/1000;  % crude birth rate
 crude_birth_rate= repelem( crude_birth_rate,12);
 crude_birth_rate= crude_birth_rate(7:end);         %start from july 1980
 
 crude_death_rate=annual_death_rate(:,istate)/1000;  % crude death rate
 crude_death_rate= repelem(crude_death_rate,12);
 crude_death_rate=  crude_death_rate(7:end);          %start from july 1980
 
 


tmax=726;  %length of simulation from Jul 1980 to Dec 2040
t0=246;  %length of "burn-in" period from  Jul 1980 - Dec 2000

N=total_pop_1980*agep; %Nstate_NY*agep; %population size (in each age group)
N=(N/exp(.008));
B=[crude_birth_rate zeros(tmax,al-1)]; %number of new births per week into each age class
u=[ones(1,12) 1/12*ones(1,4) 1/(12*5) 1/120 1/240 1/240 1/240]; %rate for aging out of each age class (per person per week)


cdr=log(1+crude_death_rate)/12;
immig=ones(length(cdr),1); %interp1(datenum(datepop),[.008*ones(6,1); .012*ones(12,1)],datesim);
immig=log(1+immig);
um=(cdr-pop_scaling_factor(istate)*immig)*ones(1,al)/12;	
		


par=estimated_parameter(:,istate); %previously estimated model parameters



%%% CONTACT RATES %%%
contact_rates=[par(5,1)*ones(al,12) par(6,1)*ones(al,1) par(7,1)*ones(al,1) ones(al,al-14)]; %Age-related acquisition for <3 yr olds

%%% INFECTION PARAMETERS %%%
dur=1/4.3; %duration of infectiousness (1 week = 0.23 months)
d1=1/dur; %rate of recovery from primary infection (per month)
d2=2*d1; %rate of recovery from subsequent infection (per month)
rr1=0.62;  %relative risk of second infection 
rr2=0.35; %relative risk of third infection 
ri2=0.5;   %relative infectiousness of secondary infection
ri3=0.1;   %relative infectiousness of asymptomatic infection 
wm=1/3; %rate of waning maternal immunity (avg duration of immunity = 3 months)
wi1=1/9; %rate of waning immunity following primary infection (avg duration of immunity = 9 months)
wi2=1/12; %rate of waning immunity following 2nd infection (avg duration of immunity = 12 months)
wA=0; %rate of waning of immunity to symptomatic infections (S2->S1)

%%% INPUT PARAMETERS %%%
ptrans=par(1)/dur; %probability of transmission given contact 
beta=ptrans*contact_rates; %transmission matrix

b1=par(2); %seasonal forcing 
phi=par(3); %seasonal offset


h=hosp_scaling_factor(istate)*par(4); %proportion of severe diarrhea cases hospitalized
hosp1=0.13*h*ones(1,al); %proportion of primary infections with severe diarrhea who are hospitalized
hosp2=0.03*h*ones(1,al); %proportion of secondary infections with severe diarrhea who are hospitalized
hosp3=zeros(1,al); %proportion of subsequent infections with severe diarrhea who are hospitalized
delta1=0.45*ones(1,al); %proportion of primary infections that are symptomatic (any diarrhea)
delta2=0.25*ones(1,al); %proportion of secondary infections that are symptomatic (any diarrhea)
delta3=zeros(1,al); %rate of detection of subsequent infections

R0=max(eig(dur*beta.*(ones(al,1)*N)));

%%% INITIALIZE OUTCOME VARIABLES %%%
Hn_1=zeros(tmax-t0,4);
Hn_2=zeros(tmax-t0,4);
Hn_3=zeros(tmax-t0,4);
Dn_1=zeros(tmax-t0,4);
Dn_2=zeros(tmax-t0,4);
Dn_3=zeros(tmax-t0,4);

%%% VACCINATION PARAMETERS %%%
tvacc=60  ; %number of months prior to vaccine introduction (=January 2006)


%for v=19 %1:21 %1:5  
for v=VE_scaling_factor(istate) %36:36 %simulated between v from 18 to 55   
ve1=(69+v)/100; %(65+5*v)/100; %relative effectiveness of 1-dose coverage estimates (compared to full coverage efficacy) 
ve2=0.95; %relative effectiveness of second dose of vaccine (based on seroconversion data)
v2=zeros(tmax,al); %initialize vaccination rate across all ages
boost=zeros(tmax,al); %boosting effect of second dose of vaccine
v2(:,3)=[zeros(t0+tvacc,1); vacc_cov(:,istate)]*ve1; %proportion vaccinated at 2 months of age
boost(:,6)=[zeros(t0+tvacc,1); vacc_cov(:,istate)]*ve2; %proportion vaccinated with second dose at 4 months of age


%Initialize vector to keep track of the numb%er of people in each state
St0=[];
St0(1:al,1)=[N(1) zeros(1,al-1)]; %Maternal immunity
St0(al+1:2*al,1)=[0 N(2:al)-ones(1,al-1)]; %Susceptible_0
St0(2*al+1:3*al,1)=[0 ones(1,al-1)]; %Infectious_1 (primary) 
St0(3*al+1:4*al,1)=zeros(1,al); %Recovered_1
St0(4*al+1:5*al,1)=zeros(1,al); %Susceptible_1
St0(5*al+1:6*al,1)=zeros(1,al); %Infectious_2 (2nd time)
St0(6*al+1:7*al,1)=zeros(1,al); %Recovered_2
St0(7*al+1:8*al,1)=zeros(1,al); %Susceptible-Resistant
St0(8*al+1:9*al,1)=zeros(1,al); %Asymptomatic Infectious_3 (subsequent)
St0(9*al+1:10*al,1)=zeros(1,al); %Temp Resistant

clear St lambda H %clear outcome variables which may be in memory

options=odeset('NonNegative',1:length(St0)); %force solutions to differential equations to be non-negative
[time St]=ode45('ode_equations',1:tmax,St0,options); %solve differential equations defined in 'rasisAW'

time(1:t0,:)=[]; %delete output from from burn-in period
St(1:t0,:)=[]; 

%Initialize outcome variables
lambda=zeros(tmax-t0,al); H=zeros(tmax-t0,al); D=zeros(tmax-t0,al);

for t=1:length(St) %calculate force of infection at time t
    lambda(t,:)=(1+b1*cos(2*pi*(time(t)-phi*12)/12))*((St(t,2*al+1:3*al)+ri2*St(t,5*al+1:6*al)+ri3*St(t,8*al+1:9*al))*beta)./sum(St(t,:));
end

for i=1:al %calculate number of rotavirus hospitalizations (H) and diarrhea cases (D) in each age group across time
    H(:,i)=hosp1(i)*St(:,al+i).*lambda(:,i)+hosp2(i)*rr1*St(:,4*al+i).*lambda(:,i)+hosp3(i)*rr2*St(:,7*al+i).*lambda(:,i);
    D(:,i)=delta1(i)*St(:,al+i).*lambda(:,i)+delta2(i)*rr1*St(:,4*al+i).*lambda(:,i)+delta3(i)*rr2*St(:,7*al+i).*lambda(:,i);
end

pop=St(:,1:al);
for j=1:9
    pop=pop+St(:,j*al+1:(j+1)*al);
end

Hn_1(:,v)=sum(H(1:end,1:13),2); 
Hn_2(:,v)=sum(H(1:end,14:16),2);
Hn_3(:,v)=sum(H(1:end,17),2); 
Dn_1(:,v)=sum(D(1:end,1:13),2);
Dn_2(:,v)=sum(D(1:end,14:16),2);
Dn_3(:,v)=sum(D(1:end,17),2);


%end


aaa=sum(pop(:,1:16),2);
a1=mean(reshape(aaa,12,[]),1).';

cases_output_u=[Hn_1(:,v) Hn_2(:,v) Hn_1(:,v)+Hn_2(:,v) Hn_3(:,v)];  %results

case_data = reshape(cases_output_u, 12, [] , size(cases_output_u, 2));  % Reshape into (12 x n_blocks x columns)
annual_data = sum(case_data, 1);
annual_data = reshape(annual_data, [], size(cases_output_u, 2));  % Resulting size will be (rows/12, columns)
inci_per_10000(:,v)=10000*(annual_data(:,3))./a1;
annual_cases(:,v)=annual_data(:,3);
monthly_cases(:,v)=cases_output_u(:,3);
annual_pop(:,v)=a1;


%% - - - seaosnal incidence from Jul-jun 
Jul_jun_aaa=sum(pop(7:474,1:16),2);
Jul_jun_a1=mean(reshape(Jul_jun_aaa,12,[]),1).';
jul_jun_cases_output_u=[Hn_1(7:474,v) Hn_2(7:474,v) Hn_1(7:474,v)+Hn_2(7:474,v) Hn_3(7:474,v)];  %results
jul_jun_case_data = reshape(jul_jun_cases_output_u, 12, [] , size(jul_jun_cases_output_u, 2));  % Reshape into (12 x n_blocks x columns)
jul_jun_annual_data = sum(jul_jun_case_data, 1);
jul_jun_annual_data = reshape(jul_jun_annual_data, [], size(jul_jun_cases_output_u, 2));  % Resulting size will be (rows/12, columns)
jul_jun_inci_per_10000(:,v)=10000*(jul_jun_annual_data(:,3))./Jul_jun_a1;
jul_jun_pop(:,v)=Jul_jun_a1;

%% for number of infant receiving first dose - - - - - -
zcov=v2(252:719,3);     % from july 2001 and june 2040
zpop=sum(St(7:474,3:21:210),2);  %from july 2001 and june 2040
zvacpop=zcov.*zpop;
zvac_results=sum(reshape(zvacpop,12,[]),1).';
numb_vac_1st_dose(:,v)=zvac_results;

end

%save results_usa.mat inci_per_10000 annual_cases monthly_cases annual_pop

filename = sprintf('results_%s.mat', states(istate));
%filename = sprintf('results_%d.mat', istate);
save(filename, 'inci_per_10000', 'annual_cases', 'monthly_cases', 'annual_pop','jul_jun_inci_per_10000','jul_jun_pop','numb_vac_1st_dose');

end


