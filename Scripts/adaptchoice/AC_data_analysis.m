 function AC_data_analysis
%%%%%%%%%%%%%%%%%%%%%%%%%

% Data analysis script for AdaptChoice task. 
% J Irons Jan 2017
% 
% Instructions:
% 
% 1) Ensure this file is in the same folder as the individual data folders
% 
% 2) Update the list of subject numbers:
 sublist = [1:17,19:31]; % Missing: 18, 32
% 
% 3) Hit run
% 
% 4) Four text files will be created:
% 
% Data_AdaptChoice_allsubs_Summary: The main one. Includes Sub number, Acc, RT, Proportion of Optimal Choices, and Switch rate
% Data_AdaptChoice_allsubs_RTAccmeans: RTs broken down into conditions
% Data_AdaptChoice_allsubs_switches: Some more measures of switches
% Data_AdaptChoice_allsubs_earlylateprop: Choices at each position in a run of trials


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Data file column conditions
subNocond = 1;
trial = 2;
blocktrial = 3;
block = 4;
plat = 5; %(red-to-blue = 1, blue-to-red = 2)
trans = 6; %(red-to-blue = 1, blue-to-red = 2)
runnum = 7;
choice = 14;
Acc = 15;
RT = 16;
optchoice = 17; %(0 or 1)
SEopt = 18; %(1 or 2)
repsw = 19; %(1 or 2)

blockno = 3;

run = 1:12;

expname = 'AdaptChoice';
session = 1;

for s = 1:length(sublist)
    
    subNo = sublist(s)
    
     datafilename = strcat(num2str(subNo),'/adaptchoice/Data_',expname,'_',num2str(subNo),'_sess',num2str(session),'.txt');
     datafile = dlmread(datafilename,'',4,0);
     
     colnums = size(datafile,2);
     
     %Exclude Prac trials
     datafile = datafile((datafile(:,block)>0),:);   
     
     % Exclude RT outliers >3SD above mean or <300ms
     meanRT = mean(datafile(datafile(:,Acc)>0,RT));
     SDRT = std(datafile(datafile(:,Acc)>0,RT));
     cutoffRT = meanRT + 3*SDRT;     
     datafile(datafile(:,RT)<300,RT) = NaN;
     datafile(datafile(:,RT)>cutoffRT,RT) = NaN;
     datafile(datafile(:,Acc)==0,RT) = NaN;
     
     % Fix error in code - exclude switches/reps for error trials
     datafile(datafile(:,Acc)==0,repsw) = NaN;

     %% Get Means %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

     % Start/End opt analysis
    for i = run    %for each RunNum
         % Calculate % 
         StartOptPercents(s,i) = (size(datafile((datafile(:,runnum)==i)&(datafile(:,SEopt)==1)&(datafile(:,Acc)==1),:),1))/(size(datafile((datafile(:,runnum)==i)&(datafile(:,Acc)==1),:),1)); % early
         EndOptPercents(s,i) = (size(datafile((datafile(:,runnum)==i)&(datafile(:,SEopt)==2)&(datafile(:,Acc)==1),:),1))/(size(datafile((datafile(:,runnum)==i)&(datafile(:,Acc)==1),:),1)); % late
        
        % Calculate RT   
        StartOptRT(s,i) = nanmean(datafile((datafile(:,runnum)==i)&(datafile(:,SEopt)==1),RT)); % early
        EndOptRT(s,i) = nanmean(datafile((datafile(:,runnum)==i)&(datafile(:,SEopt)==2),RT)); % late
        
        % Calculate cumulative switches
        if i == 1
            CumulativeSwitches(s,i) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,runnum)==i),:),1))/((size(datafile((datafile(:,repsw)>0)&(datafile(:,runnum)==i),:),1))); % Proportion of trials that are switches
        else
            CumulativeSwitches(s,i) = CumulativeSwitches(s,i-1) + ((size(datafile((datafile(:,repsw)==2)&(datafile(:,runnum)==i),:),1))/((size(datafile((datafile(:,repsw)>0)&(datafile(:,runnum)==i),:),1)))); % Proportion of trials that are switches
        end
    end    

    % Mean acc & RT
    MeanAcc(s,1) = nanmean(datafile(:,Acc)); %Accuracy
    MeanRT(s,1) = nanmean(datafile(:,RT)); % RT
    
    %Proportion Efficient trials on Plat
    SubPlatEfficient(s,1) = (size(datafile((datafile(:,Acc)==1)&(datafile(:,plat)>0)&(datafile(:,optchoice)==1),:),1))/(size(datafile((datafile(:,Acc)==1)&(datafile(:,plat)>0),:),1)); %Accuracy trials, plat, efficient
    
    % Proportion efficient per block
    for i = 1:blockno
         BlockPlatEfficient(s,i) = (size(datafile((datafile(:,Acc)==1)&(datafile(:,plat)>0)&(datafile(:,optchoice)==1)&(datafile(:,block)==i),:),1))/(size(datafile((datafile(:,Acc)==1)&(datafile(:,plat)>0)&(datafile(:,block)==i),:),1)); %Accuracy trials, plat, efficient,blockno
         BlockRT(s,i) = nanmean(datafile((datafile(:,block)==i),RT)); % Block RT
    end
     
    % RT for Efficient & Inefficient trials
    SubPlatRT(s,1) = nanmean(datafile((datafile(:,Acc)==1)&(datafile(:,plat)>0)&(datafile(:,optchoice)==1),RT)); %Accuracy trials, plat, efficient
    SubPlatRT(s,2) = nanmean(datafile((datafile(:,Acc)==1)&(datafile(:,plat)>0)&(datafile(:,optchoice)==0),RT)); %Accuracy trials, plat, inefficient
    SubPlatRT(s,3) = SubPlatRT(s,2)-SubPlatRT(s,1);
    SubPlatRT(s,4) = SubPlatRT(s,3)/(SubPlatRT(s,2)+SubPlatRT(s,1));
    
    % Total num Switches per run & switch pos
    Switches(s,1) = (size(datafile((datafile(:,repsw)==2),:),1))/((size(datafile((datafile(:,repsw)>0),:),1))); % Proportion of trials that are switches
    Switches(s,2) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,plat)>0),:),1))/((size(datafile((datafile(:,repsw)>0)&(datafile(:,plat)>0),:),1))); %Plateau: Proportion Switches
    Switches(s,3) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,plat)>0)&(datafile(:,runnum)>1),:),1))/((size(datafile((datafile(:,repsw)>0)&(datafile(:,plat)>0)&(datafile(:,runnum)>1),:),1))); %Plateau - 1st plat trial: Proportion Switches
    Switches(s,4) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,trans)>0),:),1))/((size(datafile((datafile(:,repsw)>0)&(datafile(:,trans)>0),:),1))); %Transition: Proportion switches
    Switches(s,5) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,runnum)>=8)&(datafile(:,runnum)<=10),:),1))/((size(datafile((datafile(:,repsw)>0)&(datafile(:,runnum)>=8)&(datafile(:,runnum)<=10),:),1))); %Mid 3 trials of Transition: Proportion switches
    Switches(s,6) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,optchoice)==1),:),1))/(size(datafile((datafile(:,repsw)==2),:),1)); % Proportion of switches that are efficient 
    Switches(s,7) = nanmean(datafile((datafile(:,repsw)==2)&(datafile(:,trans)>0),runnum));                            % Average run num position for each switch, on transitions only
    
    SwitchNums(s,1) = (size(datafile((datafile(:,repsw)==2),:),1))/((size(datafile((datafile(:,repsw)>0),:),1)))*12; %Num switches/((num switches + reps) / run number)
    SwitchNums(s,2) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,plat)>0),:),1))/((size(datafile((datafile(:,repsw)>0),:),1)))*12; %Plateau: Num switches/((num switches + reps) / run number)
    SwitchNums(s,3) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,trans)>0),:),1))/((size(datafile((datafile(:,repsw)>0),:),1)))*12; %Transition: Num switches/((num switches + reps) / run number)
   
    % Switches in each block
    for i = 1:blockno
         BlockSwitches(s,i) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,block)==i),:),1))/((size(datafile((datafile(:,repsw)>0)&(datafile(:,block)==i),:),1))); % Proportion of trials that are switches
         BlockPlatm1Switches(s,i) = (size(datafile((datafile(:,repsw)==2)&(datafile(:,plat)>0)&(datafile(:,runnum)>1)&(datafile(:,block)==i),:),1))/((size(datafile((datafile(:,repsw)>0)&(datafile(:,plat)>0)&(datafile(:,runnum)>1)&(datafile(:,block)==i),:),1))); %Plateau - 1st plat trial: Proportion Switches
    end
    
    % Switch cost
    SubRepSwRT(s,1) = nanmean(datafile((datafile(:,repsw)==1),RT)); % Rep trials
    SubRepSwRT(s,2) = nanmean(datafile((datafile(:,repsw)==2),RT)); % Switch trials
    SubRepSwRT(s,3) = SubRepSwRT(s,2)-SubRepSwRT(s,1);
    SubRepSwRT(s,4) = SubRepSwRT(s,3)/(SubRepSwRT(s,2)+SubRepSwRT(s,1));
    
    % Switch cost eff vs in
    SubRepSwEffInRT(s,1) = nanmean(datafile((datafile(:,repsw)==1)&(datafile(:,optchoice)==1),RT)); % Rep, efficient
    SubRepSwEffInRT(s,2) = nanmean(datafile((datafile(:,repsw)==1)&(datafile(:,optchoice)==0),RT)); % Rep, inefficient
    SubRepSwEffInRT(s,3) = nanmean(datafile((datafile(:,repsw)==2)&(datafile(:,optchoice)==1),RT)); % Sw, efficient
    SubRepSwEffInRT(s,4) = nanmean(datafile((datafile(:,repsw)==2)&(datafile(:,optchoice)==0),RT)); % Sw, inefficient
   
    
end % SubNo

%% Output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format long g

outputfile1 = strcat('Data_',expname,'_allsubs_Summary.txt');
printoutquicksummary = [sublist',MeanAcc,MeanRT,SubPlatEfficient,Switches(:,1)];
header = {'SubNo','Acc','RT','ProportionOptimalChoices','SwitchRate'};
txt=sprintf('%s\t',header{:});
txt(end)='';
dlmwrite(outputfile1,txt,'');
dlmwrite(outputfile1,printoutquicksummary,'-append','delimiter','\t','precision',6);

outputfile1 = strcat('Data_',expname,'_allsubs_RTmeans.txt');
printoutallmeans = [sublist',MeanAcc,MeanRT,SubPlatEfficient,SubPlatRT,SubRepSwRT,SubRepSwEffInRT];
header = {'SubNo','RT','PlatOptimal','OptimalRT','NonoptRT','OptCost','StndOptCost','RepeatRT','SwitchRT','SwitchCost','StndSwitchCost','OptRepRT','NonoptRepRT','OptSwitchRT','NonoptSwitchRT'};
txt=sprintf('%s\t',header{:});
txt(end)='';
dlmwrite(outputfile1,txt,'');
dlmwrite(outputfile1,printoutallmeans,'-append','delimiter','\t','precision',6);

printoutprops = [sublist',StartOptPercents,EndOptPercents];
outputfile1 = strcat('Data_',expname,'_allsubs_earlylateprop.txt');
header = {'SubNo','StartOptP1','StartOptP2','StartOptP3','StartOptP4','StartOptP5','StartOptT1','StartOptT2','StartOptT3','StartOptT4','StartOptT5','StartOptT6','StartOptT7',...
                'EndOptP1','EndOptP2','EndOptP3','EndOptP4','EndOptP5','EndOptT1','EndOptT2','EndOptT3','EndOptT4','EndOptT5','EndOptT6','EndOptT7'};
txt=sprintf('%s\t',header{:});
txt(end)='';
dlmwrite(outputfile1,txt,'');
dlmwrite(outputfile1,printoutprops,'-append','delimiter','\t','precision',6);

outputfile1 = strcat('Data_',expname,'_allsubs_switches.txt');
printoutswitches = [sublist',Switches,CumulativeSwitches];
header = {'SubNo','AllSw','PlatSw','Platm1Sw','TransSw','MidTransSw','OptSw','SwitchPoint','SwP1','SwP2','SwP3','SwP4','SwP5','SwT1','SwT2','SwT3','SwT4','SwT5','SwT6','SwT7'};
txt=sprintf('%s\t',header{:});
txt(end)='';
dlmwrite(outputfile1,txt,'');
dlmwrite(outputfile1,printoutswitches,'-append','delimiter','\t','precision',6);


end