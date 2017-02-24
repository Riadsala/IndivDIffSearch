 function MultiTargForag_analysis
%%%%%%%%%%%%%%%%%%%%%%%%%
% Data analysis script for Multi-target foraging task 
% J Irons Jan 2017

% ***NOTE*** For subjects 1-4, I made a few changes to the data file to correct an error in the early version of the task (version 1).
%  The files with the error corrected are uploaded with this script on GitHub (IndivDIffSearch/Scripts/adaptchoice/)

% 
% Instructions:
% 
% 1) Ensure all data files are in the same folder as this file
% 
% 2) Update the list of subject numbers:
 sublist = [1,2,3,4,7]; 
% 
% 3) Hit run
% 
% 4) Text file will be created:
% 
% Data_MultiTargForag_allsubs: Includes Sub number, Mean Feature trial RT, Feature search number of runs per trial, Feature search average run length,
%                              Mean Conjunction trial RT, Conjunction search numner of runs, Conjunction search run length
%
% Can also print out individual trial data by uncommenting the section
% below starting at row 175

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Data file column conditions
trial = 1;
click = 2;
targclick = 3;
clickX = 4;
clickY = 5;
itemnum = 6; 
itemX = 7; 
itemY = 8;
targset = 9; % Can be either 1 or 2 (e.g. in feature condition, 1 = red, 2 = green)
RT = 10;

expname = 'MultiTargForag';
alldata = [];

for s = 1:length(sublist)
    
    subdata = [];
    data1 = [];
    data2 = [];
    
     subNo = sublist(s)
     if subNo < 7
        version = 1;    % old version of exp 
     else
        version = 2;
     end
        
     % Get Data from both sessions
     datafilename1 = strcat('Data_',expname,'_',num2str(version),'_',num2str(subNo),'.txt');
     data1 = dlmread(datafilename1,'',3,0);
     
     datafilename2 = strcat('Data_',expname,'_',num2str(version),'_',num2str(subNo),'a.txt');
     data2 = dlmread(datafilename2,'',3,0);
     
     % Open one file to get the header info
     fid = fopen(datafilename1); 
     if fid==-1
         error(['ASC file not found']);
     end
     
     % Find the target type (feature or conj) of the first data file
     tline = fgetl(fid);
     tline = fgetl(fid);
     cond = strfind(tline,'Search_Type = 1');
     
     if cond == 1
         condorder = [1,2]; % Feature first, then Conj
     else
         condorder = [2,1]; % Conj first, then feature
     end
     
     fclose(fid);
     
     % Add new column with new trial number to separate prac trials from
     % exp trials
     newtrial = size(data1,2)+1;
     newtrialcount = 1;
     data1(1,newtrial) = newtrialcount;
     for c = 2:size(data1,1)
         if data1(c,click) <= data1(c-1,click)  %If click number doesn't increase, then it's a new trial
             newtrialcount = newtrialcount + 1;
         end
         data1(c,newtrial) = newtrialcount;
     end
     newtrialcount = 1;
     data2(1,newtrial) = newtrialcount;
     for c = 2:size(data2,1)
         if data2(c,click) <= data2(c-1,click)
             newtrialcount = newtrialcount + 1;
         end
         data2(c,newtrial) = newtrialcount;
     end
     
     % Add new column with search type (feature or conj) to file then join togethe
     searchtype = size(data1,2)+1;
     data1(:,searchtype) = condorder(1);
     data2(:,searchtype) = condorder(2);
     ntrials = [data1(end,newtrial),data2(end,newtrial)];
     subdata = [data1;data2];    
     
     %Exclude Prac trials 
     %subdata = subdata((subdata(:,trial)>5),:);
     
     % Add interclick RT, excluding click misses
     clickRT = size(subdata,2)+1;
     for c = 1:size(subdata,1)
         if subdata(c,targclick) == 1
            runningtime = subdata(c,RT);
            subdata(c,clickRT) = runningtime;
         elseif subdata(c,targclick) > 0
            subdata(c,clickRT) = subdata(c,RT) - runningtime;
            runningtime = subdata(c,RT);
         end
     end
     
     % Save new data
%     trialoutputfile = strcat('CombinedData_',expname,'_',num2str(subNo),'.txt');
%     dlmwrite(trialoutputfile,subdata,'delimiter','\t','precision',6);  
    
     
     %%%%%%%%%%%%%%%%%%%%%%
     % Calculate run lengths per trial
     tdata = [];
       
     for st = 1:2   % Do for feature first, then conj    
        condtdata = [];
        for t = 1:ntrials(st)

            % Extra data for trial t and condition st. Exclude click misses
            trialsubdata = subdata((subdata(:,newtrial)==t)&(subdata(:,searchtype)==st)&(subdata(:,targclick)>0),:);

            % Count runs
            if trialsubdata(end,targclick) == 40    % only do it for correct trials
                rt = trialsubdata(end,RT);
                prevtarg = trialsubdata(1,targset);
                runnum = 1;
                runlengths = [];
                runlengths(1,runnum) = 1;
                for c = 2:size(trialsubdata,1)
                    currenttarg = trialsubdata(c,targset);
                    if currenttarg == prevtarg
                        runlengths(1,runnum) = runlengths(1,runnum) + 1;
                    else
                        runnum = runnum + 1;
                        runlengths(1,runnum) = 1;
                    end 
                    prevtarg = currenttarg;
                end
                avrunlength = mean(runlengths);
                condtdata(t,:) = [subNo,st,t,rt,runnum,avrunlength];
            end
        end
        
        % Take out error trials & first correct trial as practice
        condtdata = condtdata((condtdata(:,1)>0),:);
        CorrectCount = size(condtdata,1);
        if CorrectCount < 21   % Remove first trial 
            condtdata = condtdata(2:end,:);
        else
            condtdata = condtdata(2:21,:);
        end
        
        tdata = [tdata;condtdata];
     end
     
     
    % Save trial data
%     trialoutputfile = strcat('TrialData_',expname,'_',num2str(subNo),'.txt');
%     header = {'SubNo','SearchType_(1=Feat_2=Conj)','TrialNum','RT','RunNumber','AvRunLength'};
%     txt=sprintf('%s\t',header{:});
%     txt(end)='';
%     dlmwrite(trialoutputfile,txt,'');
%     dlmwrite(trialoutputfile,tdata,'-append','delimiter','\t','precision',6);
    
    %%%%%%%%%%%%%%%%%%%
    % Record mean data
    alldata(s,1) = subNo;
    alldata(s,2) = nanmean(tdata((tdata(:,2)==1),4));   % Feature RT
    alldata(s,3) = nanmean(tdata((tdata(:,2)==1),5));   % Feature run num
    alldata(s,4) = nanmean(tdata((tdata(:,2)==1),6));   % Feature run length
    alldata(s,5) = nanmean(tdata((tdata(:,2)==2),4));   % Conj RT
    alldata(s,6) = nanmean(tdata((tdata(:,2)==2),5));   % Conj run num
    alldata(s,7) = nanmean(tdata((tdata(:,2)==2),6));   % Conj run length
        
end % SubNo

%% Output %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format long g

allsubsoutputfile = strcat('Data_',expname,'_allsubs.txt');
header = {'SubNo','Feature_RT','Feature_RunNum','Feature_RunLength','Conj_RT','Conj_RunNum','Conj_RunLength'};
txt=sprintf('%s\t',header{:});
txt(end)='';
dlmwrite(allsubsoutputfile,txt,'');
dlmwrite(allsubsoutputfile,alldata,'-append','delimiter','\t','precision',6);

end