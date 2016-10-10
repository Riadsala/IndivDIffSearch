% ARH/AL March 2014. Code to process data from Anna's project looking at the effect of% gaze-contingent filtering on search strategies.% Data recorded from EyeLink 1000.% open the .asc file, get fixation and trial data, output to two separate% files for Alasdair.clear all;numSub = 14;filetomake = input('name the fixation results file: ', 's');F2ID = fopen(filetomake, 'a');fprintf(F2ID, 'subNum\t trialNo\t fixNo\t xFix\t yFix\t fixStartTime\t fixEndTime\t targPresent\t targSide\t easySide\t var\t con\n');for subNo=1:numSub            condition = 'Ul';              filetoread = strcat(condition, num2str(subNo),'.asc'); % name of search data file to read        if (fopen(filetoread, 'r')~=-1)            FID=fopen(filetoread, 'r');            trialRunning = 0;            endtrialRun = 0;            trialCount = 0;            sacOnTime = 0;            fileRun = 1;                        while fileRun                line=fgetl(FID); %get the next line of data                                if line == -1  % if the end of the file has been reached                    fclose(FID);                    fileRun = 0; %leave the main loop                    break;                    %     elseif findstr(line, 'TRIALNO_80')                    %         fclose(F2ID);                    %         fclose(FID);                    %         fileRun = 0; %leave the main loop                    %         break;                end                                if findstr(line, 'DISPLAY_ON')                    faceOn = 0;                    numFix = 0;                    targetsOn=0;                    trialCount = trialCount+1;                    failSacc=0;                    s=1;                    xPos=zeros(1,70);                    yPos=zeros(1,70);                    targPres='x';                    targSide='x';                    TargX1='-1';                    mes='GoodTrial';                    easySide='x';                    cond='x';                     var='x';                                        numDistLY2=0;                    FaceOnTarget=0;                    FaceOnDistrL4=0;                    FaceOnRightDistr=0;                    FaceOnLeftDistr=0;                    trialRunning =1; %start loop to get timing / saccade info from trial                    %         maxXvalue=0;                    while trialRunning                        line = fgetl(FID); %get data                        if findstr(line, 'FACES_DRAWN')                            index=findstr(line,'F');                                                        faceOnTime=str2num(line(4:index-1));                            faceOn = 1;                        end                        while faceOn                            line = fgetl(FID);                                                        if findstr(line, 'EFIX')                                efix = str2num(line(10:length(line)));                                numFix= numFix+1;                                xPos(1,numFix)=round(efix(4));                                yPos(1,numFix)=round(efix(5));                                fixStartT(1,numFix)=round(efix(1));                                fixEndT(1,numFix)=round(efix(2));                            end                            if findstr(line, 'Done')                                %endTime = str2num(line(4:11));                                faceOn = 0;                                break;                            end                                                        if strfind(line, 'TargPres_') %                                index = strfind(line, 'TargPres')+9;                                targPres =str2num(line(index:length(line)));                                                            end                                                                                    if strfind(line, 'TargSide_') %                                index = strfind(line, 'TargSide_')+9;                                targSide = line(index:length(line));                            end                                                                                    if findstr(line, 'Message_')                            index= strfind(line,'ge_')+3;                            message=(line(index:length(line)));                            cor=strcmp(message,mes);                            end                                                                       if findstr(line, '-1.7.jpg')                            index= strfind(line,'-1.7.jpg')-1;                            index2=strfind(line,'-1.7.jpg');                            easySide=line(index:(index2-1))                            var='x';                            end                                                        if easySide=='x'                               if findstr(line, 'v6')                               index=strfind(line,'v6');                               var=line(index+1);                               elseif findstr(line, 'v1.7')                               index=strfind(line,'v1.7')+1;                               indexJPG=strfind(line,'.jpg')-1;                               var=(line(index:indexJPG));                                 end                             end                        end                                                if strfind(line, 'Done')                            %if strfind(line, 'DRIFTCORRECT') %if trial is over                            if var=='x'                            cond='half'                            else                            cond='full'                            end                                                        for s=1:numFix                                fprintf(F2ID, '%d\t %d\t %d\t %d\t %d\t %d\t %d\t %d\t %s\t %s\t %s\t %s\n', subNo,trialCount, s, round(xPos(1,s)), round(yPos(1,s)), fixStartT(1,s), fixEndT(1,s), targPres, targSide, easySide, var, cond);                                               end                            %save location of each item                                                        trialRunning = 0;                                                        break;                                 end                                            end                end            end        endendfclose(F2ID);