function MultiTargForag_1
%%%%%%%%%%%%%%%%%%%%%%%%%

% Based on Kristjansson et al (2014)
% Forage for two types of targets among two types of distractors
% Select targets using mouse click

% Feature search (all shapes circles):
%   Counterbalance 1: Targets = red + green, Distractors = blue + yellow
%   Counterbalance 2: Targets = blue + yellow, Distractors = red + green
% Conjunction search:
%   Counterbalance 1: Targets = red circle + green square, Distractors = red square + green circle
%   Counterbalance 2: Targets = blue circle + yellow square, Distractors = blue square + yellow circle
%   Counterbalance 3: Targets = red square + green circle, Distractors = red circle + green square
%   Counterbalance 4: Targets = blue square + yellow circle, Distractors = blue circle + yellow square

% To quite during trial, hold down esc key and click mouse

% programmed by J Irons August 2016

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
skipsync = 1;               % skip sync tests. 1 = yes, 0 = no

%%% Trial info
nblocks = 1;                % num blocks (with breaks in between)   
nblocktrials = 20;          % num trials/block
nprac = 5;                  % num prac trials
ITItime = 2;                % Duration of intertrial interval

%%% Stumulus color
Red = [180 0 0];            % Target colors (RGB 1-255)
Green = [20 100 40];
Blue = [0 0 220];
Yellow = [200 150 0];
backgroundcol = [0 0 0];    % Display background col 
textcol = [255 255 255];    % Text & fixation point col

%%% Stimulus size/shape
nsetitems = 20;             % Number of items per distractor set
ngridX = 10;                % num items across array
ngridY = 8;                 % num items down array
isq = 20;                   % width for square shape
icir = 22;                  % width for circle shape
ibuffer = 15;               % Max possible buffer between two items
clickrange = 25;            % Must click within this radius from centre of item to count as selected
xborder = 150;              % Size of blank border around edge of display
yborder = 100;              % Kristjansson uses x = 150 & y = 100
    
%%%%%%%%%%%%%%%%%%%%%%%%%% STARTUP INFO %%%%%%%%%%%%%%%%%%%%%%%%%%%

clc;            % Clear Matlab/Octave window

prompt = {'Participant code:','Search Type (1 = Feature, 2 = Conjunction)','CounterBal targets (Feature:1,2. Conj:1-4)','Use Eyetracking? (1 = yes, 0 = no)'};
dlg_title = 'Participant information';
num_lines= 1;
defaults = {'99','1','1','0'};      
answer = inputdlg(prompt,dlg_title,num_lines,defaults);
subNo = answer{1};                                      % 99 for debugging with outwriting output
FeatConj = str2num(answer{2});
CBTargCond = str2num(answer{3});                        % Can counterbalance combination of targets
UseEL = str2num(answer{4});          % Use Eyelink

% Record errors etc. in a diary file
diarytext = strcat('Tracking Files/Diary_',num2str(subNo),'.txt');
diary(diarytext)
diary on
commandwindow;

% Send message to command window
expname = mfilename
experiment_run_at = clock

Screen('Preference', 'SkipSyncTests', skipsync);

%%%%%%%%%%%%%%%%%%%%%%%%%% FILE HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Results file:
datafilename = strcat('Data/Data_',expname,'_',subNo,'.txt');           % results file
stimfilename = strcat('Tracking Files/Stim_',expname,'_',subNo,'.txt'); % logs stimulus color/shape and position, for eye tracking
edffilename= strcat('MTF',subNo,'.edf');

% check for existing result file:
if (subNo ~= 99)
    datafilepointer = fopen(datafilename,'wt'); % open ASCII file for writing 
    stimfilepointer = fopen(stimfilename,'wt'); 
elseif fopen(datafilename, 'rt')~=-1
    fclose('all');
    error('Result data file already exists! Choose a different subject number.');
else
    datafilepointer = fopen(datafilename,'wt'); % open ASCII file for writing
    stimfilepointer = fopen(stimfilename,'wt'); 
end

fprintf(datafilepointer,'%s\tParticipant_Code = %s\n',char(expname),subNo);
fprintf(datafilepointer,'Search_Type = %i\tCB_Targets = %i\n',FeatConj,CBTargCond);
fprintf(stimfilepointer,'%s\tParticipant_Code = %s\n',char(expname),subNo);
fprintf(stimfilepointer,'Search_Type = %i\tCB_Targets = %i\n',FeatConj,CBTargCond);


%%%%%%%%%%%%%%%%%%%%%%%%%%% SCREEN SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try        
    if IsOSX==1
        TopPriority=0;
        sysTxtOff=0;  % no text adjustment needed
        ptbPipeMode=kPsychNeedFastBackingStore;  % enable imaging pipeline for osx
    elseif IsWin==1
        TopPriority=1;
        sysTxtOff=1;  % windows draws text from the upper left corner instead of lower left.  to correct,
        % an adjustment factor of 1*letterheight is subtracted
        % from the y coordinate of the starting point
        ptbPipeMode=[];  % don't need to enable imaging pipeline
    else
        ListenChar(0); clear screen; error('Operating system not OS X or Windows.')
    end
    
    ListenChar(2);
    Screen('Preference', 'SkipSyncTests', 1);
    AssertOpenGL;
    rand('state',sum(100*clock));   % Reseed the random-number generator for each expt.
       
    % Do dummy calls
    WaitSecs(0.1);
    GetSecs;
    
    screens=Screen('Screens');
    screenNumber=max(screens);
    [w, wRect]=Screen('OpenWindow',screenNumber, backgroundcol,[],[],[],[],[],ptbPipeMode);
    ScreenX = wRect(RectRight) - wRect(RectLeft);
    ScreenY = wRect(RectBottom) - wRect(RectTop);
    
    Priority(MaxPriority(w));
    vbl=Screen('Flip', w);
    
%%%%%%%%%%%%%%%%%%%%%%%%%%% KEYBOARD SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Make sure keyboard mapping is the same on all supported operating systems
    % Apple MacOS/X, MS-Windows and GNU/Linux:
    KbName('UnifyKeyNames');
    escapeKey = KbName('ESCAPE'); 
    spaceKey = KbName('SPACE');
    
%%%%%%%%%%%%%%%%%%%%%%%%%%% SET UP STIMULI %%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %%%%%%%%%%%%%%%%%%%%%%% EXPERIMENT VARS %%%%%%%%%%%%%%%%%%
    
    centrex = ScreenX/2;
    centrey = ScreenY/2;
    
    % Target shapes
    circle = [icir/2*-1, icir/2*-1; icir/2, icir/2*-1; icir/2, icir/2; icir/2*-1, icir/2];
    square = [isq/2*-1, isq/2*-1; isq/2, isq/2*-1; isq/2, isq/2; isq/2*-1, isq/2];
    
    arrayX = ScreenX-xborder*2;   % Size of search array
    arrayY = ScreenY-yborder*2;    
    
    % Create array of xy coords for grid square centres
    ngridsquares = ngridX * ngridY;
    gridpos = ones(ngridX*ngridY,3);
    gridsizeX = arrayX/(ngridX);
    gridsizeY = arrayY/(ngridY);

    for gridY = 1:ngridY
        for gridX = 1:ngridX            
            gridpos(gridX+(ngridX*(gridY-1)),1) = gridX+(ngridX*(gridY-1));     % Grid position number (1 - N items)
            gridpos(gridX+(ngridX*(gridY-1)),2) = (centrex-(arrayX/2)) + (gridX*gridsizeX - gridsizeX/2);   % Grid position X
            gridpos(gridX+(ngridX*(gridY-1)),3) = (centrey-(arrayY/2)) + (gridY*gridsizeY - gridsizeY/2);   % Grid position Y
        end
    end
    
     % Amount of available jitter within the grid, make sure there's a buffer between items
     jitterX = gridsizeX-isq*2-ibuffer;       % amount by which targ shape can jitter horizontally
     jitterY = gridsizeY-isq-ibuffer;     
        
    %%%%%%%%%%%%%%%%%%%% Counterbalancing %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %%% Create target sets. First two col/shape combinations are the targets,
    %%% second two sets are the distractors
    if FeatConj == 1                            % Feature condition                     
        TargShape = [1;1;1;1];                  % All items are circles
        if CBTargCond == 1
            TargCols = [Red; Green; Blue; Yellow]; % Targs=Red,Green. Dists=Blue,Yellow
        elseif CBTargCond == 2
            TargCols = [Blue; Yellow; Red; Green]; % Targs=Blue,Yellow. Dists=Red,Green
        end
    else                                        % Conjunction condition
        if CBTargCond == 1
            TargShape = [1;2;2;1];              % half circles, half squares
            TargCols = [Red; Green; Red; Green];        % Targs=Red circle, Green squares. Dists = Red sq, Green cir
        elseif CBTargCond == 2
            TargShape = [1;2;2;1];
            TargCols = [Blue; Yellow; Blue; Yellow];    % Targs=Blue circle, Yellow squares. Dists = Blue sq, Yellow cir
        elseif CBTargCond == 3
            TargShape = [2;1;1;2];
            TargCols = [Red; Green; Red; Green];        % Targs=Red square, Green circle. Dists = Red cir, Green sq
        elseif CBTargCond == 4
            TargShape = [2;1;1;2];
            TargCols = [Blue; Yellow; Blue; Yellow];    % Targs=Blue square, Yellow circle. Dists = Blue cir, Yellow sq
        end
    end
    
    % Creat array of all items.
    TargConds = 1;  
    nitems = nsetitems*size(TargCols,1);
    for i = 1:nitems
       set = ceil(i/nsetitems);
       TargConds(i,1:3) = TargCols(set,:);
       TargConds(i,4) = TargShape(set); 
       TargConds(i,5) = set;
    end 
    
%%%%%%%%%%%%%%%%%%%%% EYELINK SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if UseEL == 1
        dummymode = 0;
        ListenChar(2); 
        % Provide Eyelink with details about the graphics environment
        % and perform some initializations. The information is returned
        % in a structure that also contains useful defaults
        % and control codes (e.g. tracker state bit and Eyelink key values).
        el=EyelinkInitDefaults(w);
        % Set calibration background col to black and target to gray
        el.backgroundcolour = BlackIndex(el.window);
        el.calibrationtargetcolour = WhiteIndex(el.window);
        el.foregroundcolour = WhiteIndex(el.window);
        el.msgfontcolour    = WhiteIndex(el.window);
        el.imgtitlecolour   = WhiteIndex(el.window);
%        EyelinkUpdateDefaults(el);
        % Disable key output to Matlab window:
        % Initialization of the connection with the Eyelink Gazetracker.
        % exit program if this fails.
         if ~EyelinkInit(dummymode, 1)
             cleanup;  % cleanup function
             return;
         end
        [v vs]=Eyelink('GetTrackerVersion');
        % make sure that we get gaze data from the Eyelink
        Eyelink('Command', 'link_sample_data = LEFT,RIGHT,GAZE,AREA');
        
        % eyelink file
        i = Eyelink('Openfile', edffilename);        
        if i~=0
            printf('Cannot create EDF file');
            EYELINK( 'Shutdown');
            return;
        end
        %Do initial setup to check eye position  
        EyelinkDoTrackerSetup(el);
    end
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INSTRUCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

    Beeper;
    % Present instructions, SPACEBAR to continue
    Screen('TextSize', w, 24);
    message = strcat('\n\nWelcome! In this experiment, you will be asked to find and click on\n\n',...
                      'all of the target shapes in each display as quickly as you can.\n\n',...
                      'If you click on the distractor shapes, the trial will restart.\n\n\n',...
                      'Press SPACEBAR to begin some practice trials.');
    DrawFormattedText(w, message, 'center', [], textcol);
    message2 = char(strcat('Targets:'));
    DrawFormattedText(w, message2, centrex/4, centrey, textcol);
    message2 = char(strcat('Distractors:'));
    DrawFormattedText(w, message2, centrex+centrex/4, centrey, textcol);
    % Draw target 1
    if TargShape(1) == 1          % If circle
       Screen(w,'FillOval',TargCols(1,1:3),[centrex-centrex/2+circle(1,1);centrey+circle(2,2);centrex-centrex/2+circle(3,1);centrey+circle(4,2)]);
    else                            % If square
       Screen(w,'FillPoly',TargCols(1,1:3),[centrex-centrex/2+square(1,1),centrey+square(1,2);centrex-centrex/2+square(2,1),centrey+square(2,2);centrex-centrex/2+square(3,1),centrey+square(3,2);centrex-centrex/2+square(4,1),centrey+square(4,2)]);
    end 
    % Draw target 2
    if TargShape(2) == 1        % If circle
       Screen(w,'FillOval',TargCols(2,1:3),[centrex-centrex/2+icir*2+circle(1,1);centrey+circle(2,2);centrex-centrex/2+icir*2+circle(3,1);centrey+circle(4,2)]);
    else                            % If square
       Screen(w,'FillPoly',TargCols(2,1:3),[centrex-centrex/2+icir*2+square(1,1),centrey+square(1,2);centrex-centrex/2+icir*2+square(2,1),centrey+square(2,2);centrex-centrex/2+icir*2+square(3,1),centrey+square(3,2);centrex-centrex/2+icir*2+square(4,1),centrey+square(4,2)]);
    end     
    % Draw distractor 1
    if TargShape(3) == 1          % If circle
       Screen(w,'FillOval',TargCols(3,1:3),[centrex+centrex/2+circle(1,1);centrey+circle(2,2);centrex+centrex/2+circle(3,1);centrey+circle(4,2)]);
    else                            % If square
       Screen(w,'FillPoly',TargCols(3,1:3),[centrex+centrex/2+square(1,1),centrey+square(1,2);centrex+centrex/2+square(2,1),centrey+square(2,2);centrex+centrex/2+square(3,1),centrey+square(3,2);centrex+centrex/2+square(4,1),centrey+square(4,2)]);
    end 
    % Draw target 2
    if TargShape(4) == 1        % If circle
       Screen(w,'FillOval',TargCols(4,1:3),[centrex+centrex/2+icir*2+circle(1,1);centrey+circle(2,2);centrex+centrex/2+icir*2+circle(3,1);centrey+circle(4,2)]);
    else                            % If square
       Screen(w,'FillPoly',TargCols(4,1:3),[centrex+centrex/2+icir*2+square(1,1),centrey+square(1,2);centrex+centrex/2+icir*2+square(2,1),centrey+square(2,2);centrex+centrex/2+icir*2+square(3,1),centrey+square(3,2);centrex+centrex/2+icir*2+square(4,1),centrey+square(4,2)]);
    end     
    
    Screen('Flip',w);
    WaitSecs(.5);
    while (1)
        [~,~,keyCode] = KbCheck;
        if keyCode(spaceKey)
            break
        elseif keyCode(escapeKey) 
            if UseEL == 1
                Eyelink('StopRecording');
                Eyelink('CloseFile');
                Eyelink('ReceiveFile');
                Eyelink('Shutdown');
            end
            ListenChar(1);
            Screen('CloseAll');
            ShowCursor;
            fclose('all');
            Priority(0);
            clear all
            break;
        end
    end  
    
    Screen('Flip',w);
    WaitSecs(.5);
    
%%%%%%%%%%%%%%%%%%%%%%%%% BEGIN TRIALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     
    fprintf(datafilepointer,'Trial\tClick_Num\tTarget_Click_Order\tClick_X\tClick_y\tItem_selected\tItem_X\tItem_Y\tTarget_Set\tRT\n'); 
    fprintf(stimfilepointer,'Trial\tItem_Position_Num\tItem_X\tItemY\tItem_Shape\tItem_Set\n');         
    
    %%%%%%%%%%%%%%%%%%%%% BLOCK LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for block=0:nblocks

        if block == 0
            trialno = nprac;                    % Block 0 = prac trials
        else 
            trialno = nblocktrials;
            if block == 1
                message = char(strcat('The experimental trials will now begin.\nThere will be',{' '},num2str(nblocks),{' '},'blocks of',{' '},num2str(nblocktrials),{' '},'trials.\n\n Press SPACE to begin.'));
                DrawFormattedText(w, message, 'center', 'center', textcol);
                Screen('Flip',w);
                WaitSecs(.5);
                while (1)
                    [~,~,keyCode ] = KbCheck;
                    if keyCode(spaceKey)
                        break                
                    elseif keyCode(escapeKey) 
                        if UseEL == 1
                            Eyelink('StopRecording');
                            Eyelink('CloseFile');
                            Eyelink('ReceiveFile');
                            Eyelink('Shutdown');
                        end
                        ListenChar(1);
                        Screen('CloseAll');
                        ShowCursor;
                        fclose('all');
                        Priority(0);
                        clear all
                        break;
                    end
                end  
            end
        end
        % Calibrate
        if UseEL == 1
            Eyelink('StopRecording');
            EyelinkDoTrackerSetup(el);
            EyelinkDoDriftCorrection(el);
            Eyelink('StartRecording');
        end
        
    %%%%%%%%%%%%%%%%%%%%% TRIAL LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
        for trial = 1:trialno
            
            %%%%%%%%%%%%%%% PREPARE STIM %%%%%%%%%%%%%%%%%%%%%%%%%%%
            
            DrawFormattedText(w, '+', 'center', 'center', textcol);
            Screen('Flip',w);
            if UseEL == 1
                Eyelink('Message', 'SYNCTIME');
                Eyelink('Message', 'Trial%i',trial);
            end
            WaitSecs(ITItime);
            
            % randomise targetconds
            TrialTargConds = TargConds(randperm(size(TargConds,1)),:);
            
            % Get each item position and randomly jitter within grid position
            itempos = [];
            for p = 1:nitems
                itempos(p,1) = gridpos(p,1);
                itempos(p,2) = gridpos(p,2) + ((rand-.5)*jitterX);
                itempos(p,3) = gridpos(p,3) + ((rand-.5)*jitterY);
            end  
            
            % Log everything in the stimfile
            stimprint = [repmat(trial,nitems,1),itempos,TrialTargConds(:,4:5)]';
            fprintf(stimfilepointer,'%f\t%f\t%f\t%f\t%f\t%f\n',stimprint); 
            
            %%%%%%%%%%%%%% GET RESPS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            
            response = [];
            targcount = 0;
            clicki = 1;
            distresp = 0;
            starttime = GetSecs();
            while (1)  
                % draw items
                for i = 1:nitems
                   ipXY = itempos(i,2:3);
                   ipX = itempos(i,2);
                   ipY = itempos(i,3);
                   if TrialTargConds(i,4) == 1          % circle
                       Screen(w,'FillOval',TrialTargConds(i,1:3),[ipX+circle(1,1);ipY+circle(2,2);ipX+circle(3,1);ipY+circle(4,2)]);
                   else                                 % square
                       Screen(w,'FillPoly',TrialTargConds(i,1:3),[ipXY+square(1,:);ipXY+square(2,:);ipXY+square(3,:);ipXY+square(4,:)]);
                   end 
                end
                Screen('Flip', w);
                
                % Wait for mouseclick
                [~,mx,my] = GetClicks(w,0);
                clickRT = GetSecs()-starttime;
                if UseEL == 1
                    Eyelink('Message', 'Click%i',clicki);
                end
                
                % Get the grid number of the clicked location
                mousedist(:,1) = abs(itempos(:,2)-mx);  % get X distance of click from each item
                mousedist(:,2) = abs(itempos(:,3)-my);  % get Y distance of click from each item
                mousedist(:,3) = sqrt(mousedist(:,1).^2 + mousedist(:,2).^2);   % get absolute distance
                [mindist,closestitem] = min(mousedist(:,3)); % find item at minimum distance
                
                % Get info for item clicked
                if mindist < clickrange                             % If click fell within acceptable range of item              
                    response(1,clicki) = trial;                         % trial num
                    response(2,clicki) = clicki;                        % click number
                    response(3,clicki) = targcount;                     % Order number of item selected
                    response(4,clicki) = mx;                            % click x
                    response(5,clicki) = my;                            % click y
                    response(6,clicki) = closestitem;                   % closest item number
                    response(7,clicki) = itempos(closestitem,2);        % item x
                    response(8,clicki) = itempos(closestitem,3);        % item y
                    response(9,clicki) = TrialTargConds(closestitem,5); % set (1,2 = targets, 3,4 = distractors)
                    response(10,clicki) = clickRT;
                    targcount = targcount + 1;
                    
                    % Remove clicked item from display: turn to background color and move offscreen (in case Ps accidentally click close to previous location)
                    TrialTargConds(closestitem,1:3) = backgroundcol;
                    itempos(closestitem,2) = -1000;
                    itempos(closestitem,3) = -1000;   
                    
                    % End trial
                    if (response(3,clicki)>0) && (response(9,clicki)>2) % If click on distractor, end trial immediately 
                       distresp = 1;                              
                       break
                    end
                    if targcount == nsetitems*2                          % If all targets selected, end trial
                       break
                    end
                    
                else                                                % If click was outside acceptable range of item, don't include it as a target but record info anyway
                    response(1,clicki) = trial;                         % trial num
                    response(2,clicki) = clicki;                        % click number
                    response(3,clicki) = 0;                             % Log the click but don't include it in the target order count
                    response(4,clicki) = mx;                            % click x
                    response(5,clicki) = my;                            % click y
                    response(6,clicki) = closestitem;                   % closest item number
                    response(7,clicki) = itempos(closestitem,2);        % item x
                    response(8,clicki) = itempos(closestitem,3);        % item y
                    response(9,clicki) = TrialTargConds(closestitem,5); % set (1,2 = targets, 3,4 = distractors)
                    response(10,clicki) = clickRT;
                end   
                
                clicki = clicki + 1;
                
                % To escape, hold down esc key and click mouse
                [keyIsDown,~,keyCode] = KbCheck(-1);
                if keyIsDown
                    if keyCode(escapeKey)           
                        ListenChar(1);
                        Screen('CloseAll');
                        ShowCursor;
                        fclose('all');
                        Priority(0);
                        break;
                    end
                    FlushEvents('keyDown');
                end
            end     
            
            if UseEL == 1
                Eyelink('Message', 'EndTrial');
            end
            
            % Beep if error made
            if distresp == 1
                Beeper();
                trialno = trialno + 1;      % Replace with an extra trial in this block
            end
            
            %%%%%%%%%%% RECORD DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%

            fprintf(datafilepointer,'%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n',response); 
            
            %%%%%%%%%%% BREAK %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            if (trial == trialno) && (block > 0)
                if  (block < nblocks)
                    message = 'Have a short break.\n\nPress SPACEBAR to start the next block';
                else
                    message = 'That is the end of this task.\n\nPress see your experimenter.';
                end
                DrawFormattedText(w, message, 'center', 'center', textcol);
                Screen('Flip', w);
                WaitSecs(.5);
                while (1)
                    [~,~,keyCode ] = KbCheck;
                    if keyCode(spaceKey)
                        break
                    end
                end
                FlushEvents('keyDown');
            end            
        end     % End trials
    end         % End blocks
    
%%%%%%%%%%%%%%%%%%%%% END EXPERIMENT %%%%%%%%%%%%%%%%%%%%%%%%%%
    if UseEL == 1
        %Eyelink('StopRecording');
        Eyelink('CloseFile');
        % download data file
        try
            status=Eyelink('ReceiveFile');            
        catch rdf
            rdf;
        end
        Eyelink('Shutdown');
    end

    ListenChar(1);
    Priority(0);
    ShowCursor
    Screen('CloseAll');
    clear all
catch
    psychrethrow(psychlasterror);
    if UseEL == 1
        Eyelink('StopRecording');
        Eyelink('CloseFile');
        % download data file
        try
            status=Eyelink('ReceiveFile');            
        catch rdf
            rdf;
        end
        Eyelink('Shutdown');
    end    
    ListenChar(1);
    Priority(0);
    ShowCursor
    Screen('CloseAll');
    clear all
end

% To copy data after the fact
% Eyelink('StopRecording');
% Eyelink('CloseFile');
% status=Eyelink('ReceiveFile');
% Eyelink('Shutdown');
