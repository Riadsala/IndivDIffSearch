 
function AdaptChoice
%%%%%%%%%%%%%%%%%%%%%%%%%

% Adaptive Choice visual search task
%
% Search array of colored squares containing digits. Two targets on every
% trials (red square with digit between 2 and 5 inclusive, and blue square with digit
% between 2 and 5). Participants can choose to search for either one on
% each trial. Distractors are colored red, blue, green or "variable".
% Variable distractors change color from red through magenta to blue and
% back again over cycles of 24 trials. 

% See Irons & Leber (2016) AP&P
% programmed by J Irons

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%% THINGS THAT MIGHT NEED TO BE ADJUSTED 
skipsync = 1;                       % skip sync tests. 1 = yes, 0 = no
innerradius = 240;                  % Radius of the inner ring in pixels. Might need to adjust to fit display. Default is 240 (had been used successfully with monitor resolutions of 1920x1080 and 1280x1024).
                                    % innerrad also determines radius of middle ring (innerrad*1.5) and outer ring (innerrad*2)
squaresize = 40;                    % Width/height of each square in search display in pixels. Might need to adjust. Default is 40
Digitsize = 24;                     % Font size for digits
Digitfont = 'Arial';                % Font for digits
digadjx = 12;                        % x-pos. Small adjustment to position of the digit to centre on the square. Might need to adjust
digadjy = 7;                       % y-pos. Small adjustment to position of the digit to centre on the square. Might need to adjust

% For EyeTracking
autocalibrate = 0;                  % If set to 1, will automatically calibrate at the start of each block, without experimenter having to be in the room. Needs additional eyelink code EyelinkDoCalibration
recalibrate = 1;                    % If set to 1, will recalibrate automatically during the block if centre is not fixated during ITI for a specified number of trials
fixerror = 100;                     % Radius of fixation circle for the fixation control
allowfixmisses = 10;                % Number of allowable fixation check misses before recalibration

%%%%%%%%%% OTHER THINGS THAT CAN BE ADJUSTED

% Display set-up
ringitemnum = [12,18,24];           % Number of items in inner, middle and outer ring of display. Default is [12,18,24]
TargDistnum = 12;                   % Number of red and blue distractors (excluding target). Default is 12
VarDistnum = 14;                    % Number of purple/variable distractors. Default is 14
IrrDistnum = 14;                    % Number of green distractors. Default is 14

%Target & distractor digits
targd = [2,3,4,5];                  % Target digits
distd = [6,7,8,9];                  % Distractor digits
targkeys = ['V','B','N','M'];       % Response keys corresponding to the 4 target digits
Digitcol = [255 255 255];           % Font colour for digits

% Trial set up
ncycles = 4;                        % Number of full cycles per block (e.g. blue plateau, blue-red trans, red plateau, red-blue trans)
nplat = 5;                          % Number of trials on plateau
ntrans = 7;                         % Number of trials on transition
nruntrials = nplat + ntrans;        % Number of trials in a run (one plateau + one trans)
ncycletrials = nruntrials*2;        % Number trials per full cycle (2 runs)
nblocktrials = ncycletrials * ncycles;  % Number trials per block
nprac = 10;                         % Number prac trials

transcolvals = [1,1.5,2,4,6,6.5,7]; % Specify the interval size in transition. If all items equally spaced, use 1:1:ntrans
ITItime = 2;                        % Duration of ITI. Default is 2 s

%%%%%%%%%%%%%%%%%%%%%%%%%% STARTUP INFO %%%%%%%%%%%%%%%%%%%%%%%%%%%

clc;            % Clear Matlab/Octave window

prompt = {'Participant code:','Session:','Number Blocks:','Use Eyetracking? (1 = yes, 0 = no)'};
dlg_title = 'Participant information';
num_lines= 1;
defaults = {'','1','3','1'};      
answer = inputdlg(prompt,dlg_title,num_lines,defaults);
subNo = answer{1};                   % Use 99 for bug-checking/overwriting etc.
StartDistCol = 1;%str2num(answer{2});   % Starting color of variable distractors (1 = red plateau, 2 = blue plateau) KEEP CONSTANT FOR INDIV DIFFS STUDY
Session = answer{2};                 % Session for multi-session exp
nblocks = str2num(answer{3});        % Number of blocks per session
UseEL = str2num(answer{4});          % Use Eyelink

% Record errors etc. in a diary file
diarytext = strcat('Tracking Files/Diary_AC_',num2str(subNo),'.txt');
diary(diarytext)
diary on
commandwindow;

% Send message to command window
expname = mfilename
experiment_run_at = clock

%%%%%%%%%%%%%%%%%%%%%%%%%%% FILE HANDLING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Results file:
datafilename = strcat('Data/Data_',expname,'_',subNo,'_sess',Session,'.txt'); % name of data file to write to
stimfilename = strcat('Tracking Files/Stim_',expname,'_',subNo,'_sess',Session,'.txt'); % name of stim file to write to

% check for existing result file:
if (str2num(subNo) ~= 99)
    if fopen(datafilename, 'rt')~=-1
        fclose('all');
        error('Result data file already exists! Choose a different subject number.');
    else
        datafilepointer = fopen(datafilename,'wt'); % open ASCII file for writing
        stimfilepointer = fopen(stimfilename,'wt'); % open ASCII file for writing
    end
else
    datafilepointer = fopen(datafilename,'wt'); % open ASCII file for writing
    stimfilepointer = fopen(stimfilename,'wt'); % open ASCII file for writi
end

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
    Screen('Preference', 'SkipSyncTests', skipsync);
    AssertOpenGL;
    rand('state',sum(100*clock));   % Reseed the random-number generator for each expt.
    HideCursor;	% Hide the mouse cursor
    
    % Do dummy calls
    WaitSecs(0.1);
    GetSecs;    
        
    % Set up screen
    screens=Screen('Screens');
    screenNumber=max(screens);
    [w, wRect]=Screen('OpenWindow',screenNumber, [0 0 0],[],[],[],[],[],ptbPipeMode);
    ScreenX = wRect(RectRight) - wRect(RectLeft);
    ScreenY = wRect(RectBottom) - wRect(RectTop);

    Priority(MaxPriority(w));
    vbl=Screen('Flip', w);
    
%     refresh = Screen('GetFlipInterval', w);  %removed last parameter (200)
%     % test to make sure monitor is set to correct resolution and frequency
%     if abs(refresh-(1/hertz))>0.001   % should be within 1 ms
%         ListenChar(0); clear screen; error(['Monitor setup incorrect; please set refresh rate to ' num2str(hertz) ' hz.'])
%     end

%%%%%%%%%%%%%%%%%%%%%%%%%%% KEYBOARD SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % Make sure keyboard mapping is the same on all supported operating systems
    % Apple MacOS/X, MS-Windows and GNU/Linux:
    KbName('UnifyKeyNames');
    
    % Allowable responses: exc, space, target keys. For eyetracking: enter, c (calibrate), v (validate)   
    targcode1 = KbName(targkeys(1));
    targcode2 = KbName(targkeys(2));
    targcode3 = KbName(targkeys(3));
    targcode4 = KbName(targkeys(4));
    escapeKey = KbName('ESCAPE'); 
    spaceKey = KbName('space');
    enterKeys = KbName('Return');
    enterKey = enterKeys(1);   % First is normal enter key
    calibKey = KbName('c');
    validKey = KbName('v');
    if UseEL == 1
        RestrictKeysForKbCheck([targcode1, targcode2, targcode3, targcode4, escapeKey, spaceKey, enterKey, calibKey, validKey]);
    else
        RestrictKeysForKbCheck([targcode1, targcode2, targcode3, targcode4, escapeKey, spaceKey]);
    end
    
%%%%%%%%%%%%%%%%%%%%%%%%%%% SET UP STIMULI %%%%%%%%%%%%%%%%%%%%%%%%%%%%
       
    centrex = ScreenX/2;
    centrey = ScreenY/2;
   
%%% Create matrix gridpos with position information for each location in the display
    gridpos = [];
    
    % inner ring of display
    inneritems = ringitemnum(1);    % items in inner ring
    for item = 1:inneritems
        gridpos(item,1) = (centrex+innerradius*sind(item*360/inneritems));  % X position
        gridpos(item,2) = (centrey-1*innerradius*cosd(item*360/inneritems));% Y position  
        gridpos(item,3) = 1; % ring number: 1 = inner ring
        gridpos(item,4) = item; % number indicating overall location in the display
    end
    % middle ring
    miditems = ringitemnum(2);
    midradius = innerradius*1.5;
    for item = 1:miditems
        gridpos(item+inneritems,1) = (centrex+midradius*sind(item*360/miditems));  % X position
        gridpos(item+inneritems,2) = (centrey-1*midradius*cosd(item*360/miditems));% Y position    
        gridpos(item+inneritems,3) = 2; % ring number: 2 = middle ring
        gridpos(item+inneritems,4) = item+inneritems; % number indicating overall location in the display
    end  
    % outer ring
    outeritems = ringitemnum(3);
    outerradius = innerradius*2;
    for item = 1:outeritems
        gridpos(item+miditems+inneritems,1) = (centrex+outerradius*sind(item*360/outeritems));  % X position
        gridpos(item+miditems+inneritems,2) = (centrey-1*outerradius*cosd(item*360/outeritems));% Y position 
        gridpos(item+miditems+inneritems,3) = 3; % ring number: 3 = outer ring
        gridpos(item+miditems+inneritems,4) = item+miditems+inneritems; % number indicating overall location in the display
    end 
    
    % Total number of items
    nitems = inneritems + miditems + outeritems;
    
%%% Draw rect for each location in display 
    gridrects = [];
    for i = 1:nitems
        gridrects(1,i) = gridpos(i,1) - squaresize/2; % left
        gridrects(2,i) = gridpos(i,2) - squaresize/2; % top 
        gridrects(3,i) = gridpos(i,1) + squaresize/2; % right
        gridrects(4,i) = gridpos(i,2) + squaresize/2; % bottom
        gridrects(5,i) = i;
    end
    
%%% Set colour values
    white = [255 255 255];
    grey = [128 128 128];
    Red = [255,0,0];
    Blue = [0,0,255];
    Green = [0 150 0];
    
    % Set colors of the targets and distractors
    Targ1col = Red;
    Targ2col = Blue;
    IrrDistcol = Green; % Irrelevant distractor color    
    
    % Set variable distractor colors
    % RedtoBlue & BluetoRed: RGB values for each item in each transition    
    mid = (ntrans+1)/2; % number of middle (magenta) item in the transition. If transition has even number of items, this is excluded
    for t = 1:ntrans
        tval = transcolvals(t);
        if t < mid
            RedtoBlue(t,:) = [Targ1col(1),0,((Targ2col(3)-Targ1col(3))/mid)*tval+Targ1col(3)];
            BluetoRed(t,:) = [((Targ1col(1)-Targ2col(1))/mid)*tval+Targ2col(1),0,Targ2col(3)];
        elseif t == mid
            RedtoBlue(t,:) = [Targ1col(1),0,Targ2col(3)];
            BluetoRed(t,:) = [Targ1col(1),0,Targ2col(3)];
        else
            RedtoBlue(t,:) = [((Targ1col(1)-Targ2col(1))/mid)*(ntrans-tval+1)+Targ2col(1),0,Targ2col(3)];
            BluetoRed(t,:) = [Targ1col(1),0,((Targ2col(3)-Targ1col(3))/mid)*(ntrans-tval+1)+Targ1col(3)];  
        end
    end

%%% Create matrix of colors for all item on each trial
    itemcols = [Targ1col,1;                           % 1 = Targ 1 [code used in stim file to analyse fixations]
                Targ2col,2;                           % 2 = Targ 2
                repmat([Targ1col,3],TargDistnum,1);   % 3 = Targ 1 col distractors
                repmat([Targ2col,4],TargDistnum,1);   % 4 = Targ 2 col distractors
                repmat([0,0,0,5],VarDistnum,1);       % 5 = Variable col distractors (color values are updated on each trial)
                repmat([IrrDistcol,6],IrrDistnum,1)]; % 6 = Irrelevant col distractors
    itemcols = itemcols';   % translate to be in correct format for fill rect
    
%%% Set digits
    
    % Set digits for each target on each trial in a block. Ensures the two targets always have different digits.  
    to = [targd(1),targd(2);targd(1),targd(3);targd(1),targd(4);targd(2),targd(1);targd(2),targd(3);targd(2),targd(4);targd(3),targd(1);targd(3),targd(2);targd(3),targd(4);targd(4),targd(1);targd(4),targd(2);targd(4),targd(3)];
    targdigits = repmat(to,ceil(nblocktrials/size(to,1)),1);
    
    % Dist digit - creates arrays of distractor digits for each distractor set
    % Targ colored distractors
    distdigitstarg = repmat(distd,1,ceil((TargDistnum)/length(distd))); 
    % Variable distractors
    distdigitsvar = repmat(distd,1,ceil((VarDistnum)/length(distd))); 
    % Irrelevant/green distractors (half are from distd array and half from targd array to ensure Ps can't just search for digits and ignore color)
    distdigitsirr = repmat([targd,distd],1,ceil((IrrDistnum)/length([targd,distd]))); 
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SET UP TRIAL CONDS %%%%%%%%%%%%%%%%%%%%%%%%

    % Conditions for each trial in a block. Each block starts on a plateau
    % Columns 1:3 = variable distractor color (R G B)
    % Column 4 = Plateau condition (1 = Targ 1 (usually red), 2 = Targ 2 (usually blue), 0 = transition
    % Column 5 = Transition condition (1 = from Targ1 to Targ 2 (usually RedtoBlue), 2 = Targ2 to Targ1 (usually BluetoRed),0 = plateau
    % Column 6 = Trial position in a single run (from 1 = start of plateau to N = end of transition)
    
    % Targ1 Plateau matrix
    Targ1Plat(1:nplat,1:3) = repmat(Targ1col,nplat,1);
    Targ1Plat(1:nplat,4) = 1;
    Targ1Plat(1:nplat,5) = 0;
    Targ1Plat(1:nplat,6) = 1:nplat;

    % Targ1toTarg2 (RedtoBlue) Trans matrix
    Trans1(1:ntrans,1:3) = RedtoBlue;
    Trans1(1:ntrans,4) = 0;
    Trans1(1:ntrans,5) = 1;
    Trans1(1:ntrans,6) = nplat+1:nplat+ntrans;

    % Targ2 Plateau matrix
    Targ2Plat(1:nplat,1:3) = repmat(Targ2col,nplat,1);
    Targ2Plat(1:nplat,4) = 2;
    Targ2Plat(1:nplat,5) = 0;
    Targ2Plat(1:nplat,6) = 1:nplat;

    % Targ2toTarg1 (RedtoBlue) Trans matrix
    Trans2(1:ntrans,1:3) = BluetoRed;
    Trans2(1:ntrans,4) = 0;
    Trans2(1:ntrans,5) = 2;
    Trans2(1:ntrans,6) = nplat+1:nplat+ntrans;
    
    if StartDistCol == 1    % Start with transition distractors matching target 1 color
        cycle = [Targ1Plat;Trans1;Targ2Plat;Trans2];
    else                    % Start with transition distractors matching target 2 color
        cycle = [Targ2Plat;Trans2;Targ1Plat;Trans1];
    end
    
    % Create Full list of trial conditions
    trialconds = repmat(cycle,(ncycles*nblocks),1);    
    dlmwrite(strcat('Tracking Files/Conds_',expname,'_',subNo,'_sess',Session,'.txt'),trialconds,'delimiter','\t');   % write to file in case need to restart
    
    % Print header info to file
    fprintf(datafilepointer,'%s\tCode = %s\tSession = %s\tStarting Dist Col = %s\n',char(expname),subNo,Session,num2str(StartDistCol));
    fprintf(datafilepointer,'Targ 1 = \t');
    fprintf(datafilepointer,'%f\t',Targ1col);
    fprintf(datafilepointer,'\nTarg 2 = \t');
    fprintf(datafilepointer,'%f\t',Targ2col);
    fprintf(datafilepointer,'\n');
    fprintf(datafilepointer,'SubNo\tTrial\tBlock_trial\tBlock_num\tPlateau_cond\tTransition_cond\tRun_number\tTarg1_position\tTarg2_position\tFixationCheck\tRed_targ_digit\tBlue_targ_digit\tResponse\tTarg_Choice\tAcc\tRT\tOptimal_Choice\tStarEndOptimal\tRepeat_Switch\n');   
                            
    % record stim location positions in Stim file
    fprintf(stimfilepointer,'00\t');
    fprintf(stimfilepointer,'%f\t',gridpos(:,1));
    fprintf(stimfilepointer,'\n');
    fprintf(stimfilepointer,'00\t');
    fprintf(stimfilepointer,'%f\t',gridpos(:,2));
    fprintf(stimfilepointer,'\n');
    
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
        edfFile= strcat('AC',subNo,'.edf');            
        i = Eyelink('Openfile', edfFile);        
        if i~=0
            printf('Cannot create EDF file');
            EYELINK( 'Shutdown');
            return;
        end
        %Do initial setup to check eye position  
        EyelinkDoTrackerSetup(el);
    end
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INSTRUCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    % Present instructions, SPACEBAR to continue    
    if (ScreenY/ScreenX)<(2/3)
        InstrRect = SetRect(centrex-(ScreenY*1.333/2),0,centrex+(ScreenY*1.333/2),ScreenY);
    else
        InstrRect = SetRect(0,centrey-(ScreenX*.667/2),ScreenX,centrey+(ScreenX*.667/2));
    end
    instrfolder = 'Instructions_AC';
    ninstr = 4;
    Beeper;
    
    for instructions = 1:ninstr;
        instrpic=imread(strcat(instrfolder,'/startinstr',num2str(instructions),'.png'));
        instrtex = Screen('MakeTexture',w,instrpic);
        Screen('DrawTexture',w,instrtex,[],InstrRect);
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
        FlushEvents('keyDown');
        Screen('Close',instrtex);
     end
     
    Screen('Flip',w);
    WaitSecs(1.000);
    
%%%%%%%%%%%%%%%%%%%%%%%%% BEGIN TRIALS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
             
    %%%%%%%%%%%%%%%%% EYELINK CALIBRATE AND BEGIN RECORDING %%%%%%%%%%%%%%%%%
    if UseEL == 1
        %Calibrate the eye tracker
        EyelinkDoTrackerSetup(el);

        % do a final check of calibration using driftcorrection
        EyelinkDoDriftCorrection(el);
        eye_used = 0; %0 = left, 1 = right
        %eye_used = Eyelink('EyeAvailable') % get eye that's tracked

        % start recording eye position
        Eyelink('StartRecording');
        % record a few samples before we actually start displaying
        WaitSecs(0.1);
                
        % Total drift correct misses
        fixmisses = 0;
        
        %Eyelink('command','link_sample_data = INPUT,HMARKER')
        %Eyelink('command','inputword_is_window = ON')
        
    end
    
    %%%%%%%%%%%%%%%%%%%%% BLOCK LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    for block=0:nblocks
        
        if block == 0            % Blocks 0 = prac trials  
            trialno=nprac;  
            totaltrialcount = 1;
        else                     % Blocks 1:nblocks = experimental trials  
            trialno = nblocktrials;  
        end
        
        % Reset block counters
        blockacc = 0;
        blockRT = 0; 
        prevchoice = 0;
        
        % Shuffle target digits for each block
        targdigits=targdigits(randperm(size(targdigits,1)),:);
       
        % Instructions for starting experimental trials
        if block == 1 
            totaltrialcount = 1;
            Screen('TextSize', w, 40); 
            message = char(strcat('The experimental trials will now begin.\nThere will be',{' '},num2str(nblocks),{' '},'blocks of',{' '},num2str(nblocktrials),{' '},'trials.\n\n Press SPACE to begin.'));
            DrawFormattedText(w, message, 'center', 'center', white);
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
        
        % Recalibrate at the start of each new block
        if (UseEL == 1) && (block > 0)
            if autocalibrate == 1
                %%%%% Auto-Calibration    
                Eyelink('StopRecording');
                EyelinkDoCalibration(el,'c'); 
                EyelinkDoDriftCorrection(el);
                Eyelink('StartRecording'); 
                fixmisses = 0;
            else
                %%%%% If no auto-calibrate, go to Do Tracker Setup    
                Eyelink('StopRecording');
                EyelinkDoTrackerSetup(el);
                EyelinkDoDriftCorrection(el);
                Eyelink('StartRecording');
                fixmisses = 0;
            end
        end
        
    %%%%%%%%%%%%%%%%%%%%% TRIAL LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
        WaitSecs(.5);    
        for trial = 1:trialno

            % Update new variable distractor color in itemcols array
            VarDistcol = trialconds(totaltrialcount,1:3);   % Variable distractor color for current trial 
            itemcols(1:3,(itemcols(4,:)==5)) = repmat(VarDistcol',1,VarDistnum);  % Variable distractors have code 5 in itemcols array
            
            % Set target digits
            redtargdigit = targdigits(trial,1);
            bluetargdigit = targdigits(trial,2);
            
            % Shuffle distractor digits                 
            distdigitstarg1 = distdigitstarg(:,randperm(size(distdigitstarg,2))); 
            distdigitstarg2 = distdigitstarg(:,randperm(size(distdigitstarg,2)));
            distdigitsvar = distdigitsvar(:,randperm(size(distdigitsvar,2))); 
            distdigitsirr = distdigitsirr(:,randperm(size(distdigitsirr,2)));            
            
            % Create array of all digits        
            itemdigits = [redtargdigit, bluetargdigit,distdigitstarg1(1:TargDistnum),distdigitstarg2(1:TargDistnum),distdigitsvar(1:VarDistnum),distdigitsirr(1:IrrDistnum)];
            Screen('TextSize', w,Digitsize);
            Screen('TextFont', w,Digitfont);
            
            % randomise item locations
            gridrects=gridrects(:,randperm(size(gridrects,2)));
            redtargitem = gridrects(5,1);
            bluetargitem = gridrects(5,2);
                      
            % Check eye recording.
            if UseEL == 1
                elerror=Eyelink('CheckRecording');
                if(elerror~=0)
                    break;
                end
                % If using fixation check, recalibrate if necessary
                if (fixmisses >= allowfixmisses) && (recalibrate == 1)
                    if autocalibrate == 1
                        %%%%% Auto-Calibration    
                        Eyelink('StopRecording');
                        EyelinkDoCalibration(el,'c'); 
                        EyelinkDoDriftCorrection(el);
                        Eyelink('StartRecording'); 
                        fixmisses = 0;
                    else
                        %%%%% If no auto-calibrate, go to Do Tracker Setup    
                        Eyelink('StopRecording');
                        EyelinkDoTrackerSetup(el);
                        EyelinkDoDriftCorrection(el);
                        Eyelink('StartRecording'); 
                        fixmisses = 0;
                    end
                end
                
                Eyelink('Message', 'SYNCTIME');
                Eyelink('Message', 'Trial%i',trial);
            end
            
            
            %%%%%%%%%%%%%%% Fixaton %%%%%%%%%%%%%%%%%%%%%%%%%%%
            
            DrawFormattedText(w, '+', 'center', 'center', grey);
            Screen('Flip',w);
            
            if (UseEL == 1) && (recalibrate == 1)     
                % If recalibrate is on, monitor fixation position during the fixation period
                startfix = GetSecs();
                startfixtimer = GetSecs();
                logfix = GetSecs();
                fixcheck = 0;
                                
                while (logfix - startfix) < ITItime   
                    if Eyelink('NewFloatSampleAvailable') > 0
                        evt = Eyelink('NewestFloatSample');
                        logfix = GetSecs();
                        ex = evt.gx(eye_used+1);
                        ey = evt.gy(eye_used+1);
                        edist = sqrt((ex - centrex)^2+(ey - centrey)^2);
                        if edist > fixerror
                            startfixtimer = GetSecs();   % if fixation deviates, start timing again
                        end
                    end                     
                    if fixcheck == 0    
                        if (logfix - startfixtimer) > .3    %if fixated close to centre for a period of at least 300ms
                            fixcheck = 1;    % fixation check complete
                        end
                    end
                end
                
                if fixcheck == 0
                    fixmisses = fixmisses + 1;
                end
       
            else
                WaitSecs(ITItime);
                fixcheck = 0;
            end
            
            %%%%%%%%%%%%%%% DRAW ITEMS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        
            % Present items
            Screen('FillRect', w, itemcols(1:3,:),gridrects(1:4,:));
            for i = 1:nitems
                Screen('DrawText', w, num2str(itemdigits(i)),gridrects(1,i)+digadjx, gridrects(2,i)+digadjy, Digitcol); % Position of text adjusted a little to center
            end                     

            %%%%%%%%%%%%%%% Present trial %%%%%%%%%%%%%%%%%%%%%%%%%
            DrawFormattedText(w, '+', 'center', 'center', grey);
            Screen('Flip', w);
            if UseEL == 1
                Eyelink('Message', 'SearchDisp%i',trial);
            end
            tic;
            %%%%%%%%%%%%%% GET RESPS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

            while (1) 
                [keyIsDown,~,keyCode] = KbCheck(-1);
                if keyIsDown
                    resp = upper(KbName(keyCode));
                    if length(resp) == 1
                        if strfind(targkeys,resp)>0
                            RT = toc;
                            if UseEL == 1
                                Eyelink('Message', 'Response%i',trial);
                            end
                            break
                        end
                    end
                    if keyCode(escapeKey) 
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
                    FlushEvents('keyDown');
                end  
            end
            
            
            %%%%%%%%%%% RECORD DATA %%%%%%%%%%%%%%%%%%%%%%%%%%%
                        
            response = targd(strfind(targkeys,resp)); % Response digit   
            resptime = round(RT*1000); %RT in ms
            platcond = trialconds(totaltrialcount,4);
            transcond = trialconds(totaltrialcount,5);
            runnum = trialconds(totaltrialcount,6);
            
            % Target choice
            if response == redtargdigit   %Red
                targchoice = 1;
            elseif response == bluetargdigit %Blue
                targchoice = 2;
            else
                targchoice = 0;
            end
            
            % Accuracy: If resp matches either target num
            if targchoice > 0
                Acc = 1;
                blockacc = blockacc + 1;
                blockRT = blockRT + RT;
            else
                Beeper;
                Acc = 0;
            end
            
            % Determine is choice is optimal (1) or not (0)
            if targchoice == 0                      % error trials
                optchoice = NaN;
            elseif platcond > 0                     % Plateau trials
                if targchoice == platcond           % if choice matches the variable distractor color
                    optchoice = 0;                  % non-optimal
                else 
                    optchoice = 1;                  % optimal
                end
            else                                    % Transition trials
                if runnum < (nplat + mid)           % first half of trans
                    if targchoice == transcond      % if choice matches the color closest to start of trans
                        optchoice = 0;              % non-optimal
                    else 
                        optchoice = 1;              % optimal
                    end     
                elseif runnum > (nplat + mid)       % second half of trans
                    if targchoice == transcond      % if choice matches the color closest to start of trans
                        optchoice = 1;              % optimal
                    else 
                        optchoice = 0;              % non-optimal
                    end   
                else                                % Middle trial of trans - exclude because both options are equally optimal
                    optchoice = NaN;
                end
            end
                        
            % Determine if choice matches optimal color at start of plateau (1) or end of transition (2)            
            if targchoice == 0
                startendopt = NaN;
            elseif (targchoice == platcond)||(targchoice == transcond) % if choice matches the variable distractor color on plat
                startendopt = 2;           % optimal at end of transition
            else
                startendopt = 1;           % optimal at start of plateau
            end
                    
            % Determine if choice is switch or repeat
            
            if prevchoice == 0              % if this is the first trial of the block or previous trial was an error
                switchrep = NaN;
            elseif targchoice == prevchoice
                switchrep = 1;              % Repeat
            else
                switchrep = 2;              % Switch
            end
            prevchoice = targchoice;
            
            % Print data to text file
            fprintf(datafilepointer,'%s\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%i\n',subNo,totaltrialcount,trial,block,platcond,transcond,runnum,redtargitem,bluetargitem,fixcheck,redtargdigit,bluetargdigit,response,targchoice,Acc,resptime,optchoice,startendopt,switchrep);
                        
            % Print color code and digit for each location to stimulus file for eye tracker analysis 
            % Sort items first to order by location code
            stimfileprint = sortrows([gridrects(5,:);itemcols(4,:);itemdigits]',1)';
            fprintf(stimfilepointer,'%i\t%f\t',totaltrialcount,stimfileprint(2,:));
            fprintf(stimfilepointer,'\n');
            fprintf(stimfilepointer,'%i\t%f\t',totaltrialcount,stimfileprint(3,:));
            fprintf(stimfilepointer,'\n');
            
            % Count total number of trials           
            totaltrialcount = totaltrialcount + 1;
            
         %%%%%%%%%%% BREAK %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            
            if (trial == trialno)
                if UseEL == 1
                    Eyelink('StopRecording');
                end
                blockacc = round(blockacc/trial*100);
                blockRT = round((blockRT/blockacc)*1000);
                if block < nblocks
                   message = strcat('In this block, your accuracy was\n',num2str(blockacc),'%\nand your response time was\n',num2str(blockRT),'ms.\n\n',...
                   'Have a short break.\n\nPress SPACE to start the next block');
                else %final block
                    message = strcat('In this block, your accuracy was\n',num2str(blockacc),'%\nand your response time was\n',num2str(blockRT),'ms.\n\n',...
                    'You have completed all the trials in this task!\nPlease see the experimenter.');
                end
                DrawFormattedText(w, message, 'center', 'center', white);
                Screen('Flip', w);
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
