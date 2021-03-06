function RunExperimentPractice(subNo, block)
%RunExperiment(subNo, block)
%Simulated hemianopia search experiment. 
%Based on Anna's experiment dated Jan 9th 2014.
%blocks should use one of these two-char codes:
% Blocks = {...
%     'Ul','Ur',...
%     'Bl','Br',...
%     'Fl','Fr',...
%     'Dl','Dr',...
%     };


%% check block is valid!
if isempty(regexp(block, '[UB][lr]'))
    disp('invalid block. please try again. see help for options. Thank you.')
    return
end

addpath('Scripts/');

%%%%%%%%%%%%%%%%%%%%%%%%%
% preliminary stuff
%%%%%%%%%%%%%%%%%%%%%%%%%

%setup our full screen window and other initialization
display.w = InitializeWindow(  );


[fixation, ~, alphafix]= imread('letter_x.png');% read the fixation image from the file
fixation(:,:,4)=alphafix; %add alpha channel to our fixation point
fixationTex = Screen('MakeTexture', display.w, fixation);



iLink.doILink = 1;
%output.empy=0;
%%%%%%%%%%%%%%%%%%%%%%
% experiment
%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Set experiment variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%this is the primary function to change all experiment parameters
[display] = SetUpTrials(display.w);
output.folder=strcat('Data/', num2str(subNo), '/' ,block,'/');
output.filename = strcat(output.folder,block,num2str(subNo),'.dat'); % name of data file to write to
iLink.edfdatafilename = strcat(block,num2str(subNo),'.edf');
% check for existing result file to prevent accidentally overwriting
% files from a previous subject/session (except for subject numbers > 99):
if subNo>99
    fopen(output.filename, 'rt');  % its a test trial
else
    while (fopen(output.filename, 'rt')~=-1)
        
        output.filename = strcat(output.folder,block,num2str(subNo),'_A.dat'); % name of search data file to write to
        %iLink.edfdatafilename = strcat(output.folder,num2str(subNo),'.edf');
        iLink.edfdatafilename = strcat(block,num2str(subNo),'_A.edf');
%        iLink.edfdatafilename = strcat(num2str(subNo),'.edf');
    
    end;
    strcat('Result data file already exists! Choosing: ', num2str(subNo))
end
mkdir(output.folder);
output.resultsFile = fopen(output.filename,'wt'); % open ASCII file for writing


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize all eyelink and calibrate
%%%%%%%%%%%%%%%%%%%%%%%%%%%
if iLink.doILink == 1
    iLink = InitEyeLink(iLink, display.w);
end

%print header for output file
fprintf(output.resultsFile,'Trial\t');
fprintf(output.resultsFile,'Target_pr\t');
fprintf(output.resultsFile,'Target_side\t');
% fprintf(output.resultsFile,'Row\t');
% fprintf(output.resultsFile,'Column\t');
% fprintf(output.resultsFile,'Variability\t');
fprintf(output.resultsFile,'Name\t');
fprintf(output.resultsFile,'Key\t');
fprintf(output.resultsFile,'RT\t');
fprintf(output.resultsFile,'Message\t');
fprintf(output.resultsFile,'Trial_Type\t');
fprintf(output.resultsFile,'Hemianopia\n');

% Write instruction message for subject
%now the experiment part of the block
message = ' EXPERIMENT Trials \n Search the display for a line tilted 45 degrees to the right \n Press left arrow key if present and right arrow key if absent.';
DrawFormattedText(display.w, message, 'center', 'center', WhiteIndex(display.w));

% Update the display to show the instruction text:
Screen('Flip', display.w);
% Wait for mouse click:
GetClicks(display.w);

%create offscreen window for temporary drawing and enable alpha
[osw.Orig,osw.rect] = Screen('OpenOffscreenWindow',-1,[],display.winrect);
Screen('Blendfunction', osw.Orig,GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
[osw.Filt] = Screen('OpenOffscreenWindow',-1,[],display.winrect);
Screen('Blendfunction', osw.Filt,GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%run experiment, one trial at a time
%%%%%%%%%%%%%%%%%%%%%%%%%%%
trials=100;
randomOrder = randperm(trials); %create random order from that.
trials = randomOrder;
for t = 1:length(trials)  % change back to one to do practice blocks!!
    %block order holds the order to run for this subject

    % make trial textures: original and filtered (=dot or blank)
    MakeTrialImage;
    
    % Run a trial
    RunTrial(trials, t, iLink, output, display, osw, fixationTex, block);
    if t==25
    CleanUpExpt(iLink, output);
    end
    
end



CleanUpExpt(iLink, output);
sca


end

