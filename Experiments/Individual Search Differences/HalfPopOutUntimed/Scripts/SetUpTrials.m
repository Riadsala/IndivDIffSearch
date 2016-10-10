function [display] = SetUpTrials(w)
%set all variables for running the blocks.
%these values canbe hard coded in this file, or they can be read in from a
%text file (not yet implemented)
%for this experiment....

% Block 0:  Training
% Block 1:  all conditions are now randomized within this block
% including

%%%%%%%%%%%%%%%%%%%%%%
% file handling
%%%%%%%%%%%%%%%%%%%%%%
if nargin<1
    display.w = [];
    display.white=255;
    display.black=0;
    display.grey = 127;
    display.width = 1024;
    display.height = 768;
else
    display.w = w;
    display.white=WhiteIndex(w);
    display.black=BlackIndex(w);
    display.grey = ((WhiteIndex(w)+BlackIndex(w))/2) +6;
    [display.width, display.height]=Screen('WindowSize', w);%screen returns the size of the window
    
end



% Define filenames of input files and result file:


display.winrect= [0 0 display.width display.height];

%these variables are common to all experiment blocks
display.centerX                   = display.width/2;% divides the width of the screen into two
display.centerY                   = display.height/2; %divides the height of the screen into two

centW = 40.0; %screen width, cm
centD = 45.0; %dist to screen, cm

rad = 2.0*atan(centW/(centD*2));%radians of entire monitor
deg = rad*(180/pi);%Degrees of entire monitor
display.pixelsperdegree  = floor(display.width/deg);  %pixels per degree

% fixation needed here
display.fixXLoc                   = display.centerX;
display.fixYLoc                   = display.centerY;
display.fixHalfBox                = floor(0.5*display.pixelsperdegree);      %max distance we allow to still be on target for fix and saccade land
display.fixRect                   = [display.fixXLoc-display.fixHalfBox display.fixYLoc-display.fixHalfBox display.fixXLoc+display.fixHalfBox display.fixYLoc+display.fixHalfBox];

%Now details of faces

%file = strcat(folder,'F02HA.png'); %if we just wanted one file we select it
%from our folder
%display.numFaces = length(filelist);
display.maxResponseTime = 60.0;
display.stimOnset       = 1.0;

LoadInLineStimuli;


end

