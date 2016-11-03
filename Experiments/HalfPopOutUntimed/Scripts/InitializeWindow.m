function [ w ] = InitializeWindow(  )
    % Check if all needed parameters given:
    if nargin < 1
        subNo = 1;
        strcat('no subject number assigned, assigning automatically')
        %error('Must provide required input parameter "subNo"!');
    end
% Clear Matlab/Octave window:
    clc;
    % check for Opengl compatibility, abort otherwise:
    AssertOpenGL;
    % Reseed the random-number generator for each expt.
    rand('state',sum(100*clock));

    % Make sure keyboard mapping is the same on all supported operating systems
    % Apple MacOS/X, MS-Windows and GNU/Linux:
    KbName('UnifyKeyNames');
    % Do dummy calls to GetSecs, WaitSecs, KbCheck to make sure
    % they are loaded and ready when we need them - without delays
    % in the wrong moment:
    KbCheck;
    WaitSecs(0.1);
    GetSecs;
    %%%%%%%%%%%%%%%%%%%%%%
    % open window
    %%%%%%%%%%%%%%%%%%%%%%
    % Get screenNumber of stimulation display. We choose the display with
    % the maximum index, which is usually the right one, e.g., the external
    % display on a Laptop:
    screens=Screen('Screens');
    %screenNumber=max(screens);
    screenNumber=min(screens);
    
    % Hide the mouse cursor:
    HideCursor();
    % Open a double buffered fullscreen window on the stimulation screen
    % 'screenNumber' and choose/draw a gray background. 'w' is the handle
    % used to direct all drawing commands to that window - the "Name" of
    % the window. 'wRect' is a rectangle defining the size of the window.
    % See "help PsychRects" for help on such rectangles and useful helper
    % functions:
  [w, wRect] = Screen('OpenWindow',screenNumber,128,[],32,2);
  %[w, wRect]=Screen('OpenWindow',screenNumber, 128, [0 0 512 512],32,2);  %try a partial window
    % Set text size (Most Screen functions must be called after
    % opening an onscreen window, as they only take window handles 'w' as
    % input:
    Screen('TextSize', w, 32);
    %this is needed to display gif images with tranparancy 
    Screen('Blendfunction', w,GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);


end

