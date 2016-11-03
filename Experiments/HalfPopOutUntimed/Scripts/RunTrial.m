
function RunTrial(trials,t, iLink, output, display, osw, fixationTex, block);

trial_type=block(1);
hemianopia=block(2);

%initialize output
output.message = 'GoodTrial'; %assume a good trial till proven otherwise
output.RT = 0;
output.keypressed = 'x';
space=KbName('space');
leftArrowKey=KbName('leftArrow');
rightArrowKey= KbName('rightArrow');
arrayOnset = 0;


%display fixation and start trial timer
%DrawFormattedText(display.w, 'test', 'center', 'center', WhiteIndex(display.w));
if iLink.doILink
    Screen('DrawTexture', display.w, fixationTex , [],display.fixRect );
    Screen('Flip', display.w );%to clear buffer
    EyelinkDoDriftCorrection(iLink.el, display.fixXLoc, display.fixYLoc, 0, 1);
    % Start data recording to EDF file, BEFORE DISPLAY. */
    status=Eyelink('startrecording');
    % record a few samples before we actually start displaying
    WaitSecs(0.1);
end
%make sure space is released
[KeyIsDown,endrt, KeyCode]=KbCheck();
while (KeyIsDown==1)
    [KeyIsDown,endrt, KeyCode]=KbCheck();
end

%  while (KeyIsDown==1)
%      [KeyIsDown, endrt, KeyCode]=KbCheck();
%  end;

trialStart = GetSecs();%to clear buffer

%event flags.  set these when the events happen in the timing loop
stimPresentedFlag =0;

if iLink.doILink
    Eyelink('message', 'DISPLAY_ON');	 % message for RT recording in analysis
    Eyelink('message','SYNCTIME');
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%primary timing loop.
%each if statement in here checks for one of our events like cue onset, or
%button response
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
xCurr = display.fixXLoc;
yCurr = display.fixYLoc;

while (trialStart + display.maxResponseTime > GetSecs() && output.RT ==0)
    
    %%%%%%%%%%%%%%%%%%%%%%%%
    % 0) grab current eye position
    if iLink.doILink
        if EYELINK('isconnected')==iLink.el.notconnected   % Check link often so we don't lock up if tracker lost
            output.message = 'ForceQuitProgram';
            return;
        end;
        
        %ensure we maintain fixation.  Just check location
        
        if Eyelink( 'NewFloatSampleAvailable') > 0
            %if yes, grab it
            evt = Eyelink( 'NewestFloatSample');
            if hemianopia=='l'
            xCurr = (max(evt.gx))-10;
            %             xCurrUp = (max(evt.gx))+10;
            %             xCurrDown = (max(evt.gx))+10; %this is to make upper/lower half different to match patient deficits. but shouldn't it be gaze contingent on the y???
            yCurr = max(evt.gy);
            elseif hemianopia=='r'
            xCurr = (max(evt.gx))+10;
            %             xCurrUp = (max(evt.gx))+10;
            %             xCurrDown = (max(evt.gx))+10; %this is to make upper/lower half different to match patient deficits. but shouldn't it be gaze contingent on the y???
            yCurr = max(evt.gy); 
            end
        end;
        
    end
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%
    % 1) poll for a keypress.  all keys are errors at this point
    [KeyIsDown, endrt, KeyCode]=KbCheck();
    if (KeyIsDown==1)
        output.RT = GetSecs() - trialStart;
        if KeyCode(leftArrowKey)
            output.keypressed = 'l';
        end
        if KeyCode(rightArrowKey)
            output.keypressed = 'r';
        end
        if arrayOnset == 0
            output.message = 'EARLY_RESPONSE';
            if iLink.doILink
                Eyelink('message', 'EARLY_RESPONSE');	 % message for RT recording in analysis
            end
        else
            outmsg = ['RESPONSE_' output.keypressed];
            if iLink.doILink
                Eyelink('message', outmsg);	 % message for RT recording in analysis
            end
        end
        %return;
        
    end
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %2) is it time to present the search array?
    if trialStart + display.stimOnset < GetSecs()
    
        if xCurr<0
            xCurr = 0;
        end
        if xCurr>osw.rect(3)
            xCurr = osw.rect(3);
        end
        
        
        if strcmp(block(2), 'l')
            newFiltRect = [osw.rect(1) osw.rect(2) xCurr osw.rect(4)];
            newOrigRect = [xCurr osw.rect(2) osw.rect(3) osw.rect(4)];
        else
            newFiltRect = [xCurr osw.rect(2) osw.rect(3) osw.rect(4)];
            newOrigRect = [osw.rect(1) osw.rect(2) xCurr osw.rect(4)];
        end
        
        Screen('CopyWindow',osw.Filt,display.w,newFiltRect,newFiltRect);
        Screen('CopyWindow',osw.Orig,display.w,newOrigRect,newOrigRect);
        
        
        arrayOnset = Screen('Flip', display.w );%to clear buffer
        if iLink.doILink && stimPresentedFlag ==0
            Eyelink('message', 'FACES_DRAWN');	 % message for RT recording in analysis
        end
        stimPresentedFlag =1;%only draw this once
    end
    if display.lineSegm(trials(t)).targPres==1 &&output.keypressed=='r';
        Beeper(700,.6,.3);
    elseif display.lineSegm(trials(t)).targPres==0 && output.keypressed=='l';
        Beeper(700,.6,.3);
    end
    
    
    
end

%print the results of this file to file
if iLink.doILink
    Eyelink('message','TRIAL_OVER');
    Eyelink('message','TRIAL_RESULTS');
    Eyelink('stoprecording');
end
if iLink.doILink
    TrialNo = ['TRIALNO_' num2str(t)];
    Eyelink('message',TrialNo);
    
    TargPres=display.lineSegm(trials(t)).targPres;
    TargPres1 = ['TargPres_' num2str(TargPres)];
    Eyelink('message',TargPres1);
    
    TargSide=display.lineSegm(trials(t)).targSide;
    TargSide1 = ['TargSide_' TargSide];
    Eyelink('message',TargSide1);
    
    Row= num2str(display.lineSegm(trials(t)).row);
    Row1=['Row_' Row];
    Eyelink('message', Row1);
    
    Column= num2str(display.lineSegm(trials(t)).column);
    Column1=['Column_' Column];
    Eyelink('message', Column1);
    
    
    Name=display.lineSegm(trials(t)).name;
    Name1=['Name_' Name];
    Eyelink('message',Name1);
    
    Key = ['Key_' output.keypressed];
    Eyelink('message',Key);
    
    RT = ['RT_' num2str(output.RT)];
    Eyelink('message',RT);
    
    Message = ['Message_' output.message];
    Eyelink('message',Message);
    
    TrialType = ['TrialType_' trial_type];
    Eyelink('message', TrialType);
    
    Hemianopia=['Hemianopia_', hemianopia];
    Eyelink('message', Hemianopia);
    
    Done='Done';
    Eyelink('message', Done);
    
end
fprintf(output.resultsFile,'%d\t %d\t %s\t %s\t %s\t %2.3f\t %s\t %s\t %s\n',t, TargPres, TargSide, Name, output.keypressed, output.RT, output.message,trial_type, hemianopia);


%     fprintf(output.resultsFile,'%d\n',tr);
WaitSecs(.2);
%make sure space is released
[KeyIsDown, endrt, KeyCode]=KbCheck();
while (KeyIsDown==1)
    [KeyIsDown, endrt, KeyCode]=KbCheck();
end;
return;
end

