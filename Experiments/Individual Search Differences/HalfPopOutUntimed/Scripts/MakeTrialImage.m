Screen('FillRect', osw.Orig ,display.grey);
Screen('FillRect', osw.Filt ,display.grey);

%create Original offscreen faces
%Screen('FillRect',osw.Orig ,display.grey);


    % load in original
    pic = display.lineSegm(trials(t)).im;
    targTexOrig = Screen('MakeTexture', display.w, pic);
    Screen('DrawTexture', osw.Orig, targTexOrig , []);%add location here ,display.winrect
    % load in filtered
    switch (block(1))
        case 'U'
            % unchanged - control condition
            pic = display.lineSegm(trials(t)).im;
            targTexFilt = Screen('MakeTexture', display.w, pic);
            Screen('DrawTexture', osw.Filt, targTexFilt , []); %,display.winrect
        case 'B'
            % blank - grey level background
       
    end
    
    






%to make an image of each trial search display
 im = Screen('GetImage', osw.Orig);
 imwrite(im, 'orig.png');
%  im = Screen('GetImage', osw.Filt);
%  imwrite(im, 'filt.png');

clear pic numDistTex
