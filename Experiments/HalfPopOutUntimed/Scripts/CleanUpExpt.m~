function  CleanUpExpt( iLink, output )
    if iLink.doILink
        EYELINK('ReceiveFile',[iLink.edfdatafilename]);
        EYELINK('closefile');
        EYELINK('shutdown');
    end

    Screen('CloseAll');% closes all windows and textures

    ShowCursor();
    fclose('all');
    Priority(0);
 
end

