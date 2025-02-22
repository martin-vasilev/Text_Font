function [answer]=Question(question, corr_ans, item, cond, opt1, opt0, adjustFontSize)
% Presents a binary question (Yes/No, TRUE/FALSE)
% Martin Vasilev, 2017

global Visual Monitor const;

if adjustFontSize % changes font size of question to match item size (in font size manip. studies)
    Screen(Monitor.window, 'TextSize', Visual.FontSize);
end

% Setup window:
Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window); % clean screan

Screen('DrawText', Monitor.window, 'QUESTION:' , Visual.sentPos(1), Visual.sentPos(2)-100, Visual.FGC); % sentence
Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence

if const.checkPPL % for testing
    MLcheck= strfind(question, '\n');

    if ~isempty(MLcheck)
        sentenceString= strrep(question, '\n', '@');
        sentenceString= strsplit(sentenceString, '@');
        sentenceString= char(sentenceString{1});
    else
        sentenceString= question;
    end

    lngth= length(sentenceString)*Visual.Pix_per_Letter;
    Screen('FrameRect', Monitor.window, Visual.FGC, [Visual.sentPos(1) Visual.resY/2- Visual.GazeBoxSize/2 ...
        Visual.sentPos(1)+lngth Visual.resY/2+ Visual.GazeBoxSize]);
end

% adjust response boxes by label inside:
yes_length= round((length(opt1)*Visual.Pix_per_Letter)*1.3);
no_length= round((length(opt0)*Visual.Pix_per_Letter)*1.3);

yes_dim= [Visual.sentPos(1)- yes_length/2  Visual.sentPos(2)+150-10  Visual.sentPos(1)+yes_length/2 Visual.sentPos(2)+150+40];
no_dim= [Visual.sentPos(1)+400-no_length/2 Visual.sentPos(2)+150-10 Visual.sentPos(1)+400+no_length/2 Visual.sentPos(2)+150+40];

Screen('FillRect', Monitor.window , [210 210 210], yes_dim);
Screen('FillRect', Monitor.window , [210 210 210], no_dim);
Screen('DrawText', Monitor.window, opt1 , yes_dim(1) + round((0.25*yes_length)/2), Visual.sentPos(2)+150, Visual.FGC); % option 1
Screen('DrawText', Monitor.window, opt0 , no_dim(1) + round((0.25*no_length)/2), Visual.sentPos(2)+150, Visual.FGC); % option 0
Screen('Flip', Monitor.window);

SetMouse(Visual.resX/2,Visual.resY/2);
ShowCursor(0); 

imageArray= Screen('GetImage', Monitor.window, [0 0 1920 1080]);
imwrite(imageArray, 'disp.bmp');
        
        
Eyelink('Command', 'set_idle_mode');
Eyelink('Command', 'clear_screen 0');
status= Eyelink('ImageTransfer', 'disp.bmp', 0, 0, 0, 0,0, 0, 16);

% Initial question stamps:
Eyelink('Message', ['TRIALID F' num2str(cond) 'I' num2str(item) 'D1']);
Eyelink('Message', ['QUESTION_ANSWER ' num2str(corr_ans)]);
Eyelink('Message', 'DELAY 500 MS');
Eyelink('StartRecording');
WaitSecs(0.05);
Eyelink('command', ['record_status_message ' ['Question ' 'F' num2str(cond) 'I' num2str(item) 'D1']]);

WaitSecs(0.5);
Eyelink('Message', 'DISPLAY ON');
Eyelink('Message', 'SYNCTIME');


answer=-1;
escapeKey= KbName('ESCAPE');
confirmKey= KbName('Y');

while answer<0
    [x,y,buttons] = GetMouse(Monitor.window);
    
    % monitor keyboard presses for manual exit:
    check1= 0;
    
    [keyIsDown, seconds, keyCode]= KbCheck;
    keyCode= find(keyCode,1);
    
    if keyCode== escapeKey % first request to exit
           status= Eyelink('ReceiveFile');
           Eyelink('Shutdown');
           Screen('CloseAll');
           error('Experiment terminated by user');
    end
    
    if buttons(1)==1
        if IsInRect(x,y, yes_dim) %x> yes_dim(1) && y> yes_dim(2) && x< yes_dim(3) && y< yes_dim(4)
            Screen('DrawText', Monitor.window, 'QUESTION:' , Visual.sentPos(1), Visual.sentPos(2)-100, Visual.FGC); % sentence
            Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
            Screen('FillRect', Monitor.window , [189 255 183], yes_dim);
            Screen('DrawText', Monitor.window, opt1 , yes_dim(1)+ round((0.25*yes_length)/2), Visual.sentPos(2)+150, Visual.FGC); % opt 1
            Screen('FillRect', Monitor.window , [210 210 210], no_dim);
            Screen('DrawText', Monitor.window, opt0 , no_dim(1)+ round((0.25*no_length)/2), Visual.sentPos(2)+150, Visual.FGC); % opt 0
            Screen('Flip', Monitor.window);
            WaitSecs(0.3);
            answer= 1;
        end
        
        if IsInRect(x,y, no_dim) %x> no_dim(1) && y> no_dim(2) && x< no_dim(3) && y< no_dim(4)
            Screen('DrawText', Monitor.window, 'QUESTION:' , Visual.sentPos(1), Visual.sentPos(2)-100, Visual.FGC); % sentence
            Screen('DrawText', Monitor.window, question , Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
            Screen('FillRect', Monitor.window , [210 210 210], yes_dim);
            Screen('DrawText', Monitor.window, opt1 , yes_dim(1)+ round((0.25*yes_length)/2), Visual.sentPos(2)+150, Visual.FGC); % opt 1
            Screen('FillRect', Monitor.window , [189 255 183], no_dim);
            Screen('DrawText', Monitor.window, opt0 , no_dim(1)+ round((0.25*no_length)/2), Visual.sentPos(2)+150, Visual.FGC); % opt 0
            Screen('Flip', Monitor.window);
            WaitSecs(0.3);
            answer= 0;
        end
    end
    
%questEnd= mouseClicked && answered;
end

if answer==corr_ans
    Eyelink('command', ['record_status_message ' 'CORRECT!']);
else
    Eyelink('command', ['record_status_message ' 'INCORRECT!']);
end
WaitSecs(0.5);

Eyelink('StopRecording');

% Print end of question stamps to edf:
Eyelink('Message', ['ENDBUTTON ' num2str(answer)]);
Eyelink('Message', 'DISPLAY OFF');
Eyelink('Message', ['TRIAL_RESULT ' num2str(answer)]);
Eyelink('Message', 'TRIAL OK');


Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);
Eyelink('command', 'clear_screen 0'); % clear tracker screen
HideCursor;

end
