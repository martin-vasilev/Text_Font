function [stimuliOn] = gazeBox(stimuliOn)
% Presents a gaze-box for reading experiments
%   Detailed explanation goes here

global Visual Monitor el

% print gaze box on screen:
Screen('FillRect', Monitor.buffer(1), Visual.FGC, [Visual.offsetX Visual.resY/2- Visual.GazeBoxSize/2 Visual.offsetX+Visual.GazeBoxSize ...
    Visual.resY/2+ Visual.GazeBoxSize]); % gazebox
gazeBnds_x= [Visual.offsetX-+Visual.GazeBoxSize/2 Visual.offsetX+Visual.GazeBoxSize/2]; % centre at xpos offset
gazeBnds_y= [Visual.resY/2- Visual.GazeBoxSize/2 Visual.resY/2+ Visual.GazeBoxSize];

Eyelink('Command', ['draw_filled_box ' num2str(Visual.offsetX) ' ' num2str(Visual.resY/2- Visual.GazeBoxSize/2) ' ' ...
            num2str(Visual.offsetX+Visual.GazeBoxSize) ' ' num2str(Visual.resY/2+ Visual.GazeBoxSize/2) '3']);
        
WaitSecs(0.1);
Eyelink('StartRecording');

Screen('CopyWindow', Monitor.buffer(1), Monitor.window);
Screen('Flip', Monitor.window);
Eyelink('Message', 'GAZE TARGET ON');
gazeBoxTriggered=false;
onTarget= false;
gazeStart= GetSecs;

% loop that triggers the gaze-box
while ~gazeBoxTriggered && ~onTarget
    
    % get eye position:
    evt= Eyelink('NewestFloatSample');
    x = max(evt.gx); 
    y = max(evt.gy);
    % psst this is a workaround to work with either eye!
    % the positive number (always max) is the value of the eye that is tracked 

    elapsedTime= GetSecs-gazeStart; % time since gaze box appeared
    onTarget= x>= gazeBnds_x(1) && x<= gazeBnds_x(2) && y>= gazeBnds_y(1) && y<= gazeBnds_y(2);

    if onTarget % the eye is on the gaze box
        WaitSecs(Visual.gazeBoxDur/1000);
        onTarget= x>= gazeBnds_x(1) && x<= gazeBnds_x(2) && y>= gazeBnds_y(1) && y<= gazeBnds_y(2);
        if onTarget % eye still on gaze box after x ms
            gazeBoxTriggered= true;
            stimuliOn= true;
        else
            onTarget= false;
        end
    end

    if elapsedTime> Visual.gazeBoxDisplayTime % gaze box timeout
        Eyelink('Message', 'TRIAL ABORTED');
        Eyelink('StopRecording');
        % re-calibrate:
        EyelinkDoTrackerSetup(el);
        % drift check:
        EyelinkDoDriftCorrection(el);
        
        % exit while loop; go back to main function
        onTarget= true;
        gazeBoxTriggered= true;
    end
end

Eyelink('Message', 'GAZE TARGET OFF');
Eyelink('Message', 'DISPLAY ON');
Eyelink('Message', 'SYNCTIME');

end
