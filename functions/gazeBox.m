function [stimuliOn] = gazeBox(stimuliOn, gazeBnds_x, gazeBnds_y)
% Presents a gaze-box for reading experiments
%   Detailed explanation goes here

global Visual Monitor el

Eyelink('Command', ['draw_filled_box ' num2str(Visual.offsetX) ' ' num2str(Visual.resY/2- Visual.GazeBoxSize/2) ' ' ...
            num2str(Visual.offsetX+Visual.GazeBoxSize) ' ' num2str(Visual.resY/2+ Visual.GazeBoxSize/2) '3']);
        
WaitSecs(0.1);
Eyelink('StartRecording');

Screen('CopyWindow', Monitor.buffer(1), Monitor.window);
Screen('Flip', Monitor.window);
Eyelink('Message', 'GAZE TARGET ON');
gazeBoxTriggered=false;
onTarget= false;
% gazeTimeOut= false;
gazeStart= GetSecs;

% loop that triggers the gaze-box
while ~gazeBoxTriggered && ~onTarget
    evt= Eyelink('NewestFloatSample');
    x = evt.gx(2); 
    y = evt.gy(2);

    %sample= tracker.sample(); % get current eye position
    elapsedTime= GetSecs-gazeStart; % time since gaze box appeared
    onTarget= x>= gazeBnds_x(1) && x<= gazeBnds_x(2) && y>= gazeBnds_y(1) && y<= gazeBnds_y(2);

    if onTarget % the eye is on the gaze box
        WaitSecs(Visual.gazeBoxDur/1000);
        onTarget= x>= gazeBnds_x(1) && x<= gazeBnds_x(2) && y>= gazeBnds_y(1) && y<= gazeBnds_y(2);
        if onTarget % eye still on gaze box after x ms
            gazeBoxTriggered= true;
            stimuliOn= true;
            %tracker.send_command("clear_screen %d" % (0))
        else
            onTarget= false;
        end
    end

    if elapsedTime> Visual.gazeBoxDisplayTime % gaze box timeout
        Eyelink('Message', 'TRIAL ABORTED');
        Eyelink('StopRecording');
        EyelinkDoTrackerSetup(el);
        % drift check:
        EyelinkDoDriftCorrection(el);
        onTarget= true;
        gazeBoxTriggered= true;
    end
end

Eyelink('Message', 'GAZE TARGET OFF');
Eyelink('Message', 'DISPLAY ON');
Eyelink('Message', 'SYNCTIME');

end

