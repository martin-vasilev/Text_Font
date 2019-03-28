% Presentation of Experimental trials
% Martin Vasilev, 2017

global const Visual sent Monitor el; %Audio; 

% Blackbox toolkit testing:
%s= serial('COM11');
%set(s, 'BaudRate', 115200, 'DataBits', 8, 'StopBits', 1, 'Parity', 'none')
%fopen(s);
%fprintf(s, 'RR');
%fprintf(s,'FF');

%const.ntrials=10; % TEMPORARY!!! Use only for testing

HideCursor; % hide the mouse cursor

% Calibrate the eye tracker
EyelinkDoTrackerSetup(el);

% Trial presentation loop:
for i=1:const.ntrials
    
    %calResult = Eyelink('CalResult');
%%  stimuli set-up:
    
    trialEnd= false; 
	item= design(i,1); % item is 1st column
    cond= design(i,2); % condition is 2nd column
    
    % get sent string:
    whichRow= find(sent.item== item & sent.Cond== cond, 1);
    line1= char(sent.Line1(whichRow));
    line2= char(sent.Line2(whichRow));
    sentenceString= [line1, '\n', line2];
    
    % get ppl, etc. for current sent:
    if ismember(cond, [1,3,9])
        Visual.Pix_per_Letter= 12;
        Visual.LetterHeight= 21;
        Visual.FontSize= 16;
    else
        Visual.Pix_per_Letter= 16;
        Visual.LetterHeight= 28;
        Visual.FontSize= 22;
    end
    
	
	% get image dir:
    imageFile= ['img/Item' num2str(item) '_Cond' num2str(cond) '.bmp'];
    img= imread(imageFile); % read in image
    
    % drift check:
    EyelinkDoDriftCorrection(el);
    
    %% Eyelink & Screen trial set-up:
	stimuliOn= false;
    
    while ~stimuliOn
        if item> const.Maxtrials % if practice
            Eyelink('Message', ['TRIALID ' 'P' num2str(cond) 'I' num2str(item) 'D0']);
			% print trial ID on tracker screen:
            Eyelink('command', ['record_status_message ' [ num2str(round((i/const.ntrials)*100)) 'Prcnt:' 'P' num2str(cond) 'I' num2str(item) 'D0']]);
        else
			Eyelink('Message', ['TRIALID ' 'E' num2str(cond) 'I' num2str(item) 'D0']);
			% print trial ID on tracker screen:
			Eyelink('command', ['record_status_message ' [ num2str(round((i/const.ntrials)*100)) 'Prcnt:' 'E' num2str(cond) 'I' num2str(item) 'D0']]); 
        end
        
        % print image url to edf:
        Eyelink('Message', imageFile);

% 		if cond<9
%             Eyelink('Message', ['SOUND ONSET DELAY: ' num2str(design.delay(i))]);
%             Eyelink('Message', ['CRITICAL REGION 1 @ ' num2str(Bnds(2)) ' ' num2str(Bnds(2+1))]);
% 			Eyelink('Message', ['CRITICAL REGION 2 @ ' num2str(Bnds(4)) ' ' num2str(Bnds(4+1))]);
% 			Eyelink('Message', ['CRITICAL REGION 3 @ ' num2str(Bnds(6)) ' ' num2str(Bnds(6+1))]);
% 			Eyelink('Message', ['CRITICAL REGION 4 @ ' num2str(Bnds(8)) ' ' num2str(Bnds(8+1))]);
% 			Eyelink('Message', ['CRITICAL REGION 5 @ ' num2str(Bnds(10)) ' ' num2str(Bnds(10+1))]);
%         end
        
        % print text stimuli to edf:
        %stim2edf(sentenceString); % Single line
        stim2edfML(sentenceString); % Multi-line
        
        % prepare Screens:
        % sentence presentation:
        Screen('FillRect', Monitor.buffer(2), Visual.BGC);
        % put image on the screen:
        Screen('PutImage', Monitor.buffer(2), img);
       
        % Use this for printing text as a string:
        %Screen('DrawText', Monitor.buffer(2), sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
        
        if const.checkPPL
            MLcheck= strfind(sentenceString, '\n');
            
            if ~isempty(MLcheck)
                sentenceString= strrep(sentenceString, '\n', '@');
                sentenceString= strsplit(sentenceString, '@');
                sentenceString= char(sentenceString{1});
            end
            
			lngth= length(sentenceString)*Visual.Pix_per_Letter;
            Screen('FrameRect', Monitor.buffer(2), Visual.FGC, [Visual.offsetX Visual.resY/2- Visual.GazeBoxSize/2 ...
                Visual.offsetX+lngth Visual.resY/2+ Visual.GazeBoxSize]);
        end
        
        % Print stimuli to Eyelink monitor:
        % draw gaze box on tracker monitor:
        imageArray= Screen('GetImage', Monitor.buffer(2), [0 0 1920 1080]);
%         if cond==3
%             B= eval(['boundary' num2str(soundPos)]);
%             imageArray(:, B-5:B+5)= 179;
%         end
        
        imwrite(imageArray, 'disp.bmp');
        
        Eyelink('Command', 'set_idle_mode');
        Eyelink('Command', 'clear_screen 0');
        status= Eyelink('ImageTransfer', 'disp.bmp', 0, 0, 0, 0,0, 0, 16);
        
        %% Present Gaze-box:
        stimuliOn= gazeBox(stimuliOn);
        
    end
    
    %% Present text stimuli:
    
    Screen('CopyWindow', Monitor.buffer(2), Monitor.window);
    Screen('Flip', Monitor.window);
	trialStart= GetSecs;
    
    while ~trialEnd
        trialTime= GetSecs- trialStart;
        [x,y,buttons] = GetMouse(Monitor.window);
        trialEnd= buttons(1); %KbCheck; 
        
        % use this for gaze-contingent manipulations:
        %evt= Eyelink('NewestFloatSample');
        %xpos = evt.gx(2);

        
        if const.seeEye % for testing only (enable code above)
            Screen('FillRect', Monitor.window, Visual.BGC);
            Screen('DrawText', Monitor.window, sentenceString, Visual.sentPos(1), Visual.sentPos(2), Visual.FGC); % sentence
            Screen('DrawDots', Monitor.window, [xpos, 540], 10, [0 0 0], [],2);
            Screen('Flip', Monitor.window);
        end
        
        % end trial automatically if no response by participant
        if trialTime> const.TrialTimeout 
             trialEnd= true;
             %tracker.log('TRIAL ABORTED')
 			 Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
             Screen('Flip', Monitor.window);
        end
        
     end
    
    Screen('FillRect', Monitor.window, Visual.BGC); % clear subject screen
    Screen('Flip', Monitor.window);
    Eyelink('command', 'clear_screen 0'); % clear tracker screen	
	
	% end of trial messages:
    Eyelink('Message', 'ENDBUTTON 5');
    Eyelink('Message', 'DISPLAY OFF');
    Eyelink('Message', 'TRIAL_RESULT 5');
    Eyelink('Message', 'TRIAL OK');

    Eyelink('StopRecording');
    
    
     %% Questioms:
      if sent.hasQuest(whichRow)==1
          % present question:
          answer= Question(char(sent.Question(whichRow)), sent.Answer(whichRow), ...
              item, cond, 'TRUE', 'FALSE', true);
      end     
    
end


% end of Experiment text:
text= 'The experiment is finished! Thank you for participating!';
Screen(Monitor.window, 'TextSize', Visual.InstrTextSize);
Screen('DrawText', Monitor.window, text, Visual.resX/2- (Visual.Pix_per_Letter+3)*length(text)/2, Visual.resY/2, [139, 0, 0]);
Screen('Flip', Monitor.window);
