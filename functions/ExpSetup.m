function ExpSetup
global Visual const Monitor el Audio;

% get participant number:
const.ID= input('Please enter participant number: ');
const.edffilename= [const.expName '' num2str(const.ID) '' '.edf'];
if exist(const.edffilename, 'file')==2 %check if file already exists
    error('Participant file already exists!');
end

%% Open screen
oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 3);
oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 1);
	
% Find out how many screens and use largest screen number.
whichScreen = max(Screen('Screens'));

% Setup window:
Monitor.window = Screen('OpenWindow', whichScreen);
Screen('FillRect', Monitor.window, Visual.BGC);
Screen('Flip', Monitor.window);

for i=1:2
   Monitor.buffer(i)= Screen(Monitor.window, 'OpenOffscreenWindow');
end

for i=1:2
   Screen(Monitor.buffer(i), 'TextSize', Visual.FontSize);
   Screen(Monitor.buffer(i), 'TextFont', Visual.Font);
   Screen(Monitor.buffer(i), 'TextStyle', 0); % normal
end

%% Open Eyelink connection:
dummymode=0;
el=EyelinkInitDefaults(Monitor.window);

% Initialization of the connection with the Eyelink Gazetracker.
% exit program if this fails.
if ~EyelinkInit(dummymode)
    fprintf('Eyelink Init aborted.\n');
    cleanup;  % cleanup function
    return;
end

% open file to record data to
Eyelink('openfile', const.edffilename);

%% Eyelink setup:

% log some details to EDF file:
Eyelink('Message', ['DISPLAY COORDS ' num2str(0) ' ' num2str(0) ' ' num2str(Visual.resX-1) ' ' num2str(Visual.resY-1)]);
Eyelink('Message', ['FRAMERATE ' num2str(Visual.frameRate)]);

% send commands to tracker:
Eyelink('command', ['screen_pixel_coords = 0 0 ' num2str(Visual.resX-1) ' ' num2str(Visual.resY-1)]);
Eyelink('command', ['saccade_velocity_threshold = ' num2str(const.saccvelthresh)]);
Eyelink('command', ['saccade_acceleration_threshold = ' num2str(const.saccaccthresh)]);
Eyelink('command', ['calibration_type= ' '' const.caltype]);
Eyelink('command', 'file_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,MESSAGE,BUTTON');
Eyelink('command', 'file_sample_data  = LEFT,RIGHT,GAZE,AREA,GAZERES,STATUS,HTARGET');
Eyelink('command', 'link_event_filter = LEFT,RIGHT,FIXATION,SACCADE,BLINK,BUTTON');
Eyelink('command', 'link_sample_data  = LEFT,RIGHT,GAZE,GAZERES,AREA,STATUS,HTARGET');
Eyelink('command', 'sample_rate= 1000');

%% Open sounds:
if const.hasAudio
    InitializePsychSound;

    [y, freq] = wavread([cd '\sounds\' 'standard.wav']);
    wavedata = y';
    wavedata(2,:)= wavedata;
    nrchannels = size(wavedata,1); % Number of rows == number of channels.

    Audio.pamaster = PsychPortAudio('Open', [], 1+8, 1, freq, nrchannels);
    PsychPortAudio('Volume', Audio.pamaster, 1)
    PsychPortAudio('Start', Audio.pamaster, 0, 0, 1);

    % create slaves:
    Audio.standard1 = PsychPortAudio('OpenSlave', Audio.pamaster, 1, nrchannels);
    Audio.standard2 = PsychPortAudio('OpenSlave', Audio.pamaster, 1, nrchannels);
    Audio.standard3 = PsychPortAudio('OpenSlave', Audio.pamaster, 1, nrchannels);
    Audio.standard4 = PsychPortAudio('OpenSlave', Audio.pamaster, 1, nrchannels);
    Audio.standard5 = PsychPortAudio('OpenSlave', Audio.pamaster, 1, nrchannels);
    Audio.deviant = PsychPortAudio('OpenSlave', Audio.pamaster, 1, nrchannels);
    %Audio.silence = PsychPortAudio('OpenSlave', Audio.pamaster, 1, nrchannels);

    % Fill standard buffers:
    PsychPortAudio('FillBuffer', Audio.standard1, wavedata);
    PsychPortAudio('FillBuffer', Audio.standard2, wavedata);
    PsychPortAudio('FillBuffer', Audio.standard3, wavedata);
    PsychPortAudio('FillBuffer', Audio.standard4, wavedata);
    PsychPortAudio('FillBuffer', Audio.standard5, wavedata);
else
    Audio= 0;
end
% Fill deviant buffer:
%[y, freq] = wavread([cd '\corpus\' 'deviant.wav']);
%wavedata = y';
%nrchannels = size(wavedata,1); % Number of rows == number of channels.
%PsychPortAudio('FillBuffer', Audio.deviant, wavedata);


% % Sound 1:
% [y, freq] = wavread([cd '\corpus\' 'standard.wav']);
% wavedata = y';
% nrchannels = size(wavedata,1); % Number of rows == number of channels.
% InitializePsychSound;
% Audio.standard = PsychPortAudio('Open', [], [], 0, freq, nrchannels);
% PsychPortAudio('FillBuffer', Audio.standard, wavedata);
% %t1 = PsychPortAudio('Start', Audio.standard, const.repetitons, 0, 1);
% 
% % Sound 2:
% [y, freq] = wavread([cd '\corpus\' 'deviant.wav']);
% wavedata = y';
% nrchannels = size(wavedata,1); % Number of rows == number of channels.
% InitializePsychSound;
% Audio.deviant = PsychPortAudio('Open', [], [], 0, freq, nrchannels);
% PsychPortAudio('FillBuffer', Audio.deviant, wavedata);
% %t1 = PsychPortAudio('Start', Audio.deviant, const.repetitons, 0, 1);
 