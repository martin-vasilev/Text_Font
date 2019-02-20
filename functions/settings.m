global Visual const;

%% Visual settings
Visual.resX= 1920;
Visual.resY= 1080;
Visual.frameRate= 100;
Visual.offsetX= 150;
Visual.sentPos= [Visual.offsetX Visual.resY/2];
Visual.FGC= [0 0 0]; % stimuli colour
Visual.BGC= [255 255 255]; % background colour
Visual.Pix_per_Letter= 14;
Visual.FontSize= 18; % ppl: 14; 16: 13ppl; 14: 11ppl

Visual.Font= 'Courier New';
Visual.TextSize= 18; %
Visual.InstrTextSize= 32;
Visual.GazeBoxSize= 50; % in pixels
Visual.GazeBoxColor= [0 0 0];
Visual.gazeBoxDur= 100; % how many ms the eye needs to stay on the gaze box before triggering it
Visual.gazeBoxDisplayTime= 7; % how many seconds to wait to trigger the gaze box

%% Experiment settings:
const.hasAudio= false; % is there audio in the experiment (used for set-up)
const.TrialTimeout= 60; % automatically terminates trial after x seconds
const.ncond= 3; % number of conditions, currently not in use
const.Maxtrials= 24; % number of experimental trials 
const.soundDur= 0.06; % min duration between playing 2 sounds (in seconds)
const.repetitons=1; % how many times to play sounds
const.seeEye= false; % if true, shows gaze position as a dot on the screen (for testing only!!)
const.maxCross= 1800; % what is the max location for crossing a gaze-contingent boundary? helps against blinks (sometimes)

const.checkPPL= false;  % if true, draws a rectangle around sentence to make sure letter width is correct
const.expName = 'FONT'; % used for saving data (keep at <= 5 letters)
const.caltype= 'HV9'; % calibration; use 'HV9' for 9-point grid and 'H3' for three-point calibration
const.saccvelthresh = 35;% degrees per second, saccade velocity threshold; don't change
const.saccaccthresh = 9500; % degrees per second, saccade acceleration threshold;don't change
