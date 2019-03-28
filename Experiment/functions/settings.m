global Visual const;

%% Visual settings
Visual.resX= 1920; % Screen resolution, X-axis; P111
Visual.resY= 1080; % Screen resolution, Y-axis; P111
Visual.frameRate= 120; % frame rate of monitor (saved to edf file)
Visual.offsetX= 200; % X offset of the text
Visual.offsetY= Visual.resY/2; % Y offset of the text
Visual.sentPos= [Visual.offsetX Visual.resY/2]; % sentence position
Visual.FGC= [0 0 0]; % stimuli colour
Visual.BGC= [255 255 255]; % background colour
Visual.Pix_per_Letter= 14; % letter width in pixels
Visual.FontSize= 16; % 18pt: 14ppl; 16pt: 13ppl; 14pt: 11ppl
Visual.TextSpacing= 2; % text spacing between lines (multi-line only)
Visual.LetterHeight= 22; % letter height in pixels  (multi-line only)

Visual.Font= 'Consolas'; %'Courier New';
Visual.TextSize= 18; % possibly unused?
Visual.InstrTextSize= 24; % unused
Visual.GazeBoxSize= 50; % gaze box size in pixels
Visual.GazeBoxColor= [0 0 0]; % colour of the gaze-box
Visual.gazeBoxDur= 100; % how many ms the eye needs to stay on the gaze box before triggering it
Visual.gazeBoxDisplayTime= 7; % how many seconds to wait to trigger the gaze box

%% Experiment settings:
const.hasAudio= false; % is there audio in the experiment (used for set-up)
const.TrialTimeout= 60; % automatically terminates trial after x seconds
const.ncond= 4; % number of conditions, currently not in use?
const.Maxtrials= 100; % number of experimental trials 
const.soundDur= 0.06; % min duration between playing 2 sounds (in seconds)
const.repetitons=1; % how many times to play sounds
const.seeEye= false; % if true, shows gaze position as a dot on the screen (for testing only!!)
const.maxCross= 1800; % what is the max location for crossing a gaze-contingent boundary? helps against blinks (sometimes)

const.checkPPL= false;  % if true, draws a rectangle around sentence to make sure letter width is correct
const.expName = 'RSFNT'; % used for saving edf data (keep at <= 5 letters)
const.caltype= 'HV9'; % calibration type; use 'HV9' for 9-point grid and 'H3' for three-point calibration
const.saccvelthresh = 35; % degrees per second, saccade velocity threshold; don't change
const.saccaccthresh = 9500; % degrees per second, saccade acceleration threshold; don't change

% KbName('UnifyKeyNames');
% const.escapeKey= KbName('ESCAPE');
% const.confirmKey= KbName('Y');

% 16 vs 22