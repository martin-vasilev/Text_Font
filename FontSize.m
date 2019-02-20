% Reading experiment with font size manipulation

% Martin R. Vasilev, 2019

global const; 

%% settings:
clear all;
clear mex;
clear functions;

cd('C:\Users\EyeTracker\Desktop\Calvin\Font_size');
addpath([cd '\functions'], [cd '\corpus'], [cd '\design'], [cd '\img']);

settings; % load settings
ExpSetup; % do window and tracker setup

%% Load stimuli and design:
importDesign;
load('sent.mat');
const.ntrials= height(design);

%% Run Experiment:
runTrials;

%% Save file & Exit:
status= Eyelink('ReceiveFile');
Eyelink('Shutdown');

Screen('CloseAll');