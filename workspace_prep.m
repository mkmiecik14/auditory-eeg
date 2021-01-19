% Workspace Preparation
% This script is meant to be run at the beginning of each script in this
% project to prepare MATLAB with paths and other code that is redundant in
% each script
%
% Matt Kmiecik
%
% Edited 15 JAN 2021

% Sets working directory ----
wd = 'M:\crampp-auditory-eeg';
cd(wd); % changes directory to working dir
disp('Preparing CRAMPP Auditory EEG Project Workspace...');

% Starts EEGLAB ----
%[ALLEEG EEG CURRENTSET ALLCOM] = eeglab;

% Loads in participant information ----
%[NUM,TXT,RAW] = xlsread('C:\Users\pains\Desktop\matt-eeglab\data\0-global-vars\vis-subj-info.xlsx');
