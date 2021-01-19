% EEG File copy script
% This script facilitates the copying of a large amount of eeg files from
% one place to another
% Matt Kmiecik
% Edited 15 JAN 2021

workspace_prep % Prepares workspace (see src/...)

% Initializations ----
from = 'Q:\gyrl-data\CRAMPP\Assessment 1'; % dir where files are stored
to = 'M:\crampp-auditory-eeg\data\eeg'; % dir where files are going

% Gather names of all sub dirs ----
files = dir(from);

% Go into each of these sub directories and pull out the auditory data ----
for i = 1:size(files, 1)
    % gathers audio eeg file names
    this_files = dir(fullfile(from, files(i).name, 'audio*')); 
    % checks to see if all 3 audio eeg files are there
    if length(this_files) < 3
        disp(strcat('Not all files are there for this subject/dir'));
        disp('Skipping import...');
    else
        for j = 1:length(this_files)
            % Checks to see if these files are in the raw data path
            if isfile(fullfile(to, this_files(j).name))
                disp('file already exists...skipping import...');
            else
                disp('copying file...')
                copyfile(fullfile(this_files(j).folder, this_files(j).name), to);
            end
        end
    end
end

