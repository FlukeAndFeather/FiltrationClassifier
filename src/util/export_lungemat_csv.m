% Starting in the FiltrationClassifier data folder...
dir_data = dir("~/Documents/GitHub/FiltrationClassifier/data");
% Iterate through each subfolder...
for i = 1:size(dir_data, 1)
	subfolder = dir_data(i);
  % Ensure it's a directory... (as opposed to e.g. "." or ".DS_Store"
  if ~subfolder.isdir
    continue
  end
  subfolder_path = fullfile(subfolder.folder, subfolder.name);
  dir_subfolder = dir(subfolder_path);
  % Iterate through each file in the subfolder...
  for j = 1:size(dir_subfolder, 1)
    file = dir_subfolder(j);
    file_path = fullfile(file.folder, file.name);
    % Ensure it's a lunge table...
    islungetable = ~isempty(regexp(file_path, ".*LungeTable.mat$", 'once'));
    % And write to CSV!
    if islungetable
      lungetable = load(file_path);
      csv_path = strrep(file_path, ".mat", ".csv");
      writetable(lungetable.LungeTable, csv_path);
    end
  end
end