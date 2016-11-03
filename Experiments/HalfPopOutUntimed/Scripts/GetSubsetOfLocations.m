%%function to select a subset of possible search item locations

function [loc] = GetSubsetOfLocations(destrect, nLoc)

if nargin < 2
    nLoc = 8;
end

if length(nLoc) == 1
    locSplit = [floor(nLoc/2), floor(nLoc/2)];
else
    locSplit = nLoc;
end


randR = randperm(length(destrect.R));
randL = randperm(length(destrect.L));
destrect.R = destrect.R(randR);
destrect.L = destrect.L(randL);

loc.L = destrect.L(1:locSplit(1));
loc.R = destrect.R(1:locSplit(2));


end
