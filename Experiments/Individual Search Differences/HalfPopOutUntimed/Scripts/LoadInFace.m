function out = LoadInFace(fname)

xCut = 35;
yCut = 25;


[pic, ~, alpha] = imread(fname);
pic(:,:,4) = alpha;

x1 = xCut+1;
x2 = size(pic,1)-xCut;

y1 = yCut+1;
y2 = size(pic,2)-yCut;

out = pic(x1:x2, y1:y2, :);