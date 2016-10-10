%% check item grid!
%% make a plot with a cross at the centre of each potential face postion
clear all
close all
[destrect display] = SetUpStimGrid();
nL = length(destrect.L);
for i = 1:length(destrect.L)
    x(i) = destrect.L(i).imrect(1) + (destrect.L(i).imrect(3)- destrect.L(i).imrect(1))/2;
    y(i) = destrect.L(i).imrect(2) + (destrect.L(i).imrect(4)- destrect.L(i).imrect(2))/2;    
end
for i = 1:length(destrect.R)
    x(i+nL) = destrect.R(i).imrect(1) + (destrect.R(i).imrect(3)- destrect.R(i).imrect(1))/2;
    y(i+nL) = destrect.R(i).imrect(2) + (destrect.R(i).imrect(4)- destrect.R(i).imrect(2))/2;    
end

subplot(1,2,1);
plot(x,y, 'x');

 xlim([1,display.width]); ylim([1,display.height]) 
    