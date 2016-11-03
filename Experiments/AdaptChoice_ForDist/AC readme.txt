
Adaptive choice visual search Instructions
- august 2016 JI

Experiment file: AdaptChoice.m

Search display size: The size of the search display and target/distractor squares are defined in pixels and may need to be adjusted. 
The parameters (innerradius & squaresize) are at the top of the script. The specific size doesn't matter too much, but for eye-tracking 
around 5 degree radius for the inner ring of the search display is probably good (we've used 240 pix for a 1280x1040 display and that 
seemed to work ok).

Participant Code: Text or integers are fine, but use max 6 characters (eye tracker EDF file does not save if the filename is too long). 
Use 99 for debugging.

Eyetracking: Set to 1 in the dialog box. To include a recalibration option (i.e. checks whether Ps fixate centre between trials and 
recalibrate if they they miss too many fixation checks), set the ‘recalibrate’ option at the top of the script to 1. To use auto calibrate 
(i.e. calibrates automatically without having to go through tracker setup) set ‘autocalibrate to 1’ and make sure EyelinkDoCalibration.m is 
in the experiment folder.

Data: Data are saved in the Data and Tracking Files folders. Eye tracking data is transferred automatically to the stimulus computer at the 
end of the experiment and will be in the format [participant code]AC.edf 


Instructions for completing the task:

The display contains 3 concentric rings of colored squares, each containing a white digit. There are two targets presented on each trial:

	- Red square with the number 2,3,4 or 5
	- Blue square with the number 2,3,4 or 5

All other red and blue squares contain digits 6,7,8 or 9. Green squares can contain a number 2-5 but these are distractors. Both targets are 
present on every trial, but participants only need to find one on each trial. It is completely up to them which target they would like to 
search for on each trial. Participants respond by pressing a key (V,B,N,M) on the keyboard that corresponds to the number inside the target 
that they found. They don’t need to indicate the color in anyway. Participants should also be instructed to start each trial by fixating the
fixation cross in the centre of the screen (especially if the recalibration option is used).

Accuracy and RT feedback is given at the end of each block.


