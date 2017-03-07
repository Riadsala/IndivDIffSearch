 function summaryData
%%%%%%%%%%%%%%%%%%%%%%%%%

    % Get and combine data
     lsdata = dataset('File','lineseg/scratch/lineseg_output.csv','Delimiter',',','ReadVarNames',true);
     acdata = dataset('File','adaptchoice/Data_AdaptChoice_allsubs_Summary.txt','Delimiter','\t','ReadVarNames',true,'format','%s%f%f%f%f%f%f');
     fgdata = dataset('File','foraging/Data_MultiTargForag_allsubs.txt','Delimiter','\t','ReadVarNames',true,'format','%s%f%f%f%f%f%f%f%f%f%f');
     
     lsdata.Properties.VarNames = {'observer','ls_propHetro_absent','ls_medianRT_hard','ls_meanlogrt_hard'};
     lsdata.observer = strrep(lsdata.observer, '"', '');
     acdata.Properties.VarNames = {'observer','ac_Acc','ac_meanRT','ac_propopt','ac_swrate','ac_logRT','ac_medianRT'};
     fgdata.Properties.VarNames = {'observer','fg_Feature_meanRT','fg_Feature_medianRT','fg_Feature_logRT','fg_Feature_RunNum','fg_Feature_RunLength','fg_Conj_meanRT','fg_Conj_medianRT','fg_Conj_logRT','fg_Conj_RunNum','fg_Conj_RunLength'};
     alldata = join(lsdata,acdata,'Type','inner','MergeKeys',true);
     alldata = join(alldata,fgdata,'Type','inner','MergeKeys',true);
     
     % Correlations: logRT  
     correlations = corrcoef([alldata.ls_meanlogrt_hard,alldata.ac_logRT,alldata.fg_Feature_logRT,alldata.fg_Conj_logRT]);
     corrplot = mat2dataset(correlations,'VarNames',{'ls_meanlogrt_hard','ac_logRT','fg_Feature_logRT','fg_Conj_logRT'},'ObsNames',{'ls_meanlogrt_hard','ac_logRT','fg_Feature_logRT','fg_Conj_logRT'})

end