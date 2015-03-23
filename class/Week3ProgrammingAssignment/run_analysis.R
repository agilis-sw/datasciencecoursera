run_analysis = function() {
    # get feature labels and activity labels as vectors for appending to feature table before averaging
    measurement_labels=read.table("./data/features.txt")
    activity_labels=read.table("./data/activity_labels.txt")
    names(activity_labels)[1] = "activity_id"
    names(activity_labels)[2] = "activity"
    
    # get test and train feature data augmented with subject and activity, and create a merged data set
    findexes=grep("mean\\(\\)|std\\(\\)",features$V2,value=FALSE)
    flabels=grep("mean\\(\\)|std\\(\\)",features$V2,value=TRUE)
    testdata = getdata("test",findexes,flabels,activity_labels)
    traindata = getdata("train",findexes,flabels,activity_labels)
    alldata = rbind(testdata,traindata)
    
    #summarize, grouping by subject and activity
    alldatamelt = melt(alldata,id=c("activity","subject"),measure.vars=c(3:68))
    tidydata=dcast(alldatamelt,activity+subject ~ variable, mean)
    
    #save the tidy data in a file
    if (!file.exists("./output")){dir.create("./output")}
    write.table(tidydata,"./output/run_tidydata.txt",col.names=TRUE,row.names=FALSE)
}

getdata = function(dataType,findexes,flabels,activity_labels) {
    prefix=paste("./data/",dataType,"/",sep="")
    suffix=paste("_",dataType,".txt",sep="")
    fdata = read.table(paste(prefix,"X",suffix,sep=""))
    names(fdata)[findexes]=fnames
    fdatas = select(fdata, findexes)
    sdata = read.table(paste(prefix,"subject",suffix,sep=""))
    names(sdata)[1]="subject"
    adata = read.table(paste(prefix,"y",suffix,sep=""))
    names(adata)[1]="activity_id"
    adatas = select(merge(adata,activity_labels),activity)
    return(cbind(adatas,sdata,fdatas))
}
