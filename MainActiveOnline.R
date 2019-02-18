library(RMySQL)
library(RODBC)
library("upclass")
library("data.tree")
library(hashr)
library(stringdist)

con <- dbConnect(RMySQL::MySQL(),dbname="db",user = "db_user",password = "SA9eK4VUpXhg",host = "193.206.170.140")

#conn=odbcConnect("pdsdata")
#list<-sqlTables(conn)
#q=sqlQuery(conn, "select* from ", list, sep="")
#t=sqlQuery(conn,q)

tables<-dbListTables(con)
num_table=length(tables)-1
l=0
v=0
d=0

if(num_table==0)
{
  print("There is no uploaded table ") 
} else {
  
  
  
  for (g in 1 : (length(tables)-1)){
    walton<-(dbReadTable(con, tables[g]))
    write.table(walton, file = "C:/Users/singh/Desktop/Analysis/user10.csv", 
                row.names = F, append=T, sep="," , col.names = F, qmethod = "double")
    
    
    #setwd("C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files")
    #lists<-list.files(path = "C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files", pattern = NULL, all.files = FALSE,
    #                 full.names = FALSE, recursive = FALSE,
    #                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
    
    # for (y in 1:length(lists))
    # {
    #   XX = read.csv(lists[y], header = F)
    
    
    XX = read.csv("C:/Users/singh/Desktop/Analysis/user10.csv", header = F)
    file.remove("C:/Users/singh/Desktop/Analysis/user10.csv")
    X.numeric <- data.frame(data.matrix(XX))
    X.numeric=replace(X.numeric, is.na(X.numeric), 0)
    
    l11=sum(X.numeric$V7 != 0)          #number of label by users
    v1=sum(X.numeric$V6 != 0)          #number of verified by users
    d1=sum(X.numeric$V5 != 0)          #number of decision by system
    active_check=sum(X.numeric$V4 != 0 & X.numeric$V7==0) 
    
    #length1=nrow(XX)-290
    
    
    
    
    ##############################################################################################################################
    #Begin: Ensembel Learning Approach
    ##############################################################################################################################
    
    #1-Begin:Purpose-centric learning
    op=c(8,11) # offer value and purpose
    sp=c(9,11) # service type and purpose  
    cp=c(10,11) # consumers and purpose
    dp=c(13:18,11) # data and purpose
    od=c(8,13:18) # offer value and data
    sd=c(9,13:18) # service type and data
    cd=c(10,13:18) # consumers and data
    os=c(8,9) # offer value and service
    cs=c(10,9) # consumers and service
    co=c(10,8) #consumers and offer 
    
    #i-Begin: offer value and purpose
    
    
    #Tlength=colSums ((cl != 0), na.rm = TRUE)        #Set the initial training dataset
    
    
    
    
    
    
    #Begin: Active learning#######################################################################################################################################
    
    
    #Begin: Service-type ontology
    service_tree=Node$new("Root")
    E_commerce=service_tree$AddChild("E-commerce")
    E_Reservation=E_commerce$AddChild("E-Reservation")
    Filght_reservation=E_Reservation$AddChild("Flight reservation")
    Train_ticket_reservation=E_Reservation$AddChild("Train ticket reservation")
    Bus_ticket_reservation=E_Reservation$AddChild("Bus ticket reservation")
    Hotel_reservation=E_Reservation$AddChild("Hotel reservation")
    Online_shopping=E_commerce$AddChild("Online shopping")
    Banking=service_tree$AddChild("Banking")
    Loan=Banking$AddChild("Loan")
    Money_deposit=Banking$AddChild("Money deposit")
    Money_transfer=Banking$AddChild("Money transfer")
    Healthcare=service_tree$AddChild("Healthcare")
    Heart_treatment=Healthcare$AddChild("Heart treatment")
    Cancer_treatment=Healthcare$AddChild("Cancer treatment")
    Blood_pressure=Healthcare$AddChild("Blood pressure treatment")
    Navigation=service_tree$AddChild("Navigation")
    Restaurant=service_tree$AddChild("Restaurant")
    E_Government=service_tree$AddChild("eGovernment")
    
    # Healthcare=Banking$AddChild("Healthcare")
    # eGovernment=service_tree$AddChild("eGovernment")
    # OnlineShopping=service_tree$AddChild("Online Shopping")
    
    distt=NULL
    dis=list()
    simi_consumers=list()
    #End: Service-type ontology
    
    
    
    
    label.active <- function(X_Y) {
      
      iris2=X.numeric[,X_Y]
      #iris2=XX[,3:7]
      
      X=as.matrix(iris2)
      cl=as.matrix(XX$V7)
      bb=which(X.numeric$V4==1 & X.numeric$V7!=0)
      
      indtrain=c(1:Tlength,bb)
      
      #indtrain=c(1:Tlength)
      Xtrain=X[indtrain,]
      cltrain=cl[indtrain]
      indtest=setdiff(1:nrow(X.numeric), indtrain)
      Xtest=X[indtest,]
      #cltest=cl[indtest]
      
      op1=upclassifymodel(Xtrain, cltrain, Xtest, cltest = NULL,
                          modelName = "EEE", tol = 10^-5, iterlim = 1000, 
                          Aitken = TRUE)
      return(op1)
    }
    
    
    #Begin:function for insert new row for avoiding NA value for each classifier
    insertRow <- function(existingDF, newrow, r) {
      existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
      existingDF[r,] <- newrow
      existingDF
    }
    
    #End:function for insert new row for avoiding NA value for each classifier
    
    
    
    
    
    Tlength=9                     #Set the initial training dataset for active learning
    bb=data.frame()
    aaa=data.frame()
    
    
    if((l11>=9 && l11<40) && (active_check==0) && (v1==0) && (d1==0))          ############################# REplace 15 to 40 and 21 to 71#########################################################################
    {                                                                          ######################################
      
      
      
      
      n=label.active(op)
      o=label.active(sp)
      p=label.active(cp)
      q=label.active(dp)
      r=label.active(od)
      s=label.active(sd)
      t=label.active(cd)
      u=label.active(os)
      v=label.active(cs)
      w=label.active(co)
      
      
      
      
      
      #Begin: If any value is NA then
      cc=data.frame()
      
      # for(i in Tlength:40)
      # {
      if(is.na(n$ll)| is.na(o$ll)| is.na(p$ll)| is.na(q$ll) | is.na(r$ll) | is.na(s$ll) | is.na(t$ll) |is.na(u$ll) | is.na(v$ll) | is.na(w$ll))
      {
        
        cc=which(XX$V4==1 & XX$V7!=0)
        Tab1=c(XX$V7[1:Tlength],XX$V7[cc])
        Tab=table(Tab1)
        Tab=Tab[Tab=="1"]
        ta=as.numeric(names(Tab))
        Tabb=which(XX$V7 %in% ta)
        Tabbb=which(Tabb<=9 | Tabb>=10) #############################Should be replaced 16 to 41#############Replace##########################################################
        #browser()                                                                                          ####################################  
        iii=0
        l_Tabbb=length(Tabbb)
        ii=1
        while(l_Tabbb>0)
        {
          
          
          XXX=insertRow(X.numeric, X.numeric[Tabb[Tabbb[ii]]+iii,], Tabb[Tabbb[ii]]+1)#Add some rows to numerical files
          XXX1=insertRow(XX, XX[Tabb[Tabbb[ii]]+iii,], Tabb[Tabbb[ii]]+1)             #Add some rows to original file
          X.numeric=XXX
          XX=XXX1
          iii=iii+1
          if(Tabb[Tabbb[ii]]<=11)
            Tlength=Tlength+1
          l_Tabbb=l_Tabbb-1
          ii=ii+1
          
          
          
        }
        
        
        
        n=label.active(op)
        o=label.active(sp)
        p=label.active(cp)
        q=label.active(dp)
        r=label.active(od)
        s=label.active(sd)
        t=label.active(cd)
        u=label.active(os)
        v=label.active(cs)
        w=label.active(co)
        
        
        
        w_train_p=n$train$z+o$train$z+p$train$z+q$train$z+r$train$z+s$train$z+t$train$z+u$train$z+v$train$z+w$train$z
        w_test_p= n$test$z+o$test$z+p$test$z+q$test$z+r$test$z+s$test$z+t$test$z+u$test$z+v$test$z+w$test$z
        
        
        if(length(w_test_p)==0)
          next()
        
        service_label=which(XX$V4!=0 & XX$V7!=0)
        services=unique(c(as.character(XX$V9[1:Tlength]), as.character(XX$V9[service_label])))
        
        
        
        for(i in 1:nrow(w_test_p))
        {
          dis=list()
          simi_consumers=list()
          
          pos=Tlength+i                        #poition of upcoming access requests 
          current_service=as.character(XX$V9[pos])
          for(service_i in 1:length(services))
          {
            dis[[service_i]]=Distance(FindNode(service_tree,current_service), FindNode(service_tree, services[service_i]))
            
          }
          
          dis_min=dis[[which.min(dis)]]                  # Minimun distance between services
          closest_service=services[which.min(dis)]      
          all_services=(c(as.character(XX$V9[1:Tlength]), as.character(XX$V9[service_label])))
          all_consumers=(c(as.character(XX$V10[1:Tlength]), as.character(XX$V10[service_label])))
          same_service=(which(all_services == closest_service, arr.ind = TRUE))
          #same_service=c(same_service)
          consumers=(all_consumers[same_service])
          current_consumer=as.character(XX$V10[pos])
          current_consumer=strsplit(current_consumer, ",")
          
          for(consumers_i in 1: length(consumers))
          {
            consumers_select=consumers[consumers_i]
            consumers_select=as.character(consumers_select)
            consumers_select=strsplit(consumers_select, ",")
            simi_consumers[[consumers_i]]=length(intersect(consumers_select[[1]],current_consumer[[1]]))/length(union(consumers_select[[1]], current_consumer[[1]]))
            
          }
          
          simi_max=simi_consumers[[which.max(simi_consumers)]]   #Maximum similarity among data consumers
          dis_his=(1/(dis_min+1))*simi_max                       #this parameter need to mutiply in active learning
          
          
          a=which.max(w_test_p[i,])
          b=c(1,2,3)
          c=setdiff(b,a)
          if(dim(w_test_p)[2]==2)
          {
            if((dis_his*(abs(w_test_p[i,a]-w_test_p[i,c[1]]))<0.07) & ((XX$V7[Tlength+i])==0 | is.na(XX$V7[Tlength+i])==TRUE))  ####################Replace the marginal value##############
            {                                                                                                                       #############################################
              
              aaa=rbind(aaa,i)
              bbb=Tlength+aaa
              bbb=as.numeric(unlist(bbb))      #position of access requests for asking to users
              
              XX$V4[bbb]=1
              
              #write.table(XX, file = "C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files/user106.csv",
              #                        row.names = F, append=F, sep="," , col.names = F, qmethod = "double")
              
              break
            }
            
          }
          else{
            
            if((dis_his*(abs(w_test_p[i,a]-w_test_p[i,c[1]]))<0.07 | dis_his*(abs(w_test_p[i,a]-w_test_p[i,c[2]]))<0.07) & ((XX$V7[Tlength+i])==0 | is.na(XX$V7[Tlength+i])==TRUE)) ################### REplace the marginal value############
            {                                                                                                                                                                                 ######################################
              
              aaa=rbind(aaa,i)
              bbb=Tlength+aaa
              bbb=as.numeric(unlist(bbb))                        #positions of access requests for asking users
              
              XX$V4[bbb]=1
              #write.table(XX, file = "C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files/user106.csv",
              #           row.names = F, append=F, sep="," , col.names = F, qmethod = "double")
              
              
              break
            }
            
            
          }
          
        }
        
        
      }else
      {
        
        
        w_train_p=n$train$z+o$train$z+p$train$z+q$train$z+r$train$z+s$train$z+t$train$z+u$train$z+v$train$z+w$train$z
        w_test_p= n$test$z+o$test$z+p$test$z+q$test$z+r$test$z+s$test$z+t$test$z+u$test$z+v$test$z+w$test$z
        
        if(length(w_test_p)==0)
          next()
        
        service_label=which(XX$V4!=0 & XX$V7!=0)
        services=unique(c(as.character(XX$V9[1:Tlength]), as.character(XX$V9[service_label])))
        #constumer=unique(cbind(as.character(XX$V9[1:Tlength]), as.character(XX$V9[service_label])))
        
        for(i in 1:nrow(w_test_p))
        {
          
          dis=list()
          simi_consumers=list()
          
          pos=Tlength+i                        #poition of upcoming access requests 
          current_service=as.character(XX$V9[pos])
          for(service_i in 1:length(services))
          {
            dis[[service_i]]=Distance(FindNode(service_tree,current_service), FindNode(service_tree, services[service_i]))
            
          }
          
          dis_min=dis[[which.min(dis)]]                  # Minimun distance between services
          closest_service=services[which.min(dis)]      
          all_services=(c(as.character(XX$V9[1:Tlength]), as.character(XX$V9[service_label])))
          all_consumers=(c(as.character(XX$V10[1:Tlength]), as.character(XX$V10[service_label])))
          same_service=(which(all_services == closest_service, arr.ind = TRUE))
          #same_service=c(same_service)
          consumers=(all_consumers[same_service])
          current_consumer=as.character(XX$V10[pos])
          current_consumer=strsplit(current_consumer, ",")
          
          for(consumers_i in 1: length(consumers))
          {
            consumers_select=consumers[consumers_i]
            consumers_select=as.character(consumers_select)
            consumers_select=strsplit(consumers_select, ",")
            simi_consumers[[consumers_i]]=length(intersect(consumers_select[[1]],current_consumer[[1]]))/length(union(consumers_select[[1]], current_consumer[[1]]))
            
          }
          
          simi_max=simi_consumers[[which.max(simi_consumers)]]   #Maximum similarity among data consumers
          dis_his=(1/(dis_min+1))*simi_max                       #this parameter need to mutiply in active learning
          
          a=which.max(w_test_p[i,])
          b=c(1,2,3)
          c=setdiff(b,a)
          if(dim(w_test_p)[2]==1)
          {
            if((XX$V7[Tlength+i])==0 | is.na(XX$V7[Tlength+i])==TRUE)
            {
              aaa=rbind(aaa,i)                #positions of access requests for asking users in testing dataset
              bbb=Tlength+aaa
              bbb=as.numeric(unlist(bbb))
              
              XX$V4[bbb]=1
              #write.table(XX, file = "C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files/user106.csv",
              #           row.names = F, append=F, sep="," , col.names = F, qmethod = "double")
              
              break
            }
          }
          else if(dim(w_test_p)[2]==2)
          {
            if((dis_his*(abs(w_test_p[i,a]-w_test_p[i,c[1]]))<0.07) & ((XX$V7[Tlength+i])==0 | is.na(XX$V7[Tlength+i])==TRUE))   ############# Need to change marginal value##################
            {                                                                                                                                 #############################
              
              aaa=rbind(aaa,i)                #positions of access requests for asking users in testing dataset
              bbb=Tlength+aaa
              bbb=as.numeric(unlist(bbb))
              
              XX$V4[bbb]=1
              #write.table(XX, file = "C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files/user106.csv",
              #           row.names = F, append=F, sep="," , col.names = F, qmethod = "double")
              
              break
              
            }
            
          }
          else {
            
            if(((dis_his*(abs(w_test_p[i,a]-w_test_p[i,c[1]]))<0.07) | (dis_his*(abs(w_test_p[i,a]-w_test_p[i,c[2]]))<0.07)) & ((XX$V7[Tlength+i])==0 | is.na(XX$V7[Tlength+i])==TRUE))  ############# Need to change marginal value##################
            {                                                                                                                                                                                   ##########################################
              
              aaa=rbind(aaa,i)            #positions of access requests for asking users in testing dataset
              bbb=Tlength+aaa
              bbb=as.numeric(unlist(bbb))
              
              
              
              XX$V4[bbb]=1
              
              # write.table(XX, file = "C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files/user106.csv",
              #            row.names = F, append=F, sep="," , col.names = F, qmethod = "double")
              
              break
            }
            
          }
          
        }
        
      }
      
      XX <- unique(XX)
      X.numeric<- unique(X.numeric)
      
      colnames(XX) <- c("ID", "Npurpose", "Benefit", "Risk", "Decision", "Verified", "label", "offer", "servicetype",
                        "consumers", "purpose", "data1", "data2", "data3", "data4", "data5", "data6", "data7", "data8",
                        "data9", "date_time_upload", "date_time_label", "date_time_feedback", "active_data" )
      
      dbWriteTable(con, tables[g], XX, row.names = F, overwrite=T)
      #write.table(XX, file = "C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files/user8.csv",
      #           row.names = F, append=F, sep="," , col.names = F, qmethod = "double")
      
      XX=0
      
    }
    
    
    if(l11==40 && v1==0 && d1==0)          #for Testing I consider 21 acess request in total (ensemble+active). ################ REPlace################21 to 71##########
    {                                                                                                                         ####################################
      
      
      
      Tlength=9
      A_n=label.active(op)
      A_o=label.active(sp)
      A_p=label.active(cp)
      A_q=label.active(dp)
      A_r=label.active(od)
      A_s=label.active(sd)
      A_t=label.active(cd)
      A_u=label.active(os)
      A_v=label.active(cs)
      A_w=label.active(co)
      
      
      
      #Begin: If any value is NA then 
      cc=data.frame()
      # for(i in Tlength:40)
      # {
      if(is.na(A_n$ll)| is.na(A_o$ll)| is.na(A_p$ll)| is.na(A_q$ll) | is.na(A_r$ll) | is.na(A_s$ll) | is.na(A_t$ll) |is.na(A_u$ll) | is.na(A_v$ll) | is.na(A_w$ll))
      {
        
        cc=which(XX$V4==1 & XX$V7!=0)
        Tab1=c(XX$V7[1:Tlength],XX$V7[cc])
        Tab=table(Tab1)
        Tab=Tab[Tab=="1"]
        ta=as.numeric(names(Tab))
        Tabb=which(XX$V7 %in% ta)
        Tabbb=which(Tabb<=9 | Tabb>=10) #############################Should be replaced 16 to 41#############Replace##########################################################
        #browser()                                                                                          ####################################  
        iii=0
        l_Tabbb=length(Tabbb)
        ii=1
        while(l_Tabbb>0)
        {
          
          
          XXX=insertRow(X.numeric, X.numeric[Tabb[Tabbb[ii]]+iii,], Tabb[Tabbb[ii]]+1)#Add some rows to numerical files
          XXX1=insertRow(XX, XX[Tabb[Tabbb[ii]]+iii,], Tabb[Tabbb[ii]]+1)             #Add some rows to original file
          X.numeric=XXX
          XX=XXX1
          iii=iii+1
          if(Tabb[Tabbb[ii]]<=9)
            Tlength=Tlength+1
          l_Tabbb=l_Tabbb-1
          ii=ii+1
          
          
          
        }
        
        
        
        
        A_n=label.active(op)
        A_o=label.active(sp)
        A_p=label.active(cp)
        A_q=label.active(dp)
        A_r=label.active(od)
        A_s=label.active(sd)
        A_t=label.active(cd)
        A_u=label.active(os)
        A_v=label.active(cs)
        A_w=label.active(co)
        
        
        
        A_w_train_p=A_n$train$z+A_o$train$z+A_p$train$z+A_q$train$z+A_r$train$z+A_s$train$z+A_t$train$z+A_u$train$z+A_v$train$z+A_w$train$z
        A_w_test_p= A_n$test$z+A_o$test$z+A_p$test$z+A_q$test$z+A_r$test$z+A_s$test$z+A_t$test$z+A_u$test$z+A_v$test$z+A_w$test$z
        
        
      }else
      {
        A_w_train_p=A_n$train$z+A_o$train$z+A_p$train$z+A_q$train$z+A_r$train$z+A_s$train$z+A_t$train$z+A_u$train$z+A_v$train$z+A_w$train$z
        A_w_test_p= A_n$test$z+A_o$test$z+A_p$test$z+A_q$test$z+A_r$test$z+A_s$test$z+A_t$test$z+A_u$test$z+A_v$test$z+A_w$test$z
      }
      
      
      if(length(A_w_train_p)==0)
        next()
      
      dupl=which(duplicated(XX))
      
      #Begin: class label Using probability value (Train Data Set)
      A_train_cl_p=list()
      for (i in 1:dim(A_w_train_p)[1])
      {
        A_train_cl_p[[i]]=which.max(A_w_train_p[i,])
      }
      A_traincl_p=as.data.frame(A_train_cl_p)
      Active_traincl_p=setNames(A_traincl_p, rep(" ", length(A_traincl_p))) #Class label in training data set
      Active_traincl_p=as.numeric(Active_traincl_p)                                     ########
      if(length(dupl)!=0)    
        Active_traincl_p=Active_traincl_p[-dupl]  # delete the added new row(s)                     #########
      
      
      #End:class label Using probability value(Train Data set)
      
      
      #Begin: class label using probability value (Test Data set) 
      A_test_cl_p=list()
      
      if(length(A_w_test_p)==0)
        next()
      
      for (i in 1:dim(A_w_test_p)[1])
      {
        A_test_cl_p[[i]]=which.max(A_w_test_p[i,])
      }
      A_testcl_p=as.data.frame(A_test_cl_p)
      Active_testcl_p=setNames(A_testcl_p, rep(" ", length(A_testcl_p))) #Class label in test data set
      Active_testcl_p= as.numeric(Active_testcl_p)
      #End:class label Using probability value(Test Data set)
      
      XX=unique(XX)
      X.numeric=unique(X.numeric)
      
      A_p_train=which(XX$V4==1 & XX$V7!=0)       #Position of training dataset
      A_p_train=c(1:9,A_p_train)
      
      A_cc=c(1:length(c(Active_traincl_p,Active_testcl_p)))
      A_dd=setdiff(A_cc,A_p_train)               #testing dataset position
      
      A_ff=cbind(A_p_train,Active_traincl_p)
      A_fff=cbind(A_dd,Active_testcl_p)
      A_ff=as.data.frame(A_ff)
      A_fff=as.data.frame(A_fff)
      names(A_ff) <- c("max","min")
      names(A_fff) <- c("max","min")
      A_ffff=rbind(A_ff,A_fff)
      active_data <- A_ffff[order(A_ffff$max),] 
      Active_data=active_data$min
      #######
      # if(length(dupl)!=0 & all(dupl>=12))    # Need to replace 16 to 41####################### Replace ##############################################################
      # Active_data=Active_data[-dupl]  # delete the added new row(s)                      ########      
      # 
      

      
      # Begin Active: If for any reason, user insert only one choice or two choices (like yes (1), maybe(3)) then this code can correctly specify the value of final decision
      
      Tlength=9  # Initially number of training dataset of active learning      
      A_b=which(X.numeric$V4==1) #position of active leanring training dataset
      
      if(isTRUE(all.equal( max( XX$V7[c(1:Tlength,A_b)]) ,min( XX$V7[c(1:Tlength,A_b)])))==T)
      {
        if(any(which(c((XX$V7[c(1:Tlength,A_b)]) ,( XX$V7[c(1:Tlength,A_b)]))==1))==TRUE)
          Active_data=Active_data
        else if (any(which(c((XX$V7[c(1:Tlength,A_b)]) ,( XX$V7[c(1:Tlength,A_b)]))==2))==TRUE)
          Active_data=Active_data+1
        else
          Active_data=Active_data+2
        
      }
      
      else
      {
        if(length(unique( XX$V7[c(1:Tlength,A_b)]))==2){
          if(((unique( XX$V7[c(1:Tlength,A_b)])[1]==2) | (unique( XX$V7[c(1:Tlength,A_b)][1]==3))) & ((unique( XX$V7[c(1:Tlength,A_b)])[2]==2) | (unique( XX$V7[c(1:Tlength,A_b)])[2]==3)))
          {
            Active_data=Active_data+1
          }
          else if(((unique( XX$V7[c(1:Tlength,A_b)])[1]==1) | (unique( XX$V7[c(1:Tlength,A_b)])[1]==3)) & ((unique( XX$V7[c(1:Tlength,A_b)])[2]==1) | (unique( XX$V7[c(1:Tlength,A_b)])[2]==3)))
          {
            Active_data[Active_data=="2"]<-3
          }
          else{
            Active_data=Active_data
          }
        }else
        {
          Active_data=Active_data
        }
        
        # End Active: If for any reason, user insert only one choice or two choices (like yes (1), maybe(3)) then this code can correctly specify the value of final decision
        
      }
      
      
      
      n_len=nrow(XX)-7+1
      
      
      
      Eva_position=c(n_len:length(Active_data))
      XX$V5[Eva_position[1:7]]=Active_data[Eva_position[1:7]]
      XX$V24[1:length(Active_data)]=Active_data
      insert_a=nrow(XX)+1
      insert_b=nrow(XX)+3
      
      XX = rbind(XX, XX[3:5,])
      XX$V1[insert_a:insert_b]=c(insert_a:insert_b)
      XX$V5[insert_a:insert_b]=XX$V7[3:5]
      XX$V7[insert_a:insert_b]=c(0,0,0)
      
      colnames(XX) <- c("ID", "Npurpose", "Benefit", "Risk", "Decision", "Verified", "label", "offer", "servicetype",
                        "consumers", "purpose", "data1", "data2", "data3", "data4", "data5", "data6", "data7", "data8",
                        "data9", "date_time_upload", "date_time_label", "date_time_feedback", "active_data" )
      
      dbWriteTable(con, tables[g], XX, row.names = F, overwrite=T)
      
      XX=0
      #write.table(XX, file = "C:/Users/singh/Desktop/Analysis/project_3/Ensemble_analysis/original_files//user100.csv",  
      #           row.names = F, append=F, sep="," , col.names = F, qmethod = "double")
      
    }
  }
}
dbDisconnect(con)
