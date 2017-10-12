
#First term

Y_bar_H=mean(encadr[which(sex==1)])
Y_bar_F=mean(encadr[which(sex==2)])
diffY=Y_bar_H-Y_bar_F

F=model_f$family$linkinv
X=model.matrix(model)
betaAll=model$coefficients
betaH=model_h$coefficients
betaF=model_f$coefficients
betaHcorrected=matrix(0,ncol=1,nrow=length(betaAll))
betaFcorrected=matrix(0,ncol=1,nrow=length(betaAll))
for(beta in 1:length(betaAll)){
	name=names(betaAll[beta])
	trouve1=0
	for(j in 1:length(betaH))
		{
		if(names(betaH[j])==name)
			{
			betaHcorrected[beta]=betaH[j]
			trouve1=1
			}}
	if(trouve1==0){betaHcorrected[beta]=0}
	trouve2=0
	for(j in 1:length(betaF)){
		if(names(betaF[j])==name){
			betaFcorrected[beta]=betaF[j]
			trouve2=1
			}}
	if(trouve2==0){
		betaFcorrected[beta]=0
		}}

### 1st term in the brackets

#1er terme
man=which(sex==1)
term1=0
for(i in man){
	term1=term1+F(X[i,]%*%betaHcorrected)
	}
term1=term1/length(man)

#2nd term in the brackets
woman=which(sex==2)
term2=0
for(i in woman){
	term2=term2+F(X[i,]%*%betaHcorrected)
	}
term2=term2/length(woman)

term_in_brackets=term1-term2

###2nd term in brackets

#Third term
woman=which(sex==2)
term3=0
for(j in woman)
	{
	term3=term3+F(X[j,]%*%betaHcorrected)
	}
term3=term3/length(woman)

#4ième terme
term4=0
for(i in woman)
	{
	term4=term4+F(X[i,]%*%betaFcorrected)
	}
term4=term4/length(woman)

term_in_brackets2=term3-term4

### Final Results
diffY_theory=term_in_brackets + term_in_brackets2
diffexplained=term_in_brackets/diffY_theory
diffDiscrim=term_in_brackets2/diffY_theory
