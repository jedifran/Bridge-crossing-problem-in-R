######################################################################
#########  algorithm to solve bridge crossing problem in R ############
######################################################################
# see http://en.wikipedia.org/wiki/Bridge_and_torch_problem

#Preamble 
#say the crossing times are t1, t2, t3, ... , tN-1, tN
#the optimal strategy will depend on the relative size of {2t2} and {t1 + tN-1}

#eg1) when the crossing times in seconds are {1,2,7,10}, the optimal crossing strategy to minimise time is:
#+{1,2}
#-{1}
#+{7,10}
#-{2}
#+{1,2}
# crossing time = 17 seconds
#this can be generalised as:  start with the two smallest numbers, get them across the bridge, and (always) return with the smallest number. Then take the largest pair of numbers across the bridge and return with the smallest number of all those already accross the bridge. Repeat until all numbers have crossed.

#eg2) when the crossing times are {1,20,21,22}, the above strategy is sub-optimal because : {2t2} > {t1 + tN-1}, i.e 40 > 22
#using this first strategy we get:
#+{1,20}
#-{1}
#+{22,21}
#-{20}
#+{1,20}
#crossing time = 83

#the more optimal strategy in this case is to do the following:
#+{1,22}
#-{1}
#+{1,21}
#-{1}
#+{1,20}
#crossing time = 63!

# jedidiah.francis@gmail.com 24/06/11
######################################################################
######################################################################



#function to solve for minimum bridge crossing time

min.bridge.crossing.time <- function(t){
tot.time= 0   #container to hold total time taken

#positions on the bridge
p1 = c()  #initial position where all people start from
p2 = c()  #position on other side of bridge where everyone wants to get to

p1=sort(t) #put times in position 1
l=length(p1)

#loop that does the cool stuff
while(l>1){
if(l==2){
	p2 = sort(c(p2,p1[c(1,2)])) #let final two cross bridge into p2, and sort at same time
	tot.time = tot.time + max(p1[c(1,2)])  #increment total time
	p1 = p1[c(-1,-2)]
	}
	
if(l==3){
	p2 = sort(c(p2,p1[c(1,3)])) #let final two cross bridge into p2, and sort at same time
	tot.time = tot.time + max(p1[c(1,3)])  #increment total time
	p1 = p1[c(-1,-3)]
	
	p1 = sort(c(p1,p2[1])) #let t1 cross over back to initial position and sort at same time
	tot.time = tot.time + p2[1]  #increment total time
	p2 = p2[-1]
	}	
	
if(l>=4){
	if( (2*p1[2])<=(p1[1]+p1[l-1]) ){
			p2 = sort(c(p2,p1[c(1,2)])); #let t1 & t2 cross bridge into p2, and sort at same time
			tot.time = tot.time + max(p1[c(1,2)]);  #increment total time
			p1 = p1[c(-1,-2)];
		
			p1 = sort(c(p1,p2[1])); #let t1 cross over back to initial position and sort at same time
			tot.time = tot.time + p2[1];  #increment total time
			p2 = p2[-1];
	
			l = length(p1);
			p2 = sort(c(p2,p1[c(l-1,l)])); #let largest two times cross bridge into p2, and sort at same time
			tot.time = tot.time + max(p1[c(l-1,l)]);  #increment total time
			p1 = p1[-c(l-1,l)];
	
			p1 = sort(c(p1,p2[1])); #let smallest time in p2 cross over back to initial position and sort at same time
			tot.time = tot.time + p2[1];  #increment total time
			p2 = p2[-1];}                                                                                                          else{	    p2 = sort(c(p2,p1[c(1,l)])); #let t1 & t2 cross bridge into p2, and sort at same time
			tot.time = tot.time + max(p1[c(1,l)]);  #increment total time
			p1 = p1[c(-1,-l)];
		
			p1 = sort(c(p1,p2[1])); #let t1 cross over back to initial position and sort at same time
			tot.time = tot.time + p2[1];  #increment total time
			p2 = p2[-1];
	
			l = length(p1);
			p2 = sort(c(p2,p1[c(1,l)])); #let largest two times cross bridge into p2, and sort at same time
			tot.time = tot.time + max(p1[c(1,l)]);  #increment total time
			p1 = p1[c(-1,-l)];
	
			p1 = sort(c(p1,p2[1])); #let smallest time in p2 cross over back to initial position and sort at same time
			tot.time = tot.time + p2[1];  #increment total time
			p2 = p2[-1];}			
	}		
	
	l = length(p1) #update number still to corss bridge
			
}	
tot.time	
}	




#test it!
min.bridge.crossing.time(c(1,2,7,10)) #answer = 17
min.bridge.crossing.time(c(1,6,10,13,15,16,17))  # answer = 75

min.bridge.crossing.time(sample(c(1:1000),round(runif(1,1,1000))))  #can solve for n number of crossing times
	
