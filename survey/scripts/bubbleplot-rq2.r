 #Let's create 2 discrete variables 
a=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3) #1=context-driven; 2=ease; 3=robustness
b=c(1,1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,3,4,4,4,4,4,4,4,4,4,4,4,4,4) #1=requirements; #2=design; 3=development; 4=testing; 5=maintenance
 
#I count the occurence of each couple of values. Eg : number of time a=1 and b=1, number of time a=1 and b=2 etc...
AA=xyTable(a,b)
 
#Now I can plot this ! I represent the dots as big as the couple occurs often
coeff_bigger=1.5

seTasks <- c("requirements", "design", "development", "testing", "maintenance")
rationales <- c("context-driven", "ease", "robustness")

plot(AA$x , AA$y , cex=AA$number*coeff_bigger  , pch=16 , col=rgb(0,0,1,0.5) , xlab= "Rationale" , ylab="SE Areas" , xlim=c(0,4), ylim=c(0,5), xaxt="n", yaxt="n", bty="n")

axis(1, at=1:3, labels=rationales)
axis(2, at=1:5, labels=seTasks)

text (AA$x , AA$y , AA$number)
 
#Note : It's easy to make a function that will compute this kind of plot automatically :
represent_discrete_variable=function(var1, var2 , coeff_bigger){
  AA=xyTable(var1,var2)
  plot(AA$x , AA$y , cex=AA$number*coeff_bigger  , pch=20 , col="red" , xlab= "Rationale" , ylab="SE Areas" )
  text (AA$x , AA$y , AA$number )
}