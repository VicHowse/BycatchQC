addWeights <- function(spec,len){
			x=NA
			wt = NA
			spec = paste('S',spec,sep="")
			 switch(spec,
				S10= { x =  ifelse(len<140,len,NA) 
					   wt = x^3.071947*0.007005},
				S11= { x =  ifelse(len<90,len,NA) 
					   wt =x^3.050064*0.008295},
				S12= { x =  ifelse(len<120,len,NA)
				       wt = x^3.22931*0.003199},
				S13 = { x =  ifelse(len<120,len,NA)
				       wt=x^3.220026*0.002887},
				S14= { x =  ifelse(len<75,len,NA) 
				       wt = x^3.209943*0.003369},
				S15= { x =  ifelse(len<100,len,NA)
					   wt = x^3.218504*0.0042},
				S16= { x =  ifelse(len<105,len,NA)
				       wt = x^3.010775*0.010377},
				S17= { x =  ifelse(len<140,len,NA)
				       wt = x^3.17753*0.004524},
				S19= { x =  ifelse(len<120,len,NA)
					   wt = x^3.110316*0.005185},
				S23= { x =  ifelse(len<60,len,NA) 
				       wt = x^3.047154*0.013098},
				S30= { x =  ifelse(len<200,len,NA)
				       wt = x^3.2*0.005},
				S40= { x =  ifelse(len<95,len,NA) 
				       wt = x^3.229647*0.003651},
				S41= { x =  ifelse(len<100,len,NA)
				       wt = x^3.156716*0.003425},
				S42= { x =  ifelse(len<100,len,NA)
				       wt = x^2.925636*0.010081},
				S43= { x =  ifelse(len<100,len,NA)
				       wt = x^3.068389*0.009385},
				S49= { x =  ifelse(len<100,len,NA)
				       wt = x^3.36*0.0023},
				S50= { x =  ifelse(len<120,len,NA)
				       wt = x^3.078871*0.006526},
				S51= { x =  ifelse(len<120,len,NA)
				       wt = x^3*0.01},
				S59= { x =  ifelse(len<120,len,NA)
				       wt = x^3.078871*0.006526},
				S61= { x =  ifelse(len<55,len,NA) 
				       wt = x^2.943786*0.015366},
				S70= { x =  ifelse(len<45,len,NA) 
				       wt = x^3.158443*0.005924},
				S90= { x =  ifelse(len<200,len,NA)
				       wt = x^3.129*0.00554},
				S118= { x =  ifelse(len<140,len,NA)
				        wt = x^3.321*0.0039},
				S120= { x =  ifelse(len<48,len,NA)
				        wt = x^2.794148*0.03093},
				S122= { x =  ifelse(len<48,len,NA)
				        wt = x^2.794148*0.03093},
				S123= { x =  ifelse(len<50,len,NA)
				        wt = x^3.040681*0.014281},
				S193= { x =  ifelse(len<120,len,NA)
				        wt = x^3.220026*0.002887},
				S201= { x =  ifelse(len<150,len,NA)
				        wt = x^3.006082*0.009493},
				S203= { x =  ifelse(len<100,len,NA)
				        wt = x^3.188176*0.003073},
				S204= { x =  ifelse(len<150,len,NA)
				        wt = x^3.245312*0.002603},
				S211= { x =  ifelse(len<150,len,NA)
				        wt = x^3.245312*0.002603},
				S220= { x =  ifelse(len<250,len,NA)
				        wt = x^3.245312*0.002603},
				S240= { x =  ifelse(len<200,len,NA)
				        wt = x^2.31*0.0215},
				S300= { x =  ifelse(len<50,len,NA)
				        wt = x^3.065909*0.008457},
				S301= { x =  ifelse(len<50,len,NA)
				        wt = x^3.431614*0.003497},
				S303= { x =  ifelse(len<50,len,NA)
				        wt = x^4.127*0.000432},
				S309= { x =  ifelse(len<50,len,NA)
				        wt = x^3.431614*0.003497},
				S310= { x =  ifelse(len<50,len,NA)
				        wt = x^3.431614*0.003497},
					S311= { x =  ifelse(len<50,len,NA)
				        wt = x^3.431614*0.003497},
				S320= { x =  ifelse(len<100,len,NA)
				        wt = x^3.151003*0.010728},
				S346= { x =  ifelse(len<100,len,NA)
				        wt = x^3.068389*0.009385},
				S400= { x =  ifelse(len<100,len,NA)
				        wt = x^2.917856*0.021818},
				S429= { x =  ifelse(len<200,len,NA)
				        wt = x^3.448*0.00087},
				S455= { x =  ifelse(len<200,len,NA)
				        wt = x^2.927404*0.000953},
				S501= { x =  ifelse(len<100,len,NA)
				        wt = x^3.05935*0.042016},
				S598= { x =  ifelse(len<200,len,NA)
				        wt = x^3.448*0.00087},
				S621= { x =  ifelse(len<30,len,NA)
				        wt = x^5.683*0.00001},
				S636= { x =  ifelse(len<50,len,NA)
				        wt = x^2.794148*0.03093},
				S640= { x =  ifelse(len<200,len,NA)  
				        wt = x^3.13776*0.002676},
				S642= { x =  ifelse(len<200,len,NA)  
				        wt = x^3.448*0.00087},
				S2511= { x =  ifelse(len<200,len,NA)
				        wt = x^2.88606*0.00024},
				S2513= { x =  ifelse(len<200,len,NA)
				         wt = x^2.88606*0.00024},
				S2515= { x =  ifelse(len<200,len,NA)
				         wt = x^2.88606*0.00024},
				S2519= { x =  ifelse(len<200,len,NA)
				         wt = x^3.071947*0.007005},
				S2520= { x =  ifelse(len<200,len,NA)
				         wt = x^2.67338*0.001863},
				S2521= { x =  ifelse(len<200,len,NA)
				         wt = x^2.580593*0.002672},
				S2523= { x =  ifelse(len<200,len,NA)
				         wt = x^3.191621*0.000148},
				S2525= { x =  ifelse(len<200,len,NA)
				         wt = x^3.191621*0.000148},
				S2526= { x =  ifelse(len<200,len,NA)
				         wt = x^2.895289*0.000547},
				S2527= { x =  ifelse(len<200,len,NA)
				         wt = x^2.67338*0.001863},
				S2531= { x =  ifelse(len<200,len,NA)
				         wt = x^2.88606*0.00024},
				S2532= { x =  ifelse(len<200,len,NA)
				         wt = x^3.228378*0.00092},
				S2550={	 x =  ifelse(len<200,len,NA)
				         wt = x^3.0583*0.000608}
							)
				return(c(x,wt))
}