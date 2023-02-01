twcrps = function(prediction, obs, u, weights){
  distr_obs = c()
  for(k in 1:length(u)){
    distr_obs = cbind(distr_obs, ifelse(u[k] < obs, 0, 1))
  }
  weights_mat = matrix(weights, ncol = length(weights), nrow = length(obs), byrow = TRUE)
  score = sum(weights_mat * (distr_obs - prediction)^2,na.rm=T)
  score
} 


valid_thresh=c(0.1,0.25,0.5,1,2,3,4,5,7.5,10,15,20,25,30,35,40,50,75,100)
weights=1-(1+(valid_thresh+1)^2)^{-1/6}
weights=weights/weights[length(weights)]

load("Data/qregress_test_df.Rdata")
obs<-c(Y_test)
pred_xi<-c(predictions[,,2])
pred_scale<-c(predictions[,,1])
pred_u<-c(pred_u_test)
q<-0.9

require("EnvStats")
lower_scale=pemp(pred_u,obs=obs)
probs=array(dim=c(length(obs),length(valid_thresh)))
library(evd)
for(i in 1:dim(probs)[2]){
  
  probs[valid_thresh[i] <= pred_u,i]=(1-q)/lower_scale[valid_thresh[i] <= pred_u]*pemp(valid_thresh[i],obs=obs)
  probs[valid_thresh[i] > pred_u,i]=(1-q)+q*pgpd(valid_thresh[i] - pred_u[valid_thresh[i] > pred_u], scale=pred_scale[valid_thresh[i] > pred_u],shape=pred_xi[1])
  
  print(i)
}

score<-twcrps(probs, obs, valid_thresh, weights)
print(score)
