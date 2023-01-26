



This report was automatically generated with the R package **knitr**
(version 1.39).


```r
---
title: "Building a Keras model in R"
author: "Jordan Richards"
date: "29/6/2022"
output:html_document
---
```

```
## Error: <text>:8:0: unexpected end of input
## 6: ---
## 7: 
##   ^
```


## Building our first model
Let's begin by loading in the required packages as well as linking up to the Python environment where we've previously installed Keras and Tensorflow.


```r
require(keras)
```

```
## Loading required package: keras
```

```r
path<- paste0(reticulate::virtualenv_root(),"/myenv/bin/python")
Sys.setenv(RETICULATE_PYTHON = path) #Set Python interpreter to that installed in myenv

reticulate::use_virtualenv("myenv", required = T)
```

Throughout this markdown, we will build and train neural network models for toy data.
We start by simulating some predictors; 2000 replications of eight predictors on a $7 \times 5$ grid, i.e.,  $\mathbf{x}\in \mathbb{R}^{2000}\times \mathbb{R}^{7}\times \mathbb{R}^5 \times \mathbb{R}^{8}$, which are just simply independent Gaussian realistions.  


```r
tensorflow::tf$random$set_seed(10)
```

```
## Loaded Tensorflow version 2.9.1
```

```r
set.seed(10)
# Create predictors
X<-rnorm(560000)

#Re-shape to a 4d array. First dimension corresponds to observations,
#last to the different components of the predictor set
dim(X) <- c(2000,7,5,8) #Eight predictors
```

Now let's create the response. We first apply a highly non-linear function $m(\mathbf{x})$ to the last dimension of $\mathbf{x}$, i.e., across the eight predictors. We then simulate our response $\mathbf{Y}\in\mathbb{R}^{2000}\times \mathbb{R}^{7}\times \mathbb{R}^5$ as Gaussian with standard deviation 2 and mean $m(\mathbf{x})$.


```r
#Non-linear transformation
m <- 2+exp(-4+X[,,,2]+X[,,,3]-X[,,,1])+cos(X[,,,1]+X[,,,5]-X[,,,4])-
  sin(X[,,,6]-X[,,,7])*(X[,,,8])-sqrt(X[,,,2]^2+X[,,,5]^2+X[,,,7]^2+X[,,,1]^2)

#Simulate Gaussian data with mean equal to m
Y <- apply(m,1:3,function(x) rnorm(1,mean=x,sd=2))
par(mfrow=c(1,2))
hist(m)
hist(Y)
```

<img src="figure/keras-build-Rmdunnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />

<blockquote>
 All of the predictors here are on the same marginal scale. Typically his does not hold for real data; it's good practice to marginally normalise or standardise your predictors to increase the numerical stability of training. 
</blockquote>

We want to build a neural network model that can estimate $m$; this is equivalent to doing least-squares/expectation regression, i.e., modelling $\mathbb{E}[Y|\mathbf{X}=\mathbf{x}]$. We know that there is no spatial/image or temporal/sequential structure to the predictor set $\mathbf{x}$ and so we will create a simple densely-connected MLP.

The first ingredient for a Keras model is the input layer. We can specify this using:

```r
  input.lay <- layer_input(shape = dim(X)[2:(length(dim(X)))], name = 'input_layer')
```

<blockquote>
Models in Keras/tensorflow act on a data type referred to as tensors (hence, tensorflow!). These can be thought of as analogues to R arrays (or np.arrays in Python) (see https://www.kdnuggets.com/2018/05/wtf-tensor.html). A 1D (or rank-1) tensor is a vector, a 2D (rank-2) tensor is a matrix, etc. We will refer to the dimensions as the axes of the tensor, e.g., a 2D tensor with shape [2,5] has two axes of length 2 and 5. The main differences are i) you cannot apply standard R functions to a tensor (this will be important when we write custom loss functions) and instead we must apply specific tensor operations from the Keras backend (`` `r
help(k_backend)` ``) which are differentiable functions (required for backpropagation) and ii) they are immutable, i.e., you cannot update the contents of a tensor or change its shape, without creating a new one.
</blockquote>

Note that I have specified the argument `` `r
shape` ``
in the input layer as the shape of the tensor $\mathbf{x}$ excluding the first axis, i.e., the axis that corresponds to observation indices.  Layers in Keras models are built for tensors of a specific shape. Although you can pass new input/ouput data to a pre-built Keras model, the shape of the corresponding tensor must be exactly the same bar the first axis. We specify the `` `r
shape` `` 
in the first layer so that Keras knows what to expect when we input our data; it needs to know how much memory to request, how many parameters to store, etc. Specifying `` `r
shape` ``
should only be necessary for the first layer as the next layer will act on the tensor `` `r
input.lay` ``
, the shape of which is determined already by `` `r
shape` ``.

The next step is to add some hidden layers. We will build a network with three hidden layers of 16, 12 and 12 neurons each. The first layer can be added as so:

```r
  hidden.lay <- input.lay %>%
    layer_dense(units=16,activation = 'relu', name = 'hidden_1')

#We could similarly write
#hidden.lay <-  layer_dense(input.lay,units=16,activation = 'relu', name = 'hidden_1')
```

We have added a dense layer and applied this to `` `r
input.lay` ``. This layer has `` `r
units=16` `` neurons, and applied at each neuron is the ReLu activation. Some other examples of activation functions that can be used are:

* "linear" - $h({x})={x}$
* "sigmoid" - $h({x})=1/(1+\exp(-{x}))$
* "exponential" - $h({x})=\exp({x})$
* "softmax", e.g., normalised probabilities - $h(\mathbf{x})=\exp(\mathbf{x})/\sum \exp(\mathbf{x})$ 
* "softplus" - $h({x})=\log(\exp({x})+1)$
* "softsign" - $h(x)={x}/(|{x}|+1)$
* "tanh" -$h(x)=(\exp(2x)-1)/(\exp(2x)+1)$
* "elu" - Exponential Linear Unit $h(x)=x$ if $x<0$ and $\alpha (\exp(x)-1)$ if $x<0$ for $\alpha > 0.$

<blockquote>
Here the type of layer applied is `` `r
layer_dense` `` as we are building a densely-connected MLP with dense layers only. We can easily replace this with a different type of layer. For example, we could instead have:

```r
  hidden.lay.cnn<-input.lay %>%
    layer_conv_2d(filters=4,kernel_size=c(3,3),padding="same",strides=1,
                  activation = 'relu', name = 'hidden_cnn_1')
```
This produces a 2D convolutional layer with 4 filters, a stride of 1 in both directions and a filter dimension of $3\times 3$. Each filter will pass over each of the images (indexed by `` `r
X[i,,,j]` ``) and every time it moves in either direction, it will move one stride. The filter return a new tensor, where the last axis is of length four. The arguments `` `r
padding ` `` and `` `r
strides ` ``
determine the shape of the rest of the tensor; if `` `r
padding="same"` `` and `` `r
strides=1` ``, then the input images will be padded and the second and third axes of the output tensor will be the same as those of the input tensor. If  `` `r
padding="valid" ` `` or  `` `r
strides!=1 ` ``, then the output tensor will have shorter second and third axes, dependent on `` `r
kernel_size` ``, see https://www.baeldung.com/cs/convolutional-layer-size. Typically the desired shape of your output tensor will be dependent on the application. If you are performing image classification, then the outputted shape will generally get smaller and smaller as information is condensed; the input may be ten images of size $20 \times 5$, i.e., a tensor of shape [20,5,10], but we may require only a single output from the network, e.g., a single Boolean or categorical value for the entire input. For our purpose, we want an estimate of $\mathbb{E}[Y|\mathbf{X}=\mathbf{x}]$ at all indices in the $20 \times 5$ image; in which case, we would use padding to ensure we get an output image of dimension $20 \times 5$.

Note that 1D and 3D convolution layers also exist, and different layer types can be used interchangeably within a single model. Similarly if we want a recurrent layer, we might use a long short-term memory layer (LSTM). We can add a dense LSTM layer using `` `r
layer_lstm` `` or a convolutional LSTM layer using `` `r
layer_conv_2d_lstm` ``. Convolution layers require substantially more parameters to operate than dense layers, and recurrent layers require computations that must be done sequentially, rather than in parallel; hence, it can be difficult to train either in a personal computer.  For simplicity, we stick with dense layers only.
</blockquote>

Let's add the final two hidden layers:

```r
  hidden.lay <- hidden.lay %>%
    layer_dense(units=12,activation = 'relu', name = 'hidden_2') %>%
      layer_dense(units=12,activation = 'relu', name = 'hidden_3')  
```
Note that the input here is `` `r
hidden.lay` ``, not `` `r
input.lay` ``. We now need to apply a final layer to i) ensure that the output tensor has the same shape as the data and ii) make sure the output has values that are sensible for our loss function. For the first part, we will just apply another dense layer but with only a single neuron (as we only have one outputted value, i.e., the expectation at each index). For the second part, as the expectation can take any real value, we will just use the identity/linear activation function so that our output can take any real value as well.


```r
  output.lay <- hidden.lay %>%
      layer_dense(units=1,activation = 'linear', name = 'output') 
```

Then we can build our model and specify the inputs and outputs and look at the summary.


```r
  model <- keras_model(
    inputs = c(input.lay), 
    outputs = c(output.lay)
  )
  summary(model)
```

```
## Model: "model"
## ________________________________________________________________________________
##  Layer (type)                       Output Shape                    Param #     
## ================================================================================
##  input_layer (InputLayer)           [(None, 7, 5, 8)]               0           
##  hidden_1 (Dense)                   (None, 7, 5, 16)                144         
##  hidden_2 (Dense)                   (None, 7, 5, 12)                204         
##  hidden_3 (Dense)                   (None, 7, 5, 12)                156         
##  output (Dense)                     (None, 7, 5, 1)                 13          
## ================================================================================
## Total params: 517
## Trainable params: 517
## Non-trainable params: 0
## ________________________________________________________________________________
```

We now need to compile our model and assign it some loss function and an optimizer. We will use adam (https://machinelearningmastery.com/adam-optimization-algorithm-for-deep-learning/) which is a popularly applied adaptive learning optimisation scheme.

```r
model %>% compile(
  optimizer=optimizer_adam(learning_rate=0.0005),
  loss = "mean_squared_error",
  metrics="mean_absolute_error"
)
```

The loss function we will be using is the MSE. For a full list of standard loss functions, see https://keras.io/api/losses/.  Custom loss functions (and optimisers) can be written, e.g., the negative log-likelihood for statistical distributions (more on this later). We can also output other standard performance metrics; here we are returning the MAE as well. Note that the metrics are not used for training, only the loss function.

Now let's train the model. We will train it for 2000 epochs using mini-batch gradient descent with a batch size of 500:

```r
  history <- model %>% fit(
    x=X, y=Y,
    epochs = 2000, batch_size = 500
  )
plot(history)
```

<img src="figure/keras-build-Rmdunnamed-chunk-11-1.png" title="plot of chunk unnamed-chunk-11" alt="plot of chunk unnamed-chunk-11" style="display: block; margin: auto;" />

And that's all there is to it! We now have a trained neural network that can provide predictions for $\mathbb{E}[Y|\mathbf{X}=\mathbf{x}]$. The final training loss (and MAE) is:


```r
model %>% evaluate(X,Y, batch_size=2000)
```

```
##                loss mean_absolute_error 
##            4.069938            1.602595
```

and we can retrieve our predictions using

```r
  predictions<-model %>% predict( X )
```
Let's compare these to the known values that we simulated from:

```r
par(mfrow=c(1,2))
hist(m,breaks=seq(min(m,predictions),max(m,predictions),length=50), 
     main="Predictions (red); m (grey)",density=TRUE)
hist(predictions,add=T,col="red",
     breaks=seq(min(m,predictions),max(m,predictions),length=50))
plot(predictions,m)
abline(a=0,b=1,col="red")
```

<img src="figure/keras-build-Rmdunnamed-chunk-14-1.png" title="plot of chunk unnamed-chunk-14" alt="plot of chunk unnamed-chunk-14" style="display: block; margin: auto;" />

Looks to be doing very well! Better predictions can be achieved by using a more complex architecture, i.e., a wider and deeper network, training for longer or just using more data!

We can save our fitted model using:

```r
model %>% save_model_tf(file="prediction_network")
```

Now let's compare our deep learning model to a linear regression model. For fair comparison, we will use the same optimisation scheme and build a linear regression model in Keras:

```r
 linear.lay <- input.lay %>%
    layer_dense(units=1,activation = 'linear', name = 'linear_1')
model.lin <- keras_model(
    inputs = c(input.lay), 
    outputs = c(linear.lay)
  )
  summary(model.lin)
```

```
## Model: "model_1"
## ________________________________________________________________________________
##  Layer (type)                       Output Shape                    Param #     
## ================================================================================
##  input_layer (InputLayer)           [(None, 7, 5, 8)]               0           
##  linear_1 (Dense)                   (None, 7, 5, 1)                 9           
## ================================================================================
## Total params: 9
## Trainable params: 9
## Non-trainable params: 0
## ________________________________________________________________________________
```

```r
  model.lin %>% compile(
  optimizer="adam",
  loss = "mean_squared_error"
)
```

So here I've taken the same input layer, but I've not used any hidden layers. Instead, the output layer just sums all of the inputs, adds a bias and then applied a linear activation; this is exactly linear regression!

Then we train the linear model:

```r
  history <- model.lin %>% fit(
    x=X, y=Y,
    epochs = 2000, batch_size = 500
  )
plot(history)
```

<img src="figure/keras-build-Rmdunnamed-chunk-17-1.png" title="plot of chunk unnamed-chunk-17" alt="plot of chunk unnamed-chunk-17" style="display: block; margin: auto;" />

and we get:

```r
  lin.preds<-model.lin %>% predict( X )
  model.lin %>% evaluate(X,Y, batch_size=2000)
```

```
##     loss 
## 5.746709
```

```r
par(mfrow=c(1,2))
hist(m,breaks=seq(min(m,lin.preds),max(m,lin.preds),length=50), 
     main="Predictions (red); m (grey)",density=TRUE)
hist(lin.preds,add=T,col="red",
     breaks=seq(min(m,lin.preds),max(m,lin.preds),length=50))
plot(lin.preds,m)
abline(a=0,b=1,col="red")
```

<img src="figure/keras-build-Rmdunnamed-chunk-18-1.png" title="plot of chunk unnamed-chunk-18" alt="plot of chunk unnamed-chunk-18" style="display: block; margin: auto;" />

A massive difference! The linear model just isn't capable of capturing the highly non-linear structure in $\mathbf{X}$ and the neural network provides a much better fit with a substantially lower training loss.

## Avoiding overfitting
There are a number of ways to mitigate the risk of overfitting. The most common approach is to use validation techniques. Here we hold out some of the training samples and use these for model selection. Let's use an 80-20 split.

```r
Y_train <- Y[1:1600,,]; Y_valid <- Y[1601:2000,,]
X_train <- X[1:1600,,,]; X_valid <- X[1601:2000,,,]
```
Now let's train the model with the training data, but keep track of the validation loss. To illustrate overfitting, we will use a much more complicated network; this has four layers and over 10000 parameters.

```r
#Here I'm just resetting the model to it's pre-trained state
hidden.lay <- input.lay %>%
      layer_dense(units=128,activation = 'relu', name = 'hidden_1') %>%
        layer_dense(units=64,activation = 'relu', name = 'hidden_2') %>%
          layer_dense(units=16,activation = 'relu', name = 'hidden_3') %>%
             layer_dense(units=16,activation = 'relu', name = 'hidden_4')

output.lay <- hidden.lay %>%
      layer_dense(units=1,activation = 'linear', name = 'output') 
model <- keras_model(
    inputs = c(input.lay), 
    outputs = c(output.lay)
  )
model %>% compile(
  optimizer="adam",
  loss = "mean_squared_error"
)

  history <- model %>% fit(
    x=X_train, y=Y_train,
    epochs = 500, batch_size = 200,
    validation_data = list(X_valid,Y_valid)
  )
plot(history)
```

<img src="figure/keras-build-Rmdunnamed-chunk-20-1.png" title="plot of chunk unnamed-chunk-20" alt="plot of chunk unnamed-chunk-20" style="display: block; margin: auto;" />

We observe that the fully trained model is overfitting as the validation loss and training loss diverge. The validation loss begins to increase with each epoch; we want to try our best to increase the number of epochs before this occurs and see if we can get a lower valdiation loss overall. 

There are a number of strategies for dealing with overfitting:

* Use more data (easier said than done!)
* Use a less complex architecture. If we had stuck with our original model, then it would take much longer for the model to begin overfitting!
* Weight regularisation - We can apply L1 or L2 penalties to the layer weights, see the argument "kernel_regularizer" in `` `r
help(layer_dense)` ``
* Or dropout

Dropout involves randomly "dropping out" a fraction of output features from a layer; this means we randomly set a proportion of the outputs to zero during training. Let's train the same model again, but with a 30% dropout rate applied to the first two layers.

```r
hidden.lay <- input.lay %>%
      layer_dense(units=128,activation = 'relu', name = 'hidden_1') %>%
      layer_dropout(0.3) %>%
        layer_dense(units=64,activation = 'relu', name = 'hidden_2') %>%
        layer_dropout(0.3) %>%
           layer_dense(units=16,activation = 'relu', name = 'hidden_3') %>%
            layer_dense(units=16,activation = 'relu', name = 'hidden_4') 

output.lay <- hidden.lay %>%
      layer_dense(units=1,activation = 'linear', name = 'output') 
model <- keras_model(
    inputs = c(input.lay), 
    outputs = c(output.lay)
  )
model %>% compile(
  optimizer="adam",
  loss = "mean_squared_error"
)

  history <- model %>% fit(
    x=X_train, y=Y_train,
    epochs = 500, batch_size = 200,
    validation_data = list(X_valid,Y_valid)
  )
plot(history)
```

<img src="figure/keras-build-Rmdunnamed-chunk-21-1.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" style="display: block; margin: auto;" />

Whilst training is taking a bit longer now, we observe much less overfitting. Using droupout has allowed us to train a much better predictive model. 

## Checkpointing and early-stopping
Even with the above techniques, overfitting is inevitable if training continues indefinitely. By default, Keras does not save the model at every epoch during training, it only saves the final state. In this way, it is not guaranteed that the best fitting model (in terms of the validation loss) will be returned at the end of training (unless the optimal number of epochs is known a priori). To overcome this, we can use either model checkpoints or early stopping.

Here we will use checkpoints to save the current state of the model at each epoch during training, but only if the validation loss has decreased (to save storage and computational time).


```r
#Here I'm just resetting the model to it's pre-trained state
hidden.lay <- input.lay %>%
      layer_dense(units=12,activation = 'relu', name = 'hidden_1') %>%
        layer_dense(units=8,activation = 'relu', name = 'hidden_2') %>%
          layer_dense(units=8,activation = 'relu', name = 'hidden_3') 

output.lay <- hidden.lay %>%
      layer_dense(units=1,activation = 'linear', name = 'output') 

model <- keras_model(
    inputs = c(input.lay), 
    outputs = c(output.lay)
  )

model %>% compile(
  optimizer="adam",
  loss = "mean_squared_error"
)

checkpoint <- callback_model_checkpoint(filepath = "model_weights", 
                                        monitor = "val_loss", verbose = 0,
                                        save_best_only = TRUE, 
                                        save_weights_only = TRUE, 
                                        mode = "min",
                                        save_freq = "epoch") 
#We will save only the weights of the model, not the entire model itself
  history <- model %>% fit(
    x=X_train, y=Y_train,
    epochs = 500, batch_size = 200,
    callback=checkpoint,
    validation_data = list(X_valid,Y_valid)
  )
plot(history)
```

<img src="figure/keras-build-Rmdunnamed-chunk-22-1.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" style="display: block; margin: auto;" />

Remember the currently saved model is still the last state from training! To remedy this, we overwrite the final training weights with the checkpoint weights:

```r
model <- load_model_weights_tf(model,filepath="model_weights")
```
Alternatively, we can use "early stopping". This technique monitors the validation loss and stops training if it starts to increase.

```r
#Here I'm just resetting the model to it's pre-trained state
hidden.lay <- input.lay %>%
      layer_dense(units=12,activation = 'relu', name = 'hidden_1') %>%
        layer_dense(units=8,activation = 'relu', name = 'hidden_2') %>%
          layer_dense(units=8,activation = 'relu', name = 'hidden_3') 

output.lay <- hidden.lay %>%
      layer_dense(units=1,activation = 'linear', name = 'output') 

model <- keras_model(
    inputs = c(input.lay), 
    outputs = c(output.lay)
  )

model %>% compile(
  optimizer="adam",
  loss = "mean_squared_error"
)

stop <- callback_early_stopping( monitor = "val_loss", 
                                 patience = 5,
                                 verbose = 0,
                                 mode = "min", 
                                 restore_best_weights = TRUE)
#The patience determines the number of epochs that the procedure is willing
#to wait to see if the validation loss decreases.

  history <- model %>% fit(
    x=X_train, y=Y_train,
    epochs = 2000, batch_size = 200,
    callback=stop,
    validation_data = list(X_valid,Y_valid)
  )
  
plot(history)
```

<img src="figure/keras-build-Rmdunnamed-chunk-24-1.png" title="plot of chunk unnamed-chunk-24" alt="plot of chunk unnamed-chunk-24" style="display: block; margin: auto;" />

So you can see that early-stopped has caused the training to run for only ~400 epochs, rather than the prespecified 2000.

## Writing a custom loss function
Sometimes the loss function that we want to use in our model is not implemented in Keras. This is particularly the case if we want to do conditional density estimation; here the loss function would be the negative log-likelihood associated with the density function.

Here I will show you how to write a custom loss function. We will fit a gamma distribution to toy data. Using the same predictors as before, we can create a response that follows a gamma distribution with non-stationary rate and shape parameters.

```r
rate <- abs(2+6*m/diff(range(m))); shape <- abs(5+5*m/diff(range(m)))
theta <- array(dim=c(dim(shape),2))
theta[,,,1]<- shape; theta[,,,2]<-rate
Y <- apply(theta,c(1,2,3),function(x) rgamma(1,shape=x[1],rate=x[2]))
par(mfrow=c(1,3))
hist(shape)
hist(rate)
hist(Y)
```

<img src="figure/keras-build-Rmdunnamed-chunk-25-1.png" title="plot of chunk unnamed-chunk-25" alt="plot of chunk unnamed-chunk-25" style="display: block; margin: auto;" />

```r
Y_train<- Y[1:1600,,]; Y_valid <- Y[1601:2000,,]
```
Let's first create a model with the shape parameter for the gamma distribution fixed to some known value, e.g., 5, which we won't estimate with our model.

Custom loss functions in keras take in arguments `` `r
y_true` `` and `` `r
y_pred` ``; the former is our response data $Y$ and the latter is the output from our final layer in the network. The loss function must be written using the backend functions from tensorflow and Keras to ensure that it is differentiable and hence can be used in a backpropogation framework.


```r
gamma_nll_loss <- function( y_true, y_pred) {

  K <- backend() #Here's the backend functions from Keras
  
  rate <- y_pred #The output from the final layer will be the shape parameter. 
  #This will be a function of the data whilst we fix the shape parameter to two.
  shape <- 5
  
  return(-K$sum(
    shape*K$log(rate)-
      tensorflow::tf$math$lgamma(shape)+ 
      (shape-1)*K$log(y_true)-
      rate*y_true
  )) 
}
```

Note that I've called `` `r
K <- backend() ` `` which is the Keras backend. This has allowed us to apply the $\log$ function. The gamma likelihood also requires evaluation of $\log\Gamma(\cdot)$, the $\log$ of the gamma function. This isn't in the Keras backend, but it is in the tensorflow backend; in particularl, the mathematical functions section of the backend, which we call using `` `r
tf$math` ``.

Easy! Now let's fit a one-parameter gamma distribution to the data. It's important to remember that the rate parameter for the gamma distribution is strictly positive, so the activation function of the final layer of our network must reflect this. The model is:


```r
hidden.lay <- input.lay %>%
      layer_dense(units=8,activation = 'relu', name = 'hidden_1') %>%
        layer_dense(units=8,activation = 'relu', name = 'hidden_2') %>%
          layer_dense(units=8,activation = 'relu', name = 'hidden_3') 


output.lay <- hidden.lay %>%
      layer_dense(units=1,activation = 'exponential', name = 'output') 
```

with the 'exponential' activation used to ensure that the predicted rate parameter is strictly positive.

We now compile our model with the new loss function and train it for 2500 epochs. Typically we need more epochs to train a CDE model (compared to a prediction model) as it's more difficult to optimise the loss. I would also recommend using a larger batch size, particularly if you expect high temporal non-stationarity in your data. Ideally (if you have the computational resources), use the full batch.

```r
model <- keras_model(
    inputs = c(input.lay), 
    outputs = c(output.lay)
  )

model %>% compile(
  optimizer="adam",
  loss = gamma_nll_loss #And here we specify our loss function
)
#We will use early stopping to avoid overfitting.
checkpoint <- callback_model_checkpoint(filepath = "model_weights", 
                                        monitor = "val_loss", verbose = 0,
                                        save_best_only = TRUE, 
                                        save_weights_only = TRUE, 
                                        mode = "min",
                                        save_freq = "epoch") 
history <- model %>% fit(
    x=X_train, y=Y_train,
    epochs = 2500, batch_size = 2000,
    callback=checkpoint,
    validation_data = list(X_valid,Y_valid)
  )
model <- load_model_weights_tf(model,filepath="model_weights")
```

The training and validation losses are:

```r
print("Training loss"); print(model %>% evaluate(X_train,Y_train,batch_size=2000))
```

```
## [1] "Training loss"
```

```
##     loss 
## 81094.58
```

```r
print("Validation loss"); print(model %>% evaluate(X_valid,Y_valid,batch_size=2000))
```

```
## [1] "Validation loss"
```

```
##     loss 
## 20338.74
```
and we can retrieve the predicted rate parameters:

```r
pred.rate <- predict(model, X)
hist(rate, main="Predicted (red); true (grey)",
     breaks=seq(min(pred.rate,rate),max(pred.rate,rate),length=50))
hist(pred.rate,add=T,col="red",
     breaks=seq(min(pred.rate,rate),max(pred.rate,rate),length=50))
```

<img src="figure/keras-build-Rmdunnamed-chunk-30-1.png" title="plot of chunk unnamed-chunk-30" alt="plot of chunk unnamed-chunk-30" style="display: block; margin: auto;" />

Not looking too bad! Let's check the fit. We'll do this by using the predicted rate parameters to transform the validation data onto standard normal margins and comparing the theoretical and empirical quantiles.

```r
pred.rate.valid <- predict(model, X_valid)
Y.norm <- qnorm(pgamma(Y_train,rate=pred.rate.valid,shape = 5))
p.seq <- seq(0, 1-1/length(Y.norm), length=3000)
plot(quantile(Y.norm,p.seq),qnorm(p.seq),
     ylab="Theoretical quantiles", xlab="Empirical quantiles")
abline(a=0,b=1,col="red")
```

<img src="figure/keras-build-Rmdunnamed-chunk-31-1.png" title="plot of chunk unnamed-chunk-31" alt="plot of chunk unnamed-chunk-31" style="display: block; margin: auto;" />

Looks very good already! Let's see if we can improve the fit in the tails by simultaneously estimating the shape parameter; we will do this in the following section.

<blockquote>
A quick note on saving/loading the model. 
We can save our fitted model using:

```r
model %>% save_model_tf(file="cde_gamma_network")
```
But then to load the model, we need to supply all of the custom parts of the model, e.g., the loss, custom activation functions, etc.

```r
model <-load_model_tf(file="cde_gamma_network",
                      custom_objects = list("gamma_nll_loss"=gamma_nll_loss))
```
</blockquote>



## Multi-input/output networks

Thus far we have only considered a network with a single input and output. We know that the gamma distribution has two parameters, and so if we want to estimate both simultaneously we will need to create a network with two outputs. We will also give the network two inputs; this could be useful if you want to use model the parameters using different predictors or the loss function requires some extra, non-estimated inputs, e.g., the exceedance thresholds for a GPD model.

For illustrative purposes, we will just duplicate the predictors and input these twice. 

```r
X1 <- X2 <-X
X1_train <- X2_train <- X_train
X1_valid <- X2_valid <- X_valid
```
We now need two input layers

```r
  input.lay1 <- layer_input(shape = dim(X1)[2:(length(dim(X1)))], name = 'input_layer1')
  input.lay2 <- layer_input(shape = dim(X2)[2:(length(dim(X2)))], name = 'input_layer2')
```
We then create two separate networks using the different inputs. One will be used for modelling the shape parameter and the other is used for modelling the rate parameter.

Here's the shape network:

```r
shape.hidden.lay <- input.lay1 %>%
      layer_dense(units=8,activation = 'relu', name = 'shape_hidden_1') %>%
        layer_dense(units=8,activation = 'relu', name = 'shape_hidden_2') %>%
          layer_dense(units=8,activation = 'relu', name = 'shape_hidden_3') 

shape.output.lay <- shape.hidden.lay %>%
      layer_dense(units=1,activation = 'exponential', name = 'shape_output')
```

and here's the rate network:

```r
rate.hidden.lay <- input.lay2 %>%
      layer_dense(units=8,activation = 'relu', name = 'rate_hidden_1') %>%
        layer_dense(units=8,activation = 'relu', name = 'rate_hidden_2') %>%
          layer_dense(units=8,activation = 'relu', name = 'rate_hidden_3')   


rate.output.lay <- rate.hidden.lay %>%
      layer_dense(units=1,activation = 'exponential', name = 'rate_output') 
```
In both cases we use an exponential activation to ensure that the parameters are strictly positive.

Recall that the loss functions take in only a single input `` `r
y_pred` ``. Hence we must combined the two networks together now so that they create a single tensor as input to our custom loss function.

```r
output.joined.lay <- layer_concatenate(c(shape.output.lay,rate.output.lay))
```
The final axes of the tensor `` `r
output.joined.lay` `` will have length two, with the first index being the predicted shape parameter and the second being the rate parameter. We can adapt our custom loss function accordingly.

```r
gamma_nll_loss <- function( y_true, y_pred) {

  K <- backend() 
  
  shape <- y_pred[tensorflow::all_dims(),1] 
  rate <- y_pred[tensorflow::all_dims(),2]
  #The all_dims() function just ensures that its the last axis that is indexed.
  #This can be very useful if the shape of the tensor isn't known.
  
  return(-K$sum(
    shape*K$log(rate)-
     tensorflow::tf$math$lgamma(shape)+ 
      (shape-1)*K$log(y_true)-
      rate*y_true
  )) 
}
```

And now we can compile and train our model. Remember that we have two inputs now.

```r
model <- keras_model(
    inputs = c(input.lay1,input.lay2), 
    outputs = c(output.joined.lay)
  )
model %>% compile(
  optimizer="adam",
  loss = gamma_nll_loss #And here we specify our loss function
)
checkpoint <- callback_model_checkpoint(filepath = "model_weights", 
                                        monitor = "val_loss", verbose = 0,
                                        save_best_only = TRUE, 
                                        save_weights_only = TRUE, 
                                        mode = "min",
                                        save_freq = "epoch") 
  history <- model %>% fit(
    list(X1_train,X2_train), Y_train,
    epochs = 2500, batch_size = 2000,
    callback=checkpoint,
    validation_data = list(list(X1_valid,X2_valid),Y_valid)
  )
plot(history)
```

<img src="figure/keras-build-Rmdunnamed-chunk-40-1.png" title="plot of chunk unnamed-chunk-40" alt="plot of chunk unnamed-chunk-40" style="display: block; margin: auto;" />

```r
model <- load_model_weights_tf(model,filepath="model_weights")
```

Then we can get the predicted parameters:

```r
preds <- model %>% predict( list(X1,X2))
pred.shape<-preds[,,,1]
pred.rate<-preds[,,,2]
par(mfrow=c(2,2))
hist(shape, main="Predicted (red); true (grey)",
     breaks=seq(min(pred.shape,shape),max(pred.shape,shape),length=50))
hist(pred.shape,add=T,col="red", 
     breaks=seq(min(pred.shape,shape),max(pred.shape,shape),length=50))
hist(rate, main="Predicted (red); true (grey)",
     breaks=seq(min(pred.rate,rate),max(pred.rate,rate),length=50))
hist(pred.rate,add=T,col="red", 
     breaks=seq(min(pred.rate,rate),max(pred.rate,rate),length=50))
plot(pred.shape,shape)
abline(a=0,b=1,col="red")

plot(pred.rate,rate)
abline(a=0,b=1,col="red")
```

<img src="figure/keras-build-Rmdunnamed-chunk-41-1.png" title="plot of chunk unnamed-chunk-41" alt="plot of chunk unnamed-chunk-41" style="display: block; margin: auto;" />

and check the fit.

```r
preds.valid <- model %>% predict( list(X1_valid,X2_valid))
pred.shape.valid<-preds[,,,1]
pred.rate.valid<-preds[,,,2]
Y.norm <- qnorm(pgamma(Y_valid,shape=pred.shape.valid,rate = pred.rate.valid))
p.seq <- seq(0, 1-1/length(Y.norm), length=3000)
plot(quantile(Y.norm,p.seq),qnorm(p.seq), ylab="Theoretical quantiles", 
     xlab="Empirical quantiles")
abline(a=0,b=1,col="red")
```

<img src="figure/keras-build-Rmdunnamed-chunk-42-1.png" title="plot of chunk unnamed-chunk-42" alt="plot of chunk unnamed-chunk-42" style="display: block; margin: auto;" />

```r
print("Training loss"); model %>% evaluate(list(X1_train,X2_train),
                                           Y_train,batch_size=2000)
```

```
## [1] "Training loss"
```

```
##     loss 
## 80956.88
```

```r
print("Validation loss"); model %>% evaluate(list(X1_valid,X2_valid),
                                             Y_valid,batch_size=2000)
```

```
## [1] "Validation loss"
```

```
##     loss 
## 20337.96
```

Are we doing much better? Maybe, but not dramatically so. Even though we know that the underlying distribution that generates the response is highly non-stationary in both parameters, it seems that fixing the shape to 5 and modelling only the rate parameter already gives a satisfactory fit. Why could this be?

* Maybe we need more data to train the two-parameter model? This uses two neural networks, and so requires estimation of twice as many parameters
* If a neural network has lots of parameters, there's no guarantee that the log-likelihood/loss function is convex. We could have gotten stuck in a local minima during training (something to look out for!)
* We could use a less complex network for the shape parameter as this is 
* Or maybe we can even keep the shape parameter constant over all indices, but estimate this jointly with the non-stationary rate parameter (rather than fixing it at 5). Here's a quick tip to on how to achieve this; we can rewrite the hidden layers for the shape parameter in the following way:


```r
shape.hidden.lay <- input.lay1 %>%
      layer_dense(units=1,activation = 'linear', name = 'shape_hidden_1',
                  trainable=F,
                  weights=list(matrix(0,nrow=dim(X1)[length(dim(X1))],ncol=1),
                               array(1,dim=1))) 

shape.output.lay <- shape.hidden.lay %>%
      layer_dense(units=1,activation = 'exponential', name = 'shape_output', use_bias = F)
```
The `` `r
trainable=F` `` argument means that the weights and biases of the first layer are fixed; they do not update during training. I've then supplied initial weights and biases through the `` `r
weight` ` argument; we have a matrix of all zero weights and a bias of one. That means that the first layer will output a single constant value, one, regardless of the input. The second layer does not have any biases; only a single trainable weight. This will be multiplied by the one from the output of the first layer and, as the weight takes any real value, we can get shape parameter (with range controlled by the second activation function) fixed over all inputs.

Let's build and train a model with a homogeneous shape parameter. First the shape network:

```r
init.shape<-5
shape.hidden.lay <- input.lay1 %>%
      layer_dense(units=1,activation = 'linear', name = 'shape_hidden_1',
                  trainable=F,
                  weights=list(matrix(0,nrow=dim(X1)[length(dim(X1))],ncol=1),
                               array(1,dim=1))) 

shape.output.lay <- shape.hidden.lay %>%
      layer_dense(units=1,activation = 'exponential', name = 'shape_output', use_bias = F,
                  weights=list(as.matrix(log(init.shape))))
```
Here I've set the initial weight in the output layer to $\log(5)$. We know that the weights of the previous layer are all zeroes and the bias is one, so this means that the initial homogeneous shape parameter will be $\exp(\log(5))=5$, thanks to the exponential activation.



```r
rate.hidden.lay <- input.lay2 %>%
      layer_dense(units=8,activation = 'relu', name = 'rate_hidden_1') %>%
        layer_dense(units=8,activation = 'relu', name = 'rate_hidden_2') %>%
          layer_dense(units=8,activation = 'relu', name = 'rate_hidden_3')   


rate.output.lay <- rate.hidden.lay %>%
      layer_dense(units=1,activation = 'exponential', name = 'rate_output') 

output.joined.lay <- layer_concatenate(c(shape.output.lay,rate.output.lay))

model <- keras_model(
    inputs = c(input.lay1,input.lay2), 
    outputs = c(output.joined.lay)
  )
model %>% compile(
  optimizer="adam",
  loss = gamma_nll_loss #And here we specify our loss function
)
checkpoint <- callback_model_checkpoint(filepath = "model_weights", 
                                        monitor = "val_loss", verbose = 0,
                                        save_best_only = TRUE, 
                                        save_weights_only = TRUE, 
                                        mode = "min",
                                        save_freq = "epoch") 
  history <- model %>% fit(
    list(X1_train,X2_train), Y_train,
    epochs = 2500, batch_size = 2000,
    callback=checkpoint,
    validation_data = list(list(X1_valid,X2_valid),Y_valid)
  )
plot(history)
```

<img src="figure/keras-build-Rmdunnamed-chunk-45-1.png" title="plot of chunk unnamed-chunk-45" alt="plot of chunk unnamed-chunk-45" style="display: block; margin: auto;" />

```r
model <- load_model_weights_tf(model,filepath="model_weights")

preds <- model %>% predict( list(X1,X2))
pred.shape<-preds[,,,1]
pred.rate<-preds[,,,2]
print("Estimated shape:");   print(mean(pred.shape))
```

```
## [1] "Estimated shape:"
```

```
## [1] 4.983407
```

```r
par(mfrow=c(2,2))
hist(shape, main="Predicted (red); true (grey)",
     breaks=seq(min(pred.shape,shape),max(pred.shape,shape),length=50))

hist(pred.shape,add=T,col="red", 
     breaks=seq(min(pred.shape,shape),max(pred.shape,shape),length=50))
hist(rate, main="Predicted (red); true (grey)",
     breaks=seq(min(pred.rate,rate),max(pred.rate,rate),length=50))
hist(pred.rate,add=T,col="red", 
     breaks=seq(min(pred.rate,rate),max(pred.rate,rate),length=50))
```

<img src="figure/keras-build-Rmdunnamed-chunk-45-2.png" title="plot of chunk unnamed-chunk-45" alt="plot of chunk unnamed-chunk-45" style="display: block; margin: auto;" />

and check the fit.

```r
preds.valid <- model %>% predict( list(X1_valid,X2_valid))
pred.shape.valid<-preds[,,,1]
pred.rate.valid<-preds[,,,2]
Y.norm <- qnorm(pgamma(Y_valid,shape=pred.shape.valid,rate = pred.rate.valid))
p.seq <- seq(0, 1-1/length(Y.norm), length=3000)
plot(quantile(Y.norm,p.seq),qnorm(p.seq), ylab="Theoretical quantiles", 
     xlab="Empirical quantiles")
abline(a=0,b=1,col="red")
```

<img src="figure/keras-build-Rmdunnamed-chunk-46-1.png" title="plot of chunk unnamed-chunk-46" alt="plot of chunk unnamed-chunk-46" style="display: block; margin: auto;" />

```r
print("Training loss"); model %>% evaluate(list(X1_train,X2_train),
                                           Y_train,batch_size=2000)
```

```
## [1] "Training loss"
```

```
##     loss 
## 81043.25
```

```r
print("Validation loss"); model %>% evaluate(list(X1_valid,X2_valid),
                                             Y_valid,batch_size=2000)
```

```
## [1] "Validation loss"
```

```
##    loss 
## 20301.7
```

Which model should we pick? We should expect the more complicated model to give the best predictions, but due to the practicalities of training a deep learning model this might not always be the case! Sometimes it's better to opt for parsimonity over complexity.
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 4.2.1 (2022-06-23)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.2 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] keras_2.9.0
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.8.3     lattice_0.20-45  png_0.1-7        zeallot_0.1.0   
##  [5] grid_4.2.1       R6_2.5.1         jsonlite_1.8.0   magrittr_2.0.3  
##  [9] evaluate_0.15    highr_0.9        tfruns_1.5.0     cli_3.3.0       
## [13] rlang_1.0.3      stringi_1.7.6    whisker_0.4      Matrix_1.4-1    
## [17] ellipsis_0.3.2   reticulate_1.25  generics_0.1.3   tools_4.2.1     
## [21] stringr_1.4.0    xfun_0.31        compiler_4.2.1   base64enc_0.1-3 
## [25] tensorflow_2.9.0 knitr_1.39
```

```r
Sys.time()
```

```
## [1] "2022-07-09 12:16:30 +03"
```

