# ----------------------------- #
# Econ217 (J. Li, W2021)        #
# Section 8 (Feb. 24-26, 2021)  # NOTE: all the path directories are based on my own computer, 
# TA: David Sungho Park         #       so you should change them according to yours.
# ----------------------------- #

# clear environment
rm(list = ls())

# set working directory
setwd('/Users/DSP/Google Drive/UCSC/Teaching/Econ217_W21/SectionMaterial/')

# loading (installed) packages
packages<-c("keras","gridExtra")
invisible(lapply(packages,library,character.only=TRUE))

###################################################################################
# keras example
###################################################################################
set.seed(1)
mnist <- dataset_mnist() # handwritten digits dataset with 60K examples for training set and 10K for test set

# training set
x_train <- mnist$train$x # array of images (each example in 28 x 28 pixels, where pixel values range from 0 to 255)
y_train <- mnist$train$y # vector of labels (i.e. 0, 1, 2, ..., 9)

print(x_train[1,,]) # just to visualize the stored pixel values for the 1st example in the training set

# test set
x_test <- mnist$test$x
y_test <- mnist$test$y

# Visualize the first 49 observations (in the training set)
pdf('tex_raw/handwrittendigits.pdf')
par(mfcol=c(7,7)) # to combine multiple plots into one overall graph, and put the 49 images in 7 x 7 layout
par(mar=c(0, 0, 3, 0)) # margin size (bottom, left, top, right)
for (idx in 1:49) { 
  im <- x_train[idx,,] # 28x28 array slice for each example
  im <- t(apply(im, 2, rev)) # transposing the array slice for the image fn (basically rotating 90 degrees clockwise)
  image(1:28, 1:28, im, col=gray((0:255)/255), 
        yaxt='n',xaxt='n', main=paste(y_train[idx]))
}
dev.off()

# Reshape covariates so that we have 784 = 28*28 columns per image.
x_train <- array_reshape(x_train, c(nrow(x_train), 784)) # now, 60000x784 matrix
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# Rescale covariates, dividing by maximum value of x_train (recall pixel values ranges from 0 to 255).
x_train <- x_train / 255
x_test <- x_test / 255

# Make outcomes categorical.
y_train <- to_categorical(y_train, 10) # now, 60000x10 matrix (where 0/1 dummy for each column)
y_test <- to_categorical(y_test, 10)

# Network structure: (784,256,128,10)
  # 784 input units  
  # 256 hidden units in 1st hidden layer (activation fn = RELU)
  # 128 hidden units in 2nd hidden layer (activation fn = RELU)
  # 10 output units (activation fn = sigmoidal/softmax)
  # Regularization: dropout method (40% b/w 1st & 2nd layer; 30% b/w 2nd & output layer)

# Defining the model (i.e. organizing the layers)
model1 <- keras_model_sequential() 
model1 %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% #input layer plus first hidden layer
  layer_dropout(rate = 0.4) %>% #dropout layer (to prevent overfitting, randomly dropping 40% of the weights)
  layer_dense(units = 128, activation = 'relu') %>%  #second hidden layer
  layer_dropout(rate = 0.3) %>% #dropout layer
  layer_dense(units = 10, activation = 'softmax') #output layer
summary(model1) # just to print the details of the model

# Compiling the model  
model1 %>% compile(
  loss = 'categorical_crossentropy', # cross-entropy loss function (suitable for multi-classification) 
  optimizer = optimizer_sgd(), # stochastic gradient descent algorithm to estimate weights
  metrics = 'accuracy' # list of metrics to be evaluated by the model during training and testing
)

# Training the model
  # At the end of each epoch, compute accuracy on training and validation data.
history1 <- model1 %>% fit(
    x_train, y_train, # using training data
    epochs = 30, batch_size = 128, # 30 passes using batches of 128 images to estimate the gradient
    validation_split = 0.2 # leaving aside 20% of the training data as validation data (mimicking out of sample)
) 

# Evaluating the model's performance on test set
  # accuracy: frequency of correct classification
  # loss: cross-entropy (negative log-likelihood)
score1 <- model1 %>% evaluate(x_test, y_test)
print(score1)

# Now, instead of the 'dropout' method, let's do the L1 regularization (using penalty parameter=0.01)
model2 <- keras_model_sequential() 
model2 %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784), kernel_regularizer = regularizer_l1(l = 0.01)) %>% 
  layer_dense(units = 128, activation = 'relu', kernel_regularizer = regularizer_l1(l = 0.01)) %>% 
  layer_dense(units = 10, activation = 'softmax')

model2 %>% compile(loss = 'categorical_crossentropy',optimizer = optimizer_sgd(),metrics = 'accuracy')
history2 <- model2 %>% fit(x_train, y_train, epochs = 30, batch_size = 128, validation_split = 0.2) 
score2 <- model2 %>% evaluate(x_test, y_test)
print(score2)

# Now, L2 regularization (using penalty parameter=0.01)
model3 <- keras_model_sequential() 
model3 %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784), kernel_regularizer = regularizer_l2(l = 0.01)) %>% 
  layer_dense(units = 128, activation = 'relu', kernel_regularizer = regularizer_l2(l = 0.01)) %>% 
  layer_dense(units = 10, activation = 'softmax')

model3 %>% compile(loss = 'categorical_crossentropy',optimizer = optimizer_sgd(),metrics = 'accuracy')
history3 <- model3 %>% fit(x_train, y_train, epochs = 30, batch_size = 128, validation_split = 0.2) 
score3 <- model3 %>% evaluate(x_test, y_test)
print(score3)

# Compare performance on test data and training history
print(c(score1,score2,score3))

pdf('tex_raw/train_history_compare.pdf', width=21, height=7)
grid.arrange(
  plot(history1)+ggtitle("Dropout method"),
  plot(history2)+ggtitle("L1 regularization"),
  plot(history3)+ggtitle("L2 regularization"),
  ncol=3
  )
dev.off()

